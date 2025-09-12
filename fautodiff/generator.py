from __future__ import annotations

# Suffix used for all AD related names. Change here if a different suffix is
# desired throughout the project.
AD_SUFFIX = "_ad"
FWD_SUFFIX = "_fwd_ad"
REV_SUFFIX = "_rev_ad"

import re
import sys
from pathlib import Path
from typing import Any, Dict, List, Optional, Set, Tuple, Union

# Ensure other modules use the same AD suffix
from . import code_tree as code_tree
from . import fadmod
from . import operators as operators
from .code_tree import (
    Allocate,
    Assignment,
    Block,
    BlockConstruct,
    CallStatement,
    ClearAssignment,
    Deallocate,
    Declaration,
    DoAbst,
    DoLoop,
    DoWhile,
    ForallBlock,
    Function,
    IfBlock,
    Interface,
    Module,
    Node,
    OmpDirective,
    PointerAssignment,
    PointerClear,
    PreprocessorLine,
    Program,
    PushPop,
    ReturnStmt,
    Routine,
    SaveAssignment,
    SelectBlock,
    Statement,
    Subroutine,
    TypeDef,
    Use,
    WhereBlock,
    render_program,
)
from .operators import (
    AryIndex,
    Kind,
    Operator,
    OpComplex,
    OpFunc,
    OpInt,
    OpNot,
    OpRange,
    OpReal,
    OpTrue,
    OpVar,
    VarType,
)

code_tree.AD_SUFFIX = AD_SUFFIX
code_tree.FWD_SUFFIX = FWD_SUFFIX
code_tree.REV_SUFFIX = REV_SUFFIX
operators.AD_SUFFIX = AD_SUFFIX

from . import parser
from .var_list import VarList


def _warn(
    warnings: Optional[List[str]],
    info: Optional[Dict[str, Any]],
    code: str,
    reason: str,
) -> None:
    """Append a formatted warning message to ``warnings`` list."""
    if warnings is not None and info is not None:
        filename = info.get("file", "<unknown>")
        line = info.get("line", "?")
        msg = f"{filename}:{line}: {code} - {reason}"
        warnings.append(msg)


def _contains_pushpop(node: Node) -> bool:
    """Return ``True`` if ``node`` or any child is a ``PushPop``."""
    if isinstance(node, PushPop):
        return True
    for child in getattr(node, "iter_children", lambda: [])():
        if _contains_pushpop(child):
            return True
    return False


def _strip_sequential_omp(
    node: Node, warnings: Optional[List[str]], *, reverse: bool
) -> Node:
    """Remove OpenMP directives for loops with modified indices in reverse mode."""

    if isinstance(node, Block):
        new_children = []
        for child in list(node.iter_children()):
            new_child = _strip_sequential_omp(child, warnings, reverse=reverse)
            if isinstance(new_child, Block):
                new_children.extend(list(new_child.iter_children()))
            else:
                new_children.append(new_child)
        node._children = new_children
        return node

    if isinstance(node, OmpDirective):
        body = node.body
        if body is not None:
            body = _strip_sequential_omp(body, warnings, reverse=reverse)
            check_body = body
            if isinstance(body, Block):
                children = list(body.iter_children())
                if len(children) == 1 and isinstance(children[0], DoLoop):
                    check_body = children[0]
            if reverse:
                dir_norm = node.directive.split("(")[0].strip().lower()
                if isinstance(check_body, DoLoop):
                    if check_body.has_modified_indices():
                        if warnings is not None:
                            warnings.append(
                                "Dropped OpenMP directive: loop runs sequentially in reverse mode due to index dependency",
                            )
                        return check_body
                    node.body = body
                    return node
                if "workshare" in dir_norm:
                    if isinstance(body, Block):
                        if body.recurrent_vars() or body.conflict_vars():
                            if warnings is not None:
                                warnings.append(
                                    "Dropped OpenMP directive: workshare runs sequentially in reverse mode due to dependency",
                                )
                            return body
                        node.body = body
                        return node
                    return body
                return body
            node.body = body
            return node
        return Block([]) if reverse else node

    for child in list(getattr(node, "iter_children", lambda: [])()):
        _strip_sequential_omp(child, warnings, reverse=reverse)
    return node


def _add_fwd_rev_calls(
    node: Node,
    routine_map: Dict[str, Any],
    generic_map: Dict[str, Any],
    donot_prune: bool = False,
) -> None:
    """Add *_fwd_rev_ad wrapper routine before the call when available."""
    if isinstance(node, CallStatement):
        arg_info = Node.get_arg_info(node, routine_map, generic_map)
        if arg_info is not None and arg_info.get("name_fwd_rev_ad"):
            # print(node.name)
            args = node.args
            if node.result is not None:
                args = args.copy()
                args.append(node.result)
            args = CallStatement.rename_args(
                args, arg_info["args"], arg_info["args_fwd_rev_ad"]
            )
            if node.associated_vars is None:
                associated_vars = None
            else:
                associated_vars = []
                for var in node.associated_vars:
                    associated_vars.append(var.add_suffix(AD_SUFFIX))
            call = CallStatement(
                name=arg_info["name_fwd_rev_ad"],
                args=args,
                intents=arg_info["intents_fwd_rev_ad"],
                associated_vars=associated_vars,
                donot_prune=True,
            )
            node.parent.insert_before(node.get_id(), call)
            # print(node.parent.get_id())
            return
    for child in list(getattr(node, "iter_children", lambda: [])()):
        _add_fwd_rev_calls(child, routine_map, generic_map, donot_prune)


def _module_var_fwd_rev(
    routine_org: Routine,
    subroutine: Subroutine,
    mod_vars: List[OpVar],
    assigned: VarList,
    required: VarList,
) -> Optional[Subroutine]:
    """Return a forward wrapper for module variables and pointers.

    The wrapper stores required module variable values to the stack before
    executing ``routine_org`` and reloads them in reverse mode.  This enables
    reverse differentiation when a routine reads from and later writes to
    module level state.
    """

    # save module variables
    common = assigned & required
    mod_names = [v.name_ext() for v in mod_vars]
    cross_vars = []
    for var in common:
        if var.is_module_var(mod_names):
            name = var.name_ext()
            cross_var = next((v for v in mod_vars if v.name_ext() == name), None)
            if cross_var is not None and cross_var not in cross_vars:
                cross_vars.append(cross_var)
    cross_vars = sorted(cross_vars, key=lambda v: v.name_ext())
    args: List[str] = []
    sub_fwd_rev: Optional[Subroutine] = None
    index: List[OpVar] = []
    for var in cross_vars:
        block_fwd = Block([])
        block_rev = Block([])
        node_fwd = block_fwd
        node_rev = block_rev
        var_org = var.deep_clone().change_index(None)
        var = var_org
        var_ref = var.ref_var
        while var_ref:
            idx = []
            range = []
            if var_ref.dims is not None:
                for n, dim in enumerate(var_ref.dims):
                    idx.append(
                        OpVar(f"n{len(index)+n}_ad", var_type=VarType("integer"))
                    )
                    if dim is None:
                        vv = var_ref.change_index(None)
                        i0 = OpFunc("lbound", args=[vv, OpInt(n + 1)])
                        i1 = OpFunc("ubound", args=[vv, OpInt(n + 1)])
                    elif isinstance(dim, OpRange):
                        vv = var_ref.change_index(None)
                        i0 = OpFunc("lbound", args=[vv, OpInt(n + 1)]) if d[0] is None else d[0]
                        i1 = OpFunc("ubound", args=[vv, OpInt(n + 1)]) if d[1] is None else d[1]
                    elif isinstance(dim, Operator):
                        i0 = OpInt(1)
                        i1 = dim
                    else:
                        raise RuntimeError(f"Unexpected value type: {type(dim)}")
                    range.append(OpRange([i0, i1]))
            if var_ref.index is not None:
                for n, i in enumerate(var_ref.index):
                    if isinstance(i, OpVar):
                        idx[n] = i
            if idx:
                var_ref = var_ref.change_index(AryIndex(idx))
                var.ref_var = var_ref
                index.extend(idx)
                for i, r in zip(idx, range):
                    if not isinstance(node_fwd, Block):
                        node_fwd = Block([node_fwd])
                    if not isinstance(node_rev, Block):
                        node_rev = Block([node_rev])
                    node_fwd = DoLoop(node_fwd, i, r)
                    node_rev = DoLoop(node_rev, i, OpRange([r[1], r[0], OpInt(-1)]))
            var = var_ref
            var_ref = var.ref_var
        pushpop = PushPop(var_org, subroutine.get_id(), pointer=var_org.pointer)
        block_fwd.append(pushpop)
        block_rev.append(pushpop.to_load())
        if node_fwd is block_fwd:
            node_fwd = block_fwd[0]
        if node_rev is block_rev:
            node_rev = block_rev[0]
        if sub_fwd_rev is None:
            sub_fwd_rev = Subroutine(f"{routine_org.name}_fwd_rev_ad", args)
            sub_fwd_rev.ad_content = Block([])
        sub_fwd_rev.content.append(node_fwd)
        subroutine.content.insert_begin(node_rev)
    for idx in index:
        decl = Declaration(name=idx.name, var_type=VarType("integer"))
        if decl.name not in sub_fwd_rev.args:
            sub_fwd_rev.decls.append(decl)
        subroutine.decls.append(decl)

    # pointer variables
    def _search_pointer(
        node: Node,
        sub_fwd_rev: Optional[Subroutine],
        map: Optional[Dict[str, Tuple[OpVar, OpVar, bool]]] = None,
        allocated: Optional[Dict[str, bool]] = None,
    ) -> Optional[Subroutine]:
        """Locate pointer assignments and record their associations.

        Traverses ``node`` collecting pointer targets so that forward mode can
        save them on the stack and reverse mode can restore the original
        associations.
        """
        if map is None:
            map = {}
            allocated = {}
            top = True
        else:
            top = False
        for child in node.iter_children():
            sub_fwd_rev = _search_pointer(child, sub_fwd_rev, map, allocated)
        if isinstance(node, PointerAssignment):
            lhs = node.lhs
            rhs = node.rhs
            lhs_name = lhs.name_ext()
            if (
                lhs.pointer
                and lhs.is_module_var(mod_var_names)
                and isinstance(rhs, OpVar)
                and rhs.is_module_var(mod_var_names)
            ):
                rhs_name = rhs.name_ext()
                if rhs.allocatable and rhs_name not in allocated:
                    allocated[rhs_name] = True
                    al = True
                else:
                    al = False
                map[lhs_name] = (lhs, rhs, al)
        if top and map:
            if sub_fwd_rev is None:
                sub_fwd_rev = Subroutine(f"{routine_org.name}_fwd_rev_ad", args)
                sub_fwd_rev.ad_content = Block([])
            for lhs_name, value in map.items():
                lhs, rhs, allocate = value
                rhs_ad = rhs.add_suffix(AD_SUFFIX)
                if allocate:
                    v = rhs_ad.change_index(None)
                    cond = OpNot([OpFunc("allocated", [v])])
                    block = Block(
                        [
                            Allocate([v], mold=rhs),
                            ClearAssignment(v),
                        ]
                    )
                    sub_fwd_rev.ad_content.append(IfBlock([(cond, block)]))
                pa = PointerAssignment(lhs.add_suffix(AD_SUFFIX), rhs_ad)
                sub_fwd_rev.ad_content.append(pa)
        return sub_fwd_rev

    mod_var_names = [v.name for v in mod_vars]
    sub_fwd_rev = _search_pointer(routine_org.content, sub_fwd_rev)

    return sub_fwd_rev


def _parse_allocate(
    node: Node,
    mod_vars: List[OpVar],
    map: Optional[Dict[str, List[AryIndex | None]]] = None,
    local: Optional[Set[str]] = None,
) -> None:
    """Record indices of allocated arrays for later deallocation.

    The function walks ``node`` collecting indices used in ``Allocate`` and
    ``Deallocate`` statements so the same indices can be reused when
    generating the adjoint code.
    """
    if map is None:
        map: Dict[str, List[AryIndex | None]] = {}
        local = set()
        top = True
    else:
        top = False

    for child in list(node.iter_children()):
        _parse_allocate(child, mod_vars, map, local)

    if isinstance(node, Allocate):
        for var in node.vars:
            if var.is_constant:
                continue
            name = var.name_ext()
            if name not in map:
                map[name] = []
            map[name].append(var.index)
            root = var
            while root.ref_var is not None:
                root = root.ref_var
            if root.declared_in == "routine":
                local.add(name)
    elif isinstance(node, Deallocate):
        for var in node.vars:
            name = var.name_ext()
            if name in map and map[name]:
                node.index = map[name].pop()
            root = var
            while root.ref_var is not None:
                root = root.ref_var
            if root.declared_in == "routine":
                local.add(name)
    # Insert deallocations before early returns for arrays that remain allocated
    elif isinstance(node, ReturnStmt):
        mod_var_names = [v.name_ext() for v in mod_vars]
        for name in list(map.keys()):
            if map[name]:
                if name not in mod_var_names or name in local:
                    var = OpVar(
                        name,
                        index=map[name][0],
                        allocatable=True,
                        ad_target=True,
                    )
                    node.parent.insert_before(
                        node.get_id(),
                        Allocate._add_if(Deallocate([var]), var, name in mod_var_names),
                    )
                    del map[name]
    else:
        for var in node.collect_vars():
            name = var.name_ext()
            if name in map and map[name]:
                index = map[name][-1]
                ndims = len(index)
                dims_new: List[Operator | None] = []
                dims_raw_new: List[str | None] = list(var.dims_raw) if var.dims_raw is not None else []
                for n in range(ndims):
                    if var.dims is not None and var.dims[n] is not None:
                        dims_new.append(var.dims[n])
                    else:
                        idx = index[n] if index is not None else None
                        if isinstance(idx, OpRange):
                            dims_new.append(idx[1] if idx[0] == OpInt(1) else (OpRange([idx[0], idx[1]]) if idx[0] is not None and idx[1] is not None else idx))
                        else:
                            dims_new.append(idx)
                        dims_raw_new.append(str(dims_new[-1]))
                var.dims = tuple(dims_new)
                var.dims_raw = tuple(dims_raw_new)

    if top:
        if not isinstance(node, Block):
            raise RuntimeError(f"Top node must be Block: {type(node)}")
        if len(node) > 1 and isinstance(node[-1], ReturnStmt):
          node.remove_child(node[-1])

        mod_var_names = [v.name_ext() for v in mod_vars]
        for name in map:
            if map[name]:
                if name not in mod_var_names or name in local:
                    # Mark automatically deallocated arrays as adjoint targets so
                    # their corresponding `_ad` arrays are allocated in the
                    # reverse pass.
                    var = OpVar(
                        name,
                        index=map[name][0],
                        allocatable=True,
                        ad_target=True,
                    )
                    node.append(
                        Allocate._add_if(Deallocate([var]), var, name in mod_var_names)
                    )



def _parse_pointer(
    node: Node,
    mod_vars: List[OpVar],
    map_ptr: Optional[Dict[str, OpVar]] = None,
    map_ref: Optional[Dict[str, Optional[OpVar]]] = None,
    inloop: bool = False,
) -> Tuple[Dict[str, OpVar], Dict[str, Optional[OpVar]]]:
    """Insert ``PointerClear`` nodes and track pointer associations.

    ``map_ptr`` keeps the latest pointer variables while ``map_ref`` remembers
    their targets.  This information is required when restoring pointer state
    in the generated reverse code.
    """
    if map_ptr is None:
        if not isinstance(node, Block):
            raise ValueError(f"node must be Block: {type(node)}")
        map_ptr = {}
        map_ref = {}
        top = True
    else:
        top = False
    if isinstance(node, SelectBlock):
        for child in node.iter_children():
            map_ptr, map_ref_new = _parse_pointer(
                child, mod_vars, map_ptr, map_ref.copy(), inloop
            )
            for key, value in map_ref.items():
                if value != map_ref_new[key]:  # pointer is updated in the branch
                    map_ref[key] = None
    elif isinstance(node, DoAbst):
        map_ptr, map_ref_new = _parse_pointer(node._body, mod_vars, map_ptr, {}, True)
        for key in map_ref_new:  # pointer is updated in the loop
            map_ref[key] = None
    elif isinstance(node, ForallBlock):
        map_ptr, map_ref_new = _parse_pointer(node._body, mod_vars, map_ptr, {}, True)
        for key in map_ref_new:
            map_ref[key] = None
    else:
        for child in [n for n in node.iter_children()]:
            map_ptr, map_ref = _parse_pointer(child, mod_vars, map_ptr, map_ref, inloop)
    if isinstance(node, PointerAssignment):
        lhs_name = node.lhs.name_ext()
        rhs = node.rhs
        map_ptr[lhs_name] = node.lhs
        if lhs_name in map_ref:
            node.parent.insert_before(
                node.get_id(),
                PointerClear(node.lhs, previous=map_ref[lhs_name], info=node.info),
            )
        else:
            node.parent.insert_before(
                node.get_id(), PointerClear(node.lhs, previous=None, info=node.info)
            )
        if isinstance(rhs, OpVar):
            rhs_name = rhs.name_ext()
            if rhs_name in map_ref:
                map_ref[lhs_name] = map_ref[rhs_name]
            else:
                map_ref[lhs_name] = rhs
        elif isinstance(rhs, OpFunc) and rhs.name == "null":
            map_ref[lhs_name] = None
    if isinstance(node, Deallocate):
        for var in node.vars:
            name = var.name_ext()
            for ptr_name, tgt in map_ref.items():
                if isinstance(tgt, OpVar) and name == tgt.name_ext():
                    node.parent.insert_before(
                        node.get_id(), PointerClear(map_ptr[ptr_name], previous=tgt)
                    )
                    map_ref[ptr_name] = None
    if top:
        mod_var_names = [v.name for v in mod_vars]
        for name, var in map_ptr.items():
            if var.is_module_var(mod_var_names, check_ad=True):
                continue
            node.append(PointerClear(var, previous=map_ref[name]))

    return (map_ptr, map_ref)


def _parse_mpi_calls(
    node: Node, calls: Optional[Dict[str, List[CallStatement]]] = None
) -> None:
    """Collect MPI calls that should not be pruned.

    MPI routines may have side effects even if their results are unused.  The
    collected calls are marked to remain in the AD output and their buffers
    are associated when necessary.
    """
    if calls is None:
        top = True
        calls = {}
    else:
        top = False
    if isinstance(node, CallStatement):
        name = node.name.lower()
        if name.startswith("mpi"):
            if name in {"mpi_send", "mpi_isend"}:
                if not "send" in calls:
                    calls["send"] = []
                calls["send"].append(node)
            if name in {"mpi_recv", "mpi_irecv"}:
                if not "recv" in calls:
                    calls["recv"] = []
                calls["recv"].append(node)
            if name in {"mpi_start", "mpi_startall"}:
                if not "start" in calls:
                    calls["start"] = []
                calls["start"].append(node)
            if name in {"mpi_wait", "mpi_waitall"}:
                if not "wait" in calls:
                    calls["wait"] = []
                calls["wait"].append(node)
    else:
        for child in node.iter_children():
            _parse_mpi_calls(child, calls)

    if top:
        # Top-level call: mark certain MPI routines to avoid pruning and
        # associate buffer variables so that the generated code preserves
        # communication semantics.
        if "start" in calls:
            for key in ("start", "wait"):
                if key in calls:
                    for node in calls[key]:
                        node.donot_prune = True
        if "recv" in calls:
            vars = [node.args[0] for node in calls["recv"]]
            for key in ("send", "recv", "wait"):
                if key in calls:
                    for node in calls[key]:
                        if node.associated_vars is None:
                            node.associated_vars = vars


def _mark_mpi_rev_calls_donot_prune(node: Node) -> None:
    """Mark reverse-mode MPI calls as non-prunable.

    Reverse-mode MPI wrappers have hidden side effects (request wait/free,
    internal maps update, gradient accumulation). Ensure DCE never drops them
    by setting ``donot_prune=True``.
    """
    if isinstance(node, CallStatement):
        name = node.name.lower()
        # Focus on nonblocking and their finalizers in reverse mode
        keep_names = {
            "mpi_isend_rev_ad",
            "mpi_irecv_rev_ad",
            "mpi_wait_rev_ad",
            "mpi_waitall_rev_ad",
            "mpi_start_rev_ad",
            "mpi_startall_rev_ad",
            "mpi_send_init_rev_ad",
            "mpi_recv_init_rev_ad",
        }
        if name in keep_names:
            node.donot_prune = True
    for child in getattr(node, "iter_children", lambda: [])():
        _mark_mpi_rev_calls_donot_prune(child)


def _set_call_intents(
    node: Node, routine_map: Dict[str, Any], generic_map: Dict[str, Any]
) -> None:
    """Populate ``CallStatement.intents`` using ``routine_map`` recursively."""
    if isinstance(node, CallStatement):
        arg_info = Node.get_arg_info(node, routine_map, generic_map)
        if arg_info is not None and "intents" in arg_info:
            intents = list(arg_info["intents"])
            params = list(arg_info["args"])
            if node.result is not None:
                params_no_res = params[:-1]
            else:
                params_no_res = params
            used = [False] * len(params_no_res)
            pos = 0
            reordered = []
            for key in node.arg_keys:
                if key is None:
                    while pos < len(params_no_res) and used[pos]:
                        pos += 1
                    if pos < len(params_no_res):
                        idx = pos
                        used[idx] = True
                        pos += 1
                    else:
                        idx = None
                else:
                    idx = params_no_res.index(key)
                    used[idx] = True
                if idx is not None:
                    reordered.append(intents[idx])
                else:
                    reordered.append(None)
            if node.result is not None:
                reordered.append(intents[-1])
            node.intents = reordered
    for child in getattr(node, "iter_children", lambda: [])():
        _set_call_intents(child, routine_map, generic_map)


def _collect_calls(node: Node, calls: Optional[set] = None) -> set:
    """Return set of routine names called within ``node``."""

    if calls is None:
        calls = set()
    if isinstance(node, CallStatement):
        calls.add(node.name)

    def _scan(obj):
        if hasattr(obj, "find_userfunc"):
            for ufunc in obj.find_userfunc():
                calls.add(ufunc.name)
        elif isinstance(obj, (list, tuple)):
            for item in obj:
                _scan(item)

    for attr in vars(node).values():
        _scan(attr)

    for child in getattr(node, "iter_children", lambda: [])():
        _collect_calls(child, calls)
    return calls


def _dependency_graph(routines: List[Routine]) -> Dict[str, set]:
    """Return dependency graph mapping routine names to called routine names."""

    lookup = {r.name: r for r in routines}
    deps: Dict[str, set] = {}
    for name, r in lookup.items():
        calls = {c for c in _collect_calls(r.content) if c in lookup}
        deps[name] = calls
    return deps


def _dependency_groups(
    routines: List[Routine],
) -> tuple[List[List[str]], Dict[str, set]]:
    """Return SCCs of routines and their dependency graph.

    Returns
    -------
    (groups, deps)
        ``groups`` is a list of strongly connected components in dependency
        order. ``deps`` maps routine names to the set of routines they call.
    """

    deps = _dependency_graph(routines)
    index = 0
    stack: List[str] = []
    indices: Dict[str, int] = {}
    lowlink: Dict[str, int] = {}
    comps: List[List[str]] = []

    def strongconnect(v: str) -> None:
        nonlocal index
        indices[v] = index
        lowlink[v] = index
        index += 1
        stack.append(v)
        for w in deps[v]:
            if w not in indices:
                strongconnect(w)
                lowlink[v] = min(lowlink[v], lowlink[w])
            elif w in stack:
                lowlink[v] = min(lowlink[v], indices[w])
        if lowlink[v] == indices[v]:
            comp: List[str] = []
            while True:
                w = stack.pop()
                comp.append(w)
                if w == v:
                    break
            comps.append(comp)

    for v in deps:
        if v not in indices:
            strongconnect(v)

    return comps, deps


def _has_modified_module_grad_var(routine_org: Routine, mod_vars: List[OpVar]) -> bool:
    """Return True if ``routine_org`` assigns to a module gradient variable."""

    grad_names = set()
    target_names = set()
    for var in mod_vars:
        if var.ad_target:
            if var.name.endswith(AD_SUFFIX):
                grad_names.add(var.name)
                target_names.add(var.name[: -len(AD_SUFFIX)])
            else:
                grad_names.add(f"{var.name}{AD_SUFFIX}")
                target_names.add(var.name)
    if not grad_names and not target_names:
        return False

    def _scan(node: Node) -> bool:
        if isinstance(node, (Allocate, Deallocate, PointerClear)):
            vars = getattr(node, "vars", None)
            if vars is None and hasattr(node, "var"):
                vars = [node.var]
            if vars is not None:
                for v in vars:
                    root = v
                    while isinstance(root, OpVar) and root.ref_var is not None:
                        root = root.ref_var
                    if root.declared_in != "routine" and (
                        root.name in grad_names or root.name in target_names
                    ):
                        return True
        for v in node.iter_assign_vars():
            root = v
            while isinstance(root, OpVar) and root.ref_var is not None:
                root = root.ref_var
            if root.declared_in != "routine" and (
                root.name in grad_names or root.name in target_names
            ):
                return True
        for child in node.iter_children():
            if _scan(child):
                return True
        return False

    return _scan(routine_org.content)


def _prepare_fwd_ad_header(
    routine_org: Routine, has_mod_grad_var: bool
) -> Dict[str, Any]:
    """Create forward-mode AD subroutine header and argument info.

    The function duplicates the original arguments, appends gradient
    counterparts when required and returns both the created ``Subroutine``
    and metadata describing these arguments.
    """
    args = []
    grad_args = []
    in_grad_args = []
    out_grad_args = []
    has_grad_input = False

    arg_info = {
        "args": [],
        "intents": [],
        "dims": [],
        "type": [],
        "kind": [],
        "name_fwd_ad": None,
        "args_fwd_ad": [],
        "intents_fwd_ad": [],
    }

    for arg in routine_org.arg_vars():
        name = arg.name
        typ = arg.var_type.typename
        intent = arg.intent or "inout"
        var_type = arg.var_type
        kind = var_type.kind
        arg_info["args"].append(name)
        arg_info["intents"].append(intent)
        arg_info["type"].append(typ)
        arg_info["dims"].append(arg.dims_raw)
        arg_info["kind"].append(kind.val if kind is not None else None)

        # Always pass original argument to the forward AD routine
        args.append(arg)

        if arg.ad_target and not arg.is_constant:
            ad_name = f"{name}{AD_SUFFIX}"
            var = OpVar(
                ad_name,
                var_type=var_type,
                dims=arg.dims,
                intent=arg.intent,
                ad_target=True,
                is_constant=arg.is_constant,
                allocatable=arg.allocatable,
                pointer=arg.pointer,
                target=arg.target,
                save=arg.save,
                value=arg.value,
                volatile=arg.volatile,
                asynchronous=arg.asynchronous,
            )
            args.append(var)
            grad_args.append(var)
            if intent in ("in", "inout"):
                has_grad_input = True
                in_grad_args.append(var)
            if intent in ("out", "inout"):
                out_grad_args.append(var)

    ad_name = f"{routine_org.name}{FWD_SUFFIX}"
    subroutine = Subroutine(ad_name, [v.name for v in args])
    # Preserve preprocessor lines from the original routine declarations
    decl_children = list(routine_org.decls.iter_children())
    for node in decl_children:
        if isinstance(node, PreprocessorLine):
            subroutine.decls.append(node.copy())
    arg_info["name_fwd_ad"] = ad_name
    # Copy use statements from the original routine
    for node in decl_children:
        if isinstance(node, Use):
            subroutine.decls.append(node.deep_clone())
            if not node.name.endswith(AD_SUFFIX):
                subroutine.decls.append(Use(f"{node.name}{AD_SUFFIX}"))

    # Copy parameter declarations from the original routine
    for node in decl_children:
        if (
            isinstance(node, Declaration)
            and node.parameter
            and node.name not in arg_info["args"]
        ):
            clone = node.deep_clone()
            clone.donot_prune = True
            subroutine.decls.append(clone)

    for var in args:
        subroutine.decls.append(
            Declaration(
                name=var.name,
                var_type=var.var_type.copy(),
                dims=var.dims,
                intent=var.intent,
                allocatable=var.allocatable,
                pointer=var.pointer,
                optional=var.optional,
                target=var.target,
                save=getattr(var, "save", False),
                value=getattr(var, "value", False),
                volatile=getattr(var, "volatile", False),
                asynchronous=getattr(var, "asynchronous", False),
                declared_in="routine",
            )
        )
        arg_info["args_fwd_ad"].append(var.name)
        arg_info["intents_fwd_ad"].append(var.intent)

    subroutine.ad_init = Block([])
    subroutine.ad_content = Block([])

    skip = bool(routine_org.directives.get("SKIP")) or (
        len(out_grad_args) == 0 and not has_mod_grad_var
    )
    if skip:
        arg_info["name_fwd_ad"] = None

    return {
        "subroutine": subroutine,
        "grad_args": grad_args,
        "in_grad_args": in_grad_args,
        "out_grad_args": out_grad_args,
        "has_grad_input": has_grad_input,
        "arg_info": arg_info,
        "skip": skip,
    }


def _prepare_rev_ad_header(
    routine_org: Routine, has_mod_grad_var: bool
) -> Dict[str, Any]:
    """Prepare AD subroutine header and return argument info."""

    args = []
    grad_args = []
    in_grad_args = []
    out_grad_args = []
    has_grad_input = False

    arg_info = {
        "args": [],
        "intents": [],
        "dims": [],
        "type": [],
        "kind": [],
        "name_rev_ad": None,
        "args_rev_ad": [],
        "intents_rev_ad": [],
    }

    for arg in routine_org.arg_vars():
        name = arg.name
        typ = arg.var_type.typename
        intent = arg.intent or "inout"
        var_type = arg.var_type
        kind = var_type.kind
        arg_info["args"].append(name)
        arg_info["intents"].append(intent)
        arg_info["type"].append(typ)
        arg_info["dims"].append(arg.dims_raw)
        arg_info["kind"].append(kind.val if kind is not None else None)
        if intent == "out":
            if arg.ad_target and not arg.is_constant:
                ad_name = f"{name}{AD_SUFFIX}"
                var = OpVar(
                    ad_name,
                    var_type=var_type,
                    dims=arg.dims,
                    intent="inout",
                    ad_target=True,
                    is_constant=arg.is_constant,
                    allocatable=arg.allocatable,
                    pointer=arg.pointer,
                    target=arg.target,
                    save=arg.save,
                    value=arg.value,
                    volatile=arg.volatile,
                    asynchronous=arg.asynchronous,
                )
                args.append(var)
                grad_args.append(var)
                in_grad_args.append(var)
                out_grad_args.append(var)
                has_grad_input = True
        else:
            args.append(arg)
            if arg.ad_target and not arg.is_constant:
                ad_name = f"{name}{AD_SUFFIX}"
                var = OpVar(
                    ad_name,
                    var_type=var_type,
                    dims=arg.dims,
                    intent="inout",
                    ad_target=True,
                    is_constant=arg.is_constant,
                    allocatable=arg.allocatable,
                    pointer=arg.pointer,
                    target=arg.target,
                    save=arg.save,
                    value=arg.value,
                    volatile=arg.volatile,
                    asynchronous=arg.asynchronous,
                )
                args.append(var)
                grad_args.append(var)
                out_grad_args.append(var)
                in_grad_args.append(var)
                has_grad_input = True

    ad_name = f"{routine_org.name}{REV_SUFFIX}"
    subroutine = Subroutine(ad_name, [v.name for v in args])
    # Preserve preprocessor lines from the original routine declarations
    decl_children = list(routine_org.decls.iter_children())
    for node in decl_children:
        if isinstance(node, PreprocessorLine):
            subroutine.decls.append(node.copy())
    arg_info["name_rev_ad"] = ad_name
    # Copy use statements from the original routine
    for node in decl_children:
        if isinstance(node, Use):
            subroutine.decls.append(node.deep_clone())
            if not node.name.endswith(AD_SUFFIX):
                subroutine.decls.append(Use(f"{node.name}{AD_SUFFIX}"))

    # Copy parameter declarations from the original routine
    for node in decl_children:
        if (
            isinstance(node, Declaration)
            and node.parameter
            and node.name not in arg_info["args"]
        ):
            clone = node.deep_clone()
            clone.donot_prune = True
            subroutine.decls.append(clone)

    for var in args:
        subroutine.decls.append(
            Declaration(
                name=var.name,
                var_type=var.var_type.copy(),
                dims=var.dims,
                intent=var.intent,
                allocatable=var.allocatable,
                pointer=var.pointer,
                optional=var.optional,
                target=var.target,
                save=getattr(var, "save", False),
                value=getattr(var, "value", False),
                volatile=getattr(var, "volatile", False),
                asynchronous=getattr(var, "asynchronous", False),
                declared_in="routine",
            )
        )
        arg_info["args_rev_ad"].append(var.name)
        arg_info["intents_rev_ad"].append(var.intent)

    subroutine.ad_init = Block([])
    subroutine.ad_content = Block([])
    subroutine.ad_init.set_parent(subroutine)
    subroutine.ad_content.set_parent(subroutine)

    skip = bool(routine_org.directives.get("SKIP")) or (
        len(out_grad_args) == 0 and not has_mod_grad_var
    )
    if skip:
        arg_info["name_rev_ad"] = None

    return {
        "subroutine": subroutine,
        "grad_args": grad_args,
        "in_grad_args": in_grad_args,
        "out_grad_args": out_grad_args,
        "has_grad_input": has_grad_input,
        "arg_info": arg_info,
        "skip": skip,
    }


def _collect_called_ad_modules(
    blocks: List[Block],
    routine_map: Dict[str, Dict[str, Any]],
    reverse: bool,
) -> Set[str]:
    """Return a set of modules whose AD routines are called within ``blocks``."""

    modules = set()

    def _visit(node):
        if isinstance(node, CallStatement):
            name_key = "name_rev_ad" if reverse else "name_fwd_ad"
            for info in routine_map.values():
                if info.get(name_key) == node.name and "module" in info:
                    modules.add(info["module"])
                    break
        for child in getattr(node, "iter_children", lambda: [])():
            _visit(child)

    for block in blocks:
        for stmt in block:
            _visit(stmt)
    return modules


def _generate_ad_subroutine(
    mod_org: Module,
    routine_org: Routine,
    routine_map: Dict[str, Any],
    generic_map: Dict[str, Any],
    mod_vars: List[OpVar],
    routine_info: Dict[str, Any],
    warnings: List[str],
    const_var_names: List[str],
    *,
    reverse: bool,
) -> tuple[Optional[Subroutine], bool, Set[str]]:
    """Generate forward or reverse AD subroutine.

    Returns the generated ``Subroutine`` (or ``None`` when skipped), a flag
    indicating whether stack push/pop operations are required and the set of
    modules whose AD wrappers are invoked.
    """

    if routine_info.get("skip"):
        return None, False, set()

    subroutine = routine_info["subroutine"].deep_clone()
    grad_args = [v.deep_clone() for v in routine_info["grad_args"]]
    in_grad_args = [v.deep_clone() for v in routine_info["in_grad_args"]]
    out_grad_args = [v.deep_clone() for v in routine_info["out_grad_args"]]
    has_grad_input = routine_info["has_grad_input"]
    # list of original intent(out) arguments
    out_args = [
        var
        for var in routine_org.arg_vars()
        if (var.intent is None or var.intent in ("out", "inout"))
    ]
    ad_block = subroutine.ad_content

    sub_fwd_rev: Optional[Subroutine] = None

    # Collect gradient variables originating from module scope
    has_mod_grad_input = False
    mod_ad_vars = []
    for var in mod_vars:
        if var.ad_target:
            mod_ad_vars.append(var.add_suffix(AD_SUFFIX))
            has_mod_grad_input = True
    mod_var_names = [v.name for v in mod_vars]

    # SAVE variables behave like module variables and may carry gradients
    save_vars = []
    save_ad_vars = []
    has_save_grad_input = False
    for var in routine_org.decls.collect_vars():
        if var.save:
            save_vars.append(var)
            if var.ad_target:
                save_ad_vars.append(var.add_suffix(AD_SUFFIX))
                has_save_grad_input = True
    if (
        reverse
        and not has_grad_input
        and not has_mod_grad_input
        and not has_save_grad_input
    ):
        for arg in out_grad_args:
            kind = arg.var_type.kind
            if kind is None and arg.var_type.typename.lower() == "double precision":
                kind = Kind(OpInt(8), val=8, use_kind_keyword=False)
            lhs = OpVar(
                arg.name,
                kind=kind,
            )
            if arg.is_complex_type:
                zero = OpComplex(
                    OpReal("0.0", kind=kind),
                    OpReal("0.0", kind=kind),
                    kind=kind,
                )
            else:
                zero = OpReal("0.0", kind=kind)
            ad_block.append(Assignment(lhs, zero))
        subroutine.ad_content = ad_block
        return subroutine, False, set()

    saved_vars: List[OpVar] = []
    if reverse:
        assigned_advars = None
    else:
        assigned_advars = VarList(in_grad_args + mod_ad_vars + save_ad_vars)
    return_flags = [node.flag() for node in routine_org.content.collect_return()]
    # Generate AD statements from the original routine body
    if reverse:
        return_flags = [node.flag() for node in routine_org.content.collect_return()]
    else:
        return_flags = None

    # Collect potential pointer-target alias pairs for self-referential
    # assignment detection.
    ptr_pairs: Set[Tuple[str, str]] = set()

    def _collect_ptr_pairs(node: Node) -> None:
        if isinstance(node, PointerAssignment):
            ptr_pairs.add((node.lhs.name_ext(), node.rhs.name_ext()))
        for child in getattr(node, "iter_children", lambda: [])():
            _collect_ptr_pairs(child)

    _collect_ptr_pairs(routine_org.content)
    Assignment.pointer_alias_pairs = ptr_pairs

    nodes = routine_org.content.generate_ad(
        saved_vars,
        reverse=reverse,
        assigned_advars=assigned_advars,
        routine_map=routine_map,
        generic_map=generic_map,
        mod_vars=mod_vars,
        return_flags=return_flags,
        warnings=warnings,
    )
    ad_code = Block()
    for node in nodes:
        if not node.is_effectively_empty():
            ad_code.append(node)

    # print(subroutine.name)
    # print(render_program(ad_code)) # for debugging
    # print(render_program(routine_org))

    if not ad_code.is_effectively_empty():
        # Before any pruning, mark reverse MPI calls to avoid being dropped
        if reverse:
            _mark_mpi_rev_calls_donot_prune(ad_code)
        # Declare any temporary gradient variables introduced by AD code
        for var in ad_code.assigned_vars(without_savevar=True):
            name = var.name
            if name.endswith(AD_SUFFIX):
                if any(arg.name == name for arg in grad_args):
                    continue
                if var.is_module_var(mod_var_names, check_ad=True):
                    continue
                base_decl = routine_org.decls.find_by_name(name.removesuffix(AD_SUFFIX))
                if base_decl is not None and not subroutine.is_declared(name):
                    subroutine.decls.append(
                        Declaration(
                            name=name,
                            var_type=base_decl.var_type.copy(),
                            dims=base_decl.dims,
                            dims_raw=base_decl.dims_raw,
                            parameter=base_decl.parameter,
                            init_val=base_decl.init_val,
                            allocatable=base_decl.allocatable,
                            pointer=base_decl.pointer,
                            optional=base_decl.optional,
                            target=base_decl.target,
                            save=base_decl.save,
                            value=base_decl.value,
                            volatile=base_decl.volatile,
                            asynchronous=base_decl.asynchronous,
                            declared_in="routine",
                        )
                    )

        if reverse:
            targets = VarList(in_grad_args + mod_ad_vars + save_ad_vars)
            ad_code.check_initial(targets)

        if reverse:
            targets = VarList(grad_args + mod_ad_vars + save_ad_vars)
        else:
            targets = VarList(
                grad_args + out_args + mod_ad_vars + mod_vars + save_ad_vars + save_vars
            )
        # print(render_program(ad_code))

        # Remove statements unrelated to derivative targets
        ad_code = ad_code.prune_for(targets, mod_vars + save_vars, base_targets=targets)
        # print(render_program(ad_code))

        if reverse:
            ad_code.check_initial(VarList(grad_args + mod_ad_vars + save_ad_vars))
            ad_code = ad_code.prune_for(
                targets, mod_vars + save_vars, base_targets=targets
            )
            # print(render_program(ad_code))

    for node in ad_code:
        if not node.is_effectively_empty():
            ad_block.append(node)

    if reverse:
        _strip_sequential_omp(ad_block, warnings, reverse=True)
        targets = ad_block.required_vars()

        fw_block = Block(
            routine_org.content.set_for_returnexitcycle(
                return_flags, None, set_return_cond=True, set_exitcycle_cond=False
            )
        )
        if return_flags:
            for flag in return_flags:
                fw_block.insert_begin(Assignment(flag, OpTrue()))

    # For non-AD-target mirror variables (e.g. integer request arrays),
    # insert an initialization for the "_ad" variable mirroring the
    # original assignment site. We do this by locating the first
    # assignment to the original variable in the primal content and
    # inserting the mirrored assignment immediately before it.
    target_block = fw_block if reverse else ad_block
    target_block.build_parent()
    for var in ad_block.required_vars():
        if not var.name.endswith(AD_SUFFIX):
            continue
        org_name = var.name.removesuffix(AD_SUFFIX)
        decl = routine_org.get_var(org_name)
        if decl is not None and not decl.ad_target:

            def _insert_mirror_init(node: Node) -> bool:
                # Depth-first search for first matching assignment
                for ch in node.iter_children():
                    if isinstance(ch, Assignment) and ch.lhs.name == org_name:
                        assign = Assignment(ch.lhs.add_suffix(AD_SUFFIX), ch.rhs)
                        # Insert before in the parent block
                        ch.parent.insert_before(ch.get_id(), assign)
                        return True
                    if _insert_mirror_init(ch):
                        return True
                return False

            _insert_mirror_init(target_block)

    if reverse:
        _add_fwd_rev_calls(fw_block, routine_map, generic_map)

        fw_block = fw_block.prune_for(
            targets, mod_vars + save_vars, base_targets=targets
        )
        _strip_sequential_omp(fw_block, warnings, reverse=True)

        assigned = routine_org.content.assigned_vars(without_savevar=True)
        required = ad_block.required_vars(without_savevar=True)
        required = fw_block.required_vars(required, without_savevar=True)
        sub_fwd_rev = _module_var_fwd_rev(
            routine_org, subroutine, mod_vars, assigned, required
        )
        if sub_fwd_rev:
            name_org = routine_org.name
            name_fwd_rev = f"{name_org}_fwd_rev_ad"
            intents = [v.intent for v in sub_fwd_rev.arg_vars()]
            routine_map_new = {
                name_org: {
                    "args": routine_map[name_org]["args"],
                    "name_fwd_rev_ad": name_fwd_rev,
                    "args_fwd_rev_ad": sub_fwd_rev.args,
                    "intents_fwd_rev_ad": intents,
                }
            }
            routine_map[name_org]["name_fwd_rev_ad"] = name_fwd_rev
            routine_map[name_org]["args_fwd_rev_ad"] = sub_fwd_rev.args
            routine_map[name_org]["intents_fwd_rev_ad"] = intents

            _add_fwd_rev_calls(fw_block, routine_map_new, None, donot_prune=True)

        flag = True
        while flag:
            last = fw_block.last()
            first = ad_block.first()
            if (
                isinstance(last, SaveAssignment)
                and isinstance(first, SaveAssignment)
                and last.var.name == first.var.name
                and last.id == first.id
                and last.load != first.load
            ):
                fw_block.remove_child(last)
                ad_block.remove_child(first)
                continue
            if (
                isinstance(last, PointerAssignment)
                and isinstance(first, PointerAssignment)
                and last.lhs.name_ext() == first.rhs.name_ext()
                and last.rhs.name_ext() == first.lhs.name_ext()
            ):
                fw_block.remove_child(last)
                ad_block.remove_child(first)
                continue
            if isinstance(last, DoLoop) and isinstance(first, DoLoop):
                item1 = first._body[0]
                item2 = last._body[-1]
                flag = False
                while (
                    isinstance(item1, SaveAssignment)
                    and isinstance(item2, SaveAssignment)
                    and item1.pushpop == item2.pushpop
                    and item1.var.name == item2.var.name
                    and item1.id == item2.id
                    and item1.load != item2.load
                ):
                    first._body.remove_child(item1)
                    last._body.remove_child(item2)
                    flag = True
                    if (
                        first._body.is_effectively_empty()
                        or last._body.is_effectively_empty()
                    ):
                        break
                    item1 = first._body[0]
                    item2 = last._body[-1]
                if last.is_effectively_empty():
                    fw_block.remove_child(last)
                if first.is_effectively_empty():
                    ad_block.remove_child(first)
                continue
            flag = False
        fw_nodes = [n for n in fw_block.iter_children()]
        for i, node_fw in enumerate(fw_nodes):
            if isinstance(node_fw, SaveAssignment) and not node_fw.pushpop:
                var = node_fw.var
                if i < len(fw_nodes) - 1:
                    flag = False
                    for node in fw_nodes[i + 1 :]:
                        if node.has_assignment_to(var.name_ext()):
                            flag = True
                            break
                    if flag:
                        break
                for node_ad in ad_block.iter_children():
                    if (
                        isinstance(node_ad, SaveAssignment)
                        and node_ad.load
                        and node_fw.id == node_ad.id
                    ):
                        fw_block.remove_child(node_fw)
                        ad_block.remove_child(node_ad)
                        break
                    if node_ad.has_assignment_to(var.name_ext()):
                        break
        # print(render_program(ad_block))

        if not fw_block.is_effectively_empty():
            # Simple prune: drop redundant allocates left in the forward block
            # (e.g., a standalone loop/if that only allocates a variable which
            # is allocated again later inside the reverse block context).
            def _allocates_var(node: Node, vname: str) -> bool:
                # Check if this node (recursively) contains an Allocate of vname
                if isinstance(node, Allocate):
                    return any(var.name_ext() == vname for var in node.vars)
                if isinstance(node, IfBlock):
                    for _, body in node.cond_blocks:
                        for ch in body:
                            if _allocates_var(ch, vname):
                                return True
                    return False
                if isinstance(node, DoAbst):
                    for ch in node._body:
                        if _allocates_var(ch, vname):
                            return True
                    return False
                if isinstance(node, OmpDirective) and node.body is not None:
                    for ch in node.body:
                        if _allocates_var(ch, vname):
                            return True
                    return False
                for ch in getattr(node, "iter_children", lambda: [])():
                    if _allocates_var(ch, vname):
                        return True
                return False

            def _prune_fw_allocates_simple(
                block: Block, other: Optional[Block]
            ) -> Block:
                children = list(block)
                i = 0
                while i < len(children):
                    node = children[i]
                    # IfBlock whose branches only allocate; drop if later reallocated
                    if isinstance(node, IfBlock):
                        only_alloc = True
                        vnames: list[str] = []
                        for _, body in node.cond_blocks:
                            # empty bodies are fine
                            if len(body) == 0:
                                continue
                            if len(body) == 1 and isinstance(body[0], Allocate):
                                vnames.extend(v.name_ext() for v in body[0].vars)
                            else:
                                only_alloc = False
                                break
                        if only_alloc and vnames:
                            remove = False
                            for vname in vnames:
                                found = False
                                for later in children[i + 1 :]:
                                    if _allocates_var(later, vname):
                                        found = True
                                        break
                                if not found and other is not None:
                                    for ch in other:
                                        if _allocates_var(ch, vname):
                                            found = True
                                            break
                                if found:
                                    remove = True
                                    break
                            if remove:
                                children.pop(i)
                                continue
                    # IfBlock with a single Allocate in body
                    if isinstance(node, IfBlock) and len(node.cond_blocks) == 1:
                        body = node.cond_blocks[0][1]
                        if len(body) == 1 and isinstance(body[0], Allocate):
                            vnames = [v.name_ext() for v in body[0].vars]
                            remove = False
                            for vname in vnames:
                                found = False
                                for later in children[i + 1 :]:
                                    if _allocates_var(later, vname):
                                        found = True
                                        break
                                if not found and other is not None:
                                    for ch in other:
                                        if _allocates_var(ch, vname):
                                            found = True
                                            break
                                if found:
                                    remove = True
                                    break
                            if remove:
                                children.pop(i)
                                continue
                    # Do loop whose body only contains Allocate(s)
                    if isinstance(node, DoAbst):
                        body = node._body
                        if all(isinstance(ch, Allocate) for ch in body):
                            vnames = []
                            for ch in body:
                                vnames.extend(v.name_ext() for v in ch.vars)
                            remove = False
                            for vname in vnames:
                                found = False
                                for later in children[i + 1 :]:
                                    if _allocates_var(later, vname):
                                        found = True
                                        break
                                if not found and other is not None:
                                    for ch in other:
                                        if _allocates_var(ch, vname):
                                            found = True
                                            break
                                if found:
                                    remove = True
                                    break
                            if remove:
                                children.pop(i)
                                continue
                    i += 1
                return Block(children)

            fw_block = _prune_fw_allocates_simple(fw_block, ad_block)

            # In reverse mode, the forward block (fw_block) may reference
            # derivative arrays (e.g., z_ad) before the reverse AD logic
            # allocates them. Proactively allocate required AD arrays here
            # using sizes derived from the original forward allocate/deallocate
            # indices captured in ad_block.
            def _collect_dealloc_indices(node: Node) -> dict[str, Any]:
                idx_map: dict[str, Any] = {}
                # DFS over all nodes; capture deallocate indices for base vars
                if isinstance(node, Deallocate):
                    for v in node.vars:
                        name = v.name
                        if (
                            getattr(node, "index", None) is not None
                            and name not in idx_map
                        ):
                            idx_map[name] = node.index
                for ch in getattr(node, "iter_children", lambda: [])():
                    idx_map.update(_collect_dealloc_indices(ch))
                return idx_map

            def _collect_ad_allocate_indices(node: Node) -> dict[str, Any]:
                idx_map: dict[str, Any] = {}
                if isinstance(node, Allocate):
                    for v in node.vars:
                        if (
                            v.name.endswith(AD_SUFFIX)
                            and v.index is not None
                            and v.name not in idx_map
                        ):
                            idx_map[v.name] = v.index
                for ch in getattr(node, "iter_children", lambda: [])():
                    idx_map.update(_collect_ad_allocate_indices(ch))
                return idx_map

            def _collect_required_ad_vars(node: Node) -> set[str]:
                req: set[str] = set()
                # Scan all variables referenced anywhere in fw_block
                for var in node.collect_vars():
                    if getattr(var, "name", None) and var.name.endswith(AD_SUFFIX):
                        req.add(var.name)
                for ch in getattr(node, "iter_children", lambda: [])():
                    req |= _collect_required_ad_vars(ch)
                return req

            if reverse and not fw_block.is_effectively_empty():
                # Insert AD allocations immediately before each primal allocate
                # in the forward block, so any size expressions computed in
                # fw_block are already available.
                mod_var_names = [v.name for v in mod_vars]

                allocated_ad_names: set[str] = set()

                def _insert_for_block(block: Block) -> None:
                    children = list(block.iter_children())
                    for ch in children:
                        # Recurse into nested blocks
                        if isinstance(ch, IfBlock):
                            for _, b in ch.cond_blocks:
                                _insert_for_block(b)
                            continue
                        if isinstance(ch, SelectBlock):
                            for _, b in ch.cond_blocks:
                                _insert_for_block(b)
                            continue
                        if isinstance(ch, WhereBlock):
                            for _, b in ch.cond_blocks:
                                _insert_for_block(b)
                            continue
                        if isinstance(ch, DoAbst):
                            _insert_for_block(ch._body)
                            continue
                        if isinstance(ch, OmpDirective) and ch.body is not None:
                            body = ch.body
                            if isinstance(body, Block):
                                _insert_for_block(body)
                            continue

                        # Handle the allocate site
                        if isinstance(ch, Allocate):
                            pre = Block([])
                            for var in ch.vars:
                                if not var.ad_target:
                                    continue
                                name_ad = f"{var.name}{AD_SUFFIX}"
                                decl = subroutine.decls.find_by_name(name_ad)
                                if decl is None:
                                    continue
                                if not decl.allocatable and not decl.pointer:
                                    continue
                                is_mod = var.is_module_var(mod_var_names)
                                mold = None
                                ad_var = OpVar(name_ad)
                                if is_mod:
                                    # For module variables, prefer mold-based
                                    # allocation (or size(...) for derived types)
                                    if var.var_type.typename.startswith(
                                        ("type", "class")
                                    ):
                                        # build index via size() for each dim
                                        idx_list: list[Any] = []
                                        ndim = (
                                            len(var.index)
                                            if var.index is not None
                                            else 0
                                        )
                                        vclean = var.change_index(None)
                                        for n in range(ndim):
                                            idx_list.append(
                                                OpFunc(
                                                    "size", args=[vclean, OpInt(n + 1)]
                                                )
                                            )
                                        index = AryIndex(idx_list) if idx_list else None
                                    else:
                                        mold = var
                                        index = None
                                else:
                                    index = var.index
                                if index is not None:
                                    ad_var = ad_var.change_index(index)
                                # Note: Block.insert_before inserts Block children
                                # in reverse, so append Clear before Allocate to
                                # render as: allocate -> clear
                                if not var.var_type.typename.startswith(
                                    ("type", "class")
                                ):
                                    pre.append(
                                        ClearAssignment(ad_var.change_index(None))
                                    )
                                node = Allocate([ad_var], mold=mold)
                                pre.append(Allocate._add_if(node, ad_var, is_mod))
                                allocated_ad_names.add(name_ad)
                            if not pre.is_effectively_empty():
                                block.insert_before(ch.get_id(), pre)

                _insert_for_block(fw_block)

                # Remove redundant ClearAssignment in the reverse block for
                # variables we have just zero-initialized in fw_block.
                def _prune_clears(node: Node, names: set[str]) -> None:
                    if isinstance(node, Block):
                        to_remove = []
                        for c in list(node.iter_children()):
                            _prune_clears(c, names)
                            if isinstance(c, ClearAssignment):
                                v = c.lhs
                                # Only prune clears that target the whole variable.
                                # Slice clears (e.g., var_ad(i:j, k:l) = 0) are
                                # required in reverse mode between overwrites and
                                # must not be removed.
                                if (
                                    v is not None
                                    and v.index is None
                                    and v.name in names
                                ):
                                    to_remove.append(c)
                        for n in to_remove:
                            node.remove_child(n)
                    else:
                        for c in getattr(node, "iter_children", lambda: [])():
                            _prune_clears(c, names)

                if allocated_ad_names:
                    _prune_clears(ad_block, allocated_ad_names)

                # After pruning, drop empty conditional blocks introduced for clears
                def _drop_empty_branches(node: Node) -> None:
                    if isinstance(node, Block):
                        to_remove = []
                        for ch in list(node.iter_children()):
                            _drop_empty_branches(ch)
                            from .code_tree import IfBlock as _If
                            from .code_tree import SelectBlock as _Se
                            from .code_tree import WhereBlock as _Wh

                            if isinstance(ch, _If):
                                ch.cond_blocks = [
                                    (cond, body)
                                    for cond, body in ch.cond_blocks
                                    if not body.is_effectively_empty()
                                ]
                                if len(ch.cond_blocks) == 0:
                                    to_remove.append(ch)
                            elif isinstance(ch, _Wh):
                                ch.cond_blocks = [
                                    (cond, body)
                                    for cond, body in ch.cond_blocks
                                    if not body.is_effectively_empty()
                                ]
                                if len(ch.cond_blocks) == 0:
                                    to_remove.append(ch)
                            elif isinstance(ch, _Se):
                                ch.cond_blocks = [
                                    (conds, body)
                                    for conds, body in ch.cond_blocks
                                    if not body.is_effectively_empty()
                                ]
                                if len(ch.cond_blocks) == 0:
                                    to_remove.append(ch)
                        for n in to_remove:
                            node.remove_child(n)
                    else:
                        for ch in getattr(node, "iter_children", lambda: [])():
                            _drop_empty_branches(ch)

                _drop_empty_branches(ad_block)

            # Symmetric prune: drop redundant allocates in the reverse block
            # when the forward block already allocates the same AD variables.
            # First apply the simple pattern-based prune, then remove any
            # direct Allocate nodes inside larger blocks.
            ad_block = _prune_fw_allocates_simple(ad_block, fw_block)

            def _prune_ad_redundant_allocates(block: Block, other: Block) -> None:
                # Walk and remove Allocate nodes for AD vars already allocated
                # in the forward block. Recurse into nested blocks conservatively.
                to_remove: list[Node] = []
                for ch in list(block.iter_children()):
                    if isinstance(ch, Allocate):
                        vnames = [v.name_ext() for v in ch.vars if v.name.endswith(AD_SUFFIX)]
                        drop = False
                        for vname in vnames:
                            if _allocates_var(other, vname):
                                drop = True
                                break
                        if drop:
                            to_remove.append(ch)
                            continue
                    if isinstance(ch, IfBlock):
                        for _, b in ch.cond_blocks:
                            _prune_ad_redundant_allocates(b, other)
                    elif isinstance(ch, SelectBlock):
                        for _, b in ch.cond_blocks:
                            _prune_ad_redundant_allocates(b, other)
                    elif isinstance(ch, WhereBlock):
                        for _, b in ch.cond_blocks:
                            _prune_ad_redundant_allocates(b, other)
                    elif isinstance(ch, DoAbst):
                        _prune_ad_redundant_allocates(ch._body, other)
                    elif isinstance(ch, OmpDirective) and ch.body is not None:
                        body = ch.body if isinstance(ch.body, Block) else Block(list(ch.body))
                        _prune_ad_redundant_allocates(body, other)
                for n in to_remove:
                    block.remove_child(n)

            _prune_ad_redundant_allocates(ad_block, fw_block)


            # Ensure the subroutine references the pruned reverse block
            subroutine.ad_content = ad_block

            if not fw_block.is_effectively_empty():
                subroutine.content.extend(fw_block)

    if reverse and (ad_block is not None) and (not ad_block.is_effectively_empty()):
        # initialize ad_var if necessary
        vars = ad_block.required_vars(
            VarList(out_grad_args + save_ad_vars), without_savevar=True
        )
        if not fw_block.is_effectively_empty():
            vars = fw_block.required_vars(vars)
        for name in vars.names():
            if not name.endswith(AD_SUFFIX):
                continue
            if not any(v for v in grad_args if v.name == name):
                if subroutine.is_declared(name):
                    var = subroutine.get_var(name)
                    if var is not None and not var.ad_target:  # skip if not ad_target
                        continue
                else:
                    var = None
            else:
                if any(v for v in in_grad_args if v.name == name):
                    continue
                var = next(v for v in out_grad_args if v.name == name)
            if var is not None and not var.save and not var.pointer:
                if var.dims is not None and len(var.dims) > 0:
                    index = AryIndex((None,) * len(var.dims))
                else:
                    index = None
                kind = var.var_type.kind
                if kind is None and var.var_type.typename.lower() == "double precision":
                    kind = Kind(OpInt(8), val=8, use_kind_keyword=False)
                if var.is_complex_type:
                    zero = OpComplex(
                        OpReal("0.0", kind=kind),
                        OpReal("0.0", kind=kind),
                        kind=kind,
                    )
                else:
                    zero = OpReal(0.0, kind=kind)
                vt = var.var_type.copy()
                vt.kind = kind
                subroutine.ad_init.append(
                    Assignment(OpVar(name, index=index, var_type=vt), zero)
                )

    var_names = []
    for var in subroutine.collect_vars():
        if var.name not in var_names:
            var_names.append(var.name)
    for name in var_names:
        if subroutine.decls.find_by_name(name) is None:
            decl = routine_org.decls.find_by_name(name)
            if decl is None and name.endswith(AD_SUFFIX):
                if any(v.name == name for v in mod_vars):
                    continue
                base = name.removesuffix(AD_SUFFIX)
                base_decl = routine_org.decls.find_by_name(base)
                if base_decl is not None:
                    subroutine.decls.append(
                        Declaration(
                            name=name,
                            var_type=base_decl.var_type.copy(),
                            dims=base_decl.dims,
                            dims_raw=base_decl.dims_raw,
                            parameter=base_decl.parameter,
                            init_val=base_decl.init_val,
                            allocatable=base_decl.allocatable,
                            pointer=base_decl.pointer,
                            optional=base_decl.optional,
                            target=base_decl.target,
                            save=base_decl.save,
                            value=base_decl.value,
                            volatile=base_decl.volatile,
                            asynchronous=base_decl.asynchronous,
                            declared_in="routine",
                        )
                    )
                elif name.startswith(("cycle_flag", "exit_flag", "return_flag")):
                    subroutine.decls.append(
                        Declaration(
                            name,
                            var_type=VarType("logical"),
                            declared_in="routine",
                        )
                    )
                elif name.startswith(("exit_do_start")):
                    subroutine.decls.append(
                        Declaration(
                            name,
                            var_type=VarType("integer"),
                            declared_in="routine",
                        )
                    )
                continue
            if decl is not None:
                decl = decl.copy()
                if decl.intent is not None and decl.intent == "out":
                    decl.intent = None
                subroutine.decls.append(decl)

    subroutine.build_parent()

    def _find_save_assign(node: Node, save_assigns: dict) -> None:
        """Collect ``SaveAssignment`` nodes used for restoring values."""

        if isinstance(node, SaveAssignment) and not node.pushpop:
            name = node.tmpvar.name
            if name not in save_assigns:
                save_assigns[name] = []
            save_assigns[name].append(node)
        for child in node.iter_children():
            _find_save_assign(child, save_assigns)

    save_assigns = dict()
    _find_save_assign(subroutine, save_assigns)
    # print(render_program(subroutine))

    def _find_loop(parent: Node, index_list: List[str]):
        """Return nearest ``DoLoop`` whose index appears in ``index_list``."""

        while parent is not None and not (
            isinstance(parent, DoLoop) and parent.index.name in index_list
        ):
            parent = parent.parent
        return parent

    def _get_size(node: Node, name: str, dims: List[Operator]):
        """Populate ``dims`` with loop bounds for array ``name``."""

        for child in node.iter_children():
            _get_size(child, name, dims)
        if isinstance(node, DoLoop):
            do_index = node.index
            for var in node.collect_vars():
                if var.name == name:
                    for i, idx in enumerate(var.concat_index()):
                        if idx == do_index:
                            i0, i1, _ = node.range.ascending()
                            if i0 == OpInt(1):
                                dim = i1
                            else:
                                dim = OpRange([i0, i1])
                            dims[i] = dim
                            return
        return

    # declaration of saved variables
    for var in reversed(saved_vars):
        if var.index is not None and var.name in save_assigns:
            if len(save_assigns[var.name]) < 2:
                raise RuntimeError(f"Unexpected: {var.name}")

            # find reduced_dims
            index_list = var.index_list()
            loops = []
            for save in save_assigns[var.name]:
                loops.append(_find_loop(save.parent, index_list))
            common_index = []
            while all(loop is not None for loop in loops):
                if len({loop.get_id() for loop in loops}) == 1:
                    common_index.append(loops[0].index.name)
                loops_new = []
                for loop in loops:
                    loops_new.append(_find_loop(loop.parent, index_list))
                loops = loops_new
            var.reduced_dims = []
            for i, idx in enumerate(var.index):
                if isinstance(idx, OpVar) and idx.name in common_index:
                    var.reduced_dims.append(i)

        base_decl = None
        if var.reference is not None:
            ref = var.reference.remove_suffix(AD_SUFFIX)
            base_decl = routine_org.find_decl(ref)

        allocatable = False
        pointer = False
        if base_decl is not None:
            allocatable = base_decl.allocatable
            pointer = base_decl.pointer
            if (
                allocatable and base_decl.declared_in != "routine"
            ):  # module or use variable
                allocatable = False
                if var.index:
                    dims_list: List[Operator] = []
                    for n, idx in enumerate(var.index):
                        if isinstance(idx, OpRange):
                            if idx[0] == OpInt(1) and idx[1] is not None:
                                dims_list.append(idx[1])
                                continue
                            elif idx[0] is not None and idx[1] is not None:
                                dims_list[n].append(OpRange([idx[0], idx[1]]))
                                continue
                        dims_list.append(OpFunc("size", [var.change_index(None), OpInt(n)]))
                else:
                    dims_list = [OpFunc("size", [var.change_index(None), OpInt(n)]) for n in range(len(base_decl.dims))]
                _get_size(subroutine, var.name, dims_list)
                dims = tuple(dims_list)
            else:
                dims = tuple(base_decl.dims) if base_decl.dims else None
        else:
            dims = tuple(var.dims) if var.dims else None

        if (
            dims is not None
            and var.reference is not None
            and not (allocatable or pointer)
        ):
            dims_new = []
            for i, dim in enumerate(dims):
                if isinstance(dim, OpRange):
                    refv = var.reference.change_index(None)
                    if dim[0] is None:
                       dim.args[0] = OpFunc("lbound", [refv, OpInt(i+1)])
                    if dim[1] is None:
                        dim.args[1] = OpFunc("ubound", [refv, OpInt(i+1)])
                dims_new.append(dim)
            dims = tuple(dims_new)

        dims_new = []
        if dims is not None and var.reduced_dims is not None:
            for i, idx in enumerate(dims):
                if i not in var.reduced_dims:
                    dims_new.append(idx)
            if len(dims_new) == 0:
                dims = None
            else:
                dims = tuple(dims_new)

        if base_decl:
            var_type = base_decl.var_type.copy()
        else:
            if var.var_type is not None:
                var_type = var.var_type.copy()
            elif var.ad_target:
                var_type = VarType("real")
            else:
                print([var])
                print([var.reference])
                print([base_decl])
                raise RuntimeError(f"typename cannot be identified {var}")

        subroutine.decls.append(
            Declaration(
                name=var.name,
                var_type=var_type,
                dims=dims,
                parameter=base_decl.parameter if base_decl else False,
                init_val=base_decl.init_val if base_decl else None,
                allocatable=allocatable,
                pointer=pointer,
                optional=base_decl.optional if base_decl else False,
                target=base_decl.target if base_decl else False,
                declared_in="routine",
            )
        )

    if reverse:
        targets = VarList(grad_args + mod_ad_vars + save_ad_vars)
    else:
        targets = VarList(
            grad_args + mod_vars + mod_ad_vars + out_args + save_vars + save_ad_vars
        )

    # print(render_program(subroutine))
    mod_vars_all = mod_vars + save_vars
    subroutine = subroutine.prune_for(
        targets, mod_vars_all, decl_map=subroutine.decl_map, base_targets=targets
    )

    # Remove arguments that are no longer used after pruning
    used = {v.name for v in subroutine.content.collect_vars()}
    used.update(v.name for v in subroutine.ad_init.collect_vars())
    used.update(v.name for v in subroutine.ad_content.collect_vars())
    for decl in subroutine.decls.iter_children():
        if not isinstance(decl, Declaration):
            continue
        if decl.name in used and decl.dims:
            for dim in decl.dims:
                if dim is None:
                    continue
                for var in dim.collect_vars():
                    if var.name in subroutine.args:
                        used.add(var.name)
    for name in list(subroutine.args):
        if name not in used:
            decl = subroutine.decls.find_by_name(name)
            if decl is not None:
                subroutine.decls.remove_child(decl)
            if subroutine.decl_map is not None and name in subroutine.decl_map:
                del subroutine.decl_map[name]
            subroutine.args.remove(name)

    # After pruning, remove duplicate and unused allocate statements.
    # This is particularly important for reverse mode AD where multiple
    # allocates for the same variable may appear.
    #
    # Collect module/use variables that are allocatable or pointer so that
    # the routine-level pass can reason about externally managed variables.
    mod_allocatable_vars = []
    for v in mod_vars:
        if getattr(v, "allocatable", False) or getattr(v, "pointer", False):
            mod_allocatable_vars.append(v)
    subroutine.remove_redundant_allocates(mod_allocatable_vars)

    # Additional prune: drop allocate/deallocate for variables that are never
    # accessed anywhere in the generated routine (content, ad_init, ad_content).
    # This captures cases like temporary arrays that are allocated but unused
    # (e.g., xtmp, xtmp_ad in examples/return_example).
    def _drop_allocs_of_unused(sub: Subroutine) -> None:
        blocks = [sub.content]
        if sub.ad_init is not None:
            blocks.append(sub.ad_init)
        if sub.ad_content is not None:
            blocks.append(sub.ad_content)

        def is_used(vname: str) -> bool:
            # A variable counts as "used" only if it appears in a statement
            # other than housekeeping nodes (Allocate/Deallocate/ClearAssignment)
            # within any of the blocks (content, ad_init, ad_content).
            def has_meaningful_access(node: Node, varname: str) -> bool:
                # Skip housekeeping nodes for usage detection
                if isinstance(node, (Allocate, Deallocate, ClearAssignment)):
                    return False
                if node.has_access_to(varname):
                    return True
                for ch in node.iter_children():
                    if has_meaningful_access(ch, varname):
                        return True
                return False

            for blk in blocks:
                if blk is None:
                    continue
                for stmt in blk:
                    if has_meaningful_access(stmt, vname):
                        return True
            return False

        def _can_drop_var(v: OpVar) -> bool:
            # Only drop if declared locally in this subroutine (not module/use)
            if getattr(v, "declared_in", None) != "routine":
                return False
            # Restrict to allocatable/pointer temps typical for locals
            if not (getattr(v, "allocatable", False) or getattr(v, "pointer", False)):
                return False
            return True

        # Collect droppable local temp names from (de)allocate sites
        cand_names: set[str] = set()
        def _collect_candidates(block: Block) -> None:
            for ch in list(block.iter_children()):
                if isinstance(ch, Allocate):
                    for v in ch.vars:
                        if _can_drop_var(v):
                            cand_names.add(v.name_ext())
                elif isinstance(ch, Deallocate):
                    for v in ch.vars:
                        if _can_drop_var(v):
                            cand_names.add(v.name_ext())
                # Recurse into structured blocks
                if isinstance(ch, IfBlock):
                    for _, b in ch.cond_blocks:
                        _collect_candidates(b)
                elif isinstance(ch, SelectBlock):
                    for _, b in ch.cond_blocks:
                        _collect_candidates(b)
                elif isinstance(ch, WhereBlock):
                    for _, b in ch.cond_blocks:
                        _collect_candidates(b)
                elif isinstance(ch, DoAbst):
                    _collect_candidates(ch._body)
                elif isinstance(ch, OmpDirective) and ch.body is not None:
                    body = ch.body if isinstance(ch.body, Block) else Block(list(ch.body))
                    _collect_candidates(body)

        for blk in blocks:
            if blk is not None:
                _collect_candidates(blk)

        unused_names = {name for name in cand_names if not is_used(name)}

        def prune_block(block: Block) -> None:
            to_remove: list[Node] = []
            for ch in list(block.iter_children()):
                if isinstance(ch, Allocate):
                    ch.vars = [v for v in ch.vars if v.name_ext() not in unused_names]
                    if not ch.vars:
                        to_remove.append(ch)
                        continue
                elif isinstance(ch, Deallocate):
                    ch.vars = [v for v in ch.vars if v.name_ext() not in unused_names]
                    if not ch.vars:
                        to_remove.append(ch)
                        continue
                # Recurse into structured blocks
                if isinstance(ch, IfBlock):
                    for _, b in ch.cond_blocks:
                        prune_block(b)
                elif isinstance(ch, SelectBlock):
                    for _, b in ch.cond_blocks:
                        prune_block(b)
                elif isinstance(ch, WhereBlock):
                    for _, b in ch.cond_blocks:
                        prune_block(b)
                elif isinstance(ch, DoAbst):
                    prune_block(ch._body)
                elif isinstance(ch, OmpDirective) and ch.body is not None:
                    body = ch.body if isinstance(ch.body, Block) else Block(list(ch.body))
                    prune_block(body)
            for n in to_remove:
                block.remove_child(n)

        for blk in blocks:
            if blk is not None:
                prune_block(blk)

    _drop_allocs_of_unused(subroutine)

    # update routine_map with pruned argument information
    arg_info = routine_map.get(routine_org.name)
    if arg_info is not None:
        args_new = list(subroutine.args)
        intents_new = []
        for a in args_new:
            decl = subroutine.decls.find_by_name(a)
            intents_new.append(decl.intent if decl is not None else None)
        if reverse:
            arg_info["args_rev_ad"] = args_new
            arg_info["intents_rev_ad"] = intents_new
        else:
            arg_info["args_fwd_ad"] = args_new
            arg_info["intents_fwd_ad"] = intents_new

    mod_all_var_names = [v.name_ext() for v in mod_vars]
    for v in mod_ad_vars:
        mod_all_var_names.append(v.name_ext())
    mod_all_var_names.extend(const_var_names)
    required_vnames = []
    for var in subroutine.required_vars():
        if var.name in mod_all_var_names or var.is_constant:
            continue
        decl = subroutine.decls.find_by_name(var.name) if subroutine.decls else None
        if decl is not None:
            if decl.parameter or getattr(decl, "constant", False):
                continue
        required_vnames.append(str(var))

    if len(required_vnames) > 0:
        _warn(
            warnings,
            {},
            f"{required_vnames} in {subroutine.name}",
            "Required variables are remained",
        )

    uses_pushpop = _contains_pushpop(subroutine) if reverse else False

    if sub_fwd_rev is not None:
        routine_info["fwd_rev_subroutine"] = (
            sub_fwd_rev  # save to output this subroutine later
        )

    called_mods = _collect_called_ad_modules(
        [subroutine.content, subroutine.ad_init, subroutine.ad_content],
        routine_map,
        reverse=reverse,
    )

    # Final cleanup: drop any empty conditional blocks that may have been
    # left after previous pruning passes (e.g., guards around removed clears).
    def _drop_empty_branches_global(node: Node) -> None:
        if isinstance(node, Block):
            to_remove = []
            for ch in list(node.iter_children()):
                _drop_empty_branches_global(ch)
                from .code_tree import IfBlock, SelectBlock, WhereBlock

                if isinstance(ch, IfBlock):
                    ch.cond_blocks = [
                        (cond, body)
                        for cond, body in ch.cond_blocks
                        if not body.is_effectively_empty()
                    ]
                    if len(ch.cond_blocks) == 0:
                        to_remove.append(ch)
                elif isinstance(ch, WhereBlock):
                    ch.cond_blocks = [
                        (cond, body)
                        for cond, body in ch.cond_blocks
                        if not body.is_effectively_empty()
                    ]
                    if len(ch.cond_blocks) == 0:
                        to_remove.append(ch)
                elif isinstance(ch, SelectBlock):
                    ch.cond_blocks = [
                        (conds, body)
                        for conds, body in ch.cond_blocks
                        if not body.is_effectively_empty()
                    ]
                    if len(ch.cond_blocks) == 0:
                        to_remove.append(ch)
            for n in to_remove:
                node.remove_child(n)
        else:
            for ch in getattr(node, "iter_children", lambda: [])():
                _drop_empty_branches_global(ch)

    for blk in (subroutine.content, subroutine.ad_init, subroutine.ad_content):
        if blk is not None:
            _drop_empty_branches_global(blk)

    return subroutine, uses_pushpop, called_mods


def generate_ad(
    in_file: Union[str, Path],
    out_file: Optional[Union[str, Path]] = None,
    warn: bool = True,
    search_dirs: Optional[List[Union[str, Path]]] = None,
    write_fadmod: bool = True,
    fadmod_dir: Optional[Union[str, Path]] = None,
    mode: str = "both",
) -> Optional[str]:
    """Generate an AD version of ``in_file``.

    If ``out_file`` is ``None`` the generated code is returned as a string.
    When ``out_file`` is provided the code is also written to that path.
    ``fadmod_dir`` selects where ``<module>.fadmod`` files are written (defaults
    to the current working directory).
    """
    modules = []
    warnings = []

    if search_dirs is None:
        search_dirs = []
    cwd = "."
    if cwd not in search_dirs:
        search_dirs.append(cwd)

    modules_org = parser.parse_file(in_file, search_dirs=search_dirs)
    warnings.extend(parser.macro_warnings)
    if fadmod_dir is None:
        fadmod_dir = Path.cwd()
    else:
        fadmod_dir = Path(fadmod_dir)

    routine_map = {}
    generic_routines = {}
    for mod_org in modules_org:
        name = mod_org.name
        mod = type(mod_org)(f"{name}{AD_SUFFIX}")
        if isinstance(mod_org, Program):
            if mod_org.uses is not None:
                mod.uses = mod_org.uses.deep_clone()
            else:
                mod.uses = Block([])
        else:
            if mod_org.uses is not None:
                uses = Block([Use(name)])
                for child in mod_org.uses.iter_children():
                    uses.append(child.deep_clone())
                mod.uses = uses
            else:
                mod.uses = Block([Use(name)])

        if mod_org.decls:
            type_map = {}
            for child in mod_org.decls.iter_children():
                ad_code = child.generate_ad([], type_map=type_map)
                if isinstance(child, Interface) and child.module_procs:
                    generic_routines[child.name] = child.module_procs
                if ad_code:
                    if isinstance(child, TypeDef):
                        type_map[child.name] = ad_code[0]
                    if mod.decls is None:
                        mod.decls = Block([])
                    for node in ad_code:
                        mod.decls.append(node)

        mod_vars = (
            [var for var in mod_org.decls.collect_vars()]
            if mod_org.decls is not None
            else []
        )

        routine_info_fwd = {}
        routine_info_rev = {}
        for r in mod_org.routines:
            has_mod_grad_var = _has_modified_module_grad_var(r, mod_vars)
            if mode in ("forward", "both"):
                routine_info = _prepare_fwd_ad_header(r, has_mod_grad_var)
                routine_info_fwd[r.name] = routine_info
                routine_map[r.name] = routine_info["arg_info"]
            if mode in ("reverse", "both"):
                routine_info = _prepare_rev_ad_header(r, has_mod_grad_var)
                routine_info_rev[r.name] = routine_info
                if r.name in routine_map:
                    routine_map[r.name].update(routine_info["arg_info"])
                else:
                    routine_map[r.name] = routine_info["arg_info"]

        used_mods = mod_org.find_use_modules()
        const_var_names: List[str] = []
        for name in used_mods:
            fad = None
            for d in search_dirs:
                path = Path(d) / f"{name}.fadmod"
                if path.exists():
                    fad = fadmod.FadmodBase.load(path)
                    break
            if fad is None:
                raise RuntimeError(f"fadmod file not found for module {name}")
            routine_map.update(fad.routines)
            generic_routines.update(fad.generics)
            for v in fad.variables:
                if v.is_constant:
                    const_var_names.append(v.name_ext())
                else:
                    mod_vars.append(v)


        # Preprocess routines for allocate/pointer/mpi once
        for routine in mod_org.routines:
            _parse_allocate(routine.content, mod_vars)
            _parse_pointer(routine.content, mod_vars)
            _parse_mpi_calls(routine.content)

        routine_lookup = {r.name: r for r in mod_org.routines}
        orig_order = [r.name for r in mod_org.routines]
        groups, deps = _dependency_groups(mod_org.routines)

        ad_modules_used = set()
        pushpop_used = False
        generated_subs: Dict[str, Dict[str, Routine]] = {}

        for group in groups:
            iterating = len(group) > 1 or any(n in deps[n] for n in group)
            while True:
                prev_args: Dict[str, Dict[str, List[str]]] = {}
                if iterating:
                    for n in group:
                        info = routine_map.get(n, {})
                        prev_args[n] = {
                            "fwd": list(info.get("args_fwd_ad", [])),
                            "rev": list(info.get("args_rev_ad", [])),
                            "fwd_rev": list(info.get("args_fwd_rev_ad", [])),
                        }
                group_subs: Dict[str, Dict[str, Routine]] = {}
                for name_r in group:
                    routine = routine_lookup[name_r]
                    _set_call_intents(routine.content, routine_map, generic_routines)
                    if mode in ("forward", "both"):
                        sub, _, mods_called = _generate_ad_subroutine(
                            mod_org,
                            routine,
                            routine_map,
                            generic_routines,
                            mod_vars,
                            routine_info_fwd[name_r],
                            warnings,
                            const_var_names,
                            reverse=False,
                        )
                        if sub is not None:
                            group_subs.setdefault(name_r, {})["fwd"] = sub
                        ad_modules_used.update(mods_called)
                    if mode in ("reverse", "both"):
                        routine_info_rev[name_r].pop("fwd_rev_subroutine", None)
                        sub, used, mods_called = _generate_ad_subroutine(
                            mod_org,
                            routine,
                            routine_map,
                            generic_routines,
                            mod_vars,
                            routine_info_rev[name_r],
                            warnings,
                            const_var_names,
                            reverse=True,
                        )
                        if sub is not None:
                            group_subs.setdefault(name_r, {})["rev"] = sub
                        wrapper = routine_info_rev[name_r].get("fwd_rev_subroutine")
                        if wrapper is not None:
                            group_subs.setdefault(name_r, {})["wrapper"] = wrapper
                        ad_modules_used.update(mods_called)
                        if used:
                            pushpop_used = True
                generated_subs.update(group_subs)
                if not iterating:
                    break
                changed = False
                for n in group:
                    info_prev = prev_args[n]
                    info_curr = routine_map.get(n, {})
                    if mode in ("forward", "both") and info_prev[
                        "fwd"
                    ] != info_curr.get("args_fwd_ad", []):
                        changed = True
                    if mode in ("reverse", "both") and info_prev[
                        "rev"
                    ] != info_curr.get("args_rev_ad", []):
                        changed = True
                    if mode in ("reverse", "both") and info_prev[
                        "fwd_rev"
                    ] != info_curr.get("args_fwd_rev_ad", []):
                        changed = True
                if not changed:
                    break

        for name_r in orig_order:
            info = generated_subs.get(name_r, {})
            if mode in ("forward", "both") and "fwd" in info:
                mod.routines.append(info["fwd"])
            if mode in ("reverse", "both") and "rev" in info:
                mod.routines.append(info["rev"])
                if "wrapper" in info:
                    mod.routines.append(info["wrapper"])

        name_mod = mod.name
        existing_uses = {u.name for u in mod.uses.iter_children() if isinstance(u, Use)}
        for m in sorted(ad_modules_used):
            if m != name_mod:
                if m not in existing_uses:
                    mod.uses.append(Use(m))
                    existing_uses.add(m)
                mod.uses.append(Use(f"{m}{AD_SUFFIX}"))
        if pushpop_used:
            mod.uses.append(Use("fautodiff_stack"))

        modules.append(render_program(mod))
        if write_fadmod:
            fad = fadmod.FadmodV1.from_module(mod_org, routine_map)
            fad.write(fadmod_dir / f"{mod_org.name}.fadmod")
    preamble = parser.file_cpp_lines
    if preamble:
        code = "\n".join(preamble + [""] + modules)
    else:
        code = "\n".join(modules)
    if out_file:
        Path(out_file).write_text(code)
    if warn and warnings:
        for msg in warnings:
            print(f"Warning: {msg}", file=sys.stderr)
    return code


if __name__ == "__main__":
    from .cli import main

    main()
