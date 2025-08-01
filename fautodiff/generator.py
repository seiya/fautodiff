from __future__ import annotations

# Suffix used for all AD related names. Change here if a different suffix is
# desired throughout the project.
AD_SUFFIX = "_ad"
FWD_SUFFIX = "_fwd_ad"
REV_SUFFIX = "_rev_ad"

import json
import sys
from pathlib import Path
from typing import Optional, Union, List, Tuple, Dict

# Ensure other modules use the same AD suffix
from . import code_tree as code_tree
from .code_tree import (
    Node,
    Module,
    Routine,
    Subroutine,
    Function,
    Use,
    Declaration,
    TypeDef,
    Block,
    CallStatement,
    Assignment,
    SaveAssignment,
    PushPop,
    ClearAssignment,
    PointerAssignment,
    PointerClear,
    DoAbst,
    DoLoop,
    DoWhile,
    IfBlock,
    SelectBlock,
    Allocate,
    Deallocate,
    Statement,
    render_program,
)
from .operators import (
    AryIndex,
    OpInt,
    OpReal,
    OpComplex,
    OpVar,
    OpRange,
    OpFunc,
    OpNot,
)

code_tree.AD_SUFFIX = AD_SUFFIX
code_tree.FWD_SUFFIX = FWD_SUFFIX
code_tree.REV_SUFFIX = REV_SUFFIX

from . import parser
from .var_list import VarList


def _warn(warnings, info, code, reason):
    """Append a formatted warning message to ``warnings`` list."""
    if warnings is not None and info is not None:
        filename = info.get("file", "<unknown>")
        line = info.get("line", "?")
        msg = f"{filename}:{line}: {code} - {reason}"
        warnings.append(msg)


def _contains_pushpop(node) -> bool:
    """Return True if ``node`` or any child is a ``PushPop``."""
    if isinstance(node, PushPop):
        return True
    for child in getattr(node, "iter_children", lambda: [])():
        if _contains_pushpop(child):
            return True
    return False


def _add_fwd_rev_calls(node, routine_map: dict, generic_map:dict, donot_prune: bool = False) -> None:
    """Add *_fwd_rev_ad wrapper routine before the call when available."""
    if isinstance(node, CallStatement):
        arg_info = Node.get_arg_info(node, routine_map, generic_map)
        if arg_info is not None and arg_info.get("name_fwd_rev_ad"):
            #print(node.name)
            args = node.args
            if node.result is not None:
                args = args.copy()
                args.append(node.result)
            args = CallStatement.rename_args(args, arg_info["args"], arg_info["args_fwd_rev_ad"])
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
                donot_prune=True)
            node.parent.insert_before(node.get_id(), call)
            #print(node.parent.get_id())
            return
    for child in list(getattr(node, "iter_children", lambda: [])()):
        _add_fwd_rev_calls(child, routine_map, generic_map, donot_prune)


def _module_var_fwd_rev(routine_org: Routine,
                        subroutine: Subroutine,
                        mod_vars: List[OpVar],
                        assigned: VarList,
                        required: VarList,
                        ) -> Optional[Subroutine]:
    """Return a forward wrapper"""

    # save module variables
    common = assigned & required
    mod_names = [v.name_ext() for v in mod_vars]
    cross_vars = []
    for var in common:
        name = var.name_ext()
        if name in mod_names and not name.endswith(AD_SUFFIX):
            cross_vars.append(next(v for v in mod_vars if v.name_ext() == name))
    cross_vars = sorted(cross_vars, key=lambda v: v.name_ext())
    args = []
    sub_fwd_rev = None
    index = []
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
                    idx.append(OpVar(f"n{len(index)+n}_ad", typename="integer"))
                    if dim == ":":
                        vv = var_ref.change_index(None)
                        i0 = OpFunc("lbound", args=[vv, OpInt(n+1)])
                        i1 = OpFunc("ubound", args=[vv, OpInt(n+1)])
                    else:
                        i0 = OpInt(1)
                        i1 = OpVar(dim)
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
        sub_fwd_rev.content.append(node_fwd)
        subroutine.content.insert_begin(node_rev)
    for idx in index:
        decl = Declaration(name=idx.name, typename="integer")
        if decl.name not in sub_fwd_rev.args:
            sub_fwd_rev.decls.append(decl)
        subroutine.decls.append(decl)

    # pointer variables
    def _search_pointer(node: Node,
                        sub_fwd_rev: Optional[Subroutine],
                        map: Optional[Dict[str, Tuple[OpVar, OpVar, bool]]] = None,
                        allocated: Optional[Dict[str, bool]] = None,
                        ) -> Optional[Subroutine]:
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
            if (lhs.pointer and lhs_name in mod_var_names and
                isinstance(rhs, OpVar) and rhs.name_ext() in mod_var_names):
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
                    block = Block([
                        Allocate([v], mold=rhs),
                        ClearAssignment(v),
                        ])
                    sub_fwd_rev.ad_content.append(IfBlock([(cond, block)]))
                pa = PointerAssignment(lhs.add_suffix(AD_SUFFIX), rhs_ad)
                sub_fwd_rev.ad_content.append(pa)
        return sub_fwd_rev
    mod_var_names = [v.name for v in mod_vars]
    sub_fwd_rev = _search_pointer(routine_org.content, sub_fwd_rev)

    return sub_fwd_rev

def _parse_allocate(node: Node,
                    mod_vars: List[OpVar],
                    map: Optional[Dict[str, List[AryIndex]]] = None
                    ) -> None:
    if map is None:
        map = {}
        top = True
    else:
        top = False
    for child in node.iter_children():
        _parse_allocate(child, mod_vars, map)
    if isinstance(node, Allocate):
        for var in node.vars:
            if var.is_constant:
                continue
            name = var.name_ext()
            if name not in map:
                map[name] = []
            map[name].append(var.index)
    if isinstance(node, Deallocate):
        for var in node.vars:
            name = var.name_ext()
            if name in map and map[name]:
                node.index = map[name].pop()
    if top:
        mod_var_names = [v.name_ext() for v in mod_vars]
        for name in map:
            if map[name]:
                if name not in mod_var_names:
                    var = OpVar(name, index=map[name][0])
                    node.append(Declaration(vars=[OpVar]))

def _parse_pointer(node: Node,
                   mod_vars: List[OpVar],
                   map_ptr: Optional[Dict[str, OpVar]] = None,
                   map_ref: Optional[Dict[str, Optional[Union[OpVar, OpNull]]]] = None,
                   inloop: bool = False,
                   ) -> Tuple[Dict[str, OpVar], Dict[str, Optional[Union[OpVar, OpNull]]]]:
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
            map_ptr, map_ref_new = _parse_pointer(child, mod_vars, map_ptr, map_ref.copy(), inloop)
            for key, value in map_ref.items():
                if value != map_ref_new[key]: # pointer is updated in the branch
                    map_ref[key] = None
    elif isinstance(node, DoAbst):
        map_ptr, map_ref_new = _parse_pointer(node._body, mod_vars, map_ptr, {}, True)
        for key in map_ref_new: # pointer is updated in the loop
            map_ref[key] = None
    else:
        for child in [n for n in node.iter_children()]:
            map_ptr, map_ref = _parse_pointer(child, mod_vars, map_ptr, map_ref, inloop)
    if isinstance(node, PointerAssignment):
        lhs_name = node.lhs.name_ext()
        rhs = node.rhs
        map_ptr[lhs_name] = node.lhs
        if lhs_name in map_ref:
            node.parent.insert_before(node.get_id(), PointerClear(node.lhs, previous=map_ref[lhs_name], info=node.info))
        else:
            node.parent.insert_before(node.get_id(), PointerClear(node.lhs, previous=None, info=node.info))
        if isinstance(rhs, OpVar):
            rhs_name = rhs.name_ext()
            if rhs_name in map_ref:
                map_ref[lhs_name] = map_ref[rhs_name]
            else:
                map_ref[lhs_name] = rhs
        elif isinstance(rhs, OpFunc) and rhs.name=="null":
            map_ref[lhs_name] = OpNull()
    if isinstance(node, Deallocate):
        for var in node.vars:
            name = var.name_ext()
            for ptr_name, tgt in map_ref.items():
                if isinstance(tgt, OpVar) and name == tgt.name_ext():
                    node.parent.insert_before(node.get_id(), PointerClear(map_ptr[ptr_name], previous=tgt))
                    map_ref[ptr_name] = None
    if top:
        mod_var_names = [v.name for v in mod_vars]
        for name in map_ptr:
            if ((name.endswith(AD_SUFFIX) and name.removesuffix(AD_SUFFIX) in mod_var_names) or
                name in mod_var_names
            ):
                continue
            node.append(PointerClear(map_ptr[name], previous=map_ref[name]))

    return (map_ptr, map_ref)

def _parse_mpi_calls(node: Node, calls: Optional[Dict[str, List[CallStatement]]] = None) -> None:
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


def _set_call_intents(node, routine_map, generic_map):
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


def _load_fadmods(mod_names: list[str], search_dirs: list[str]) -> tuple[dict, dict, dict]:
    """Load routine, variable and generic maps from ``mod_names`` using ``search_dirs``."""

    routines = {}
    variables = {}
    generics = {}
    for mod in mod_names:
        for d in search_dirs:
            path = Path(d) / f"{mod}.fadmod"
            if path.exists():
                try:
                    data = json.loads(path.read_text())
                    generics.update(data.pop("generics", {}))
                    routines_data = data.get("routines", {})
                    if routines_data:
                        for name, info in routines_data.items():
                            nargs = len(info["args"])
                            for key in ["intents", "dims", "type", "kind"]:
                                if key in info:
                                    if len(info[key]) != nargs:
                                        raise RuntimeError(f"invalid routine data: {name} {info}")
                                else:
                                    info[key] = [None] * nargs
                    routines.update(routines_data)
                    vars_data = data.pop("variables", {})
                    if vars_data:
                        vars = []
                        for name, info in vars_data.items():
                            vars.append(
                                OpVar(
                                    name=name,
                                    typename=info.get("typename"),
                                    kind=info.get("kind", None),
                                    dims=info.get("dims", None),
                                    is_constant=info.get("constant", False),
                                    allocatable=info.get("allocatable", False),
                                    pointer=info.get("pointer", False),
                                )
                            )
                        variables[mod] = vars
                except Exception:
                    pass
                break
    return routines, variables, generics


def _write_fadmod(mod: Module, routine_map: dict, directory: Path) -> None:
    """Write routine and variable info for ``mod`` to ``<mod.name>.fadmod``."""

    mod_name = mod.name
    routines_data = {}
    for r in mod.routines:
        info = routine_map.get(r.name)
        if info is None:
            continue
        skip = info.get("skip") or (
            info.get("name_fwd_ad") is None and info.get("name_rev_ad") is None
        )
        if skip:
            routines_data[r.name] = {"skip": True}
            continue
        info = dict(info)
        info["module"] = mod_name
        routines_data[r.name] = info

    variables_data = {}
    if mod.decls is not None:
        for d in mod.decls.iter_children():
            if isinstance(d, Declaration):
                info = {"typename": d.typename}
                if d.kind is not None:
                    info["kind"] = d.kind
                if d.dims is not None:
                    info["dims"] = list(d.dims)
                if d.parameter:
                    info["parameter"] = True
                if d.constant:
                    info["constant"] = True
                if d.init_val is not None:
                    info["init_val"] = d.init_val
                if d.access is not None:
                    info["access"] = d.access
                if d.allocatable:
                    info["allocatable"] = True
                if d.pointer:
                    info["pointer"] = True
                variables_data[d.name] = info

    if not routines_data and not variables_data:
        return

    path = directory / f"{mod_name}.fadmod"
    data = {"routines": routines_data, "variables": variables_data}
    path.write_text(json.dumps(data, indent=2))


def _prepare_fwd_ad_header(routine_org, has_mod_grad_var):
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
        typ = arg.typename
        dims = arg.dims
        intent = arg.intent or "inout"
        kind = arg.kind
        arg_info["args"].append(name)
        arg_info["intents"].append(intent)
        arg_info["type"].append(typ)
        arg_info["dims"].append(dims)
        arg_info["kind"].append(kind)

        # Always pass original argument to the forward AD routine
        args.append(arg)

        if arg.ad_target and not arg.is_constant:
            ad_name = f"{name}{AD_SUFFIX}"
            var = OpVar(
                ad_name,
                typename=typ,
                kind=kind,
                dims=dims,
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

    if routine_org.result is not None:
        arg_info["intents"] = arg_info["intents"][:-1]

    ad_name = f"{routine_org.name}{FWD_SUFFIX}"
    subroutine = Subroutine(ad_name, [v.name for v in args])
    arg_info["name_fwd_ad"] = ad_name
    for var in args:
        subroutine.decls.append(
            Declaration(
                name=var.name,
                typename=var.typename,
                kind=var.kind,
                char_len=var.char_len,
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

    skip = bool(routine_org.directives.get("SKIP")) or (len(out_grad_args) == 0 and not has_mod_grad_var)
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


def _prepare_rev_ad_header(routine_org, has_mod_grad_var):
    """Prepare AD subroutine header and returns argument info."""

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
        typ = arg.typename
        dims = arg.dims
        intent = arg.intent or "inout"
        kind = arg.kind
        arg_info["args"].append(name)
        arg_info["intents"].append(intent)
        arg_info["type"].append(typ)
        arg_info["dims"].append(dims)
        arg_info["kind"].append(kind)
        if intent == "out":
            if arg.ad_target and not arg.is_constant:
                ad_name = f"{name}{AD_SUFFIX}"
                var = OpVar(
                    ad_name,
                    typename=typ,
                    kind=kind,
                    dims=dims,
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
                has_grad_input = True
        else:
            args.append(arg)
            if arg.ad_target and not arg.is_constant:
                ad_name = f"{name}{AD_SUFFIX}"
                grad_intent = {
                    "in": "out",
                    "inout": "inout",
                }.get(intent)
                var = OpVar(
                    ad_name,
                    typename=typ,
                    kind=kind,
                    dims=dims,
                    intent=grad_intent,
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
                if grad_intent == "inout":
                    in_grad_args.append(var)
                    has_grad_input = True
    if routine_org.result is not None:
        arg_info["intents"] = arg_info["intents"][:-1]

    ad_name = f"{routine_org.name}{REV_SUFFIX}"
    subroutine = Subroutine(ad_name, [v.name for v in args])
    arg_info["name_rev_ad"] = ad_name
    for var in args:
        subroutine.decls.append(
            Declaration(
                name=var.name,
                typename=var.typename,
                kind=var.kind,
                char_len=var.char_len,
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

    skip = bool(routine_org.directives.get("SKIP")) or (len(out_grad_args) == 0 and not has_mod_grad_var)
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


def _collect_called_ad_modules(blocks, routine_map, reverse):
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
    routine_map: dict,
    generic_map: dict,
    mod_vars: List[OpVar],
    routine_info: dict,
    warnings: List[str],
    *,
    reverse: bool,
) -> tuple[Optional[Subroutine], bool, set]:
    """Generate forward or reverse AD subroutine."""

    if routine_info.get("skip"):
        return None, False, set()

    subroutine = routine_info["subroutine"]
    grad_args = routine_info["grad_args"]
    in_grad_args = routine_info["in_grad_args"]
    out_grad_args = routine_info["out_grad_args"]
    has_grad_input = routine_info["has_grad_input"]
    # list of original intent(out) arguments
    out_args = [var for var in routine_org.arg_vars() if (var.intent is None or var.intent in ("out", "inout"))]
    ad_block = subroutine.ad_content

    sub_fwd_rev: Optional[Subroutine] = None

    has_mod_grad_input = False
    mod_ad_vars = []
    for var in mod_vars:
        if var.ad_target:
            mod_ad_vars.append(var.add_suffix(AD_SUFFIX))
            has_mod_grad_input = True

    save_vars = []
    save_ad_vars = []
    has_save_grad_input = False
    for var in routine_org.decls.collect_vars():
        if var.save:
            save_vars.append(var)
            if var.ad_target:
                save_ad_vars.append(var.add_suffix(AD_SUFFIX))
                has_save_grad_input = True
    if reverse and not has_grad_input and not has_mod_grad_input and not has_save_grad_input:
        for arg in out_grad_args:
            lhs = OpVar(arg.name, kind=arg.kind)
            if arg.is_complex_type:
                zero = OpComplex(OpReal("0.0", kind=arg.kind), OpReal("0.0", kind=arg.kind), kind=arg.kind)
            else:
                zero = OpReal("0.0", kind=arg.kind)
            ad_block.append(Assignment(lhs, zero))
        subroutine.ad_content = ad_block
        return subroutine, False, set()

    saved_vars: List[OpVar] = []
    if reverse:
        assigned_advars = None
    else:
        assigned_advars = VarList(in_grad_args + mod_ad_vars + save_ad_vars)
    nodes = routine_org.content.generate_ad(
        saved_vars,
        reverse=reverse,
        assigned_advars=assigned_advars,
        routine_map=routine_map,
        generic_map=generic_map,
        mod_vars=mod_vars,
        warnings=warnings,
    )
    ad_code = Block()
    for node in nodes:
        if not node.is_effectively_empty():
            ad_code.append(node)

    #print(subroutine.name)
    #print(render_program(ad_code)) # for debugging
    #print(render_program(routine_org))

    if not ad_code.is_effectively_empty():
        for var in ad_code.assigned_vars(without_savevar=True):
            name = var.name
            if name.endswith(AD_SUFFIX):
                if any(arg.name == name for arg in grad_args):
                    continue
                if any(v.name == name for v in mod_ad_vars):
                    continue
                base_decl = routine_org.decls.find_by_name(name.removesuffix(AD_SUFFIX))
                if base_decl is not None and not subroutine.is_declared(name):
                    subroutine.decls.append(
                        Declaration(
                            name=name,
                            typename=base_decl.typename,
                            kind=base_decl.kind,
                            dims=base_decl.dims,
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
            targets = VarList(grad_args + out_args + mod_ad_vars + mod_vars + save_ad_vars + save_vars)

        #print("Targets:", targets) # for debugging
        #print(subroutine.name)
        ad_code = ad_code.prune_for(targets, mod_vars + save_vars)
        #print(render_program(ad_code)) # for debugging

    for node in ad_code:
        if not node.is_effectively_empty():
            ad_block.append(node)

    if reverse:
        targets = ad_block.required_vars()

        #print(render_program(ad_block))
        #print("Targets:", targets)
        fw_block = Block(routine_org.content.set_for_exitcycle(None))
        fw_block.build_parent()

        _add_fwd_rev_calls(fw_block, routine_map, generic_map)

        fw_block = fw_block.prune_for(targets, mod_vars + save_vars)

        assigned = fw_block.assigned_vars(without_savevar=True)
        assigned = ad_block.assigned_vars(assigned, without_savevar=True)
        required = ad_block.required_vars(without_savevar=True)
        required = fw_block.required_vars(required, without_savevar=True)
        sub_fwd_rev = _module_var_fwd_rev(routine_org, subroutine, mod_vars, assigned, required)
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
            if (isinstance(last, SaveAssignment) and isinstance(first, SaveAssignment) and
                last.var.name == first.var.name and last.id == first.id and last.load != first.load):
                fw_block.remove_child(last)
                ad_block.remove_child(first)
                continue
            if (isinstance(last, PointerAssignment) and isinstance(first, PointerAssignment) and
                last.lhs.name_ext() == first.rhs.name_ext() and last.rhs.name_ext() == first.lhs.name_ext()):
                fw_block.remove_child(last)
                ad_block.remove_child(first)
                continue
            if isinstance(last, DoLoop) and isinstance(first, DoLoop):
                item1 = first._body[0]
                item2 = last._body[-1]
                flag = False
                while (isinstance(item1, SaveAssignment) and not item1.pushpop and
                       isinstance(item2, SaveAssignment) and not item2.pushpop and
                       item1.var.name == item2.var.name and item1.id == item2.id and item1.load != item2.load):
                    first._body.remove_child(item1)
                    last._body.remove_child(item2)
                    flag = True
                    if first._body.is_effectively_empty() or last._body.is_effectively_empty():
                        break
                    item1 = first._body[0]
                    item2 = last._body[-1]
                if last.is_effectively_empty():
                    fw_block.remove_child(last)
                if first.is_effectively_empty():
                    ad_block.remove_child(first)
                continue
            flag = False

        if not fw_block.is_effectively_empty():
            subroutine.content.extend(fw_block)

    if (ad_block is not None) and (not ad_block.is_effectively_empty()):
        # initialize ad_var if necessary
        vars = ad_block.required_vars(
            VarList([OpVar(var.name) for var in out_grad_args + save_ad_vars]),
            without_savevar=True
        )
        if reverse and not fw_block.is_effectively_empty():
            vars = fw_block.required_vars(vars)
        for name in vars.names():
            if not name.endswith(AD_SUFFIX):
                continue
            if any(v for v in mod_ad_vars if v.name == name):
                continue
            if not any(v for v in grad_args if v.name == name):
                if subroutine.is_declared(name):
                    var = subroutine.get_var(name)
                else:
                    var = None
            else:
                if any(v for v in in_grad_args if v.name == name):
                    continue
                var = next(v for v in out_grad_args if v.name == name)
            if var is not None and not var.save:
                if var.dims is not None and len(var.dims) > 0:
                    index = AryIndex((None,) * len(var.dims))
                else:
                    index = None
                if var.is_complex_type:
                    zero = OpComplex(OpReal("0.0", kind=var.kind), OpReal("0.0", kind=var.kind), kind=var.kind)
                else:
                    zero = OpReal(0.0, kind=var.kind)
                subroutine.ad_init.append(
                    Assignment(OpVar(name, index=index), zero)
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
                            typename=base_decl.typename,
                            kind=base_decl.kind,
                            dims=base_decl.dims,
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
                elif name.startswith(("cycle_flag", "exit_flag")):
                    subroutine.decls.append(
                        Declaration(
                            name,
                            typename="logical",
                            declared_in="routine"
                        )
                    )
                elif name.startswith(("exit_do_start")):
                    subroutine.decls.append(
                        Declaration(
                            name,
                            typename="integer",
                            declared_in="routine"
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
        if isinstance(node, SaveAssignment) and not node.pushpop:
            name = node.tmpvar.name
            if name not in save_assigns:
                save_assigns[name] = []
            save_assigns[name].append(node)
        for child in node.iter_children():
            _find_save_assign(child, save_assigns)
    save_assigns = dict()
    _find_save_assign(subroutine, save_assigns)
    #print(render_program(subroutine))

    def _find_loop(parent: Node, index_list: List[str]):
        while parent is not None and not(isinstance(parent, DoLoop) and parent.index.name in index_list):
            parent = parent.parent
        return parent

    def _get_size(node: Node, name: str, dims: List[str]):
        var_list = VarList()
        for child in node.iter_children():
            _get_size(child, name, dims)
        if isinstance(node, DoLoop):
            do_index = node.index
            for var in node.collect_vars():
                if var.name == name:
                    for i, idx in enumerate(var.concat_index()):
                        if idx == do_index:
                            i0 = node.range[0]
                            i1 = node.range[1]
                            if i0 == OpInt(1):
                                dim = str(i1)
                            elif i1 == OpInt(1):
                                dim = str(i0)
                            else:
                                dim = f"{i0}:{i1}"
                            dims[i] = dim
                            return
        return

    # declaration of saved variables
    for var in reversed(saved_vars):
        if var.index is not None and var.name in save_assigns:
            if len(save_assigns[var.name]) < 2:
                raise RuntimeError(f"Unexpected: {var.name}")
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
        if base_decl is not None:
            allocatable = base_decl.allocatable
            if allocatable and base_decl.declared_in != "routine": # module or use variable
                allocatable = False
                vname = str(var.change_index(None))
                if var.index:
                    dims = []
                    for n, idx in enumerate(var.index):
                        if isinstance(idx, OpRange):
                            if idx[0]==OpInt(1) and idx[1] is not None:
                                dims.append(str(idx[1]))
                                continue
                            elif idx[0] is not None and idx[1] is not None:
                                dims[n].append(f"{idx[0]}:{idx[1]}")
                                continue
                        dims.append(f"size({vname}, {n})")
                else:
                    dims = [f"size({vname}, {n})" for n in range(len(base_decl.dims))]
                _get_size(subroutine, var.name, dims)
                dims = tuple(dims)
            else:
                dims = base_decl.dims
        else:
            dims = var.dims

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
            typename = base_decl.typename
        elif var.typename is not None:
            typename = var.typename
        elif var.ad_target:
            typename = "real"
        else:
            print([var])
            print([var.reference])
            print([base_decl])
            raise RuntimeError(f"typename cannot be identified {var}")

        if base_decl:
            kind = base_decl.kind
        else:
            kind = var.kind

        subroutine.decls.append(
            Declaration(
                name=var.name,
                typename=typename,
                kind=kind,
                dims=dims,
                parameter=base_decl.parameter if base_decl else False,
                init_val=base_decl.init_val if base_decl else None,
                allocatable=allocatable,
                pointer=base_decl.pointer if base_decl else False,
                optional=base_decl.optional if base_decl else False,
                target=base_decl.target if base_decl else False,
                declared_in="routine",
            )
        )

    if reverse:
        targets = VarList(grad_args + mod_ad_vars + save_ad_vars)
    else:
        targets = VarList(grad_args + mod_vars + mod_ad_vars + out_args + save_vars + save_ad_vars)

    #print(render_program(subroutine))
    mod_vars_all = mod_vars + save_vars
    subroutine = subroutine.prune_for(targets, mod_vars_all, decl_map=subroutine.decl_map)

    mod_all_var_names = [v.name_ext() for v in mod_vars]
    for v in mod_ad_vars:
        mod_all_var_names.append(v.name_ext())
    required_vnames = [str(var) for var in subroutine.required_vars() if var.name not in mod_all_var_names]
    if len(required_vnames) > 0:
        _warn(
            warnings,
            {},
            f"{required_vnames} in {subroutine.name}",
            "Required variables are remained",
        )

    uses_pushpop = _contains_pushpop(subroutine) if reverse else False

    if sub_fwd_rev is not None:
        routine_info["fwd_rev_subroutine"] = sub_fwd_rev # save to output this subroutine later

    called_mods = _collect_called_ad_modules(
        [subroutine.content, subroutine.ad_init, subroutine.ad_content],
        routine_map,
        reverse=reverse,
    )

    return subroutine, uses_pushpop, called_mods

def generate_ad(
    in_file,
    out_file=None,
    warn=True,
    search_dirs=None,
    write_fadmod=True,
    fadmod_dir=None,
    mode="both",
):
    """Generate an AD version of ``in_file``.

    If ``out_file`` is ``None`` the generated code is returned as a string.
    When ``out_file`` is provided the code is also written to that path.
    ``fadmod_dir`` selects where ``<module>.fadmod`` files are written (defaults
    to the current working directory).
    """
    modules_org = parser.parse_file(in_file, search_dirs=search_dirs)
    modules = []
    warnings = []

    if search_dirs is None:
        search_dirs = []
    cwd = "."
    if not cwd in search_dirs:
        search_dirs.append(cwd)
    if fadmod_dir is None:
        fadmod_dir = Path.cwd()
    else:
        fadmod_dir = Path(fadmod_dir)

    routine_map = {}
    generic_routines = {}
    for mod_org in modules_org:
        name = mod_org.name
        mod = Module(f"{name}{AD_SUFFIX}")
        mod.uses = Block([Use(name)])

        if mod_org.decls:
            type_map = {}
            for child in mod_org.decls.iter_children():
                ad_code = child.generate_ad([], type_map=type_map)
                if ad_code:
                    if isinstance(child, TypeDef):
                        type_map[child.name] = ad_code[0]
                    if mod.decls is None:
                        mod.decls = Block([])
                    for node in ad_code:
                        mod.decls.append(node)

        mod_vars = [var for var in mod_org.decls.collect_vars()] if mod_org.decls is not None else []
        has_mod_grad_var = any(var.ad_target for var in mod_vars)

        routine_info_fwd = {}
        routine_info_rev = {}
        for r in mod_org.routines:
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
        loaded, vars_info, generic_map = _load_fadmods(used_mods, search_dirs)
        routine_map.update(loaded)
        generic_routines.update(generic_map)
        for name, vars in vars_info.items():
            vars = [v for v in vars if not v.is_constant]
            if vars:
                mod_vars.extend(vars)

        # Generate AD subroutines
        ad_modules_used = set()
        pushpop_used = False
        for routine in mod_org.routines:

            _parse_allocate(routine.content, mod_vars)
            _parse_pointer(routine.content, mod_vars)
            _parse_mpi_calls(routine.content)
            _set_call_intents(routine.content, routine_map, generic_map)

            if mode in ("forward", "both"):
                sub, _, mods_called = _generate_ad_subroutine(
                    mod_org,
                    routine,
                    routine_map,
                    generic_routines,
                    mod_vars,
                    routine_info_fwd[routine.name],
                    warnings,
                    reverse=False,
                )
                if sub is not None:
                    mod.routines.append(sub)
                ad_modules_used.update(mods_called)
            if mode in ("reverse", "both"):
                sub, used, mods_called = _generate_ad_subroutine(
                    mod_org,
                    routine,
                    routine_map,
                    generic_routines,
                    mod_vars,
                    routine_info_rev[routine.name],
                    warnings,
                    reverse=True,
                )
                if sub is not None:
                    mod.routines.append(sub)
                ad_modules_used.update(mods_called)
                if used:
                    pushpop_used = True
                wrapper = routine_info_rev[routine.name].get("fwd_rev_subroutine")
                if wrapper is not None:
                    mod.routines.append(wrapper)
                    pushpop_used = True

        name = mod.name
        for m in sorted(ad_modules_used):
            if m != name:
                mod.uses.append(Use(m))
                mod.uses.append(Use(f"{m}{AD_SUFFIX}"))
        if pushpop_used:
            mod.uses.append(Use("fautodiff_stack"))

        modules.append(render_program(mod))


    code = "\n".join(modules)
    if out_file:
        Path(out_file).write_text(code)
    if write_fadmod:
        for mod_org in modules_org:
            _write_fadmod(
                mod_org,
                routine_map,
                fadmod_dir,
            )
    if warn and warnings:
        for msg in warnings:
            print(f"Warning: {msg}", file=sys.stderr)
    return code


if __name__ == "__main__":
    from .cli import main

    main()
