from __future__ import annotations

# Suffix used for all AD related names. Change here if a different suffix is
# desired throughout the project.
AD_SUFFIX = "_ad"
FWD_SUFFIX = "_fwd_ad"
REV_SUFFIX = "_rev_ad"

import json
import sys
from pathlib import Path
from typing import Optional, List, Tuple

# Ensure other modules use the same AD suffix
from . import code_tree as code_tree
from .code_tree import (
    Node,
    Assignment,
    Block,
    CallStatement,
    ClearAssignment,
    Declaration,
    DoLoop,
    Routine,
    Function,
    IfBlock,
    Module,
    PushPop,
    SaveAssignment,
    SelectBlock,
    Statement,
    Subroutine,
    Use,
    render_program,
)
from .operators import (
    AryIndex,
    OpReal,
    OpVar,
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


def _add_fwd_rev_calls(node, routine_map, generic_map):
    """Add *_fwd_rev_ad wrapper routine before the call when available."""
    if isinstance(node, CallStatement):
        arg_info = Node.get_arg_info(node, routine_map, generic_map)
        if arg_info is not None and arg_info.get("name_fwd_rev_ad"):
            node.parent.insert_before(node.get_id(), CallStatement(arg_info["name_fwd_rev_ad"], node.args))
    for child in list(getattr(node, "iter_children", lambda: [])()):
        _add_fwd_rev_calls(child, routine_map, generic_map)


def _make_fwd_rev_wrapper(routine_org: Routine, vars: list[str]) -> Subroutine:
    """Return a forward wrapper that saves module variables."""
    sub = Subroutine(f"{routine_org.name}_fwd_rev_ad", list(routine_org.args))
    for arg in routine_org.arg_vars():
        sub.decls.append(
            Declaration(
                name=arg.name,
                typename=arg.typename,
                kind=arg.kind,
                char_len=arg.char_len,
                dims=arg.dims,
                intent=arg.intent,
                allocatable=arg.allocatable,
                pointer=arg.pointer,
                optional=arg.optional,
                declared_in="routine",
            )
        )
    for name in vars:
        sub.content.append(PushPop(OpVar(name), sub.get_id()))
    sub.ad_init = Block([])
    sub.ad_content = Block([])
    return sub


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
        if info.get("cross_mod_vars"):
            info["cross_mod_vars"] = list(info["cross_mod_vars"])
        if info.get("name_fwd_rev_ad"):
            info["name_fwd_rev_ad"] = info["name_fwd_rev_ad"]
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

    has_mod_grad_input = False
    mod_ad_vars = []
    for var in mod_vars:
        if var.ad_target:
            mod_ad_vars.append(var.add_suffix(AD_SUFFIX))
            has_mod_grad_input = True
    if reverse and not has_grad_input and not has_mod_grad_input:
        for arg in out_grad_args:
            lhs = OpVar(arg.name, kind=arg.kind)
            ad_block.append(Assignment(lhs, OpReal("0.0", kind=arg.kind)))
        subroutine.ad_content = ad_block
        return subroutine, False, set()

    _set_call_intents(routine_org.content, routine_map, generic_map)

    saved_vars: List[OpVar] = []
    if reverse:
        assigned_advars = None
    else:
        assigned_advars = VarList(in_grad_args + mod_ad_vars)
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
                v_org = routine_org.get_var(name.removesuffix(AD_SUFFIX))
                base_decl = routine_org.decls.find_by_name(name.removesuffix(AD_SUFFIX))
                if v_org is not None and not subroutine.is_declared(name):
                    subroutine.decls.append(
                        Declaration(
                            name=name,
                            typename=v_org.typename,
                            kind=v_org.kind,
                            dims=v_org.dims,
                            parameter=base_decl.parameter if base_decl else False,
                            init_val=base_decl.init_val if base_decl else None,
                            allocatable=base_decl.allocatable if base_decl else False,
                            pointer=base_decl.pointer if base_decl else False,
                            optional=base_decl.optional if base_decl else False,
                            declared_in="routine",
                        )
                    )

        if reverse:
            targets = VarList(in_grad_args + mod_ad_vars)
            ad_code.check_initial(targets)

        if reverse:
            targets = VarList(grad_args + mod_ad_vars)
        else:
            targets = VarList(grad_args + mod_ad_vars + out_args)
        #print("Targets:", targets) # for debugging
        #print(subroutine.name)
        ad_code = ad_code.prune_for(targets, mod_vars)
        #print(render_program(ad_code)) # for debugging

    if (ad_code is not None) and (not ad_code.is_effectively_empty()):
        vars = ad_code.required_vars(
            VarList([OpVar(var.name) for var in out_grad_args]),
            without_savevar=True
        )
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
            if var is not None:
                if var.dims is not None and len(var.dims) > 0:
                    index = AryIndex((None,) * len(var.dims))
                else:
                    index = None
                subroutine.ad_init.append(
                    Assignment(OpVar(name, index=index), OpReal(0.0, kind=var.kind))
                )

    for node in ad_code:
        if not node.is_effectively_empty():
            ad_block.append(node)

    if reverse:
        targets = ad_code.required_vars()
        #print(render_program(ad_code))
        #print("Targets:", targets)
        fw_block = Block(routine_org.content.set_for_exitcycle(None))
        fw_block = fw_block.prune_for(targets, mod_vars)
        #print(render_program(fw_block))

        assigned = fw_block.assigned_vars(without_savevar=True)
        assigned = ad_code.assigned_vars(assigned, without_savevar=True)
        required = ad_code.required_vars(without_savevar=True)
        required = fw_block.required_vars(required, without_savevar=True)
        common = assigned & required
        mod_names = [v.name for v in mod_vars]
        cross_vars = sorted([v.name for v in common if v.name in mod_names and not v.name.endswith(AD_SUFFIX)])
        routine_info["cross_mod_vars"] = cross_vars
        routine_info["arg_info"]["cross_mod_vars"] = cross_vars
        if cross_vars:
            name_fwd_rev = f"{routine_org.name}_fwd_rev_ad"
            routine_info["arg_info"]["name_fwd_rev_ad"] = name_fwd_rev
            routine_map[routine_org.name]["cross_mod_vars"] = cross_vars
            routine_map[routine_org.name]["name_fwd_rev_ad"] = name_fwd_rev
            pops = [PushPop(OpVar(n), subroutine.get_id()).to_load() for n in cross_vars]
            for pop in reversed(pops):
                subroutine.content.insert_begin(pop)
            routine_info["fwd_rev_subroutine"] = _make_fwd_rev_wrapper(routine_org, cross_vars)

        _add_fwd_rev_calls(fw_block, routine_map, generic_map)

        flag = True
        while flag:
            last = fw_block.last()
            first = ad_block.first()
            if (isinstance(last, SaveAssignment) and isinstance(first, SaveAssignment) and
                last.var.name == first.var.name and last.id == first.id and last.load != first.load):
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
            index_new = []
            var.reduced_dims = []
            for i, idx in enumerate(var.index):
                if isinstance(idx, OpVar) and idx.name in common_index:
                    var.reduced_dims.append(i)
                else:
                    index_new.append(idx)
            var.index = AryIndex(index_new)

        v_org = None
        base_decl = None
        if var.reference is not None:
            try:
                ref_name = var.reference.name
                v_org = routine_org.get_var(ref_name)
                base_decl = routine_org.decls.find_by_name(ref_name)
                if base_decl is None and routine_org.decl_map is not None:
                    base_decl = routine_org.decl_map.get(ref_name)
            except ValueError:
                ad_block.extend(ad_code)
                print("".join(subroutine.render()))
                raise
        if var.dims is not None:
            dims = var.dims
        elif v_org is not None:
            dims = v_org.dims
        else:
            dims = None
        if dims is not None and var.reduced_dims is not None:
            dims_new = []
            for i, idx in enumerate(dims):
                if i not in var.reduced_dims:
                    dims_new.append(idx)
            if len(dims_new) == 0:
                dims = None
            else:
                dims = tuple(dims_new)
        if v_org:
            typename = v_org.typename
        elif var.typename is not None:
            typename = var.typename
        elif var.ad_target:
            typename = "real"
        else:
            raise RuntimeError("typename cannot be identified")
        if v_org:
            kind = v_org.kind
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
                allocatable=base_decl.allocatable if base_decl else False,
                pointer=base_decl.pointer if base_decl else False,
                optional=base_decl.optional if base_decl else False,
                declared_in="routine",
            )
        )

    if reverse:
        prune_targets = VarList(grad_args + mod_ad_vars)
    else:
        prune_targets = VarList(grad_args + mod_vars + mod_ad_vars + out_args)
    subroutine = subroutine.prune_for(prune_targets, mod_vars, decl_map=subroutine.decl_map)

    mod_var_names = [var.name for var in mod_vars + mod_ad_vars]
    required_vnames = [str(var) for var in subroutine.required_vars() if var.name not in mod_var_names]
    if len(required_vnames) > 0:
        _warn(
            warnings,
            {},
            f"{required_vnames} in {subroutine.name}",
            "Required variables are remained",
        )

    uses_pushpop = _contains_pushpop(subroutine) if reverse else False

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
        mod.uses.append(Use(name))

        for child in mod_org.body.iter_children():
            if not isinstance(child, Use):
                mod.body.append(child)

        mod_vars = [var for var in mod_org.decls.collect_vars() if not var.is_constant] if mod_org.decls is not None else []
        has_mod_grad_var = False
        for var in mod_vars:
            if var.ad_target:
                has_mod_grad_var = True
                if mod.decls is None:
                    mod.decls = Block([])
                decl = mod_org.decls.find_by_name(var.name)
                init_val = (
                    str(OpReal("0.0", kind=decl.kind))
                    if not (decl.allocatable or decl.pointer)
                    else None
                )
                mod.decls.append(Declaration(f"{var.name}{AD_SUFFIX}",
                                             typename = decl.typename,
                                             kind = decl.kind,
                                             char_len=decl.char_len,
                                             dims = decl.dims,
                                             init_val = init_val,
                                             #access = decl.access,
                                             allocatable = decl.allocatable,
                                             pointer = decl.pointer,
                                             optional = decl.optional,
                                             declared_in = "module",
                                             )
                                )

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
            mod.uses.append(Use("fautodiff_data_storage"))

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
