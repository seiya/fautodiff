from __future__ import annotations

# Suffix used for all AD related names. Change here if a different suffix is
# desired throughout the project.
AD_SUFFIX = "_ad"
FWD_SUFFIX = "_fwd_ad"
REV_SUFFIX = "_rev_ad"

from pathlib import Path
import sys
import json

from .operators import (
    OpReal,
    OpVar,
)

from .code_tree import (
    Module,
    Subroutine,
    Function,
    Block,
    IfBlock,
    SelectBlock,
    DoLoop,
    Declaration,
    Assignment,
    ClearAssignment,
    SaveAssignment,
    PushPop,
    Use,
    Statement,
    CallStatement,
    render_program,
)

# Ensure other modules use the same AD suffix
from . import code_tree as code_tree
code_tree.AD_SUFFIX = AD_SUFFIX
code_tree.FWD_SUFFIX = FWD_SUFFIX
code_tree.REV_SUFFIX = REV_SUFFIX

from .var_list import (
    VarList
)

from . import parser


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


def _set_call_intents(node, routine_map):
    """Populate ``CallStatement.intents`` using ``routine_map`` recursively."""
    if isinstance(node, CallStatement):
        arg_info = routine_map.get(node.name)
        if arg_info is not None and "intents" in arg_info:
            node.intents = list(arg_info["intents"])
    for child in getattr(node, "iter_children", lambda: [])():
        _set_call_intents(child, routine_map)


def _load_fadmods(mod_names: list[str], search_dirs: list[str]) -> dict:
    """Load routine maps from ``mod_names`` using ``search_dirs``."""
    result = {}
    for mod in mod_names:
        for d in search_dirs:
            path = Path(d) / f"{mod}.fadmod"
            if path.exists():
                try:
                    data = json.loads(path.read_text())
                    result.update(data)
                except Exception:
                    pass
                break
    return result


def _write_fadmod(mod_name: str, routines, routine_map: dict, directory: Path) -> None:
    """Write ``routine_map`` info for ``mod_name`` to ``<mod_name>.fadmod``.

    The stored information now includes the defining module name so that
    other files can ``use`` the appropriate ``*_ad`` module when calling
    generated AD routines.
    """

    data = {}
    for r in routines:
        info = routine_map.get(r.name)
        if info is None:
            continue
        info = dict(info)
        info["module"] = mod_name
        if info.get("name_fwd_ad") is None and info.get("name_rev_ad") is None:
            info["skip"] = True
        data[r.name] = info

    if not data:
        return

    path = directory / f"{mod_name}.fadmod"
    path.write_text(json.dumps(data, indent=2))


def _prepare_fwd_ad_header(routine_org):
    args = []
    grad_args = []
    in_grad_args = []
    out_grad_args = []
    has_grad_input = False

    arg_info = {"args": [], "intents": [], "dims" : [], "type": [], "kind": [],
                "name_fwd_ad": None, "args_fwd_ad": [], "intents_fwd_ad": []}

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
        if arg.ad_target:
            if intent in ("in", "inout"):
                args.append(arg)
            ad_name = f"{name}{AD_SUFFIX}"
            var = OpVar(
                ad_name,
                typename=typ,
                kind=kind,
                dims=dims,
                intent=arg.intent,
                ad_target=True,
                is_constant=arg.is_constant,
            )
            args.append(var)
            grad_args.append(var)
            if intent in ("in", "inout"):
                has_grad_input = True
                in_grad_args.append(var)
            if intent in ("out", "inout"):
                out_grad_args.append(var)
        else:
            if intent in ("in", "inout"):
                args.append(arg)

    if routine_org.result is not None:
        arg_info["intents"] = arg_info["intents"][:-1]

    ad_name = f"{routine_org.name}{FWD_SUFFIX}"
    subroutine = Subroutine(ad_name, [v.name for v in args])
    arg_info["name_fwd_ad"] = ad_name
    for var in args:
        subroutine.decls.append(
            Declaration(var.name, var.typename, var.kind, var.dims, var.intent)
        )
        arg_info["args_fwd_ad"].append(var.name)
        arg_info["intents_fwd_ad"].append(var.intent)

    subroutine.ad_init = Block([])
    subroutine.ad_content = Block([])

    skip = bool(routine_org.directives.get("SKIP")) or len(out_grad_args) == 0
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


def _prepare_rev_ad_header(routine_org):
    """Prepare AD subroutine header and returns argument info."""

    args = []
    grad_args = []
    in_grad_args = []
    out_grad_args = []
    has_grad_input = False

    arg_info = {"args": [], "intents": [], "dims" : [], "type": [], "kind": [],
                "name_rev_ad": None, "args_rev_ad": [], "intents_rev_ad": []}

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
            if arg.ad_target:
                ad_name = f"{name}{AD_SUFFIX}"
                var = OpVar(
                    ad_name,
                    typename=typ,
                    kind=kind,
                    dims=dims,
                    intent="inout",
                    ad_target=True,
                    is_constant=arg.is_constant,
                )
                args.append(var)
                grad_args.append(var)
                in_grad_args.append(var)
                has_grad_input = True
        else:
            args.append(arg)
            if arg.ad_target:
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
            Declaration(var.name, var.typename, var.kind, var.dims, var.intent)
        )
        arg_info["args_rev_ad"].append(var.name)
        arg_info["intents_rev_ad"].append(var.intent)

    subroutine.ad_init = Block([])
    subroutine.ad_content = Block([])

    skip = bool(routine_org.directives.get("SKIP")) or len(out_grad_args) == 0
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


def _generate_fwd_ad_subroutine(routine_org, routine_map, routine_info, warnings):
    if routine_info.get("skip"):
        return None, set()

    subroutine = routine_info["subroutine"]
    grad_args = routine_info["grad_args"]
    in_grad_args = routine_info["in_grad_args"]
    out_grad_args = routine_info["out_grad_args"]
    has_grad_input = routine_info["has_grad_input"]
    ad_block = subroutine.ad_content

    if not has_grad_input:
        for arg in out_grad_args:
            lhs = OpVar(arg.name, kind=arg.kind)
            ad_block.append(Assignment(lhs, OpReal("0.0", kind=arg.kind)))
        subroutine.ad_content = ad_block
        return subroutine, set()

    _set_call_intents(routine_org.content, routine_map)

    saved_vars = []
    ad_code = routine_org.content.generate_ad(
        saved_vars,
        reverse=False,
        assigned_advars=VarList(in_grad_args),
        routine_map=routine_map, warnings=warnings
    )[0]

    if (ad_code is not None) and (not ad_code.is_effectively_empty()):
        for var in ad_code.assigned_vars(without_savevar=True):
            name = var.name
            if name.endswith(AD_SUFFIX):
                found = False
                for arg in grad_args:
                    if arg.name == name:
                        found = True
                        break
                if found:
                    continue
                v_org = routine_org.get_var(name.removesuffix(AD_SUFFIX))
                base_decl = routine_org.decls.find_by_name(name.removesuffix(AD_SUFFIX))
                if v_org is not None and not subroutine.is_declared(name):
                    subroutine.decls.append(
                        Declaration(
                            name,
                            v_org.typename,
                            v_org.kind,
                            v_org.dims,
                            None,
                            base_decl.parameter if base_decl else False,
                            init=base_decl.init if base_decl else None,
                        )
                    )

        ad_code = ad_code.prune_for(VarList([OpVar(var.name) for var in grad_args]))

        vars = ad_code.required_vars(VarList([OpVar(var.name) for var in out_grad_args]), without_savevar=True)
        for name in vars.names():
            if not name.endswith(AD_SUFFIX):
                continue
            if not any(v for v in grad_args if v.name == name):
                # AD variables which is not in grads_args (= temporary variables in this subroutine)
                if subroutine.is_declared(name):
                    var = subroutine.get_var(name)
            else: # var in grad_args
                if any(v for v in in_grad_args if v.name == name):
                    continue
                var = next(var for var in out_grad_args if var.name == name)
            if var is not None: # uninitialized AD variables
                if var.dims is not None and len(var.dims) > 0:
                    index = (None,) * len(var.dims)
                else:
                    index = None
                subroutine.ad_init.append(Assignment(OpVar(name, index=index), OpReal(0.0, kind=var.kind)))

        ad_block.extend(ad_code)

    vars = []
    for var in subroutine.collect_vars():
        if var.name not in vars:
            vars.append(var.name)
    for var in vars:
        if subroutine.decls.find_by_name(var) is None:
            decl = routine_org.decls.find_by_name(var)
            if decl is None and var.endswith(AD_SUFFIX):
                base = var.removesuffix(AD_SUFFIX)
                base_decl = routine_org.decls.find_by_name(base)
                if base_decl is not None:
                    decl = Declaration(
                        var,
                        base_decl.typename,
                        base_decl.kind,
                        base_decl.dims,
                        None,
                        base_decl.parameter,
                        init=base_decl.init,
                    )
            if decl is not None:
                if decl.intent is not None and decl.intent == "out":
                    decl.intent = None
                subroutine.decls.append(decl)

    for var in reversed(saved_vars):
        v_org = None
        base_decl = None
        if var.reference is not None:
            try:
                v_org = routine_org.get_var(var.reference.name)
                base_decl = routine_org.decls.find_by_name(var.reference.name)
            except ValueError as e:
                ad_block.extend(ad_code)
                print("".join(subroutine.render()))
                raise
        if var.dims is not None:
            dims = v_org.dims
        elif v_org is not None:
            if var.reduced_dims is not None:
                dims = []
                for i, idx in enumerate(v_org.dims):
                    if not i in var.reduced_dims:
                        dims.append(idx)
                if len(dims) == 0:
                    dims = None
                else:
                    dims = tuple(dims)
            else:
                dims = v_org.dims
        else:
            dims = None
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
                var.name,
                typename,
                kind,
                dims,
                None,
                base_decl.parameter if base_decl else False,
                init=base_decl.init if base_decl else None,
            )
        )

    subroutine = subroutine.prune_for(VarList([OpVar(var.name) for var in grad_args]))

    required_vnames = [str(var) for var in subroutine.required_vars()]
    if len(required_vnames) > 0:
        _warn(warnings, {}, f"{required_vnames} in {subroutine.name}", "Required variables are remained")

    called_mods = _collect_called_ad_modules(
        [subroutine.content, subroutine.ad_init, subroutine.ad_content],
        routine_map,
        reverse=False,
    )

    return subroutine, called_mods


def _generate_rev_ad_subroutine(routine_org, routine_map, routine_info, warnings):
    if routine_info.get("skip"):
        return None, False, set()

    subroutine = routine_info["subroutine"]
    grad_args = routine_info["grad_args"]
    in_grad_args = routine_info["in_grad_args"]
    out_grad_args = routine_info["out_grad_args"]
    has_grad_input = routine_info["has_grad_input"]
    ad_block = subroutine.ad_content

    # If no derivative inputs exist, all output gradients remain zero
    if not has_grad_input:
        for arg in out_grad_args:
            lhs = OpVar(arg.name, kind=arg.kind)
            ad_block.append(Assignment(lhs, OpReal("0.0", kind=arg.kind)))
        subroutine.ad_content = ad_block
        return subroutine, False, set()

    # populate CallStatement intents from routine map
    _set_call_intents(routine_org.content, routine_map)

    #print("subroutine: ", subroutine.name) # for debug

    saved_vars = []
    ad_code = routine_org.content.generate_ad(
        saved_vars, reverse=True, routine_map=routine_map, warnings=warnings
    )[0]

    #print(render_program(ad_code)) # for debug

    if (ad_code is not None) and (not ad_code.is_effectively_empty()):

        # check undeclared reference for AD variables
        for var in ad_code.assigned_vars(without_savevar=True):
            name = var.name
            if name.endswith(AD_SUFFIX):  # only for AD variables
                found = False
                for arg in grad_args:
                    if arg.name == name:
                        found = True
                        break
                if found:
                    continue # already declared
                v_org = routine_org.get_var(name.removesuffix(AD_SUFFIX))
                base_decl = routine_org.decls.find_by_name(name.removesuffix(AD_SUFFIX))
                if v_org is not None and not subroutine.is_declared(name):
                    subroutine.decls.append(
                        Declaration(
                            name,
                            v_org.typename,
                            v_org.kind,
                            v_org.dims,
                            None,
                            base_decl.parameter if base_decl else False,
                            init=base_decl.init if base_decl else None,
                        )
                    )

        # check initialization for AD variables
        ad_code.check_initial(VarList(in_grad_args))

        # optimize the AD code
        ad_code = ad_code.prune_for(VarList([OpVar(var.name) for var in grad_args]))

        # check uninitialized AD variables
        vars = ad_code.required_vars(VarList([OpVar(var.name) for var in out_grad_args]), without_savevar=True)
        for name in vars.names():
            if not name.endswith(AD_SUFFIX):
                continue
            if not any(v for v in grad_args if v.name == name):
                # AD variables which is not in grads_args (= temporary variables in this subroutine)
                if subroutine.is_declared(name):
                    var = subroutine.get_var(name)
            else: # var in grad_args
                if any(v for v in in_grad_args if v.name == name):
                    continue
                var = next(var for var in out_grad_args if var.name == name)
            if var is not None: # uninitialized AD variables
                if var.dims is not None and len(var.dims) > 0:
                    index = (None,) * len(var.dims)
                else:
                    index = None
                subroutine.ad_init.append(Assignment(OpVar(name, index=index), OpReal(0.0, kind=var.kind)))

        # now ad_code is completed
        ad_block.extend(ad_code)

    #print(render_program(routine_org.content)) # debug
    fw_block = routine_org.content.prune_for(ad_block.required_vars())
    #print(render_program(fw_block)) # debug

    flag = True
    while flag:
        last = fw_block.last()
        first = ad_block.first()
        if isinstance(last, SaveAssignment) and isinstance(first, SaveAssignment) and last.var==first.var and last.load != first.load:
            fw_block.remove_child(last)
            ad_block.remove_child(first)
        else:
            flag = False

    if not fw_block.is_effectively_empty():
        subroutine.content.extend(fw_block)

    vars = []
    for var in subroutine.collect_vars():
        if var.name not in vars:
            vars.append(var.name)
    for var in vars:
        if subroutine.decls.find_by_name(var) is None:
            decl = routine_org.decls.find_by_name(var)
            if decl is None and var.endswith(AD_SUFFIX):
                base = var.removesuffix(AD_SUFFIX)
                base_decl = routine_org.decls.find_by_name(base)
                if base_decl is not None:
                    decl = Declaration(
                        var,
                        base_decl.typename,
                        base_decl.kind,
                        base_decl.dims,
                        None,
                        base_decl.parameter,
                        init=base_decl.init,
                    )
            if decl is not None:
                if decl.intent is not None and decl.intent == "out":
                    decl.intent = None
                subroutine.decls.append(decl)

    for var in reversed(saved_vars):
        v_org = None
        base_decl = None
        if var.reference is not None:
            try:
                v_org = routine_org.get_var(var.reference.name)
                base_decl = routine_org.decls.find_by_name(var.reference.name)
            except ValueError as e:
                ad_block.extend(ad_code)
                print("".join(subroutine.render()))
                raise
        if var.dims is not None:
            dims = v_org.dims
        elif v_org is not None:
            if var.reduced_dims is not None:
                dims = []
                for i, idx in enumerate(v_org.dims):
                    if not i in var.reduced_dims:
                        dims.append(idx)
                if len(dims) == 0:
                    dims = None
                else:
                    dims = tuple(dims)
            else:
                dims = v_org.dims
        else:
            dims = None
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
                var.name,
                typename,
                kind,
                dims,
                None,
                base_decl.parameter if base_decl else False,
                init=base_decl.init if base_decl else None,
            )
        )

    subroutine = subroutine.prune_for(VarList([OpVar(var.name) for var in grad_args]))

    required_vnames = [str(var) for var in subroutine.required_vars()]
    if len(required_vnames) > 0:
        _warn(warnings, {}, f"{required_vnames} in {subroutine.name}", "Required variables are remained")

    uses_pushpop = _contains_pushpop(subroutine)

    called_mods = _collect_called_ad_modules(
        [subroutine.content, subroutine.ad_init, subroutine.ad_content],
        routine_map,
        reverse=True,
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
    modules_org = parser.parse_file(in_file)
    modules = []
    warnings = []

    if search_dirs is None:
        search_dirs = []
    if fadmod_dir is None:
        fadmod_dir = Path.cwd()
    else:
        fadmod_dir = Path(fadmod_dir)

    routine_map = {}
    for mod_org in modules_org:
        routine_info_fwd = {}
        routine_info_rev = {}
        for r in mod_org.routines:
            if mode in ("forward", "both"):
                routine_info = _prepare_fwd_ad_header(r)
                routine_info_fwd[r.name] = routine_info
                routine_map[r.name] = routine_info["arg_info"]
            if mode in ("reverse", "both"):
                routine_info = _prepare_rev_ad_header(r)
                routine_info_rev[r.name] = routine_info
                if r.name in routine_map:
                    routine_map[r.name].update(routine_info["arg_info"])
                else:
                    routine_map[r.name] = routine_info["arg_info"]

        if search_dirs:
            used_mods = mod_org.find_use_modules()
            loaded = _load_fadmods(used_mods, search_dirs)
            routine_map.update(loaded)

        name = mod_org.name
        pushpop_used = False
        routines = []
        ad_modules_used = set()

        for routine in mod_org.routines:
            if mode in ("forward", "both"):
                sub, mods_called = _generate_fwd_ad_subroutine(
                    routine, routine_map, routine_info_fwd[routine.name], warnings
                )
                if sub is not None:
                    routines.append(sub)
                ad_modules_used.update(mods_called)
            if mode in ("reverse", "both"):
                sub, used, mods_called = _generate_rev_ad_subroutine(
                    routine, routine_map, routine_info_rev[routine.name], warnings
                )
                if sub is not None:
                    routines.append(sub)
                ad_modules_used.update(mods_called)
                if used:
                    pushpop_used = True
        mod = Module(f"{name}{AD_SUFFIX}")
        mod.body.append(Use(name))
        if pushpop_used:
            mod.body.append(Use("fautodiff_data_storage"))
        inserted = False
        for child in mod_org.body.iter_children():
            if (
                not inserted
                and not isinstance(child, Use)
                and isinstance(child, Statement)
                and child.body.lower().startswith("implicit none")
            ):
                for m in sorted(ad_modules_used):
                    if m != name:
                        mod.body.append(Use(f"{m}{AD_SUFFIX}"))
                inserted = True
            mod.body.append(child)
        if not inserted:
            for m in sorted(ad_modules_used):
                if m != name:
                    mod.body.append(Use(f"{m}{AD_SUFFIX}"))
        for sub in routines:
            mod.routines.append(sub)
        modules.append(render_program(mod))

    code = "\n".join(modules)
    if out_file:
        Path(out_file).write_text(code)
    if write_fadmod:
        for mod_org in modules_org:
            _write_fadmod(
                mod_org.name,
                mod_org.routines,
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
