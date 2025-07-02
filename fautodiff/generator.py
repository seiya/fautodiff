from __future__ import annotations

# Suffix used for all AD related names. Change here if a different suffix is
# desired throughout the project.
AD_SUFFIX = "_ad"

from pathlib import Path
import sys
import re
import json

from .operators import (
    OpReal,
    OpVar,
    OpFunc,
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


def _dims_spec(typ) -> str | None:
    """Return dimension specification string from ``typ`` if present."""
    text = str(typ).strip()
    low = text.lower()
    idx = low.find("dimension")
    if idx == -1:
        return None
    start = low.find("(", idx)
    if start == -1:
        return None
    depth = 0
    end = None
    for i in range(start, len(text)):
        if text[i] == "(":
            depth += 1
        elif text[i] == ")":
            depth -= 1
            if depth == 0:
                end = i
                break
    if end is not None:
        return text[start:end + 1].strip()
    return None




def _contains_pushpop(node) -> bool:
    """Return True if ``node`` or any child is a ``PushPop``."""
    if isinstance(node, PushPop):
        return True
    for child in getattr(node, "iter_children", lambda: [])():
        if _contains_pushpop(child):
            return True
    return False


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
        if info is not None:
            info = dict(info)
            info["module"] = mod_name
            data[r.name] = info

    if not data:
        return

    path = directory / f"{mod_name}.fadmod"
    path.write_text(json.dumps(data, indent=2))


def _prepare_ad_header(routine_org):
    """Prepare AD subroutine header and returns argument info."""

    args = []
    grad_args = []
    out_grad_args = []
    has_grad_input = False

    arg_info = {"args": [], "intents": [], "dims" : [], "type": [], "kind": [],
                "ad_name": None, "ad_args": [], "ad_intents": []}

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
                if grad_intent == "out":
                    out_grad_args.append(var)
                else:
                    has_grad_input = True
    if routine_org.result is not None:
        arg_info["intents"] = arg_info["intents"][:-1]

    ad_name = f"{routine_org.name}{AD_SUFFIX}"
    subroutine = Subroutine(ad_name, [v.name for v in args])
    arg_info["ad_name"] = ad_name
    for var in args:
        subroutine.decls.append(
            Declaration(var.name, var.typename, var.kind, var.dims, var.intent)
        )
        arg_info["ad_args"].append(var.name)
        arg_info["ad_intents"].append(var.intent)

    subroutine.ad_init = Block([])
    subroutine.ad_content = Block([])

    routine_org._ad_routine = subroutine
    routine_org._grad_args = grad_args
    routine_org._out_grad_args = out_grad_args
    routine_org._has_grad_input = has_grad_input

    return arg_info

def _collect_called_ad_modules(blocks, routine_map):
    """Return a set of modules whose AD routines are called within ``blocks``."""

    modules = set()

    def _visit(node):
        if isinstance(node, CallStatement):
            for info in routine_map.values():
                if info.get("ad_name") == node.name and "module" in info:
                    modules.add(info["module"])
                    break
        for child in getattr(node, "iter_children", lambda: [])():
            _visit(child)

    for block in blocks:
        for stmt in block:
            _visit(stmt)
    return modules


def _generate_ad_subroutine(routine_org, routine_map, warnings):
    subroutine = routine_org._ad_routine
    grad_args = routine_org._grad_args
    out_grad_args = routine_org._out_grad_args
    has_grad_input = routine_org._has_grad_input
    ad_block = subroutine.ad_content

    # If no derivative inputs exist, all output gradients remain zero
    if not has_grad_input:
        for arg in out_grad_args:
            lhs = OpVar(arg.name, kind=arg.kind)
            ad_block.append(Assignment(lhs, OpReal("0.0", kind=arg.kind)))
        subroutine.ad_content = ad_block
        return subroutine, False, set()


    def _backward(lhs: OpVar, rhs: Operator, info: dict) -> List[Assignment]:
        if not lhs.ad_target:
            return Block([])
        grad_lhs = lhs.add_suffix(AD_SUFFIX)

        #ad_info = f"{lhs} = {rhs} @ line {info.get('line','?')}"
        #ad_info = f"{lhs} = {rhs}"
        ad_info = info["code"]

        if isinstance(rhs, OpFunc):
            handler = rhs.special_handler(grad_lhs, rhs.args)
            if handler is not None:
                v = rhs.args[0].add_suffix(AD_SUFFIX)
                return [Assignment(v, handler, accumulate=(not v==grad_lhs), ad_info=ad_info)]

        vars = rhs.collect_vars()
        if lhs in vars:
            vars.remove(lhs)
            vars.append(lhs)
        assigns = []
        for var in vars:
            if not var.ad_target:
                continue
            dev = rhs.derivative(var, target=grad_lhs, info=info, warnings=warnings)
            v = var.add_suffix(AD_SUFFIX)
            res = grad_lhs * dev
            assigns.append(Assignment(v, res, accumulate=(not v==grad_lhs), ad_info=ad_info))
        if not lhs in vars:
            assigns.append(ClearAssignment(grad_lhs, ad_info=ad_info))
        return assigns

        raise ValueError(f"Unsupported operation: {type(rhs)}")

    # populate CallStatement intents from routine map
    def _set_call_intents(node):
        if isinstance(node, CallStatement):
            arg_info = routine_map.get(node.name)
            if arg_info is not None and "intents" in arg_info:
                node.intents = list(arg_info["intents"])
        for child in getattr(node, "iter_children", lambda: [])():
            _set_call_intents(child)

    _set_call_intents(routine_org.content)

    saved_vars = []
    ad_code = routine_org.content.convert_assignments(saved_vars, _backward, reverse=True, routine_map=routine_map)[0]
    #print("subroutine: ", subroutine.name) # for debug
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

        # check initialization for AD variables with intent(out)
        vars = []
        for var in grad_args:
            if not var in out_grad_args:
                vars.append(OpVar(var.name))
        ad_code.check_initial(VarList(vars))

        # optimize the AD code
        ad_code = ad_code.prune_for(VarList([OpVar(var.name) for var in grad_args]))

        # check uninitialized AD variables
        vars = ad_code.required_vars(VarList([OpVar(var.name) for var in out_grad_args]), without_savevar=True)
        for name in vars.names():
            if name.endswith(AD_SUFFIX) and not any(v for v in grad_args if v.name == name):
                # AD variables which is not in grads_args (= temporary variables in this subroutine)
                if subroutine.is_declared(name):
                    var = subroutine.get_var(name)
            else: # var which is in out_grad_args
                var = next((var for var in out_grad_args if var.name == name), None)
            if var is not None: # uninitialized AD variables
                if var.dims is not None and len(var.dims) > 0:
                    index = (None,) * len(var.dims)
                else:
                    index = None
                subroutine.ad_init.append(Assignment(OpVar(name, index=index), OpReal(0.0, kind=var.kind)))
                

        # now ad_code is completed
        ad_block.extend(ad_code)

    fw_block = routine_org.content.prune_for(ad_block.required_vars())

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
    )

    return subroutine, uses_pushpop, called_mods


def generate_ad(
    in_file,
    out_file=None,
    warn=True,
    search_dirs=None,
    write_fadmod=True,
    fadmod_dir=None,
):
    """Generate a very small reverse-mode AD version of ``in_file``.

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
        for r in mod_org.routines:
            routine_map[r.name] = _prepare_ad_header(r)

    if search_dirs:
        used_mods = mod_org.find_use_modules()
        routine_map.update(_load_fadmods(used_mods, search_dirs))

    for mod_org in modules_org:
        name = mod_org.name
        pushpop_used = False
        routines = []
        ad_modules_used = set()
        for routine in mod_org.routines:
            sub, used, mods_called = _generate_ad_subroutine(
                routine, routine_map, warnings
            )
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
    import argparse

    parser_arg = argparse.ArgumentParser(
        description="Generate simple reverse-mode AD code"
    )
    parser_arg.add_argument("input", help="path to original Fortran file")
    parser_arg.add_argument(
        "output",
        nargs="?",
        help=(
            "path for generated Fortran file; if omitted, the code is printed"
        ),
    )
    parser_arg.add_argument(
        "--no-warn",
        action="store_true",
        help="suppress warnings about unsupported derivatives",
    )
    parser_arg.add_argument(
        "-I",
        dest="search_dirs",
        action="append",
        default=[],
        help="add directory to .fadmod search path (may be repeated)",
    )
    parser_arg.add_argument(
        "-M",
        dest="fadmod_dir",
        default=None,
        help="directory for .fadmod files (default: current directory)",
    )
    parser_arg.add_argument(
        "--no-fadmod",
        action="store_true",
        help="do not write .fadmod information files",
    )
    args = parser_arg.parse_args()

    code = generate_ad(
        args.input,
        args.output,
        warn=not args.no_warn,
        search_dirs=args.search_dirs,
        write_fadmod=not args.no_fadmod,
        fadmod_dir=args.fadmod_dir,
    )
    if args.output is None:
        print(code, end="")
