from __future__ import annotations

# Suffix used for all AD related names. Change here if a different suffix is
# desired throughout the project.
AD_SUFFIX = "_ad"

from pathlib import Path
import sys
import re

from .operators import (
    OpReal,
    OpVar,
    OpFunc,
)

from .code_tree import (
    Variable,
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


def _split_type(typ):
    """Return base type and dimension spec from a declaration string."""
    text = str(typ).strip()
    dims = _dims_spec(text)
    if dims:
        idx = text.lower().find("dimension")
        base = text[:idx].rstrip().rstrip(',')
        remainder = text[idx + len("dimension") :]
        pos = remainder.lower().find(dims.lower())
        if pos != -1:
            remainder = remainder[pos + len(dims) :].strip()
            if remainder.startswith(','):
                remainder = remainder[1:].strip()
            if remainder:
                base = f"{base}, {remainder}" if base else remainder
        return base.strip(), dims
    return text, None




def _is_integer_type(typ) -> bool:
    """Return ``True`` if ``typ`` represents an integer type."""
    if typ is None:
        return False
    return str(typ).strip().lower().startswith("integer")


def _sanitize_var(name: str) -> str:
    """Return a string safe for use in generated variable names."""
    return re.sub(r"[^0-9A-Za-z_]", "_", name)


def _find_index_expr(expr, var, all_indices=False):
    """Return index expressions for ``var`` in ``expr``.

    If ``all_indices`` is ``True`` a list of unique index expressions is
    returned.  Otherwise the function returns the single unique expression if
    one is found, or ``None`` when there are multiple different expressions.
    """

    found = []

    def _walk(node):
        if isinstance(node, Fortran2003.Part_Ref):
            name = node.items[0].tofortran().lower()
            if name == var.lower():
                items = []
                for arg in getattr(node.items[1], "items", []):
                    if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                        arg = arg.items[1]
                    items.append(arg.tofortran())
                found.append(", ".join(items))
                return
        for itm in getattr(node, "items", []):
            if not isinstance(itm, str):
                _walk(itm)

    _walk(expr)

    if all_indices:
        result = []
        for idx in found:
            if idx not in result:
                result.append(idx)
        return result

    uniq = set(found)
    if len(uniq) == 1:
        return uniq.pop()
    return None


def _contains_pushpop(node) -> bool:
    """Return True if ``node`` or any child is a ``PushPop``."""
    if isinstance(node, PushPop):
        return True
    for child in getattr(node, "iter_children", lambda: [])():
        if _contains_pushpop(child):
            return True
    return False


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
                var = Variable(ad_name, typ, kind, dims, "inout")
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
                var = Variable(ad_name, typ, kind, dims, grad_intent)
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
        subroutine.decls.append(var.to_decl())
        arg_info["ad_args"].append(var.name)
        arg_info["ad_intents"].append(var.intent)

    subroutine.ad_init = Block([])
    subroutine.ad_content = Block([])

    routine_org._ad_routine = subroutine
    routine_org._grad_args = grad_args
    routine_org._out_grad_args = out_grad_args
    routine_org._has_grad_input = has_grad_input

    return arg_info

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
        return subroutine, False


    def _backward(lhs: OpVar, rhs: Operator, info: dict) -> List[Assignment]:
        if not lhs.is_real:
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
            if not var.is_real:
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
                if v_org is not None:
                    v = Variable(name=name, typename=v_org.typename, kind=v_org.kind, dims=v_org.dims)
                    if not subroutine.is_declared(name):
                        subroutine.decls.append(v.to_decl())

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
                    )
            if decl is not None:
                if decl.intent is not None and decl.intent == "out":
                    decl.intent = None
                subroutine.decls.append(decl)

    for var in reversed(saved_vars):
        v_org = None
        if var.reference is not None:
            try:
                v_org = routine_org.get_var(var.reference.name)
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
        elif var.is_real:
            typename = "real"
        else:
            raise RuntimeError("typename cannot be identified")
        if v_org:
            kind = v_org.kind
        else:
            kind = var.kind
        var = Variable(name=var.name, typename=typename, kind=kind, dims=dims)
        subroutine.decls.append(var.to_decl())

    subroutine = subroutine.prune_for(VarList([OpVar(var.name) for var in grad_args]))

    required_vnames = [str(var) for var in subroutine.required_vars()]
    if len(required_vnames) > 0:
        _warn(warnings, {}, f"{required_vnames} in {subroutine.name}", "Required variables are remained")

    uses_pushpop = _contains_pushpop(subroutine)

    return subroutine, uses_pushpop


def generate_ad(in_file, out_file=None, warn=True):
    """Generate a very small reverse-mode AD version of ``in_file``.

    If ``out_file`` is ``None`` the generated code is returned as a string.
    When ``out_file`` is provided the code is also written to that path.
    """
    modules_org = parser.parse_file(in_file)
    modules = []
    warnings = []

    routine_map = {}
    for mod_org in modules_org:
        for r in mod_org.routines:
            routine_map[r.name] = _prepare_ad_header(r)

    for mod_org in modules_org:
        name = mod_org.name
        pushpop_used = False
        routines = []
        for routine in mod_org.routines:
            sub, used = _generate_ad_subroutine(routine, routine_map, warnings)
            routines.append(sub)
            if used:
                pushpop_used = True

        mod = Module(f"{name}{AD_SUFFIX}")
        if pushpop_used:
            mod.body.append(Statement("use fautodiff_data_storage"))
        #mod.body.append(Statement(f"use {name}"))
        mod.body.append(Statement("implicit none"))
        for sub in routines:
            mod.routines.append(sub)
        modules.append(render_program(mod))

    code = "\n".join(modules)
    if out_file:
        Path(out_file).write_text(code)
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
    args = parser_arg.parse_args()

    code = generate_ad(args.input, args.output, warn=not args.no_warn)
    if args.output is None:
        print(code, end="")
