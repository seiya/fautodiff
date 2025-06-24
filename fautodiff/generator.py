from __future__ import annotations

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
    SaveAssignment,
    Statement,
    render_program,
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

def _generate_ad_subroutine(routine_org, warnings):
    # Collect information of aruguments
    args = []
    out_grad_args = []
    grad_args = []
    has_grad_input = False
    for arg in routine_org.arg_vars():
        name = arg.name
        typ = arg.typename
        dims = arg.dims
        intent = arg.intent
        if intent == "out":
            if arg.ad_target:
                ad_name = f"{name}_ad"
                var = Variable(ad_name, typ, arg.kind, dims, "inout")
                args.append(var)
                grad_args.append(var)
                has_grad_input = True
        else:
            args.append(arg)
            if arg.ad_target:
                ad_name = f"{name}_ad"
                grad_int = {
                    "in": "out",
                    "inout": "inout",
                }.get(intent, "out")
                var = Variable(ad_name, typ, arg.kind, dims, grad_int)
                args.append(var)
                grad_args.append(var)
                if grad_int == "out":
                    out_grad_args.append(var)
                else:
                    has_grad_input = True

    # Create Subroutine node for AD
    name = routine_org.name
    ad_name = f"{name}_ad"
    arg_names = []
    for arg in args:
        arg_names.append(arg.name)
    subroutine = Subroutine(ad_name, arg_names)
    for arg in args:
        subroutine.decls.append(arg.to_decl())
    init_block = Block([])
    ad_block = Block([])
    subroutine.ad_init = init_block
    subroutine.ad_content = ad_block

    # If no derivative inputs exist, all output gradients remain zero
    if not has_grad_input:
        for arg in out_grad_args:
            lhs = OpVar(arg.name, kind=arg.kind)
            ad_block.append(Assignment(lhs, OpReal("0.0", kind=arg.kind)))
        subroutine.ad_content = ad_block
        return subroutine

    # If there are no input gradients to propagate we can exit early
    if not out_grad_args:
        subroutine.ad_content = ad_block
        return subroutine

    def _backward(lhs: OpVar, rhs: Operator, info: dict) -> List[Assignment]:
        if not lhs.is_real:
            return Block([])
        grad_lhs = lhs.add_suffix("_ad")

        #ad_info = f"{lhs} = {rhs} @ line {info.get('line','?')}"
        ad_info = f"{lhs} = {rhs}"

        if isinstance(rhs, OpFunc):
            handler = rhs.special_handler(grad_lhs, rhs.args)
            if handler is not None:
                v = rhs.args[0].add_suffix("_ad")
                return [Assignment(v, handler, accumulate=(not v==grad_lhs), ad_info=ad_info)]

        vars = rhs.collect_vars()
        assigns = []
        for var in vars:
            if not var.is_real:
                continue
            dev = rhs.derivative(var, target=grad_lhs, info=info, warnings=warnings)
            v = var.add_suffix("_ad")
            res = grad_lhs * dev
            assigns.append(Assignment(v, res, accumulate=(not v==grad_lhs), ad_info=ad_info))
        if not lhs in vars:
            assigns.append(Assignment(grad_lhs, OpReal("0.0", kind=grad_lhs.kind), accumulate=False, ad_info=ad_info))
        return assigns

        raise ValueError(f"Unsupported operation: {type(rhs)}")

    saved_vars = []
    ad_code = routine_org.content.convert_assignments(saved_vars, _backward, reverse=True)[0]
    #print("subroutine: ", subroutine.name) # for debug
    if (ad_code is not None) and (not ad_code.is_effectively_empty()):
        # check undefined reference
        vars = []
        for var in ad_code.assigned_vars(without_savevar=True):
            name = var.name
            if name.endswith("_ad"):
                name_org = name.removesuffix("_ad")
                found = False
                for arg in grad_args:
                    if arg.name == name:
                        found = True
                        break
                if found:
                    continue
            else:
                continue
            v_org = routine_org.get_var(name_org)
            v = Variable(name=name, typename=v_org.typename, kind=v_org.kind, dims=v_org.dims)
            if not v in vars:
                vars.append(v)
                subroutine.decls.append(v.to_decl())
        for var in out_grad_args:
            vars.append(var)
        for var in vars:
            ret = ad_code.check_initial(var.name)
            if ret == -1:
                if var.dims is not None and len(var.dims) > 0:
                    index = (None,) * len(var.dims)
                else:
                    index = None
                subroutine.ad_init.append(Assignment(OpVar(var.name, index=index), OpReal(0.0, kind=var.kind)))
        ad_code = ad_code.prune_for([OpVar(var.name) for var in grad_args])
        # check undefined output variables
        vars = ad_code.required_vars([OpVar(var.name) for var in out_grad_args])
        vars = subroutine.ad_init.required_vars(vars)
        var_names = []
        for var in vars:
            if var.name not in vars:
                var_names.append(var.name)
        for name in var_names:
            var = next((var for var in out_grad_args if var.name == name), None)
            if var is not None:
                if var.dims is not None and len(var.dims) > 0:
                    index = (None,) * len(var.dims)
                else:
                    index = None
                    subroutine.ad_init.append(Assignment(OpVar(var.name, index=index), OpReal(0.0, kind=var.kind)))
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
            if decl is not None:
                if decl.intent is not None and decl.intent == "out":
                    decl.intent = None
                subroutine.decls.append(decl)

    for sa in reversed(saved_vars):
        try:
            v_org = routine_org.get_var(sa.var.name)
        except ValueError as e:
            ad_block.extend(ad_code)
            print("".join(subroutine.render()))
            raise
        if sa.reduced_dims:
            dims = []
            for i, idx in enumerate(v_org.dims):
                if not i in sa.reduced_dims:
                    dims.append(idx)
            if len(dims) == 0:
                dims = None
            else:
                dims = tuple(dims)
        else:
            dims = v_org.dims
        v = Variable(name=sa.tmpvar.name, typename=v_org.typename, kind=v_org.kind, dims=dims)
        subroutine.decls.append(v.to_decl())

    subroutine = subroutine.prune_for([OpVar(var.name) for var in grad_args])

    required_vnames = [str(var) for var in subroutine.required_vars()]
    if len(required_vnames) > 0:
        _warn(warnings, {}, f"{required_vnames} in {subroutine.name}", "Required variables are remained")

    return subroutine


def generate_ad(in_file, out_file=None, warn=True):
    """Generate a very small reverse-mode AD version of ``in_file``.

    If ``out_file`` is ``None`` the generated code is returned as a string.
    When ``out_file`` is provided the code is also written to that path.
    """
    modules_org = parser.parse_file(in_file)
    modules = []
    warnings = []
    for mod_org in modules_org:
        name = mod_org.name
        mod = Module(f"{name}_ad")
        #mod.body.append(Statement(f"use {name}"))
        mod.body.append(Statement("implicit none"))
        for routine in mod_org.routines:
            mod.routines.append(_generate_ad_subroutine(routine, warnings))
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
