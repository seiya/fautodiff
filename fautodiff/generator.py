from __future__ import annotations

from pathlib import Path
import sys
import re

from .operators import (
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
    render_program,
)

from . import parser
from .intrinsic_rules import (
    DERIVATIVE_TEMPLATES,
    NONDIFF_INTRINSICS,
    SPECIAL_HANDLERS,
)


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


def _assignment_parts(stmt, warn_info=None, warnings=None):
    """Return mapping of variables to partial derivatives and index info."""
    rhs = stmt.items[2]
    all_names = []
    _collect_names(rhs, all_names, unique=False)
    names = []
    _collect_names(rhs, names)
    parts = {}
    index_map = {}
    index_vars = set()
    for name in names:
        idx_list = _find_index_expr(rhs, name, all_indices=True)
        if idx_list:
            for idx in idx_list:
                deriv = _derivative(rhs, name, idx, warn_info, warnings)
                if deriv != "0.0":
                    key = f"{name}@{idx}"
                    parts[key] = deriv
                    index_map[key] = (name, idx)
                for var in re.findall(r"[A-Za-z_][A-Za-z0-9_]*", idx):
                    index_vars.add(var)
        else:
            deriv = _derivative(rhs, name, None, warn_info, warnings)
            if deriv != "0.0":
                parts[name] = deriv
                index_map[name] = (name, None)
    has_repeat = len(all_names) != len(set(all_names))
    return parts, has_repeat, index_map, index_vars




def _generate_ad_subroutine(routine_org, warnings):
    # Collect information of aruguments
    args = []
    out_grad_args = []
    has_grad_input = False
    for arg in routine_org.arg_vars():
        name = arg.name
        typ = arg.typename
        dims = arg.dims
        intent = arg.intent
        is_char = typ == "character"
        is_int = typ == "integer"
        if intent == "out":
            if not is_char and not is_int:
                ad_name = f"{name}_ad"
                var = Variable(ad_name, typ, arg.kind, dims, "in")
                args.append(var)
                has_grad_input = True
        else:
            args.append(arg)
            if not is_char and not is_int:
                ad_name = f"{name}_ad"
                grad_int = {
                    "in": "out",
                    "inout": "inout",
                }.get(intent, "out")
                var = Variable(ad_name, typ, arg.kind, dims, grad_int)
                args.append(var)
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
            ad_block.append(Assignment(arg, "0.0"))
        ad_block.append(Statement("return"))
        routine.ad_content = ad_block
        return subroutine

    # If there are no input gradients to propagate we can exit early
    if not out_grad_args:
        ad_block.append(Statment("return"))
        routine.ad_content = ad_block
        return subroutine

    def to_ad(lhs: OpVar, rhs: Operation, info) -> List[Assignment]:
        if isinstance(rhs, OpFunc):
            handler = SPECIAL_HANDLERS.get(rhs.name)
            if handler:
                return handler(lhs, rhs.name, rhs.args)
            raise ValueError(f"Unsupported function: {rhs.name}")
        assigs = []
        grad_lhs = lhs.add_suffix("_ad")
        vars = rhs.collect_vars()
        for var in vars:
            dev = rhs.derivative(var, info=info, warnings=warnings)
            v = var.add_suffix("_ad")
            assigs.append(Assignment(v, grad_lhs * dev, accumulate=True))
        if not var in vars:
            Assignment(grad_lhs, OpInt(0))
        return Block(assigs)

        raise ValueError(f"Unsupported operation: {type(rhs)}")

    ad_code = routine_org.content.convert_assignments(to_ad, reverse=True)
    if (ad_code is not None) and (not ad_code.is_effectively_empty()):
        ad_block.extend(ad_code)
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
        #mod.body = mod_org.body
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
