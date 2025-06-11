from pathlib import Path
import sys

from . import parser
from .parser import Fortran2003, walk
from .intrinsic_derivatives import INTRINSIC_DERIVATIVES


def _warn(warnings, info, code, reason):
    """Append a formatted warning message to ``warnings`` list."""
    if warnings is not None and info is not None:
        filename = info.get("file", "<unknown>")
        line = info.get("line", "?")
        msg = f"{filename}:{line}: {code} - {reason}"
        warnings.append(msg)


def _strip_paren(text: str) -> str:
    """Remove a single pair of parentheses from ``text`` if present."""
    if text.startswith("(") and text.endswith(")"):
        return text[1:-1]
    return text


def _to_number(val: float, keep_decimal: bool = False) -> str:
    """Return ``val`` as a Fortran number string."""
    if val.is_integer():
        return f"{int(val)}.0" if keep_decimal else str(int(val))
    return str(val)


def _minus_one(expr) -> str:
    """Return a Fortran expression for ``expr - 1``."""
    if isinstance(expr, Fortran2003.Parenthesis):
        inner = _minus_one(expr.items[1])
        return f"({_strip_paren(inner)})"
    if isinstance(expr, Fortran2003.Int_Literal_Constant):
        val = int(expr.items[0]) - 1
        return str(val)
    if isinstance(expr, Fortran2003.Real_Literal_Constant):
        val = float(expr.items[0]) - 1.0
        keep = "." in expr.items[0]
        return _to_number(val, keep_decimal=keep)
    if (
        isinstance(expr, Fortran2003.Level_2_Expr)
        and len(expr.items) == 3
        and expr.items[1] == "+"
        and isinstance(expr.items[2], (Fortran2003.Int_Literal_Constant, Fortran2003.Real_Literal_Constant))
    ):
        left, _, right = expr.items
        return f"{left.tofortran()} + {_minus_one(right)}"
    return f"{expr.tofortran()} - 1.0"


def _collect_names(expr, names, unique=True):
    """Collect variable names found in ``expr`` preserving order."""
    if isinstance(expr, Fortran2003.Intrinsic_Function_Reference):
        # Skip the function name (first item) and recurse into arguments.
        args = expr.items[1]
        for arg in getattr(args, "items", []):
            subexpr = arg.items[1] if hasattr(arg, "items") and len(arg.items) > 1 else arg
            _collect_names(subexpr, names, unique=unique)
        return
    if isinstance(expr, Fortran2003.Name):
        name = str(expr)
        if unique:
            if name not in names:
                names.append(name)
        else:
            names.append(name)
    for item in getattr(expr, "items", []):
        if not isinstance(item, str):
            _collect_names(item, names, unique=unique)


def _parenthesize_if_needed(text: str) -> str:
    """Add parentheses to ``text`` if it contains operators."""
    if text.startswith("(") and text.endswith(")"):
        return text
    if any(op in text for op in (" ", "+", "-", "*", "/")):
        return f"({text})"
    return text


def _derivative(expr, var: str, warn_info=None, warnings=None) -> str:
    """Return derivative of ``expr`` with respect to ``var`` as a string.

    ``warn_info`` should contain context (file, line, stmt) for warning messages.
    ``warnings`` is a list that collects formatted warning strings.
    """
    if isinstance(expr, Fortran2003.Name):
        return "1.0" if str(expr) == var else "0.0"
    if isinstance(expr, (Fortran2003.Int_Literal_Constant, Fortran2003.Real_Literal_Constant)):
        return "0.0"
    if isinstance(expr, Fortran2003.Parenthesis):
        return _derivative(expr.items[1], var, warn_info, warnings)
    if isinstance(expr, Fortran2003.Intrinsic_Function_Reference):
        name = expr.items[0].tofortran().lower()
        items = [a for a in getattr(expr.items[1], "items", []) if not isinstance(a, str)]
        if name in INTRINSIC_DERIVATIVES and len(items) == 1:
            arg = items[0]
            if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                arg = arg.items[1]
            arg_s = arg.tofortran()
            d_arg = _derivative(arg, var, warn_info, warnings)
            deriv = INTRINSIC_DERIVATIVES[name].format(arg=arg_s)
            if d_arg == "0.0":
                return "0.0"
            if d_arg == "1.0":
                return deriv
            return f"{deriv} * {d_arg}"
        reason = f"unsupported intrinsic '{name}'"
        _warn(warnings, warn_info, expr.tofortran(), reason)
        return "0.0"
    if isinstance(expr, Fortran2003.Level_2_Unary_Expr):
        sign = expr.items[0]
        d = _derivative(expr.items[1], var, warn_info, warnings)
        if sign == "-":
            if d == "0.0":
                return "0.0"
            if d.startswith("-"):
                return d[2:]
            return f"- {d}"
        return d
    if isinstance(expr, Fortran2003.Level_2_Expr) and len(expr.items) == 3 and isinstance(expr.items[1], str):
        left, op, right = expr.items
        if op == "+":
            d1 = _derivative(left, var, warn_info, warnings)
            d2 = _derivative(right, var, warn_info, warnings)
            if d1 == "0.0":
                return d2
            if d2 == "0.0":
                return d1
            return f"{d1} + {d2}"
        if op == "-":
            d1 = _derivative(left, var, warn_info, warnings)
            d2 = _derivative(right, var, warn_info, warnings)
            if d1 == "0.0" and d2 == "0.0":
                return "0.0"
            if d2 == "0.0":
                return d1
            if d1 == "0.0":
                return f"- {d2}"
            return f"{d1} - {d2}"
    if isinstance(expr, Fortran2003.Add_Operand) and len(expr.items) == 3:
        left, op, right = expr.items
        if op == "*":
            d1 = _derivative(left, var, warn_info, warnings)
            d2 = _derivative(right, var, warn_info, warnings)
            left_s = left.tofortran()
            right_s = right.tofortran()
            terms = []
            if d1 != "0.0":
                terms.append(right_s if d1 == "1.0" else f"{d1} * {right_s}")
            if d2 != "0.0":
                terms.append(left_s if d2 == "1.0" else f"{left_s} * {d2}")
            return " + ".join(terms) if terms else "0.0"
        if op == "/":
            u = left.tofortran()
            v = right.tofortran()
            vsq = v if v.startswith("(") else f"({v})"
            d1 = _derivative(left, var, warn_info, warnings)
            d2 = _derivative(right, var, warn_info, warnings)
            if d1 == "0.0" and d2 == "0.0":
                return "0.0"
            if d2 == "0.0":
                return f"{d1} / {v}" if d1 != "1.0" else f"1.0 / {v}"
            if d1 == "0.0":
                fac = "" if d2 == "1.0" else f" * {d2}"
                return f"- {u}{fac} / {vsq}**2"
            num1 = v if d1 == "1.0" else f"{d1} * {v}"
            num2 = f"- {u}" if d2 == "1.0" else f"- {u} * {d2}"
            return f"({num1} + {num2}) / {vsq}**2"
    if isinstance(expr, Fortran2003.Mult_Operand) and len(expr.items) == 3 and expr.items[1] == "**":
        base, _, exponent = expr.items
        base_s = base.tofortran()
        exp_s = exponent.tofortran()
        d_base = _derivative(base, var, warn_info, warnings)
        d_exp = _derivative(exponent, var, warn_info, warnings)
        terms = []
        if d_base != "0.0":
            minus = _parenthesize_if_needed(_minus_one(exponent))
            term = f"{exp_s} * {base_s}**{minus}"
            if d_base != "1.0":
                term += f" * {d_base}"
            terms.append(term)
        if d_exp != "0.0":
            log_base = _strip_paren(base_s)
            term = f"{base_s}**{exp_s} * log({log_base})"
            if d_exp != "1.0":
                term += f" * {d_exp}"
            terms.append(term)
        return " + ".join(terms) if terms else "0.0"
    reason = "unsupported expression"
    _warn(warnings, warn_info, expr.tofortran(), reason)
    return "0.0"


def _decl_names(stmt):
    """Return variable names from a ``Type_Declaration_Stmt``."""
    return [str(ed.items[0]) for ed in stmt.items[2].items]


def _parse_decls(spec):
    """Map variable names to their declared type and intent."""
    decl_map = {}
    if spec is None:
        return decl_map
    for decl in spec.content:
        if not isinstance(decl, Fortran2003.Type_Declaration_Stmt):
            continue
        type_str = decl.items[0].tofortran().lower()
        text = decl.tofortran().upper()
        if "INTENT(INOUT)" in text:
            intent = "inout"
        elif "INTENT(OUT)" in text:
            intent = "out"
        elif "INTENT(IN)" in text:
            intent = "in"
        else:
            intent = None
        for name in _decl_names(decl):
            decl_map[name] = (type_str, intent)
    return decl_map


def _routine_parts(routine):
    """Return the specification and execution parts of a routine."""
    spec = None
    exec_part = None
    for part in routine.content:
        if isinstance(part, Fortran2003.Specification_Part):
            spec = part
        elif isinstance(part, Fortran2003.Execution_Part):
            exec_part = part
    return spec, exec_part


def _assignment_parts(stmt, warn_info=None, warnings=None):
    """Return mapping of variables to partial derivative expressions and whether any variable is used twice."""
    rhs = stmt.items[2]
    all_names = []
    _collect_names(rhs, all_names, unique=False)
    names = []
    _collect_names(rhs, names)
    parts = {}
    for name in names:
        deriv = _derivative(rhs, name, warn_info, warnings)
        if deriv != "0.0":
            parts[name] = deriv
    has_repeat = len(all_names) != len(set(all_names))
    return parts, has_repeat


def _generate_ad_subroutine(routine, indent, filename, warnings):
    lines = []

    if isinstance(routine, Fortran2003.Function_Subprogram):
        stmt = routine.content[0]
        name = parser._stmt_name(stmt)
        args = [str(a) for a in (stmt.items[2].items if stmt.items[2] else [])]
        result = str(stmt.items[3].items[0])
    else:
        stmt = routine.content[0]
        name = parser._stmt_name(stmt)
        args = [str(a) for a in (stmt.items[2].items if stmt.items[2] else [])]
        result = None

    spec, exec_part = _routine_parts(routine)
    decl_map = _parse_decls(spec)

    ad_args = []
    outputs = []
    if result is not None:
        outputs.append(result)
    for arg in args:
        typ_int = decl_map.get(arg, (None, None))[1]
        if typ_int == "out":
            outputs.append(arg)
            ad_args.append(f"{arg}_ad")
        else:
            ad_args.extend([arg, f"{arg}_ad"])
    for outv in outputs:
        if outv not in args:
            ad_args.append(f"{outv}_ad")

    lines.append(f"{indent}subroutine {name}_ad({', '.join(ad_args)})\n")

    def _space(intent):
        return "  " if intent == "in" else " "

    for arg in args:
        typ, intent = decl_map.get(arg, ("real", "in"))
        arg_int = intent or "in"
        if arg_int == "out":
            lines.append(
                f"{indent}  real, intent(in){_space('in')}:: {arg}_ad\n"
            )
        else:
            if arg_int == "inout":
                ad_int = "inout"
            else:
                ad_int = "out"
            lines.append(
                f"{indent}  {typ}, intent({arg_int})"
                f"{_space(arg_int)}:: {arg}\n"
            )
            lines.append(
                f"{indent}  real, intent({ad_int})"
                f"{_space(ad_int)}:: {arg}_ad\n"
            )

    for outv in outputs:
        if outv not in args:
            lines.append(
                f"{indent}  real, intent(in){_space('in')}:: {outv}_ad\n"
            )

    statements = [
        s for s in exec_part.content if isinstance(s, Fortran2003.Assignment_Stmt)
    ]
    defined = set()
    grad_var = {v: f"{v}_ad" for v in outputs}
    decls = []
    decl_set = set()
    assign_lines = []

    for stmt in reversed(statements):
        lhs = str(stmt.items[0])
        line_no = None
        if getattr(stmt, "item", None) is not None and getattr(stmt.item, "span", None):
            line_no = stmt.item.span[0]
        info = {
            "file": filename,
            "line": line_no,
            "code": stmt.tofortran().strip(),
        }
        parts, has_repeat = _assignment_parts(stmt, info, warnings)
        for var in parts:
            name_d = f"d{lhs}_d{var}"
            if name_d not in decl_set:
                decls.append(name_d)
                decl_set.add(name_d)
            if var not in args and var not in outputs:
                name_ad = f"{var}_ad"
                if name_ad not in decl_set:
                    decls.append(name_ad)
                    decl_set.add(name_ad)
        if lhs in parts:
            name_ad = f"{lhs}_ad_"
            if name_ad not in decl_set:
                decls.append(name_ad)
                decl_set.add(name_ad)

        block = []
        for var, expr in parts.items():
            block.append(f"{indent}  d{lhs}_d{var} = {expr}\n")
        lhs_grad = grad_var.get(lhs, f"{lhs}_ad")
        order = list(parts.keys()) if has_repeat else list(reversed(list(parts.keys())))
        for var in order:
            if var == lhs:
                continue
            update = f"{lhs_grad} * d{lhs}_d{var}"
            if var in defined:
                block.append(f"{indent}  {var}_ad = {update} + {var}_ad\n")
            else:
                block.append(f"{indent}  {var}_ad = {update}\n")
                defined.add(var)
        if lhs in parts:
            new_grad = f"{lhs}_ad_"
            block.append(f"{indent}  {new_grad} = {lhs_grad} * d{lhs}_d{lhs}\n")
            grad_var[lhs] = new_grad
        assign_lines.append(block)

    for dname in decls:
        lines.append(f"{indent}  real :: {dname}\n")
    lines.append("\n")
    for block in assign_lines:
        for l in block:
            lines.append(l)
    lines.append("\n")
    lines.append(f"{indent}  return\n")

    lines.append(f"{indent}end subroutine {name}_ad\n")
    return lines


def generate_ad(in_file, out_file=None, warn=True):
    """Generate a very small reverse-mode AD version of ``in_file``.

    If ``out_file`` is ``None`` the generated code is returned as a string.
    When ``out_file`` is provided the code is also written to that path.
    """
    ast = parser.parse_file(in_file)
    output = []
    warnings = []
    for module in walk(ast, Fortran2003.Module):
        name = parser._stmt_name(module.content[0])
        output.append(f"module {name}_ad\n")
        output.append("  implicit none\n\n")
        output.append("contains\n\n")
        children = []
        for part in module.content:
            if isinstance(part, Fortran2003.Module_Subprogram_Part):
                children = [
                    c
                    for c in part.content
                    if isinstance(
                        c,
                        (
                            Fortran2003.Function_Subprogram,
                            Fortran2003.Subroutine_Subprogram,
                        ),
                    )
                ]
                break
        for child in children:
            output.extend(
                _generate_ad_subroutine(child, "  ", in_file, warnings)
            )
            output.append("\n")
        output.append(f"end module {name}_ad\n")

    code = "".join(output)
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
