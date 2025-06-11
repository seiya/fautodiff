from pathlib import Path

from . import parser
from .parser import Fortran2003, walk


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


def _assignment_parts(stmt):
    """Return mapping of variables to partial derivative expressions."""
    lhs = str(stmt.items[0])
    rhs_str = stmt.items[2].tofortran().strip()

    def _is_number(text):
        try:
            float(text)
            return True
        except ValueError:
            return False

    parts = {}

    if any(op in rhs_str for op in ("**", "/", "-")):
        return {}

    if "+" in rhs_str:
        add_terms = [t.strip() for t in rhs_str.split("+")]
        for term in add_terms:
            if "*" in term:
                mul_left, mul_right = [s.strip() for s in term.split("*")]
                if _is_number(mul_left) and not _is_number(mul_right):
                    parts[mul_right] = mul_left
                elif _is_number(mul_right) and not _is_number(mul_left):
                    parts[mul_left] = mul_right
            elif not _is_number(term):
                parts[term] = "1.0"
    elif "*" in rhs_str:
        mul_left, mul_right = [s.strip() for s in rhs_str.split("*")]
        if not _is_number(mul_left):
            parts[mul_left] = mul_right
        if not _is_number(mul_right):
            parts[mul_right] = mul_left
    else:
        return {}
    return parts


def _generate_ad_subroutine(routine, indent):
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
        parts = _assignment_parts(stmt)
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
        for var in reversed(list(parts.keys())):
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


def generate_ad(in_file, out_file=None):
    """Generate a very small reverse-mode AD version of ``in_file``.

    If ``out_file`` is ``None`` the generated code is returned as a string.
    When ``out_file`` is provided the code is also written to that path.
    """
    ast = parser.parse_file(in_file)
    output = []
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
            output.extend(_generate_ad_subroutine(child, "  "))
            output.append("\n")
        output.append(f"end module {name}_ad\n")

    code = "".join(output)
    if out_file:
        Path(out_file).write_text(code)
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
    args = parser_arg.parse_args()

    code = generate_ad(args.input, args.output)
    if args.output is None:
        print(code, end="")
