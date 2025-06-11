from pathlib import Path

from . import parser
from .parser import Fortran2003, walk


def _decl_names(stmt):
    """Return variable names from a ``Type_Declaration_Stmt``."""
    return [str(ed.items[0]) for ed in stmt.items[2].items]


def _parse_decls(spec):
    """Map variable names to their declared type and intent."""
    decl_map = {}
    for decl in spec.content:
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


def _assign_ad_lines(stmt, indent):
    """Generate reverse-mode derivative lines for a simple assignment."""
    lhs = str(stmt.items[0])
    rhs_str = stmt.items[2].tofortran().strip()

    if "+" in rhs_str:
        left, right = [s.strip() for s in rhs_str.split("+")]
        parts = {left: "1.0", right: "1.0"}
    elif "*" in rhs_str:
        left, right = [s.strip() for s in rhs_str.split("*")]
        parts = {left: right, right: left}
    else:
        return []

    lines = []
    for var in parts:
        lines.append(f"{indent}real :: d{lhs}_d{var}\n")
    for var, expr in parts.items():
        lines.append(f"{indent}d{lhs}_d{var} = {expr}\n")
    for var in parts:
        lines.append(f"{indent}{var}_ad = {lhs}_ad * d{lhs}_d{var}\n")
    return lines


def _generate_ad_subroutine(routine, indent):
    lines = []

    if isinstance(routine, Fortran2003.Function_Subprogram):
        stmt = routine.content[0]
        name = str(stmt.get_name())
        args = [str(a) for a in (stmt.items[2].items if stmt.items[2] else [])]
        result = str(stmt.items[3].items[0])
    else:
        stmt = routine.content[0]
        name = str(stmt.get_name())
        args = [str(a) for a in (stmt.items[2].items if stmt.items[2] else [])]
        result = None

    spec = routine.content[1]
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

    exec_part = routine.content[2]
    for stmt in exec_part.content:
        if isinstance(stmt, Fortran2003.Assignment_Stmt):
            lines.extend(_assign_ad_lines(stmt, indent + "  "))

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
        name = str(module.content[0].get_name())
        output.append(f"module {name}_ad\n")
        output.append("contains\n")
        children = [
            c
            for c in module.content[1].content
            if isinstance(
                c,
                (
                    Fortran2003.Function_Subprogram,
                    Fortran2003.Subroutine_Subprogram,
                ),
            )
        ]
        for i, child in enumerate(children):
            output.extend(_generate_ad_subroutine(child, "  "))
            if i != len(children) - 1:
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
