"""Fortran parsing utilities based on ``fparser``.

This module centralizes all direct interaction with :mod:`fparser` so that the
rest of the package does not rely on the underlying parser implementation.
"""

from packaging.version import Version, parse
import fparser

from fparser.common.readfortran import FortranFileReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.Fortran2008 import Block_Nonlabel_Do_Construct
from fparser.two.utils import walk

if parse(getattr(fparser, "__version__", "0")) < Version("0.2.0"):
    raise RuntimeError("fautodiff requires fparser version 0.2.0 or later")


def _stmt_name(stmt):
    """Return the name from a program unit statement.

    This helper works with both new and old versions of ``fparser`` by
    falling back to simple string parsing when ``get_name`` is not
    available.
    """
    if hasattr(stmt, "get_name"):
        return str(stmt.get_name())
    text = stmt.tofortran().strip()
    parts = text.split()
    if len(parts) >= 2:
        return parts[1].split("(")[0]
    raise AttributeError("Could not determine statement name")

# Re-export commonly used classes and utilities so other modules do not need
# to import ``fparser2`` directly.

__all__ = [
    "parse_file",
    "find_subroutines",
    "Fortran2003",
    "walk",
    "Block_Nonlabel_Do_Construct",
    "_parse_decls",
    "_routine_parts",
]


def parse_file(path):
    """Parse a Fortran source file and return the ``fparser`` AST."""
    reader = FortranFileReader(path)
    parser = ParserFactory().create(std="f2008")
    return parser(reader)


def find_subroutines(ast):
    """Return a list of subroutine names defined in the given AST."""
    names = []
    for node in walk(ast, Fortran2003.Subroutine_Subprogram):
        stmt = node.children[0]  # Subroutine_Stmt
        names.append(_stmt_name(stmt))
    return names


def _parse_decls(spec):
    """Return mapping of variable names to ``(type, intent)``."""
    decl_map = {}
    if spec is None:
        return decl_map
    for decl in spec.content:
        if not isinstance(decl, Fortran2003.Type_Declaration_Stmt):
            continue
        base_type = decl.items[0].tofortran().lower()
        text = decl.tofortran().upper()
        if "INTENT(INOUT)" in text:
            intent = "inout"
        elif "INTENT(OUT)" in text:
            intent = "out"
        elif "INTENT(IN)" in text:
            intent = "in"
        else:
            intent = None

        dim_attr = None
        attrs = decl.items[1]
        if attrs is not None:
            for attr in attrs.items:
                if hasattr(attr, "items") and str(attr.items[0]).upper() == "DIMENSION":
                    dim_attr = attr.items[1].tofortran()
                    break

        for entity in decl.items[2].items:
            name = str(entity.items[0])
            arrspec = entity.items[1]
            type_str = base_type
            dims = None
            if arrspec is not None:
                dims = arrspec.tofortran()
            elif dim_attr is not None:
                dims = dim_attr
            if dims is not None:
                type_str = f"{type_str}, dimension({dims})"
            decl_map[name] = (type_str, intent)
    return decl_map


def _routine_parts(routine):
    """Return the specification and execution parts of a routine node."""
    spec = None
    exec_part = None
    for part in routine.content:
        if isinstance(part, Fortran2003.Specification_Part):
            spec = part
        elif isinstance(part, Fortran2003.Execution_Part):
            exec_part = part
    return spec, exec_part
