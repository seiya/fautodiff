"""Utility functions for parsing Fortran using ``fparser.two``."""

from fparser.common.readfortran import FortranFileReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.utils import walk


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

__all__ = ["parse_file", "find_subroutines", "Fortran2003", "walk"]


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
