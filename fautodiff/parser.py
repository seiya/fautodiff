"""Utility functions for parsing Fortran using ``fparser.two``."""

from fparser.common.readfortran import FortranFileReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.utils import walk


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
        names.append(str(stmt.get_name()))
    return names
