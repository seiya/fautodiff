"""Fortran parsing utilities based on ``fparser``.

This module centralizes all direct interaction with :mod:`fparser` so that the
rest of the package does not rely on the underlying parser implementation.
"""

from packaging.version import Version, parse
import fparser

from fparser.common.readfortran import FortranFileReader, FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.Fortran2008 import Block_Nonlabel_Do_Construct
from fparser.two.utils import walk

from .code_tree import (
    Module,
    Subroutine,
    Function,
    Return,
    Block,
    Declaration,
    Assignment,
    IfBlock,
    DoLoop,
    SelectBlock,
    Variable,
    variable_from_expr,
)

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
    "_parse_routine",
]


def parse_file(path):
    """Parse a Fortran source file and return the ``fparser`` AST."""
    reader = FortranFileReader(path)
    factory = ParserFactory().create(std="f2008")
    ast = factory(reader)
    output = []
    warnings = []
    for module in walk(ast, Fortran2003.Module):
        name = _stmt_name(module.content[0])
        mod_node = Module(name)
        output.append(mod_node)
        for part in module.content:
            if isinstance(part, Fortran2003.Module_Subprogram_Part):
                for c in part.content:
                    if isinstance(c, (Fortran2003.Function_Subprogram, Fortran2003.Subroutine_Subprogram)):
                        mod_node.routines.append(_parse_routine(c))
                break
    return output


def _parse_routine(content):
    """Return node tree correspoinding to the input AST"""
    def _parse_decls(spec):
        """Return mapping of variable names to ``(type, intent)``."""
        decls = Block([])
        if spec is None:
            return decls
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
                decls.append(Declaration(type_str, name, intent, dims))
        return decls

    def _stmt_to_block(stmt):
        if isinstance(stmt, Fortran2003.Assignment_Stmt):
            lhs = stmt.items[0].tofortran().strip()
            rhs = stmt.items[2].tofortran().strip()
            return Block([Assignment(variable_from_expr(lhs), rhs)])
        if isinstance(stmt, Fortran2003.If_Construct):
            cond = stmt.content[0].items[0].tofortran()
            i = 1
            seg = []
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(itm, (Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
                    break
                seg.append(itm)
                i += 1
            body = _block(seg)
            elif_blocks = []
            else_block = None
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(itm, Fortran2003.Else_If_Stmt):
                    cond2 = itm.items[0].tofortran()
                    i += 1
                    seg = []
                    while i < len(stmt.content):
                        j = stmt.content[i]
                        if isinstance(j, (Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
                            break
                        seg.append(j)
                        i += 1
                    blk = _block(seg)
                    elif_blocks.append((cond2, blk))
                elif isinstance(itm, Fortran2003.Else_Stmt):
                    i += 1
                    seg = []
                    while i < len(stmt.content):
                        j = stmt.content[i]
                        if isinstance(j, Fortran2003.End_If_Stmt):
                            break
                        seg.append(j)
                        i += 1
                    else_block = _block(seg)
                elif isinstance(itm, Fortran2003.End_If_Stmt):
                    i += 1
                else:
                    i += 1
            node = IfBlock(cond, body, elif_blocks=elif_blocks, else_body=else_block)
            return Block([node])
        if isinstance(stmt, Fortran2003.Case_Construct):
            expr = stmt.content[0].items[0].tofortran()
            cases = []
            default = None
            i = 1
            while i < len(stmt.content) - 1:
                cs = stmt.content[i]
                cond = cs.tofortran().split(None, 1)[1]
                i += 1
                seg = []
                while i < len(stmt.content) - 1 and not isinstance(stmt.content[i], Fortran2003.Case_Stmt):
                    seg.append(stmt.content[i])
                    i += 1
                blk = _block(seg)
                if cond.lower().startswith("default"):
                    default = blk
                else:
                    cond = cond.strip()
                    if cond.startswith("(") and cond.endswith(")"):
                        cond = cond[1:-1]
                    cases.append((cond, blk))
            node = SelectBlock(expr, cases, default=default)
            return Block([node])
        if isinstance(stmt, Block_Nonlabel_Do_Construct):
            header = stmt.content[0].tofortran().strip()
            body = _block(stmt.content[1:-1])
            return Block([DoLoop(header, body)])
        if isinstance(stmt, Fortran2003.Return_Stmt):
            return Block([Return()])
        return None

    stmt = content.content[0]
    name = _stmt_name(stmt)
    args = [str(a) for a in (stmt.items[2].items if stmt.items[2] else [])]
    if isinstance(content, Fortran2003.Subroutine_Subprogram):
        routine = Subroutine(name, args)
    elif isinstance(content, Fortran2003.Function_Subprogram):
        result = str(stmt.items[3].items[0])
        routine = Function(name, args, result)
    else:
        raise AttributeError("content must be Subroutine of Function")

    for part in content.content:
        if isinstance(part, Fortran2003.Specification_Part):
            routine.decls = _parse_decls(part)
        elif isinstance(part, Fortran2003.Execution_Part):
            for stmt in part.content:
                node = _stmt_to_block(stmt)
                if node is not None:
                    routine.content.append(node)

    return routine
