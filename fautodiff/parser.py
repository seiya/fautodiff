"""Fortran parsing utilities based on ``fparser``.

This module centralizes all direct interaction with :mod:`fparser` so that the
rest of the package does not rely on the underlying parser implementation.
"""

from packaging.version import Version, parse
import re

import fparser
from fparser.common.readfortran import FortranFileReader, FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.Fortran2008 import Block_Nonlabel_Do_Construct
from fparser.two.utils import walk

from .operators import (
    Operator,
    OpInt,
    OpReal,
    OpVar,
    OpChr,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpPow,
    OpNeg,
    OpFunc,
    OpLog,
    OpRange,
)

from .code_tree import (
    Variable,
    Node,
    Module,
    Subroutine,
    Function,
    Block,
    Declaration,
    Assignment,
    IfBlock,
    DoLoop,
    DoWhile,
    SelectBlock,
    Statement,
)

_KIND_RE = re.compile(r"([\+\-])?([\d\.]+)([edED][\+\-]?\d+)?(?:_(.*))?$")


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

def _stmt2op(stmt, decls):
    """Return Operator from statement."""

    if isinstance(stmt, Fortran2003.Int_Literal_Constant):
        return OpInt(val=int(stmt.items[0]), kind=stmt.items[1])

    if isinstance(stmt, Fortran2003.Real_Literal_Constant):
        m = _KIND_RE.fullmatch(stmt.tofortran())
        if m:
            sign = m.group(1)
            val = m.group(2)
            expo = m.group(3)
            kind = m.group(4)
            if kind is None and expo is not None:
                if expo[0].lower() == 'd':
                    kind = "8"
            if expo is not None:
                val = f"{val}e{expo[1:]}"
            ret = OpReal(val=val, kind=kind)
            if sign is not None and sign[0] == '-':
                ret = - ret
            return ret
        else:
            raise ValueError("Failed to convert real number: #{stmt}")

    if isinstance(stmt, Fortran2003.Name):
        name = stmt.tofortran()
        decl = decls.find_by_name(name)
        if decl is not None:
            is_real = decl.is_real()
            kind = decl.kind
            if kind is None and is_real and decl.typename.lower().startswith("double"):
                kind = "8"
        else:
            raise ValueError(f"Not found in the declaration section: {name}")
            #is_real = True
            #kind = None
        return OpVar(name=name, is_real=is_real, kind=kind)

    if isinstance(stmt, Fortran2003.Part_Ref):
        name = stmt.items[0].tofortran()
        index = tuple(_stmt2op(x, decls) for x in stmt.items[1].items)
        decl = decls.find_by_name(name)
        if decl is None: # must be function
            name = name.lower()
            args = [_stmt2op(arg, decls) for arg in getattr(stmt.items[1], "items", []) if not isinstance(arg, str)]
            return OpFunc(name, args)
        else:
            return OpVar(name=name, index=index, is_real=decl.is_real())

    if isinstance(stmt, Fortran2003.Subscript_Triplet):
        args = tuple((x and _stmt2op(x, decls)) for x in stmt.items)
        return OpRange(args)

    if isinstance(stmt, Fortran2003.Intrinsic_Function_Reference):
        name = stmt.items[0].tofortran().lower()
        args = [_stmt2op(arg, decls) for arg in getattr(stmt.items[1], "items", []) if not isinstance(arg, str)]
        return OpFunc(name, args)

    if isinstance(stmt, Fortran2003.Char_Literal_Constant):
        name = stmt.items[0]
        return OpChr(name=name)

    if isinstance(stmt, Fortran2003.Mult_Operand):
        if stmt.items[1] == "**":
            args = [_stmt2op(stmt.items[0], decls), _stmt2op(stmt.items[2], decls)]
            return OpPow(args)
        else:
            raise ValueError("Unsupported Mult_operand type: f{stmt}")

    if isinstance(stmt, Fortran2003.Level_2_Unary_Expr):
        op = stmt.items[0]
        args = [_stmt2op(stmt.items[1], decls)]
        if op == "-":
            return OpNeg(args)
        else:
            raise ValueError("Unsupported Level_2_Unary_Expr type: f{stmt}")

    if isinstance(stmt, Fortran2003.Level_2_Expr):
        op = stmt.items[1]
        args = [_stmt2op(stmt.items[0], decls), _stmt2op(stmt.items[2], decls)]
        if op == "+":
            return OpAdd(args)
        elif op == "-":
            return OpSub(args)
        else:
            raise ValueError("Unsupported Level_2_Expr type: f{stmt}")

    if isinstance(stmt, Fortran2003.Add_Operand):
        op = stmt.items[1]
        args = [_stmt2op(stmt.items[0], decls), _stmt2op(stmt.items[2], decls)]
        if op == "*":
            return OpMul(args)
        elif op == "/":
            return OpDiv(args)
        else:
            raise ValueError("Unsupported Add_Operand type: f{stmt}")

    if isinstance(stmt, Fortran2003.Parenthesis):
        return _stmt2op(stmt.items[1], decls)

    if isinstance(stmt, Fortran2003.Level_4_Expr):
        op = stmt.items[1]
        args = [_stmt2op(stmt.items[0], decls), _stmt2op(stmt.items[2], decls)]
        return OpLog(op=op, args=args)

    print("other")
    print(type(stmt))
    print(stmt)
    print(stmt.tofortran())
    print(stmt.item)
    print(stmt.items)
    print(stmt.__dict__)
    raise ValueError(f"Unsupported statement type: {type(stmt)}")


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
                        mod_node.routines.append(_parse_routine(c, path))
                break
    return output


def _parse_routine(content, filename):
    """Return node tree correspoinding to the input AST"""
    def _parse_decls(spec):
        """Return mapping of variable names to ``(type, intent)``."""
        decls = Block([])
        if spec is None:
            return decls
        for decl in spec.content:
            if not isinstance(decl, Fortran2003.Type_Declaration_Stmt):
                continue
            base_type = decl.items[0].string
            text = decl.tofortran().lower()
            if "intent(inout)" in text:
                intent = "inout"
            elif "intent(out)" in text:
                intent = "out"
            elif "intent(in)" in text:
                intent = "in"
            else:
                intent = None

            dim_attr = None
            attrs = decl.items[1]
            if attrs is not None:
                for attr in attrs.items:
                    if hasattr(attr, "items") and str(attr.items[0]).lower() == "dimension":
                        dims = tuple(v.tofortran() for v in attr.items[1].items)
                        break

            for entity in decl.items[2].items:
                name = entity.items[0].tofortran()
                arrspec = entity.items[1]
                dims = None
                if arrspec is not None:
                    dims = tuple(v.tofortran() for v in arrspec.items)
                elif dim_attr is not None:
                    dims = dim_attr
                decls.append(Declaration(name, base_type, None, dims, intent))
        return decls

    def _parse_stmt(stmt, decls) -> Node:
        if isinstance(stmt, Fortran2003.Assignment_Stmt):
            lhs = _stmt2op(stmt.items[0], decls)
            rhs = _stmt2op(stmt.items[2], decls)
            line_no = None
            if getattr(stmt, "item", None) is not None and getattr(stmt.item, "span", None):
                line_no = stmt.item.span[0]
            info = {
                "file": filename,
                "line": line_no,
                "code": stmt.tofortran().strip(),
            }
            return Assignment(lhs, rhs, False, info)
        if isinstance(stmt, Fortran2003.If_Construct):
            cond = _stmt2op(stmt.content[0].items[0], decls)
            i = 1
            seg = []
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(itm, (Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
                    break
                seg.append(itm)
                i += 1
            body = _block(seg, decls)
            elif_bodies = []
            else_body = None
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(itm, Fortran2003.Else_If_Stmt):
                    cond2 = _stmt2op(itm.items[0], decls)
                    i += 1
                    seg = []
                    while i < len(stmt.content):
                        j = stmt.content[i]
                        if isinstance(j, (Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
                            break
                        seg.append(j)
                        i += 1
                    blk = _block(seg, decls)
                    elif_bodies.append((cond2, blk))
                elif isinstance(itm, Fortran2003.Else_Stmt):
                    i += 1
                    seg = []
                    while i < len(stmt.content):
                        j = stmt.content[i]
                        if isinstance(j, Fortran2003.End_If_Stmt):
                            break
                        seg.append(j)
                        i += 1
                    else_body = _block(seg, decls)
                elif isinstance(itm, Fortran2003.End_If_Stmt):
                    i += 1
                else:
                    i += 1
            return IfBlock(cond, body, elif_bodies=elif_bodies, else_body=else_body)
        if isinstance(stmt, Fortran2003.Case_Construct):
            expr = _stmt2op(stmt.content[0].items[0], decls)
            cases = []
            default = None
            i = 1
            while i < len(stmt.content) - 1:
                stmt_cond = stmt.content[i]
                i += 1
                seg = []
                while i < len(stmt.content) - 1 and not isinstance(stmt.content[i], Fortran2003.Case_Stmt):
                    seg.append(stmt.content[i])
                    i += 1
                blk = _block(seg, decls)
                if stmt_cond.tofortran() == "CASE DEFAULT":
                    default = blk
                else:
                    conds = tuple(_stmt2op(cond, decls) for cond in stmt_cond.items[0].items[0].items)
                    cases.append((conds, blk))
            return SelectBlock(expr, cases, default=default)
        if isinstance(stmt, Block_Nonlabel_Do_Construct):
            body = _block(stmt.content[1:-1], decls)
            if stmt.content[0].tofortran().startswith("DO WHILE"):
                cond = _stmt2op(stmt.content[0].items[1].items[0].items[0], decls)
                return DoWhile(body, cond)
            else:
                itm = stmt.content[0].items[1].items[1]
                index = _stmt2op(itm[0], decls)
                start = _stmt2op(itm[1][0], decls)
                end = _stmt2op(itm[1][1], decls)
                if len(itm[1]) == 2:
                    step = None
                else:
                    step = _stmt2op(itm[1][2], decls)
                return DoLoop(body, index, start, end, step)
        if isinstance(stmt, Fortran2003.Return_Stmt):
            return Statement("return")

        raise ValueError(f"stmt is not supported: {stmt}")

    def _block(body_list, decls):
        blk = Block([])
        for st in body_list:
            blk.append(_parse_stmt(st, decls))
        return blk

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
            decls = routine.decls
            for stmt in part.content:
                node = _parse_stmt(stmt, decls)
                if node is not None:
                    routine.content.append(node)

    return routine
