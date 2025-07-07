"""Fortran parsing utilities based on ``fparser``.

This module centralizes all direct interaction with :mod:`fparser` so that the
rest of the package does not rely on the underlying parser implementation.
"""

import json
import re
from pathlib import Path

import fparser
from fparser.common.readfortran import FortranFileReader, FortranStringReader
from fparser.two import Fortran2003, Fortran2008
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk
from packaging.version import Version, parse

from .code_tree import (
    Assignment,
    Block,
    CallStatement,
    Declaration,
    DoLoop,
    DoWhile,
    Function,
    IfBlock,
    Module,
    Node,
    SelectBlock,
    Statement,
    Subroutine,
    Use,
)
from .operators import (
    INTRINSIC_FUNCTIONS,
    OpAdd,
    OpChr,
    OpDiv,
    Operator,
    OpFunc,
    OpFuncUser,
    OpInt,
    OpLog,
    OpLogic,
    OpMul,
    OpNeg,
    OpPow,
    OpRange,
    OpReal,
    OpSub,
    OpVar,
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

    if isinstance(stmt, Fortran2003.Actual_Arg_Spec):
        return _stmt2op(stmt.items[1], decls)

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
                if expo[0].lower() == "d":
                    kind = "8"
            if expo is not None:
                expo = int(expo[1:])
            ret = OpReal(val=val, kind=kind, expo=expo)
            if sign is not None and sign[0] == "-":
                ret = -ret
            return ret
        else:
            raise ValueError("Failed to convert real number: #{stmt}")

    if isinstance(stmt, Fortran2003.Name):
        name = stmt.tofortran()
        decl = decls.find_by_name(name)
        if decl is not None:
            kind = decl.kind
            if (
                kind is None
                and decl.is_real()
                and decl.typename.lower().startswith("double")
            ):
                kind = "8"
        else:
            raise ValueError(f"Not found in the declaration section: {name}")
            # is_real = True
            # kind = None
        return OpVar(
            name=name,
            typename=decl.typename,
            kind=kind,
            is_constant=decl.parameter or getattr(decl, "constant", False),
        )

    if isinstance(stmt, Fortran2003.Part_Ref):
        name = stmt.items[0].tofortran()
        index = tuple(_stmt2op(x, decls) for x in stmt.items[1].items)
        decl = decls.find_by_name(name)
        if decl is None:  # must be function
            name_l = name.lower()
            args = [
                _stmt2op(arg, decls)
                for arg in getattr(stmt.items[1], "items", [])
                if not isinstance(arg, str)
            ]
            if name_l in INTRINSIC_FUNCTIONS:
                return OpFunc(name_l, args)
            return OpFuncUser(name_l, args)
        else:
            return OpVar(
                name=name,
                index=index,
                typename=decl.typename,
                kind=decl.kind,
                is_constant=decl.parameter or getattr(decl, "constant", False),
            )

    if isinstance(stmt, Fortran2003.Subscript_Triplet):
        args = tuple((x and _stmt2op(x, decls)) for x in stmt.items)
        return OpRange(args)

    if isinstance(stmt, Fortran2003.Intrinsic_Function_Reference):
        name = stmt.items[0].tofortran().lower()
        args = [
            _stmt2op(arg, decls)
            for arg in getattr(stmt.items[1], "items", [])
            if not isinstance(arg, str)
        ]
        return OpFunc(name, args)

    if isinstance(stmt, Fortran2003.Char_Literal_Constant):
        name = stmt.items[0]
        return OpChr(name=name)

    if isinstance(stmt, Fortran2003.Logical_Literal_Constant):
        return OpLogic(name=stmt.string)

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
    "find_subroutines",
]


def parse_file(path, *, search_dirs=None):
    """Parse ``path`` and return a list of :class:`Module` nodes."""
    reader = FortranFileReader(path, ignore_comments=False)
    return _parse_from_reader(reader, path, search_dirs=search_dirs)


def parse_src(src, *, search_dirs=None):
    """Parse ``src`` and return a list of :class:`Module` nodes."""
    reader = FortranStringReader(src, ignore_comments=False)
    return _parse_from_reader(reader, "<string>", search_dirs=search_dirs)


def _load_fadmod_decls(mod_name: str, search_dirs: list[str]) -> dict:
    """Return variable declaration info from ``mod_name`` fadmod file."""

    for d in search_dirs:
        path = Path(d) / f"{mod_name}.fadmod"
        if path.exists():
            try:
                data = json.loads(path.read_text())
                return data.get("variables", {})
            except Exception:
                pass
            break
    return {}


def _parse_decl_stmt(
    stmt, constant_args=None, *, allow_intent=True, allow_access=False
):
    """Parse a single ``Type_Declaration_Stmt`` and return declarations."""

    if not isinstance(
        stmt,
        (
            Fortran2003.Type_Declaration_Stmt,
            Fortran2008.type_declaration_stmt_r501.Type_Declaration_Stmt,
        ),
    ):
        raise RuntimeError(f"Unsupported statement: {type(stmt)} {stmt}")

    base_type = stmt.items[0].string
    kind = None
    base_lower = base_type.lower()
    if base_lower.startswith("real"):
        base_type = "real"
        type_spec = stmt.items[0]
        if getattr(type_spec, "items", None) is not None and len(type_spec.items) > 1:
            selector = type_spec.items[1]
            if isinstance(selector, Fortran2003.Kind_Selector):
                kind_item = selector.items[1]
                kind = kind_item.tofortran()

    text = stmt.tofortran().lower()
    if "intent(inout)" in text:
        intent = "inout"
    elif "intent(out)" in text:
        intent = "out"
    elif "intent(in)" in text:
        intent = "in"
    else:
        intent = None

    if intent is not None and not allow_intent:
        raise RuntimeError("Module variables must not specify INTENT")

    dim_attr = None
    parameter = False
    access = None
    attrs = stmt.items[1]
    if attrs is not None:
        for attr in attrs.items:
            name = getattr(
                attr, "string", str(getattr(attr, "items", [None])[0])
            ).lower()
            if name.startswith("dimension"):
                dim_attr = tuple(v.tofortran() for v in attr.items[1].items)
                break
            if name == "parameter":
                parameter = True
            if allow_access and name in ("public", "private"):
                access = name

    decls = []
    for entity in stmt.items[2].items:
        name = entity.items[0].tofortran()
        arrspec = entity.items[1]
        dims = None
        if arrspec is not None:
            dims = tuple(v.tofortran() for v in arrspec.items)
        elif dim_attr is not None:
            dims = dim_attr
        init = None
        if len(entity.items) > 3 and entity.items[3] is not None:
            init = entity.items[3].items[1].tofortran()

        constant = False
        if constant_args and name in constant_args:
            constant = True

        decls.append(
            Declaration(
                name,
                base_type,
                kind,
                dims,
                intent,
                parameter,
                constant,
                init=init,
                access=access,
            )
        )

    return decls


def _parse_decls(spec, constant_args=None, *, allow_intent=True, allow_access=False):
    """Return declarations parsed from a specification part."""

    decls = Block([])
    if spec is None:
        return decls

    for item in spec.content:
        if isinstance(item, Fortran2003.Implicit_Part):
            for cnt in item.content:
                if isinstance(cnt, Fortran2003.Comment):
                    if cnt.items[0] != "":
                        decls.append(Statement(cnt.items[0]))
                    continue
                if isinstance(cnt, Fortran2003.Implicit_Stmt):
                    decls.append(Statement(cnt.string))
            continue
        if isinstance(item, Fortran2003.Use_Stmt):
            only = None
            if item.items[4] is not None:
                only = [s.string for s in item.items[4].items]
            decls.append(Use(item.items[2].string, only=only))
            continue
        if isinstance(
            item,
            (
                Fortran2003.Type_Declaration_Stmt,
                Fortran2008.type_declaration_stmt_r501.Type_Declaration_Stmt,
            ),
        ):
            for decl in _parse_decl_stmt(
                item,
                constant_args,
                allow_intent=allow_intent,
                allow_access=allow_access,
            ):
                decls.append(decl)
            continue
        raise RuntimeError(f"Unsupported statement: {type(item)} {item}")

    return decls


def _parse_from_reader(reader, src_name, *, search_dirs=None):
    factory = ParserFactory().create(std="f2008")
    ast = factory(reader)
    output = []
    warnings = []
    module_map = {}
    for module in walk(ast, Fortran2003.Module):
        name = _stmt_name(module.content[0])
        mod_node = Module(name)
        output.append(mod_node)
        module_map[name] = mod_node

        default_access = "public"
        access_map = {}
        decl_map = {}

        for part in module.content:
            if isinstance(part, Fortran2003.Module_Stmt):
                continue
            if isinstance(part, Fortran2003.End_Module_Stmt):
                break
            if isinstance(part, Fortran2003.Comment):
                continue
            if isinstance(part, Fortran2003.Specification_Part):
                for c in part.content:
                    if isinstance(c, Fortran2003.Implicit_Part):
                        for cnt in c.content:
                            if isinstance(cnt, Fortran2003.Comment):
                                if cnt.items[0] != "":
                                    mod_node.body.append(Statement(cnt.items[0]))
                                continue
                            if isinstance(cnt, Fortran2003.Implicit_Stmt):
                                mod_node.body.append(Statement(cnt.string))
                            continue
                        continue
                    if isinstance(c, Fortran2003.Use_Stmt):
                        only = None
                        if c.items[4] is not None:
                            only = [s.string for s in c.items[4].items]
                        mod_node.body.append(Use(c.items[2].string, only=only))
                        continue
                    if isinstance(c, Fortran2003.Access_Stmt):
                        access_spec = c.items[0].lower()
                        if c.items[1] is None:
                            default_access = access_spec
                            mod_node.body.append(Statement(c.string))
                        else:
                            for n in c.items[1].items:
                                name_n = n.string
                                if name_n in decl_map:
                                    decl_map[name_n].access = access_spec
                                else:
                                    access_map[name_n] = access_spec
                        continue
                    if isinstance(
                        c,
                        Fortran2008.type_declaration_stmt_r501.Type_Declaration_Stmt,
                    ):
                        if mod_node.decls is None:
                            mod_node.decls = Block([])
                        for decl in _parse_decl_stmt(
                            c, allow_intent=False, allow_access=True
                        ):
                            if decl.access is None:
                                if decl.name in access_map:
                                    decl.access = access_map.pop(decl.name)
                                else:
                                    decl.access = default_access
                            decl.constant = True
                            decl_map[decl.name] = decl
                            mod_node.decls.append(decl)
                        continue
                    print(type(c), c)
                    print(c.items)
                    raise RuntimeError(f"Unsupported statement: {type(c)} {c.string}")
                continue
            if isinstance(part, Fortran2003.Module_Subprogram_Part):
                for c in part.content:
                    if isinstance(c, Fortran2003.Contains_Stmt):
                        continue
                    if isinstance(c, Fortran2003.Comment):
                        continue
                    if isinstance(
                        c,
                        (
                            Fortran2003.Function_Subprogram,
                            Fortran2003.Subroutine_Subprogram,
                        ),
                    ):
                        mod_node.routines.append(
                            _parse_routine(
                                c, src_name, mod_node, module_map, search_dirs
                            )
                        )
                    else:
                        print(type(c), c)
                        print(c.items)
                        raise RuntimeError(
                            "Unsupported  statement: {type(c)} {c.string}"
                        )
            else:
                print(type(part), part)
                raise RuntimeError("Unsupported statement: {type(part)} {part.string}")
    return output


def find_subroutines(modules):
    """Return the names of routines contained in ``modules``.

    Parameters
    ----------
    modules : Iterable[Module]
        Modules as returned by :func:`parse_file`.

    Returns
    -------
    List[str]
        Names of all subroutines and functions.
    """

    names = []
    for mod in modules:
        for routine in mod.routines:
            names.append(routine.name)
    return names


def _parse_routine(content, src_name, module=None, module_map=None, search_dirs=None):
    """Return node tree correspoinding to the input AST"""

    def _parse_stmt(stmt, decls) -> Node:
        if isinstance(stmt, Fortran2003.Comment):
            return None
        line_no = None
        if getattr(stmt, "item", None) is not None and getattr(stmt.item, "span", None):
            line_no = stmt.item.span[0]
        info = {
            "file": src_name,
            "line": line_no,
            "code": stmt.string,
        }
        if isinstance(stmt, Fortran2003.Assignment_Stmt):
            lhs = _stmt2op(stmt.items[0], decls)
            rhs = _stmt2op(stmt.items[2], decls)
            return Assignment(lhs, rhs, False, info)
        if isinstance(stmt, Fortran2003.Call_Stmt):
            name = stmt.items[0].tofortran()
            args = []
            arg_keys = []
            if stmt.items[1] is not None:
                for arg in stmt.items[1].items:
                    if isinstance(arg, str):
                        continue
                    key = None
                    val = arg
                    if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                        if arg.items[0] is not None:
                            key = str(arg.items[0])
                        val = arg.items[1]
                    args.append(_stmt2op(val, decls))
                    arg_keys.append(key)
            return CallStatement(name, args, arg_keys=arg_keys, info=info)
        if isinstance(stmt, Fortran2003.If_Construct):
            cond_blocks = []
            cond = _stmt2op(stmt.content[0].items[0], decls)
            i = 1
            seg = []
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(
                    itm,
                    (
                        Fortran2003.Else_If_Stmt,
                        Fortran2003.Else_Stmt,
                        Fortran2003.End_If_Stmt,
                    ),
                ):
                    break
                seg.append(itm)
                i += 1
            body = _block(seg, decls)
            cond_blocks.append((cond, body))
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(itm, Fortran2003.Else_If_Stmt):
                    cond2 = _stmt2op(itm.items[0], decls)
                    i += 1
                    seg = []
                    while i < len(stmt.content):
                        j = stmt.content[i]
                        if isinstance(
                            j,
                            (
                                Fortran2003.Else_If_Stmt,
                                Fortran2003.Else_Stmt,
                                Fortran2003.End_If_Stmt,
                            ),
                        ):
                            break
                        seg.append(j)
                        i += 1
                    blk = _block(seg, decls)
                    cond_blocks.append((cond2, blk))
                elif isinstance(itm, Fortran2003.Else_Stmt):
                    i += 1
                    seg = []
                    while i < len(stmt.content):
                        j = stmt.content[i]
                        if isinstance(j, Fortran2003.End_If_Stmt):
                            break
                        seg.append(j)
                        i += 1
                    cond_blocks.append((None, _block(seg, decls)))
                elif isinstance(itm, Fortran2003.End_If_Stmt):
                    i += 1
                else:
                    i += 1
            return IfBlock(cond_blocks)
        if isinstance(stmt, Fortran2003.Case_Construct):
            expr = _stmt2op(stmt.content[0].items[0], decls)
            cond_blocks = []
            default = None
            i = 1
            while i < len(stmt.content) - 1:
                stmt_cond = stmt.content[i]
                i += 1
                seg = []
                while i < len(stmt.content) - 1 and not isinstance(
                    stmt.content[i], Fortran2003.Case_Stmt
                ):
                    seg.append(stmt.content[i])
                    i += 1
                blk = _block(seg, decls)
                if stmt_cond.tofortran() == "CASE DEFAULT":
                    conds = None
                else:
                    conds = tuple(
                        _stmt2op(cond, decls)
                        for cond in stmt_cond.items[0].items[0].items
                    )
                cond_blocks.append((conds, blk))
            return SelectBlock(cond_blocks, expr)
        if isinstance(stmt, Fortran2008.Block_Nonlabel_Do_Construct):
            idx = 0
            while idx < len(stmt.content) and isinstance(
                stmt.content[idx], Fortran2003.Comment
            ):
                idx += 1
            # skip Nonlabel_Do_Stmt
            idx += 1
            body = _block(stmt.content[idx:-1], decls)
            if stmt.content[idx - 1].tofortran().startswith("DO WHILE"):
                cond = _stmt2op(stmt.content[idx - 1].items[1].items[0].items[0], decls)
                return DoWhile(body, cond)
            else:
                itm = stmt.content[idx - 1].items[1].items[1]
                index = _stmt2op(itm[0], decls)
                start_val = _stmt2op(itm[1][0], decls)
                end_val = _stmt2op(itm[1][1], decls)
                if len(itm[1]) == 2:
                    step = None
                else:
                    step = _stmt2op(itm[1][2], decls)
                return DoLoop(body, index, OpRange([start_val, end_val, step]))
        if isinstance(stmt, Fortran2003.Return_Stmt):
            return Statement("return")

        raise ValueError(f"stmt is not supported: {stmt}")

    def _block(body_list, decls):
        blk = Block([])
        for st in body_list:
            node = _parse_stmt(st, decls)
            if node is not None:
                blk.append(node)
        return blk

    stmt = None
    directives = {}
    for item in content.content:
        if isinstance(item, Fortran2003.Comment):
            text = item.items[0].strip()
            if text.startswith("!$FAD"):
                body = text[5:].strip()
                if ":" in body:
                    key, rest = body.split(":", 1)
                    key = key.strip().upper()
                    values = [a.strip() for a in rest.split(",") if a.strip()]
                    directives[key] = values
                else:
                    directives[body.strip().upper()] = True
            continue
        if isinstance(item, (Fortran2003.Subroutine_Stmt, Fortran2003.Function_Stmt)):
            stmt = item
            name = _stmt_name(stmt)
            args = [str(a) for a in (stmt.items[2].items if stmt.items[2] else [])]
            if isinstance(content, Fortran2003.Subroutine_Subprogram):
                routine = Subroutine(name, args)
            elif isinstance(content, Fortran2003.Function_Subprogram):
                result = str(stmt.items[3].items[0])
                routine = Function(name, args, result)
            routine.directives = directives
            continue
        if isinstance(item, Fortran2003.Specification_Part):
            const_args = (
                routine.directives.get("CONSTANT_ARGS") if routine.directives else None
            )
            routine.decls = _parse_decls(
                item, const_args, allow_intent=True, allow_access=False
            )

            # build mod declarations for use in execution parsing
            mod_decls = Block([])
            Node._id_counter -= 1
            if module is not None:
                for child in module.body.iter_children():
                    if isinstance(child, Use):
                        mod_decls.append(child)
                        used = module_map.get(child.name) if module_map else None
                        if used and used.decls is not None:
                            for d in used.decls:
                                if child.only is None or (d.name in child.only):
                                    mod_decls.append(d)
                        elif search_dirs:
                            vars_map = _load_fadmod_decls(child.name, search_dirs)
                            for name, info in vars_map.items():
                                if child.only is None or name in child.only:
                                    mod_decls.append(
                                        Declaration(
                                            name,
                                            info["typename"],
                                            info.get("kind"),
                                            (
                                                tuple(info["dims"])
                                                if info.get("dims") is not None
                                                else None
                                            ),
                                            None,
                                            info.get("parameter", False),
                                            info.get("constant", False),
                                            init=info.get("init"),
                                            access=info.get("access"),
                                        )
                                    )
            if module is not None and module.decls is not None:
                mod_decls.extend(list(module.decls.iter_children()))
            for child in routine.decls.iter_children():
                if isinstance(child, Use):
                    used = module_map.get(child.name) if module_map else None
                    if used and used.decls is not None:
                        for d in used.decls:
                            if child.only is None or (d.name in child.only):
                                mod_decls.append(d)
                    elif search_dirs:
                        vars_map = _load_fadmod_decls(child.name, search_dirs)
                        for name, info in vars_map.items():
                            if child.only is None or name in child.only:
                                mod_decls.append(
                                    Declaration(
                                        name,
                                        info["typename"],
                                        info.get("kind"),
                                        (
                                            tuple(info["dims"])
                                            if info.get("dims") is not None
                                            else None
                                        ),
                                        None,
                                        info.get("parameter", False),
                                        info.get("constant", False),
                                        init=info.get("init"),
                                        access=info.get("access"),
                                    )
                                )

            mod_decls.extend(list(routine.decls.iter_children()))
            routine.mod_decls = mod_decls
            continue
        if isinstance(item, Fortran2003.Execution_Part):
            decls = (
                routine.mod_decls if routine.mod_decls is not None else routine.decls
            )
            for stmt in item.content:
                node = _parse_stmt(stmt, decls)
                if node is not None:
                    routine.content.append(node)
            continue
        if isinstance(item, Fortran2003.End_Subroutine_Stmt):
            continue
        if isinstance(item, Fortran2003.End_Function_Stmt):
            continue
        raise RuntimeError(f"Unsupported statement: {type(item)} {item.items}")

    if routine.mod_decls is None:
        # merge declarations from module and used modules
        mod_decls = Block([])
        Node._id_counter -= 1
        if module is not None:
            for child in module.body.iter_children():
                if isinstance(child, Use):
                    mod_decls.append(child)
                    used = module_map.get(child.name) if module_map else None
                    if used and used.decls is not None:
                        for d in used.decls:
                            if child.only is None or (d.name in child.only):
                                mod_decls.append(d)
                    elif search_dirs:
                        vars_map = _load_fadmod_decls(child.name, search_dirs)
                        for name, info in vars_map.items():
                            if child.only is None or name in child.only:
                                mod_decls.append(
                                    Declaration(
                                        name,
                                        info["typename"],
                                        info.get("kind"),
                                        (
                                            tuple(info["dims"])
                                            if info.get("dims") is not None
                                            else None
                                        ),
                                        None,
                                        info.get("parameter", False),
                                        info.get("constant", False),
                                        init=info.get("init"),
                                        access=info.get("access"),
                                    )
                                )
            if module.decls is not None:
                mod_decls.extend(list(module.decls.iter_children()))

        for child in routine.decls.iter_children():
            if isinstance(child, Use):
                used = module_map.get(child.name) if module_map else None
                if used and used.decls is not None:
                    for d in used.decls:
                        if child.only is None or (d.name in child.only):
                            mod_decls.append(d)
                elif search_dirs:
                    vars_map = _load_fadmod_decls(child.name, search_dirs)
                    for name, info in vars_map.items():
                        if child.only is None or name in child.only:
                            mod_decls.append(
                                Declaration(
                                    name,
                                    info["typename"],
                                    info.get("kind"),
                                    (
                                        tuple(info["dims"])
                                        if info.get("dims") is not None
                                        else None
                                    ),
                                    None,
                                    info.get("parameter", False),
                                    info.get("constant", False),
                                    init=info.get("init"),
                                    access=info.get("access"),
                                )
                            )

        mod_decls.extend(list(routine.decls.iter_children()))
        routine.mod_decls = mod_decls

    return routine
