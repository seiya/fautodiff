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
from typing import List, Tuple, Dict, Optional, Union

from .code_tree import (
    Assignment,
    PointerAssignment,
    PointerClear,
    Block,
    CallStatement,
    Declaration,
    Interface,
    TypeDef,
    DoLoop,
    DoWhile,
    Function,
    IfBlock,
    Module,
    Node,
    SelectBlock,
    WhereBlock,
    ForallBlock,
    BlockConstruct,
    Statement,
    ExitStmt,
    CycleStmt,
    Subroutine,
    Use,
    Allocate,
    Deallocate,
)
from .operators import (
    INTRINSIC_FUNCTIONS,
    AryIndex,
    OpAdd,
    OpChar,
    OpDiv,
    Operator,
    OpFunc,
    OpFuncUser,
    OpInt,
    OpAry,
    OpFalse,
    OpTrue,
    OpLogic,
    OpMul,
    OpNeg,
    OpNot,
    OpPow,
    OpRange,
    OpReal,
    OpComplex,
    OpSub,
    OpVar,
    OpType,
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


def _stmt2op(stmt, decl_map:dict, type_map:dict) -> Operator:
    """Return Operator from statement."""

    if isinstance(stmt, Fortran2003.Actual_Arg_Spec):
        return _stmt2op(stmt.items[1], decl_map, type_map)

    if isinstance(stmt, Fortran2003.Int_Literal_Constant):
        return OpInt(val=int(stmt.items[0]), kind=stmt.items[1])

    if isinstance(stmt, Fortran2003.Signed_Int_Literal_Constant):
        text = stmt.tofortran()
        if "_" in text:
            val_str, kind = text.split("_", 1)
        else:
            val_str, kind = text, None
        val = int(val_str)
        if val < 0:
            return -OpInt(-val, kind=kind)
        return OpInt(val, kind=kind)

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
            raise ValueError(f"Failed to convert real number: {stmt}")

    if isinstance(stmt, Fortran2003.Signed_Real_Literal_Constant):
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
            raise ValueError(f"Failed to convert real number: {stmt}")

    if isinstance(stmt, Fortran2003.Complex_Literal_Constant):
        real = _stmt2op(stmt.items[0], decl_map, type_map)
        imag = _stmt2op(stmt.items[1], decl_map, type_map)
        kind = getattr(real, "kind", None)
        if getattr(imag, "kind", None) == kind:
            pass
        elif getattr(imag, "kind", None) is not None:
            kind = imag.kind
        return OpComplex(real, imag, kind=kind)

    if isinstance(stmt, Fortran2003.Name):
        name = stmt.string
        if name in decl_map:
            decl = decl_map[name]
        elif name in type_map:
            decl = type_map[name]
        else:
            raise ValueError(f"Not found in the declaration section: {name}")

        kind = decl.kind
        if kind is None and decl.typename.lower().startswith("double"):
            kind = "8"

        return OpVar(
            name=name,
            typename=decl.typename,
            kind=kind,
            char_len=decl.char_len,
            dims=decl.dims,
            ad_target=decl.ad_target(),
            is_constant=decl.parameter or getattr(decl, "constant", False),
            allocatable=getattr(decl, "allocatable", False),
            pointer=getattr(decl, "pointer", False),
            optional=getattr(decl, "optional", False),
            declared_in=decl.declared_in,
        )

    if isinstance(stmt, Fortran2003.Part_Ref):
        name = stmt.items[0].string
        index = AryIndex([_stmt2op(x, decl_map, type_map) for x in stmt.items[1].items])
        # check it is array or not
        decl = None
        if type_map is not None and name in type_map:
            decl = type_map[name]
        elif name in decl_map:
            decl = decl_map[name]
        if decl is not None:
            return OpVar(
                name=name,
                index=index,
                typename=decl.typename,
                kind=decl.kind,
                char_len=decl.char_len,
                dims=decl.dims,
                ad_target=decl.ad_target(),
                is_constant=decl.parameter or getattr(decl, "constant", False),
                allocatable=getattr(decl, "allocatable", False),
                pointer=getattr(decl, "pointer", False),
                optional=getattr(decl, "optional", False),
                declared_in=decl.declared_in,
            )
        else:  # must be function
            name_l = name.lower()
            args = [
                _stmt2op(arg, decl_map, type_map)
                for arg in getattr(stmt.items[1], "items", [])
                if not isinstance(arg, str)
            ]
            if name_l in INTRINSIC_FUNCTIONS:
                return OpFunc(name_l, args)
            return OpFuncUser(name_l, args)

    if isinstance(stmt, Fortran2003.Subscript_Triplet):
        args = [(x and _stmt2op(x, decl_map, type_map)) for x in stmt.items]
        return OpRange(args)

    if isinstance(stmt, Fortran2003.Intrinsic_Function_Reference):
        name = stmt.items[0].tofortran().lower()
        args = [
            _stmt2op(arg, decl_map, type_map)
            for arg in getattr(stmt.items[1], "items", [])
            if not isinstance(arg, str)
        ]
        return OpFunc(name, args)

    if isinstance(stmt, Fortran2003.Char_Literal_Constant):
        name = stmt.items[0]
        return OpChar(name=name)

    if isinstance(stmt, Fortran2003.Logical_Literal_Constant):
        const = stmt.string.lower()
        if const in (".true.", ".t."):
            return OpTrue()
        if const in (".false.", ".f."):
            return OpFalse()
        raise ValueError(f"Unsupported logical constant: {stmt.string}")

    if isinstance(stmt, Fortran2003.Mult_Operand):
        if stmt.items[1] == "**":
            args = [_stmt2op(stmt.items[0], decl_map, type_map), _stmt2op(stmt.items[2], decl_map, type_map)]
            return OpPow(args)
        else:
            raise ValueError(f"Unsupported Mult_operand type: {stmt}")

    if isinstance(stmt, Fortran2003.Level_2_Unary_Expr):
        op = stmt.items[0]
        args = [_stmt2op(stmt.items[1], decl_map, type_map)]
        if op == "-":
            return OpNeg(args)
        else:
            raise ValueError(f"Unsupported Level_2_Unary_Expr type: {stmt}")

    if isinstance(stmt, Fortran2003.Level_2_Expr):
        op = stmt.items[1]
        args = [_stmt2op(stmt.items[0], decl_map, type_map), _stmt2op(stmt.items[2], decl_map, type_map)]
        if op == "+":
            return OpAdd(args)
        elif op == "-":
            return OpSub(args)
        else:
            raise ValueError(f"Unsupported Level_2_Expr type: {stmt}")

    if isinstance(stmt, Fortran2003.Add_Operand):
        op = stmt.items[1]
        args = [_stmt2op(stmt.items[0], decl_map, type_map), _stmt2op(stmt.items[2], decl_map, type_map)]
        if op == "*":
            return OpMul(args)
        elif op == "/":
            return OpDiv(args)
        else:
            raise ValueError(f"Unsupported Add_Operand type: {stmt}")

    if isinstance(stmt, (Fortran2003.Equiv_Operand, Fortran2003.Or_Operand)):
        op = stmt.items[1].lower()
        args = [_stmt2op(stmt.items[0], decl_map, type_map), _stmt2op(stmt.items[2], decl_map, type_map)]
        if op == ".and." or op == ".or.":
            return OpLogic(op, args)

        else:
            raise ValueError(f"Unsupported Equiv_Operand type: {stmt}")

    if isinstance(stmt, Fortran2003.And_Operand):
        if stmt.items[0] == ".NOT.":
            return OpNot([_stmt2op(stmt.items[1], decl_map, type_map)])
        else:
            raise ValueError(f"Unsupported And_Operand: {stmt}")

    if isinstance(stmt, Fortran2003.Parenthesis):
        return _stmt2op(stmt.items[1], decl_map, type_map)

    if isinstance(stmt, Fortran2003.Level_4_Expr):
        op = stmt.items[1]
        args = [_stmt2op(stmt.items[0], decl_map, type_map), _stmt2op(stmt.items[2], decl_map, type_map)]
        return OpLogic(op=op, args=args)

    if isinstance(stmt, Fortran2003.Array_Constructor):
        elms = [_stmt2op(elm, decl_map, type_map) for elm in stmt.items[1].items]
        return OpAry(elms)

    if isinstance(stmt, Fortran2003.Data_Ref):
        ref = _stmt2op(stmt.items[0], decl_map, type_map)
        ref_name = ref.name
        if decl_map[ref_name].type_def is not None:
            comp = _stmt2op(stmt.items[1], decl_map, decl_map[ref_name].type_def.map)
            comp.ref_var = ref
            return comp
        else:
            raise RuntimeError(f"definition not found: {stmt}, {name}, {comp}")

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


def parse_file(path, *, search_dirs=None, decl_map=None, type_map=None):
    """Parse ``path`` and return a list of :class:`Module` nodes."""
    reader = FortranFileReader(path, ignore_comments=False)
    return _parse_from_reader(reader, path, search_dirs=search_dirs, decl_map=decl_map, type_map=type_map)


def parse_src(src, *, search_dirs=None, decl_map=None, type_map=None):
    """Parse ``src`` and return a list of :class:`Module` nodes."""
    reader = FortranStringReader(src, ignore_comments=False)
    return _parse_from_reader(reader, "<string>", search_dirs=search_dirs, decl_map=decl_map, type_map=type_map)


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
    stmt,
    constant_vars=None,
    type_map=None,
    *,
    allow_intent=True,
    allow_access=False,
    declared_in="routine",
) -> List[Declaration]:
    """Parse a single ``Type_Declaration_Stmt`` and return declarations."""

    if not isinstance(
        stmt,
        (
            Fortran2003.Type_Declaration_Stmt,
            Fortran2008.type_declaration_stmt_r501.Type_Declaration_Stmt,
            Fortran2008.data_component_def_stmt_r436.Data_Component_Def_Stmt,
        ),
    ):
        raise RuntimeError(f"Unsupported statement: {type(stmt)} {stmt}")

    type_spec = stmt.items[0]
    kind = None
    char_len = None
    type_def = None
    if isinstance(type_spec, Fortran2003.Intrinsic_Type_Spec):
        base_type = type_spec.items[0]
        selector = type_spec.items[1]
        if selector is None:
            pass
        elif isinstance(selector, Fortran2003.Kind_Selector):
            if selector.items[1]:
                kind = selector.items[1].string
        elif isinstance(selector, Fortran2003.Length_Selector):
            char_len = selector.items[1].string
        else:
            raise RuntimeError(f"selector is not supported: {type(selector)} {selector}")
    elif isinstance(type_spec, Fortran2003.Declaration_Type_Spec):
        name = type_spec.items[1].string
        base_type = type_spec.string
        if type_map is not None and name in type_map:
            type_def = type_map[name]
        elif name == "c_ptr": # tentative
            type_def = TypeDef(name=name, components=[])
        else:
            raise RuntimeError(f"type definition not found: {name}")
    else:
        raise RuntimeError(f"base_type is not found: {type(type_spec)} {type_spec}")

    intent = None
    dim_attr = None
    parameter = False
    access = None
    allocatable = False
    pointer = False
    optional = False
    target = False
    save = False
    value = False
    volatile = False
    asynchronous = False
    attrs = stmt.items[1]
    if attrs is not None:
        for attr in attrs.items:
            if isinstance(attr, Fortran2003.Intent_Attr_Spec):
                if not allow_intent:
                    raise RuntimeError("Module variables must not specify INTENT")
                intent = attr.items[1].string.lower()
                continue
            if isinstance(attr, Fortran2008.attr_spec_r502.Attr_Spec):
                attr_name = attr.string.upper()
                if attr_name == "PARAMETER":
                    parameter = True
                    continue
                if attr_name == "POINTER":
                    pointer = True
                    continue
                if attr_name == "OPTIONAL":
                    optional = True
                    continue
                if attr_name == "ALLOCATABLE":
                    allocatable = True
                    continue
                if attr_name == "TARGET":
                    target = True
                    continue
                if attr_name == "SAVE":
                    save = True
                    continue
                if attr_name == "VALUE":
                    value = True
                    continue
                if attr_name == "VOLATILE":
                    volatile = True
                    continue
                if attr_name in ("ASYNC", "ASYNCHRONOUS"):
                    asynchronous = True
                    continue
                raise RuntimeError(f"Unsupported attribute: {type(attr)} {attr}")
            if isinstance(attr, Fortran2008.component_attr_spec_r437.Component_Attr_Spec):
                allocatable = True
                continue
            if isinstance(attr, Fortran2003.Dimension_Attr_Spec):
                dim_attr = tuple(v.string for v in attr.items[1].items)
                continue
            if isinstance(attr, Fortran2003.Access_Spec):
                attr_name = attr.string.lower()
                if attr_name == "public":
                    if not allow_access:
                        raise RuntimeError("Routine variables must not specify PUBLIC")
                    access = "public"
                    continue
                if attr_name == "private":
                    if not allow_access:
                        raise RuntimeError("Routine variables must not specify PRIVATE")
                    access = "private"
                    continue

            print(attr.__dict__)
            raise RuntimeError(f"Unsupported attribute: {type(attr)} {attr}")

            # print(type(attr))
            # name = getattr(
            #     attr, "string", str(getattr(attr, "items", [None])[0])
            # ).lower()
            # if allow_access and name in ("public", "private"):
            #     access = name

    decls = []
    for entity in stmt.items[2].items:
        dims = None
        init_val = None
        if isinstance(entity, (Fortran2003.Entity_Decl, Fortran2003.Component_Decl)):
            name = entity.items[0].string
            dim_spec = entity.items[1]
            if isinstance(dim_spec, (Fortran2003.Explicit_Shape_Spec_List, Fortran2003.Assumed_Shape_Spec_List, Fortran2003.Deferred_Shape_Spec_List)):
                dims = tuple(v.string for v in dim_spec.items)
            elif isinstance(dim_spec, Fortran2003.Assumed_Size_Spec):
                dims = "*"
            elif dim_spec is None:
                dims = dim_attr
            else:
                raise RuntimeError(f"Unsupported dimension spec: {type(dim_spec)} {dim_spec}")
            if entity.items[3] is not None:
                init_val = entity.items[3].items[1].string
        else:
            raise RuntimeError(f"Unsupported statement: {type(entity)} {entity}")

        constant = False
        if constant_vars and name in constant_vars:
            constant = True

        decls.append(
            Declaration(
                name=name,
                typename=base_type.lower(),
                kind=kind,
                char_len=char_len,
                dims=dims,
                intent=intent,
                parameter=parameter,
                constant=constant,
                init_val=init_val,
                access=access,
                allocatable=allocatable,
                pointer=pointer,
                optional=optional,
                target=target,
                save=save,
                value=value,
                volatile=volatile,
                asynchronous=asynchronous,
                type_def=type_def,
                declared_in=declared_in,
            )
        )

    return decls

def _parse_directive(text: str, directives: dict) -> None:
    body = text[5:].strip()
    if ":" in body:
        key, rest = body.split(":", 1)
        key = key.strip().upper()
        values = [a.strip() for a in rest.split(",") if a.strip()]
        directives[key] = values
    else:
        directives[body.strip().upper()] = True
    return

def _parse_decls(
    spec,
    *,
    directives: dict,
    decl_map: dict,
    type_map: Dict[str, TypeDef],
    declared_in: str ="routine",
    allow_intent: bool = True,
    allow_access: bool = False,
    default_access: Optional[str] = None,
    module_map: Optional[dict] = None,
    search_dirs: Optional[List[str]] = None,
) -> Tuple[List[Use], List[Node], List[Node]]:
    """Return declarations parsed from a specification part."""

    uses: List[Use] = []
    decls: List[Node] = []
    nodes: List[Node] = []
    access_map = {}

    for item in spec.content:
        if isinstance(item, Fortran2003.Implicit_Part):
            for cnt in item.content:
                if isinstance(cnt, Fortran2003.Comment):
                    text = cnt.items[0].strip()
                    if text.startswith("!$FAD"):
                        _parse_directive(text, directives)
                        continue
                    if text != "":
                        decls.append(Statement(text))
                    continue
                # if isinstance(cnt, Fortran2003.Implicit_Stmt):
                #     decls.append(Statement(cnt.string))
            continue
        if isinstance(item, Fortran2003.Use_Stmt):
            only = None
            if item.items[4] is not None:
                only = [s.string for s in item.items[4].items]
            mod_name = item.items[2].string
            uses.append(Use(mod_name, only=only))
            _search_use(mod_name, only, decl_map, module_map, search_dirs)
            continue
        if isinstance(item, Fortran2003.Access_Stmt):
            if not allow_access:
                raise RuntimeError("Unexpected error")
            access_spec = item.items[0].lower()
            if item.items[1] is None:
                default_access = access_spec
                nodes.append(Statement(item.string))
            else:
                for n in item.items[1].items:
                    name_n = n.string
                    if name_n in decl_map:
                        decl_map[name_n].access = access_spec
                    else:
                        access_map[name_n] = access_spec
            continue
        if isinstance(
            item,
            (
                Fortran2003.Type_Declaration_Stmt,
                Fortran2008.type_declaration_stmt_r501.Type_Declaration_Stmt,
            ),
        ):
            constant_vars = directives.get("CONSTANT_VARS")
            for decl in _parse_decl_stmt(
                item,
                constant_vars,
                type_map,
                allow_intent=allow_intent,
                allow_access=allow_access,
                declared_in=declared_in,
            ):
                if allow_access and decl.access is None:
                    if decl.name in access_map:
                        decl.access = access_map.pop(decl.name)
                    else:
                        decl.access = default_access
                decl_map[decl.name] = decl
                decls.append(decl)
            continue
        if isinstance(item, Fortran2003.Interface_Block):
            name = None
            procs = []
            for cnt in item.content:
                if isinstance(cnt, Fortran2003.Interface_Stmt):
                    name = cnt.items[0].string
                    continue
                if isinstance(cnt, Fortran2008.procedure_stmt_r1206.Procedure_Stmt):
                    if isinstance(cnt.items[0], Fortran2008.Procedure_Name_List):
                        for proc in cnt.items[0].items:
                            procs.append(proc.string)
                        continue
                    raise RuntimeError(f"Unsupported statement: {type(cnt.items[0])} {cnt.items}")
                if isinstance(cnt, Fortran2003.End_Interface_Stmt):
                    if name is not None and procs:
                        decls.append(Interface(name, module_procs=procs))
                        continue
                raise RuntimeError(f"Unsupported statement: {type(cnt)} {cnt}")
            continue
        if isinstance(item, Fortran2003.Derived_Type_Def):
            type_name = None
            components: List[Declaration] = []
            procs: List[list] = []
            access = None
            bind = None
            abstract = False
            sequence = False
            for cnt in item.content:
                if isinstance(cnt, Fortran2003.Derived_Type_Stmt):
                    if isinstance(cnt.items[0], Fortran2003.Type_Attr_Spec_List):
                        for spec in cnt.items[0].items:
                            if isinstance(spec, Fortran2003.Type_Attr_Spec):
                                key = str(spec.items[0]).upper()
                                if key == "EXTENDS":
                                    parent = spec.items[1].string
                                    if not parent in type_map:
                                        raise RuntimeError(f"Type definition not found: {parent}")
                                    for decl in type_map[parent].iter_children():
                                        components.append(decl)
                                    continue
                                if key == "BIND":
                                    bind = spec.string[5:-1].strip()
                                    continue
                                if key == "ABSTRACT":
                                    abstract = True
                                    continue
                                if key == "SEQUENCE":
                                    sequence = True
                                    continue
                            if isinstance(spec, Fortran2003.Access_Spec):
                                if spec.string == "PUBLIC":
                                    access = "public"
                                elif spec.string == "PRIVATE":
                                    access = "private"
                                continue
                            print(cnt)
                            raise RuntimeError(f"Unsupported spec: {type(spec)} {spec}")
                    type_name = cnt.items[1].string
                    continue
                if isinstance(cnt, Fortran2003.Sequence_Stmt):
                    sequence = True
                    continue
                if isinstance(cnt, Fortran2003.Component_Part):
                    for c in cnt.content:
                        components.extend(_parse_decl_stmt(c, allow_intent=False, allow_access=False, declared_in=declared_in))
                    continue
                if isinstance(cnt, Fortran2003.Type_Bound_Procedure_Part):
                    for c in cnt.content:
                        if isinstance(c, Fortran2003.Contains_Stmt):
                            continue
                        if isinstance(c, Fortran2003.Specific_Binding):
                            attrs = None
                            if isinstance(c.items[1], Fortran2003.Binding_Attr_List):
                                attrs = [item.string for item in c.items[1].items]
                            name = c.items[3].string
                            init = c.items[4].string
                            procs.append([name, attrs, init])
                    continue

                if isinstance(cnt, Fortran2003.End_Type_Stmt):
                    if type_name is not None:
                        type_def = TypeDef(name=type_name, components=components, procs=procs, access=access, bind=bind, abstract=abstract, sequence=sequence)
                        decls.append(type_def)
                        type_map[type_name] = type_def
                        continue
                raise RuntimeError(f"Unsupported statement: {type(cnt)} {cnt}")
            continue

        raise RuntimeError(f"Unsupported statement: {type(item)} {item}")
    return (uses, decls, nodes)

def _search_use(name: str, only: Optional[List[str]], decl_map: dict, module_map: dict, search_dirs: Optional[List[str]]):
    used = module_map.get(name) if module_map else None
    if used and used.decls is not None:
        for d in used.decls:
            if only is None or (d.name in only):
                d = d.copy()
                d.declared_in = "use"
                decl_map[d.name] = d
    elif search_dirs:
        vars_map = _load_fadmod_decls(name, search_dirs)
        for vname, info in vars_map.items():
            if only is None or vname in only:
                decl_map[vname] = Declaration(
                        vname,
                        info["typename"],
                        info.get("kind"),
                        dims=(
                            tuple(info["dims"])
                            if info.get("dims") is not None
                            else None
                        ),
                        intent=None,
                        parameter=info.get("parameter", False),
                        constant=info.get("constant", False),
                        init_val=info.get("init_val"),
                        access=info.get("access"),
                        pointer=info.get("pointer", False),
                        optional=info.get("optional", False),
                        declared_in="use",
                    )

def _parse_from_reader(reader, src_name, *, search_dirs=None, decl_map=None, type_map=None) -> List[Module]:
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

        if decl_map is not None:
            decl_map_new = decl_map.copy()
        else:
            decl_map_new = {}
        if type_map is not None:
            type_map_new = type_map.copy()
        else:
            type_map_new = {}
        module_directives = {}
        allocate_vars: List[OpVar] = []

        for part in module.content:
            if isinstance(part, Fortran2003.Module_Stmt):
                continue
            if isinstance(part, Fortran2003.End_Module_Stmt):
                break
            if isinstance(part, Fortran2003.Comment):
                continue
            if isinstance(part, Fortran2003.Specification_Part):
                uses, decls, nodes = _parse_decls(
                            part,
                            directives=module_directives,
                            decl_map=decl_map_new,
                            type_map=type_map_new,
                            declared_in="module",
                            allow_intent=False,
                            allow_access=True,
                            default_access="public",
                            module_map=module_map,
                            search_dirs=search_dirs,
                        )
                if "CONSTANT_VARS" in module_directives:
                    for n in module_directives["CONSTANT_VARS"]:
                        if n in decl_map_new:
                            decl_map_new[n].constant = True
                if uses:
                    mod_node.uses = Block(uses)
                if decls:
                    mod_node.decls = Block(decls)
                if nodes:
                    mod_node.body = Block(nodes)
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
                                content=c,
                                src_name=src_name,
                                allocate_vars=allocate_vars,
                                decl_map_mod=decl_map_new,
                                type_map_mod=type_map_new,
                                module_map=module_map,
                                search_dirs=search_dirs,
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
        mod_node.directives = module_directives
    return output


def find_subroutines(modules: List[Module]) -> List[str]:
    """Return the names of routines contained in ``modules``.
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


def _parse_routine(content,
                   src_name: str,
                   allocate_vars: List[OpVar],
                   decl_map_mod: dict,
                   type_map_mod: dict,
                   module_map: dict,
                   search_dirs: Optional[List[str]]=None
                   ):
    """Return node tree correspoinding to the input AST"""

    def _parse_stmt(stmt, decl_map: dict, type_map: dict) -> Optional[Node]:
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
            lhs = _stmt2op(stmt.items[0], decl_map, type_map)
            rhs = _stmt2op(stmt.items[2], decl_map, type_map)
            return Assignment(lhs, rhs, False, info)
        if isinstance(stmt, Fortran2003.Pointer_Assignment_Stmt):
            lhs = _stmt2op(stmt.items[0], decl_map, type_map)
            if stmt.items[2].string == "null()":
                return PointerClear(lhs, None)
            rhs = _stmt2op(stmt.items[2], decl_map, type_map)
            return PointerAssignment(lhs, rhs, info=info)
        if isinstance(stmt, Fortran2003.Write_Stmt):
            return None
        if isinstance(stmt, Fortran2003.Print_Stmt):
            return None
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
                    args.append(_stmt2op(val, decl_map, type_map))
                    arg_keys.append(key)
            return CallStatement(name, args, arg_keys=arg_keys, info=info)
        if isinstance(stmt, Fortran2003.Allocate_Stmt):
            alloc_list = None
            for itm in stmt.items:
                if isinstance(itm, Fortran2003.Allocation_List):
                    alloc_list = itm
                    break
            vars = []
            if alloc_list is not None:
                for alloc in alloc_list.items:
                    var = _stmt2op(alloc.items[0], decl_map, type_map)
                    shape = alloc.items[1]
                    if shape is not None:
                        dims = []
                        if isinstance(shape, Fortran2003.Allocate_Shape_Spec_List):
                            for spec in shape.items:
                                lb = spec.items[0]
                                ub = spec.items[1]
                                if lb is None and ub is None:
                                    dims.append(None)
                                elif lb is None:
                                    dims.append(_stmt2op(ub, decl_map, type_map))
                                else:
                                    dims.append(
                                        OpRange([
                                            _stmt2op(lb, decl_map, type_map),
                                            _stmt2op(ub, decl_map, type_map),
                                            None,
                                        ])
                                    )
                        elif isinstance(shape, Fortran2003.Section_Subscript_List):
                            for spec in shape.items:
                                if isinstance(spec, str):
                                    continue
                                dims.append(_stmt2op(spec, decl_map, type_map))
                        if dims:
                            var = var.change_index(AryIndex(dims))
                    vars.append(var)
            allocate_vars.extend(vars)
            return Allocate(vars)
        if isinstance(stmt, Fortran2003.Deallocate_Stmt):
            obj_list = None
            for itm in stmt.items:
                if isinstance(itm, Fortran2003.Allocate_Object_List):
                    obj_list = itm
                    break
            vars = []
            if obj_list is not None:
                for obj in obj_list.items:
                    if isinstance(obj, str):
                        continue
                    v = _stmt2op(obj, decl_map, type_map)
                    var = next((v2 for v2 in allocate_vars if v2.name == v.name), None)
                    if var is None:
                        raise ValueError(
                            f"Variable {v.name} is not allocate_vars in this module."
                        )
                    vars.append(var)
            return Deallocate(vars)
        if isinstance(stmt, Fortran2003.If_Construct):
            cond_blocks = []
            while isinstance(stmt.content[0], Fortran2003.Comment):
                # skip comment
                stmt.content.pop(0)
            cond = _stmt2op(stmt.content[0].items[0], decl_map, type_map)
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
            body = _block(seg, decl_map, type_map)
            cond_blocks.append((cond, body))
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(itm, Fortran2003.Else_If_Stmt):
                    cond2 = _stmt2op(itm.items[0], decl_map, type_map)
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
                    blk = _block(seg, decl_map, type_map)
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
                    cond_blocks.append((None, _block(seg, decl_map, type_map)))
                elif isinstance(itm, Fortran2003.End_If_Stmt):
                    i += 1
                else:
                    i += 1
            return IfBlock(cond_blocks)
        if isinstance(stmt, Fortran2008.if_stmt_r837.If_Stmt):
            cond = _stmt2op(stmt.items[0], decl_map, type_map)
            body = _block([stmt.items[1]], decl_map, type_map)
            if body is None:
                return None
            return IfBlock([(cond, body)])
        if isinstance(stmt, Fortran2003.Case_Construct):
            expr = _stmt2op(stmt.content[0].items[0], decl_map, type_map)
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
                blk = _block(seg, decl_map, type_map)
                if stmt_cond.tofortran() == "CASE DEFAULT":
                    conds = None
                else:
                    conds = tuple(
                        _stmt2op(cond, decl_map, type_map)
                        for cond in stmt_cond.items[0].items[0].items
                    )
                cond_blocks.append((conds, blk))
            return SelectBlock(cond_blocks, expr)
        if isinstance(stmt, Fortran2003.Select_Type_Construct):
            expr = _stmt2op(stmt.content[0].items[1], decl_map, type_map)
            cond_blocks = []
            default = None
            i = 1
            while i < len(stmt.content) - 1:
                stmt_cond = stmt.content[i]
                i += 1
                seg = []
                while i < len(stmt.content) - 1 and not isinstance(
                    stmt.content[i], Fortran2003.Type_Guard_Stmt
                ):
                    seg.append(stmt.content[i])
                    i += 1
                cond = stmt_cond.items[1].string
                decl_map_new = decl_map.copy()
                decl = decl_map[expr.name].copy()
                if cond in type_map:
                    decl.typename = f"type({cond})"
                    decl.type_def = type_map[cond]
                else:
                    decl.typename = cond
                decl_map_new[expr.name] = decl
                blk = _block(seg, decl_map_new, type_map)
                cond_blocks.append(((OpType(cond),), blk))
            return SelectBlock(cond_blocks, expr, select_type=True)
        if isinstance(stmt, Fortran2003.Where_Construct):
            cond = _stmt2op(stmt.content[0].items[0], decl_map, type_map)
            cond_blocks = []
            i = 1
            seg = []
            while i < len(stmt.content):
                itm = stmt.content[i]
                if isinstance(itm, (Fortran2003.Elsewhere_Stmt, Fortran2003.End_Where_Stmt)):
                    blk = _block(seg, decl_map, type_map)
                    cond_blocks.append((cond, blk))
                    seg = []
                    if isinstance(itm, Fortran2003.End_Where_Stmt):
                        break
                    cond = (
                        _stmt2op(itm.items[1], decl_map, type_map)
                        if itm.items[1] is not None
                        else None
                    )
                else:
                    seg.append(itm)
                i += 1
            return WhereBlock(cond_blocks)
        if isinstance(stmt, Fortran2003.Forall_Construct):
            header = stmt.content[0].items[1]
            specs = []
            for spec in header.items[0].items:
                idx = _stmt2op(spec.items[0], decl_map, type_map)
                lb = _stmt2op(spec.items[1], decl_map, type_map)
                ub = _stmt2op(spec.items[2], decl_map, type_map)
                step = (
                    _stmt2op(spec.items[3], decl_map, type_map)
                    if spec.items[3] is not None
                    else None
                )
                specs.append((idx, OpRange([lb, ub, step])))
            mask = (
                _stmt2op(header.items[1], decl_map, type_map)
                if header.items[1] is not None
                else None
            )
            body = _block(stmt.content[1:-1], decl_map, type_map)
            return ForallBlock(body, specs, mask=mask)
        if isinstance(stmt, Fortran2008.Block_Construct):
            decl_map_new = decl_map.copy()
            type_map_new = type_map.copy()
            directives_blk: dict = {}
            idx = 1
            decls_nodes: List[Node] = []
            if len(stmt.content) > 1 and isinstance(
                stmt.content[1], Fortran2003.Specification_Part
            ):
                uses, decls, nodes = _parse_decls(
                    stmt.content[1],
                    directives=directives_blk,
                    decl_map=decl_map_new,
                    type_map=type_map_new,
                    declared_in="routine",
                    allow_intent=True,
                    allow_access=False,
                )
                decls_nodes = uses + decls + nodes
                idx = 2
            body = _block(stmt.content[idx:-1], decl_map_new, type_map_new)
            return BlockConstruct(Block(decls_nodes), body)
        if isinstance(stmt, Fortran2008.Block_Nonlabel_Do_Construct):
            idx = 0
            while idx < len(stmt.content) and isinstance(
                stmt.content[idx], Fortran2003.Comment
            ):
                idx += 1
            idx += 1
            body = _block(stmt.content[idx:-1], decl_map, type_map)
            if not isinstance(stmt.content[-1], Fortran2003.End_Do_Stmt):
                raise ValueError("Unexpected error")
            label = stmt.content[-1].items[1].string if stmt.content[-1].items[1] is not None else None
            if stmt.content[idx-1].items[1].items[0] is not None:
                cond = _stmt2op(stmt.content[idx - 1].items[1].items[0], decl_map, type_map)
                return DoWhile(body, cond, label=label)
            else:
                itm = stmt.content[idx - 1].items[1].items[1]
                index = _stmt2op(itm[0], decl_map, type_map)
                start_val = _stmt2op(itm[1][0], decl_map, type_map)
                end_val = _stmt2op(itm[1][1], decl_map, type_map)
                if len(itm[1]) == 2:
                    step = None
                else:
                    step = _stmt2op(itm[1][2], decl_map, type_map)
                return DoLoop(body, index, OpRange([start_val, end_val, step]), label=label)
        if isinstance(stmt, Fortran2003.Return_Stmt):
            return Statement("return")
        if isinstance(stmt, Fortran2003.Exit_Stmt):
            label = stmt.items[1].string if stmt.items[1] is not None else None
            return ExitStmt(label=label)
        if isinstance(stmt, Fortran2003.Cycle_Stmt):
            label = stmt.items[1].string if stmt.items[1] is not None else None
            return CycleStmt(label=label)

        print(type(stmt))
        print(stmt.items)
        raise ValueError(f"stmt is not supported: {stmt}")

    def _block(body_list, decl_map: dict, type_map: dict) -> Block:
        blk = Block([])
        for st in body_list:
            node = _parse_stmt(st, decl_map, type_map)
            if node is not None:
                blk.append(node)
        return blk

    stmt = None
    directives = {}
    decl_map = decl_map_mod.copy()
    type_map = type_map_mod.copy()
    for item in content.content:
        if isinstance(item, Fortran2003.Comment):
            text = item.items[0].strip()
            if text.startswith("!$FAD"):
                _parse_directive(text, directives)
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
            else:
                raise ValueError(type(content))
            routine.directives = directives
            continue
        if isinstance(item, Fortran2003.Specification_Part):
            uses, decls, nodes = _parse_decls(
                item,
                directives=directives,
                decl_map=decl_map,
                type_map=type_map,
                declared_in="routine",
                allow_intent=True,
                allow_access=False,
                module_map=module_map,
                search_dirs=search_dirs,
            )
            routine.decls = Block(uses + decls + nodes)
            routine.decl_map = decl_map
            continue
        if isinstance(item, Fortran2003.Execution_Part):
            for stmt in item.content:
                node = _parse_stmt(stmt, routine.decl_map, type_map)
                if node is not None:
                    routine.content.append(node)
            continue
        if isinstance(item, Fortran2003.End_Subroutine_Stmt):
            continue
        if isinstance(item, Fortran2003.End_Function_Stmt):
            continue
        raise RuntimeError(f"Unsupported statement: {type(item)} {item.items}")

    if routine.decl_map is None:
        routine.decl_map = decl_map

    return routine
