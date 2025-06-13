from __future__ import annotations

from pathlib import Path
import sys
import re

from .code_tree import (
    Block,
    Declaration,
    IfBlock,
    Assignment,
    Subroutine,
    Function,
    DoLoop,
    SelectBlock,
    Return,
    EmptyLine,
    DeclBlock,
    InitBlock,
    AdBlock,
    render_program,
)

from . import parser
from .parser import Fortran2003, walk, Block_Nonlabel_Do_Construct
from .intrinsic_rules import (
    DERIVATIVE_TEMPLATES,
    NONDIFF_INTRINSICS,
    SPECIAL_HANDLERS,
)


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
    if isinstance(expr, (Fortran2003.Intrinsic_Function_Reference, Fortran2003.Part_Ref)):
        name = expr.items[0].tofortran().lower()
        if name in DERIVATIVE_TEMPLATES:
            args = expr.items[1]
            for arg in getattr(args, "items", []):
                if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                    subexpr = arg.items[1]
                else:
                    subexpr = arg
                _collect_names(subexpr, names, unique=unique)
            return
        if isinstance(expr, Fortran2003.Part_Ref):
            if unique:
                if name not in names:
                    names.append(name)
            else:
                names.append(name)
            # also collect index variable names
            for arg in getattr(expr.items[1], "items", []):
                if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                    subexpr = arg.items[1]
                else:
                    subexpr = arg
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


def _derivative(expr, var: str, index=None, warn_info=None, warnings=None) -> str:
    """Return derivative of ``expr`` with respect to ``var``.

    ``index`` can be an index expression string for array elements. When
    provided the derivative is taken with respect to the specific indexed
    element ``var(index)``.

    ``warn_info`` should contain context (file, line, stmt) for warning messages.
    ``warnings`` is a list that collects formatted warning strings.
    """
    if isinstance(expr, Fortran2003.Name):
        if index is not None:
            return "0.0"
        return "1.0" if str(expr) == var else "0.0"
    if isinstance(expr, (Fortran2003.Int_Literal_Constant, Fortran2003.Real_Literal_Constant)):
        return "0.0"
    if isinstance(expr, Fortran2003.Parenthesis):
        return _derivative(expr.items[1], var, index, warn_info, warnings)
    if isinstance(
        expr,
        (
            Fortran2003.Intrinsic_Function_Reference,
            Fortran2003.Function_Reference,
            Fortran2003.Part_Ref,
        ),
    ):
        name = expr.items[0].tofortran().lower()
        if isinstance(expr, Fortran2003.Part_Ref) and name == var.lower():
            if index is None:
                return "1.0"
            items = []
            for arg in getattr(expr.items[1], "items", []):
                if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                    arg = arg.items[1]
                items.append(arg.tofortran())
            idx = ", ".join(items)
            return "1.0" if idx == index else "0.0"
        items = [a for a in getattr(expr.items[1], "items", []) if not isinstance(a, str)]
        if name in DERIVATIVE_TEMPLATES:
            templates = DERIVATIVE_TEMPLATES[name]
            args = []
            for item in items:
                if isinstance(item, Fortran2003.Actual_Arg_Spec):
                    item = item.items[1]
                args.append(item)
            arg_strs = [a.tofortran() for a in args]
            if isinstance(templates, str):
                if len(args) != 1:
                    reason = f"unsupported intrinsic '{name}'"
                    _warn(warnings, warn_info, expr.tofortran(), reason)
                    return "0.0"
                d_arg = _derivative(args[0], var, index, warn_info, warnings)
                deriv = templates.format(arg=arg_strs[0])
                if d_arg == "0.0":
                    return "0.0"
                if d_arg == "1.0":
                    return deriv
                return f"{deriv} * {d_arg}"
            else:
                if len(args) != len(templates):
                    reason = f"unsupported intrinsic '{name}'"
                    _warn(warnings, warn_info, expr.tofortran(), reason)
                    return "0.0"
                placeholder = {f"arg{i+1}": s for i, s in enumerate(arg_strs)}
                terms = []
                for arg, tmpl in zip(args, templates):
                    d_arg = _derivative(arg, var, index, warn_info, warnings)
                    if d_arg == "0.0":
                        continue
                    deriv = tmpl.format(**placeholder)
                    if d_arg != "1.0":
                        deriv = f"{deriv} * {d_arg}"
                    terms.append(deriv)
                if not terms:
                    return "0.0"
                return " + ".join(terms)
        if name in NONDIFF_INTRINSICS:
            return "0.0"
        if isinstance(expr, Fortran2003.Intrinsic_Function_Reference):
            reason = f"unsupported intrinsic '{name}'"
            _warn(warnings, warn_info, expr.tofortran(), reason)
            return "0.0"
    if isinstance(expr, Fortran2003.Level_2_Unary_Expr):
        sign = expr.items[0]
        d = _derivative(expr.items[1], var, index, warn_info, warnings)
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
            d1 = _derivative(left, var, index, warn_info, warnings)
            d2 = _derivative(right, var, index, warn_info, warnings)
            if d1 == "0.0":
                return d2
            if d2 == "0.0":
                return d1
            return f"{d1} + {d2}"
        if op == "-":
            d1 = _derivative(left, var, index, warn_info, warnings)
            d2 = _derivative(right, var, index, warn_info, warnings)
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
            d1 = _derivative(left, var, index, warn_info, warnings)
            d2 = _derivative(right, var, index, warn_info, warnings)
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
            d1 = _derivative(left, var, index, warn_info, warnings)
            d2 = _derivative(right, var, index, warn_info, warnings)
            if d1 == "0.0" and d2 == "0.0":
                return "0.0"
            if d2 == "0.0":
                num = d1 if d1 != "1.0" else "1.0"
                num = _parenthesize_if_needed(num)
                return f"{num} / {v}"
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
        d_base = _derivative(base, var, index, warn_info, warnings)
        d_exp = _derivative(exponent, var, index, warn_info, warnings)
        terms = []
        if d_base != "0.0":
            minus = _parenthesize_if_needed(_minus_one(exponent))
            base_term = base_s if minus in ("1", "1.0", "(1)", "(1.0)") else f"{base_s}**{minus}"
            term = f"{exp_s} * {base_term}"
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


def _dims_spec(typ) -> str | None:
    """Return dimension specification string from ``typ`` if present."""
    text = str(typ).strip()
    low = text.lower()
    idx = low.find("dimension")
    if idx == -1:
        return None
    start = low.find("(", idx)
    if start == -1:
        return None
    depth = 0
    end = None
    for i in range(start, len(text)):
        if text[i] == "(":
            depth += 1
        elif text[i] == ")":
            depth -= 1
            if depth == 0:
                end = i
                break
    if end is not None:
        return text[start:end + 1].strip()
    return None


def _split_type(typ):
    """Return base type and dimension spec from a declaration string."""
    text = str(typ).strip()
    dims = _dims_spec(text)
    if dims:
        idx = text.lower().find("dimension")
        base = text[:idx].rstrip().rstrip(',')
        remainder = text[idx + len("dimension") :]
        pos = remainder.lower().find(dims.lower())
        if pos != -1:
            remainder = remainder[pos + len(dims) :].strip()
            if remainder.startswith(','):
                remainder = remainder[1:].strip()
            if remainder:
                base = f"{base}, {remainder}" if base else remainder
        return base.strip(), dims
    return text, None




def _is_integer_type(typ) -> bool:
    """Return ``True`` if ``typ`` represents an integer type."""
    if typ is None:
        return False
    return str(typ).strip().lower().startswith("integer")


def _sanitize_var(name: str) -> str:
    """Return a string safe for use in generated variable names."""
    return re.sub(r"[^0-9A-Za-z_]", "_", name)


def _find_index_expr(expr, var, all_indices=False):
    """Return index expressions for ``var`` in ``expr``.

    If ``all_indices`` is ``True`` a list of unique index expressions is
    returned.  Otherwise the function returns the single unique expression if
    one is found, or ``None`` when there are multiple different expressions.
    """

    found = []

    def _walk(node):
        if isinstance(node, Fortran2003.Part_Ref):
            name = node.items[0].tofortran().lower()
            if name == var.lower():
                items = []
                for arg in getattr(node.items[1], "items", []):
                    if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                        arg = arg.items[1]
                    items.append(arg.tofortran())
                found.append(", ".join(items))
                return
        for itm in getattr(node, "items", []):
            if not isinstance(itm, str):
                _walk(itm)

    _walk(expr)

    if all_indices:
        result = []
        for idx in found:
            if idx not in result:
                result.append(idx)
        return result

    uniq = set(found)
    if len(uniq) == 1:
        return uniq.pop()
    return None


def _assignment_parts(stmt, warn_info=None, warnings=None):
    """Return mapping of variables to partial derivatives and index info."""
    rhs = stmt.items[2]
    all_names = []
    _collect_names(rhs, all_names, unique=False)
    names = []
    _collect_names(rhs, names)
    parts = {}
    index_map = {}
    index_vars = set()
    for name in names:
        idx_list = _find_index_expr(rhs, name, all_indices=True)
        if idx_list:
            for idx in idx_list:
                deriv = _derivative(rhs, name, idx, warn_info, warnings)
                if deriv != "0.0":
                    key = f"{name}@{idx}"
                    parts[key] = deriv
                    index_map[key] = (name, idx)
                for var in re.findall(r"[A-Za-z_][A-Za-z0-9_]*", idx):
                    index_vars.add(var)
        else:
            deriv = _derivative(rhs, name, None, warn_info, warnings)
            if deriv != "0.0":
                parts[name] = deriv
                index_map[name] = (name, None)
    has_repeat = len(all_names) != len(set(all_names))
    return parts, has_repeat, index_map, index_vars




def _generate_ad_subroutine(routine, filename, warnings):
    # blocks representing declarations, initialization and AD statements
    decl_block = DeclBlock([])
    init_block = InitBlock([])
    ad_block = AdBlock([])
    body = Block([])

    def _optimize_lines(raw_lines, keep=None):
        """Remove unused integer assignments."""
        if keep is None:
            keep = set()
        result = list(raw_lines)

        # remove unused integer assignments
        int_vars = {n for n, (t, _) in decl_map.items() if _is_integer_type(t)}
        assign_pat = re.compile(r"^\s*(\w+)\s*=.*$")
        i = 0
        while i < len(result):
            m = assign_pat.match(result[i].strip())
            if not m:
                i += 1
                continue
            var = m.group(1)
            if var in int_vars and var not in index_vars:
                later_use = any(re.search(rf"\b{re.escape(var)}\b", l) for l in result[i+1:])
                if not later_use:
                    del result[i]
                    continue
            i += 1

        return result


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

    spec, exec_part_node = parser._routine_parts(routine)
    # Convert execution part to a forward node block for later use
    fwd_block = parser.exec_part_to_block(exec_part_node)
    # Use the forward block to obtain a fresh execution part for AD generation
    exec_part = parser.block_to_exec_part(fwd_block)
    decl_map = parser._parse_decls(spec)
    used_vars = set()
    pre_lines = Block([])  # nodes inserted before the main reversed body
    const_vars = set()
    const_decl = Block([])  # constant declarations as nodes
    const_decl_names = set()
    index_vars = set()
    ad_args = []
    outputs = []
    if result is not None:
        r_typ = decl_map.get(result, ("real", None))[0]
        if not r_typ.strip().lower().startswith("character") and not _is_integer_type(r_typ):
            outputs.append(result)
    for arg in args:
        typ, intent = decl_map.get(arg, (None, None))
        is_char = str(typ).strip().lower().startswith("character")
        is_int = _is_integer_type(typ)
        if intent == "out":
            if not is_char and not is_int:
                outputs.append(arg)
                ad_args.append(f"{arg}_ad")
        else:
            ad_args.append(arg)
            if not is_char and not is_int:
                ad_args.append(f"{arg}_ad")
    for outv in outputs:
        if outv not in args:
            ad_args.append(f"{outv}_ad")

    sub = Subroutine(f"{name}_ad", ", ".join(ad_args), decl_block, body)

    def _space(intent):
        return "  " if intent == "in" else " "

    def _grad_type(typ):
        dims = _dims_spec(typ)
        return f"real, dimension{dims}" if dims else "real"

    def _sized_dims(typ, name):
        dims = _dims_spec(typ)
        if not dims:
            return None
        parts = [p.strip() for p in dims[1:-1].split(",")]
        new = []
        for i, p in enumerate(parts, 1):
            if ":" in p or p == "":
                new.append(f"size({name}, {i})")
            else:
                new.append(p)
        return "(" + ", ".join(new) + ")"

    out_grad_args = []
    has_grad_input = False
    for arg in args:
        typ, intent = decl_map.get(arg, ("real", "in"))
        arg_int = intent or "in"
        is_char = str(typ).strip().lower().startswith("character")
        is_int = _is_integer_type(typ)
        gtyp = _grad_type(typ)
        if arg_int == "out":
            if not is_char and not is_int:
                base, dims = _split_type(gtyp)
                decl_block.append(Declaration(base, f"{arg}_ad", "in", dims))
                has_grad_input = True
        else:
            base, dims = _split_type(typ)
            decl_block.append(Declaration(base, arg, arg_int, dims))
            if not is_char and not is_int:
                grad_int = {
                    "in": "out",
                    "inout": "inout",
                }.get(arg_int, "out")
                gbase, gdims = _split_type(gtyp)
                decl_block.append(Declaration(gbase, f"{arg}_ad", grad_int, gdims))
                if grad_int == "out":
                    out_grad_args.append(arg)
                else:
                    has_grad_input = True

    for outv in outputs:
        if outv not in args:
            out_typ = _grad_type(decl_map.get(outv, ("real",))[0])
            base, dims = _split_type(out_typ)
            decl_block.append(Declaration(base, f"{outv}_ad", "in", dims))
            has_grad_input = True

    # If no derivative inputs exist, all output gradients remain zero
    if not has_grad_input:
        for arg in out_grad_args:
            t, _ = decl_map.get(arg, ("real", None))
            _, dims = _split_type(t)
            suffix = "(:)" if dims else ""
            body.append(Assignment(f"{arg}_ad{suffix}", "0.0"))
        if out_grad_args:
            body.append(EmptyLine())
        body.append(Return())
        return render_program(sub, 1)

    # If there are no input gradients to propagate we can exit early
    if not out_grad_args:
        body.append(Return())
        return render_program(sub, 1)

    def _find_assignments(node, out_list, top=True, in_do=False):
        if isinstance(node, Fortran2003.Assignment_Stmt):
            out_list.append((node, top, in_do))
        for item in getattr(node, "content", []):
            if not isinstance(item, str):
                _find_assignments(
                    item,
                    out_list,
                    top=False,
                    in_do=in_do or isinstance(node, Block_Nonlabel_Do_Construct),
                )

    def _collect_do_indices(node, out_set):
        if isinstance(node, Block_Nonlabel_Do_Construct):
            stmt = node.content[0]
            ctrl = stmt.items[1]
            if ctrl is not None:
                lc = ctrl.items[1]
                if isinstance(lc, tuple) and lc and isinstance(lc[0], Fortran2003.Name):
                    out_set.add(str(lc[0]))
        for item in getattr(node, "content", []):
            if not isinstance(item, str):
                _collect_do_indices(item, out_set)

    statements = []
    do_indices = set()
    for stmt in exec_part.content:
        _find_assignments(stmt, statements)
        _collect_do_indices(stmt, do_indices)

    var_use_count = {}
    for st, _, _ in statements:
        names = []
        _collect_names(st.items[2], names, unique=False)
        for n in names:
            var_use_count[n] = var_use_count.get(n, 0) + 1
    const_vars.update(do_indices)
    for idx in sorted(do_indices):
        if idx not in const_decl_names:
            const_decl.append(Declaration("integer", idx))
            const_decl_names.add(idx)
    defined = set(out_grad_args)
    grad_var = {v: f"{v}_ad" for v in outputs}
    decl_names = []
    decl_set = set()
    stmt_blocks = {}

    scalar_derivs = set()

    loop_grad_vars = set()

    loop_lhs = {str(s.items[0]).split('(')[0] for s, _, in_do in statements if in_do}


    const_map = {}
    lhs_counts = {}
    self_use = set()
    for stmt, _, _ in statements:
        lhs = str(stmt.items[0]).split('(')[0]
        rhs_names = []
        _collect_names(stmt.items[2], rhs_names)
        if lhs not in const_map:
            const_map[lhs] = True
        if rhs_names:
            const_map[lhs] = False
        lhs_counts[lhs] = lhs_counts.get(lhs, 0) + 1
        if lhs in rhs_names:
            self_use.add(lhs)
    for var, is_const in const_map.items():
        if is_const:
            const_vars.add(var)

    for var in sorted(loop_lhs):
        if var in outputs and (lhs_counts.get(var, 0) > 1 or var in self_use):
            vtyp = decl_map.get(var, ("real",))[0]
            _, dims = _split_type(vtyp)
            suf = "(:)" if dims else ""
            pre_lines.append(Assignment(f"{var}_ad_{suf}", f"{var}_ad{suf}"))
            grad_var[var] = f"{var}_ad_"
            if f"{var}_ad_" not in decl_set:
                decl_names.append(f"{var}_ad_")
                decl_set.add(f"{var}_ad_")

    last_block = None
    for stmt, top, in_do in reversed(statements):
        lhs = str(stmt.items[0])
        lhs_base = lhs.split('(')[0]
        line_no = None
        if getattr(stmt, "item", None) is not None and getattr(stmt.item, "span", None):
            line_no = stmt.item.span[0]
        info = {
            "file": filename,
            "line": line_no,
            "code": stmt.tofortran().strip(),
        }

        rhs = stmt.items[2]
        if isinstance(rhs, Fortran2003.Intrinsic_Function_Reference):
            intr_name = rhs.items[0].tofortran().lower()
            items = [a for a in getattr(rhs.items[1], "items", []) if not isinstance(a, str)]
        else:
            intr_name = None
            items = []

        handler = SPECIAL_HANDLERS.get(intr_name)
        if handler:
            block, names = handler(lhs, items, grad_var, defined, decl_names, decl_set)
            stmt_blocks[id(stmt)] = block
            last_block = block
            used_vars.update(names)
            continue

        parts, has_repeat, idx_map, idx_vars = _assignment_parts(stmt, info, warnings)
        index_vars.update(idx_vars)

        rhs_names = []
        _collect_names(stmt.items[2], rhs_names)

        if lhs_base not in used_vars and lhs_base not in outputs:
            # The value of ``lhs`` does not contribute to any output so we do
            # not need to propagate a gradient through this assignment.
            continue

        lhs_typ = decl_map.get(lhs_base, ("",))[0]
        if str(lhs_typ).strip().lower().startswith("character") or _is_integer_type(lhs_typ):
            used_vars.update(rhs_names)
            if lhs_base in used_vars:
                stmt_blocks[id(stmt)] = [f"{stmt.tofortran().strip()}\n"]
                last_block = stmt_blocks[id(stmt)]
                used_vars.add(lhs_base)
            continue
        parts = {
            v: e
            for v, e in parts.items()
            if not str(decl_map.get(idx_map.get(v, (v, None))[0], ("",))[0])
            .strip()
            .lower()
            .startswith("character")
            and not _is_integer_type(decl_map.get(idx_map.get(v, (v, None))[0], ("",))[0])
            and idx_map.get(v, (v, None))[0] not in const_vars
        }
        if not parts and lhs_base in used_vars and not rhs_names and lhs_base not in outputs:
            if top:
                pre_lines.insert(0, Assignment(str(stmt.items[0]), stmt.items[2].tofortran().strip()))
                last_block = None
            else:
                stmt_blocks[id(stmt)] = [f"{stmt.tofortran().strip()}\n"]
                last_block = stmt_blocks[id(stmt)]
            used_vars.update(rhs_names)
            used_vars.add(lhs_base)
            if lhs_base not in args and lhs_base not in outputs and lhs_base not in const_decl_names:
                typ = decl_map.get(lhs_base, ("real", None))[0]
                if not str(typ).strip().lower().startswith("character") and not _is_integer_type(typ):
                    base, dims = _split_type(typ)
                    const_decl.append(Declaration(base, lhs_base, None, dims))
                    const_decl_names.add(lhs_base)
            continue
        for var in parts:
            base, idx = idx_map.get(var, (var, None))
            safe = _sanitize_var(var)
            name_d = f"d{lhs_base}_d{safe}"
            if name_d not in decl_set:
                decl_names.append(name_d)
                decl_set.add(name_d)
            if in_do and "dimension" in str(decl_map.get(base, ("",))[0]).lower():
                scalar_derivs.add(name_d)
            if base not in args and base not in outputs and not _is_integer_type(decl_map.get(base, ("",))[0]):
                name_ad = f"{base}_ad"
                if name_ad not in decl_set:
                    decl_names.append(name_ad)
                    decl_set.add(name_ad)
        if lhs_base in parts:
            if not _is_integer_type(lhs_typ):
                name_ad = f"{lhs_base}_ad_"
                if name_ad not in decl_set:
                    decl_names.append(name_ad)
                    decl_set.add(name_ad)

        block = []
        for var, expr in parts.items():
            base, idx = idx_map.get(var, (var, None))
            safe = _sanitize_var(var)
            name_d = f"d{lhs_base}_d{safe}"
            var_typ = decl_map.get(base, ("", None))[0]
            _, dims = _split_type(var_typ)
            suffix = "(:)" if dims and name_d not in scalar_derivs else ""
            block.append(f"{name_d}{suffix} = {expr}\n")
        lhs_grad = grad_var.get(lhs_base, f"{lhs_base}_ad")
        lhs_typ = decl_map.get(lhs_base, ("",))[0]
        lhs_is_array = in_do and "dimension" in str(lhs_typ).lower()
        lhs_indices = None
        if isinstance(stmt.items[0], Fortran2003.Part_Ref):
            lhs_indices = []
            for arg in getattr(stmt.items[0].items[1], "items", []):
                if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                    arg = arg.items[1]
                lhs_indices.append(arg.tofortran())
        if lhs_is_array:
            if lhs_indices:
                lhs_grad = f"{lhs_grad}({', '.join(lhs_indices)})"
            else:
                idx_list = [n for n in rhs_names if n in do_indices]
                if idx_list:
                    lhs_grad = f"{lhs_grad}({', '.join(idx_list)})"
        order = list(parts.keys()) if has_repeat else list(reversed(list(parts.keys())))
        for var in order:
            base, idx = idx_map.get(var, (var, None))
            if base == lhs_base and idx is None:
                continue
            safe = _sanitize_var(var)
            update = f"{lhs_grad} * d{lhs_base}_d{safe}"
            var_typ, var_int = decl_map.get(base, ("", None))
            is_array = "dimension" in str(var_typ).lower()
            unique_loop = (
                in_do
                and is_array
                and var_use_count.get(base, 0) == 1
                and any(n in rhs_names for n in do_indices)
            )
            if in_do and is_array:
                var_idx = idx if idx is not None else _find_index_expr(rhs, base)
                if var_idx is None:
                    idx_list = [n for n in rhs_names if n in do_indices]
                    var_idx = ", ".join(idx_list) if idx_list else "i"
                line = f"{base}_ad({var_idx}) = {update}"
                if var_int == "inout" or ((base in defined or in_do) and not unique_loop):
                    line += f" + {base}_ad({var_idx})"
            else:
                suffix = "(:)" if is_array else ""
                line = f"{base}_ad{suffix} = {update}"
                if var_int == "inout" or ((base in defined or in_do) and not unique_loop):
                    line += f" + {base}_ad{suffix}"
            block.append(line + "\n")
            if not in_do and base not in defined:
                defined.add(base)
            if in_do and not unique_loop:
                loop_grad_vars.add(base)
        if any(idx_map.get(v, (v, None))[0] == lhs_base and idx_map.get(v, (v, None))[1] is None for v in parts):
            new_grad = f"{lhs_base}_ad_"
            ltyp = decl_map.get(lhs_base, ("real",))[0]
            _, ldims = _split_type(ltyp)
            suf = "(:)" if ldims else ""
            expr = f"{lhs_grad} * d{lhs_base}_d{lhs_base}{suf}"
            block.append(f"{new_grad}{suf} = {expr}\n")
            grad_var[lhs_base] = new_grad
            if in_do:
                loop_grad_vars.add(lhs_base)
        stmt_blocks[id(stmt)] = block
        last_block = block
        used_vars.update(rhs_names)
        used_vars.add(lhs_base)

    for var in sorted(index_vars):
        typ, _ = decl_map.get(var, (None, None))
        if typ is not None and _is_integer_type(typ):
            if var not in args and var not in outputs and var not in const_decl_names:
                const_decl.append(Declaration("integer", var))
                const_decl_names.add(var)

    for cl in const_decl:
        decl_block.append(cl)
    for dname in decl_names:
        typ = "real"
        dims = None
        if dname.endswith("_ad"):
            base = dname[:-3]
            typ = _grad_type(decl_map.get(base, ("real",))[0])
            dims = _sized_dims(decl_map.get(base, ("real",))[0], base)
        elif dname.endswith("_ad_"):
            base = dname[:-4]
            if base.endswith("_ad"):
                base0 = base[:-3]
            else:
                base0 = base
            typ = _grad_type(decl_map.get(base0, ("real",))[0])
            dims = _sized_dims(decl_map.get(base0, ("real",))[0], f"{base0}_ad")
        elif dname.startswith("d") and "_d" in dname:
            base = dname.split("_d", 1)[1]
            typ = _grad_type(decl_map.get(base, ("real",))[0])
            dims = _sized_dims(decl_map.get(base, ("real",))[0], base)
        if dname in scalar_derivs:
            dims = None
            typ = "real"
        if dims is not None:
            typ = f"real, dimension{dims}"
        base, ldims = _split_type(typ)
        decl_block.append(Declaration(base, dname, None, ldims))

    # initialization assignments will be decided after AD code generation
    for pl in pre_lines:
        ad_block.append(pl)
    if len(pre_lines):
        ad_block.append(EmptyLine())
    def _reverse_block(body):
        block = Block([])
        for st in reversed(body):
            block.extend(_reverse_stmt(st))
        return block

    def _reverse_do_line(stmt):
        s = stmt.tofortran().strip()
        m = re.match(r"do\s+(\w+)\s*=\s*(.*?),\s*(.*?)(?:,\s*(.*?))?$", s, re.I)
        if not m:
            return s
        var, start, end, step = m.groups()
        start = start.strip()
        end = end.strip()
        if step is None:
            step = "1"
        step = step.strip()
        if step.startswith("-"):
            rev_step = step[1:].strip()
        else:
            rev_step = f"-{step}" if re.fullmatch(r"[\w.]+", step) else f"-({step})"
        return f"DO {var} = {end}, {start}, {rev_step}"

    def _only_int_assignments(node):
        if isinstance(node, Fortran2003.Assignment_Stmt):
            lhs = str(node.items[0]).split("(")[0]
            typ, _ = decl_map.get(lhs, (None, None))
            return _is_integer_type(typ)
        if isinstance(node, (Fortran2003.If_Then_Stmt, Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
            return True
        has_child = False
        for item in getattr(node, "content", []):
            if isinstance(item, (Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
                continue
            if isinstance(item, str):
                continue
            if not _only_int_assignments(item):
                return False
            has_child = True
        return has_child

    def _reverse_loop_body(body):
        pre = []
        main = []
        for st in body:
            if _only_int_assignments(st):
                pre.append(st)
            else:
                main.append(st)
        block = Block([])
        for st in reversed(main):
            block.extend(_reverse_stmt(st))
        pre_block = Block([])
        for st in pre:
            pre_block.extend(_reverse_stmt(st))
        block.children = pre_block.children + block.children
        return block

    def _reverse_stmt(st):
        if isinstance(st, Fortran2003.Assignment_Stmt):
            block = stmt_blocks.get(id(st), [])
            nodes = []
            for line in block:
                text = line.strip()
                if not text:
                    nodes.append(EmptyLine())
                    continue
                if "=" in text:
                    lhs, rhs = text.split("=", 1)
                    lhs = lhs.strip()
                    rhs = rhs.strip()
                    accumulate = False
                    parts = [p.strip() for p in rhs.split("+")]
                    if len(parts) == 2:
                        lhs_base = lhs.split("(")[0].strip()
                        left_base = parts[0].split("(")[0].strip()
                        right_base = parts[1].split("(")[0].strip()
                        if lhs_base in (left_base, right_base):
                            accumulate = True
                            rhs = parts[0] if right_base == lhs_base else parts[1]
                    nodes.append(Assignment(lhs, rhs, accumulate=accumulate))
            return Block(nodes)
        if isinstance(st, Fortran2003.If_Construct):
            cond = st.content[0].items[0].tofortran()
            i = 1
            seg = []
            while i < len(st.content):
                itm = st.content[i]
                if isinstance(itm, (Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
                    break
                seg.append(itm)
                i += 1
            body = _reverse_block(seg)
            elif_blocks = []
            else_block = None
            while i < len(st.content):
                itm = st.content[i]
                if isinstance(itm, Fortran2003.Else_If_Stmt):
                    cond2 = itm.items[0].tofortran()
                    i += 1
                    seg = []
                    while i < len(st.content):
                        j = st.content[i]
                        if isinstance(j, (Fortran2003.Else_If_Stmt, Fortran2003.Else_Stmt, Fortran2003.End_If_Stmt)):
                            break
                        seg.append(j)
                        i += 1
                    blk = _reverse_block(seg)
                    elif_blocks.append((cond2, blk))
                elif isinstance(itm, Fortran2003.Else_Stmt):
                    i += 1
                    seg = []
                    while i < len(st.content):
                        j = st.content[i]
                        if isinstance(j, Fortran2003.End_If_Stmt):
                            break
                        seg.append(j)
                        i += 1
                    else_block = _reverse_block(seg)
                elif isinstance(itm, Fortran2003.End_If_Stmt):
                    i += 1
                else:
                    i += 1
            if len(body) or elif_blocks or else_block is not None:
                node = IfBlock(cond, body, elif_blocks=elif_blocks, else_body=else_block)
                return Block([node])
            return Block([])
        if isinstance(st, Fortran2003.Case_Construct):
            expr = st.content[0].items[0].tofortran()
            cases = []
            default = None
            i = 1
            while i < len(st.content) - 1:
                cs = st.content[i]
                cond = cs.tofortran().split(None, 1)[1]
                i += 1
                seg = []
                while i < len(st.content) - 1 and not isinstance(st.content[i], Fortran2003.Case_Stmt):
                    seg.append(st.content[i])
                    i += 1
                blk = _reverse_block(seg)
                if cond.lower().startswith("default"):
                    default = blk
                else:
                    cond = cond.strip()
                    if cond.startswith("(") and cond.endswith(")"):
                        cond = cond[1:-1]
                    cases.append((cond, blk))
            node = SelectBlock(expr, cases, default=default)
            return Block([node])
        if isinstance(st, Block_Nonlabel_Do_Construct):
            do_line = _reverse_do_line(st.content[0])
            body = _reverse_loop_body(st.content[1:-1])
            res = DoLoop(do_line, body)
            return Block([res])
        return Block([])

    for l in _reverse_block(exec_part.content):
        ad_block.append(l)
    ad_block.append(EmptyLine())
    ad_block.append(Return())

    keep = {f"{v}_ad" for v in loop_grad_vars}
    for arg in out_grad_args:
        var_name = f"{arg}_ad"
        t, _ = decl_map.get(arg, ("real", None))
        _, dims = _split_type(t)
        suf = "(:)" if dims else ""
        if var_name in keep:
            init_block.append(Assignment(f"{var_name}{suf}", "0.0"))
            continue
        res = ad_block.remove_initial_self_add(var_name)
        if res == 1:
            init_block.append(Assignment(f"{var_name}{suf}", "0.0"))

    body.append(init_block)
    body.append(ad_block)

    raw_lines = render_program(sub, 1).splitlines(keepends=True)
    optimized = _optimize_lines(raw_lines, keep)

    return "".join(optimized)


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
            sub_code = _generate_ad_subroutine(child, in_file, warnings)
            output.append(sub_code)
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
