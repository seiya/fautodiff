from __future__ import annotations

import re
from collections import OrderedDict
from typing import Any, Dict, List, Optional, Set, Tuple

from .code_tree import Declaration, Module, Routine, Use
from .operators import OpVar, VarType


def _var_type_priority(var_type: VarType) -> tuple[int, float]:
    typename = var_type.typename.lower()
    if typename.startswith("complex"):
        category = 3
    elif typename.startswith("real") or typename == "double precision":
        category = 2
    elif typename.startswith("integer"):
        category = 1
    else:
        category = 0

    kind_val = None
    if var_type.kind is not None and getattr(var_type.kind, "val", None) is not None:
        kind_val = var_type.kind.val
    else:
        kind_defaults = {
            "double precision": 8,
            "real": 4,
            "complex": 4,
        }
        kind_val = kind_defaults.get(typename, 0)
    return (category, kind_val if kind_val is not None else 0.0)


def _select_var_type(
    infos: List[Dict[str, Any]],
    *,
    default: Optional[VarType] = None,
) -> VarType:
    best: Optional[VarType] = None
    best_pri: Optional[tuple[int, float]] = None
    for info in infos:
        decl = info.get("decl")
        if decl is None or decl.var_type is None:
            continue
        vt = decl.var_type
        pri = _var_type_priority(vt)
        if best_pri is None or pri > best_pri:
            best_pri = pri
            best = vt
    if best is not None:
        return best.copy()
    if default is not None:
        return default.copy()
    return VarType("real")


def _render_scalar_decl(name: str, var_type: VarType) -> str:
    decl = Declaration(
        name=name,
        var_type=var_type.copy(),
        dims=None,
        dims_raw=None,
        declared_in="routine",
    )
    return decl.render(indent=2)[0].rstrip()


def _render_print(indent: int, fragments: List[str]) -> List[str]:
    if not fragments:
        return []
    lines: List[str] = []
    if len(fragments) == 1:
        lines.append(_indent(f"print *, {fragments[0]}", indent))
        return lines
    lines.append(_indent(f"print *, {fragments[0]}, &", indent))
    for frag in fragments[1:-1]:
        lines.append(_indent(f"& {frag}, &", indent + 1))
    lines.append(_indent(f"& {fragments[-1]}", indent + 1))
    return lines


def _indent(line: str, level: int) -> str:
    return "  " * level + line


def _build_decl_lookup(routine: Optional[Routine]) -> Dict[str, List[Declaration]]:
    lookup: Dict[str, List[Declaration]] = {}
    if routine is None or routine.decls is None:
        return lookup
    for node in routine.decls.iter_children():
        if isinstance(node, Declaration):
            lookup.setdefault(node.name, []).append(node)
    return lookup


def _collect_extent_symbols(extent: str) -> Set[str]:
    symbols: Set[str] = set()
    for match in re.finditer(r"[A-Za-z_][A-Za-z0-9_]*", extent):
        symbols.add(match.group(0))
    return symbols


def _build_use_lookup(
    mod_org: Module, routine: Routine
) -> Tuple[Dict[str, List[Tuple[Use, Optional[str]]]], List[Use]]:
    symbol_lookup: Dict[str, List[Tuple[Use, Optional[str]]]] = {}
    fallback: List[Use] = []

    use_nodes: List[Use] = []
    if mod_org.uses is not None:
        for child in mod_org.uses.iter_children():
            if isinstance(child, Use):
                use_nodes.append(child)
    if routine.decls is not None:
        for child in routine.decls.iter_children():
            if isinstance(child, Use):
                use_nodes.append(child)

    for use in use_nodes:
        if use.only:
            for entry in use.only:
                entry_norm = entry.strip()
                local = entry_norm.split("=>")[0].strip()
                if not local:
                    continue
                symbol_lookup.setdefault(local.lower(), []).append((use, entry_norm))
        else:
            fallback.append(use)

    return symbol_lookup, fallback


def _extent_from_dim(dim: str) -> Optional[str]:
    dim = dim.strip()
    if not dim or dim in (":", "*"):
        return None
    parts = [part.strip() for part in dim.split(":")]
    if len(parts) == 1:
        return parts[0]
    if len(parts) == 2:
        lower, upper = parts
        if not upper:
            return None
        if not lower:
            lower = "1"
        return f"({upper}) - ({lower}) + 1"
    return None


def _infer_extents_from_decl(decl: Declaration) -> Optional[List[str]]:
    if not decl.dims_raw:
        return None
    extents: List[str] = []
    for dim in decl.dims_raw:
        extent = _extent_from_dim(dim)
        if extent is None:
            return None
        extents.append(extent)
    return extents


def _clone_decl(
    decl: Declaration,
    name: str,
    *,
    force_allocatable: bool = False,
    force_optional: Optional[bool] = None,
) -> Declaration:
    clone = decl.copy()
    clone.name = name
    clone.intent = None
    clone.parameter = False
    clone.access = None
    clone.save = False
    if force_allocatable:
        if clone.dims_raw is None and clone.dims is not None:
            clone.dims_raw = tuple(str(d) if d is not None else ":" for d in clone.dims)
        if clone.dims_raw is not None:
            clone.allocatable = True
            clone.pointer = False
            clone.dims = tuple(None for _ in clone.dims_raw)
            clone.dims_raw = tuple(":" for _ in clone.dims_raw)
    if force_optional is not None:
        clone.optional = force_optional
    return clone


def render_validation_driver(
    mod_org: Module,
    mod_ad: Module,
    mode: str,
    *,
    ad_suffix: str,
    fwd_suffix: str,
    rev_suffix: str,
) -> str:
    """Return the validation driver template for ``mod_org`` and ``mod_ad``."""

    forward_enabled = mode in ("forward", "both")
    reverse_enabled = mode in ("reverse", "both")

    ad_routine_lookup: Dict[str, Routine] = {r.name: r for r in mod_ad.routines}

    program_name = f"run_{mod_org.name}_validation"
    lines: List[str] = [
        f"program {program_name}",
        f"  use {mod_org.name}",
        f"  use {mod_org.name}{ad_suffix}",
        "  implicit none",
        "",
        f"  print *, 'Running validation stubs for {mod_org.name}'",
    ]

    module_declared_names: Set[str] = set()
    if mod_org.decls is not None:
        for node in mod_org.decls.iter_children():
            if isinstance(node, Declaration):
                module_declared_names.add(node.name.lower())

    additional_uses: "OrderedDict[Tuple[str, bool], Dict[str, Any]]" = OrderedDict()
    module_use_names = {
        mod_org.name.lower(),
        f"{mod_org.name}{ad_suffix}".lower(),
    }

    intrinsic_module_names = {
        "iso_fortran_env",
        "iso_c_binding",
        "ieee_arithmetic",
        "ieee_exceptions",
        "ieee_features",
    }
    ad_suffix_lower = ad_suffix.lower()

    def _to_ad_module_name(name: str) -> str:
        name_lower = name.lower()
        if name_lower in intrinsic_module_names:
            return name
        if name_lower.endswith(ad_suffix_lower):
            return name
        return f"{name}{ad_suffix}"

    def _record_additional_use(use: Use, entry: Optional[str]) -> None:
        target_name = _to_ad_module_name(use.name)
        name_lower = target_name.lower()
        key = (name_lower, use.only is not None)
        info = additional_uses.get(key)
        if info is None:
            if name_lower in module_use_names and use.only is None:
                return
            copy = use.copy()
            copy.name = target_name
            entries: Optional["OrderedDict[str, str]"] = None
            if use.only is not None:
                copy.only = []
                entries = OrderedDict()
            info = {"use": copy, "entries": entries}
            additional_uses[key] = info
            module_use_names.add(name_lower)
        entries = info.get("entries")
        if entry is not None and entries is not None:
            entry_lower = entry.lower()
            if entry_lower not in entries:
                entries[entry_lower] = entry

    routines = list(mod_org.routines)
    if not routines:
        lines.append("  ! No routines found in this module to validate.")
        lines.append("")
        lines.append(f"end program {program_name}")
        return "\n".join(lines) + "\n"

    for routine in routines:
        lines.append(f"  call validate_{routine.name}()")
    lines.extend(["", "contains", ""])

    for idx, routine in enumerate(routines):
        decls_map: Dict[str, Declaration] = {}
        if routine.decls is not None:
            for node in routine.decls.iter_children():
                if isinstance(node, Declaration):
                    decls_map[node.name] = node

        routine_fwd = (
            ad_routine_lookup.get(f"{routine.name}{fwd_suffix}")
            if forward_enabled
            else None
        )
        routine_rev = (
            ad_routine_lookup.get(f"{routine.name}{rev_suffix}")
            if reverse_enabled
            else None
        )

        decl_lookup_candidates = [
            _build_decl_lookup(routine_fwd),
            _build_decl_lookup(routine_rev),
        ]

        forward_available = routine_fwd is not None
        reverse_available = routine_rev is not None

        use_lookup, fallback_uses = _build_use_lookup(mod_org, routine)

        sub_lines: List[str] = []
        sub_lines.append(_indent(f"subroutine validate_{routine.name}()", 1))
        sub_lines.append(_indent("implicit none", 2))

        scalar_real_decls: List[str] = []
        scalar_real_names: List[str] = []
        delta_type: Optional[VarType] = None
        fd_type: Optional[VarType] = None

        inputs: List[Dict[str, Any]] = []
        outputs: List[Dict[str, Any]] = []
        differentiable_inputs: List[Dict[str, Any]] = []
        differentiable_outputs: List[Dict[str, Any]] = []

        def _classify_arg(var: OpVar) -> None:
            decl = decls_map.get(var.name)
            if decl is None:
                return
            intent = var.intent or "inout"
            is_real = decl.var_type.is_real_type()
            is_diff = bool(var.ad_target and not var.is_constant and is_real)
            info = {
                "name": var.name,
                "decl": decl,
                "intent": intent,
                "is_real": is_real,
                "is_diff": is_diff,
                "rank": len(decl.dims_raw) if decl.dims_raw else 0,
            }
            if intent != "out":
                inputs.append(info)
                if is_diff:
                    differentiable_inputs.append(info)
            if intent != "in":
                outputs.append(info)
                if is_diff:
                    differentiable_outputs.append(info)

        for arg in routine.arg_vars():
            _classify_arg(arg)

        result_info: Optional[Dict[str, Any]] = None
        if routine.kind == "function" and routine.result:
            result_decl = decls_map.get(routine.result)
            if result_decl is not None:
                existing = next(
                    (info for info in outputs if info["name"] == routine.result),
                    None,
                )
                if existing is None:
                    is_real = result_decl.var_type.is_real_type()
                    info = {
                        "name": routine.result,
                        "decl": result_decl,
                        "intent": "out",
                        "is_real": is_real,
                        "is_diff": is_real,
                        "rank": (
                            len(result_decl.dims_raw) if result_decl.dims_raw else 0
                        ),
                        "is_function_result": True,
                    }
                    outputs.append(info)
                    if is_real and info not in differentiable_outputs:
                        differentiable_outputs.append(info)
                    result_info = info
                else:
                    result_info = existing
                    if (
                        existing["is_real"]
                        and existing["is_diff"]
                        and existing not in differentiable_outputs
                    ):
                        differentiable_outputs.append(existing)

        def _unique_by_name(seq: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
            seen = set()
            result: List[Dict[str, Any]] = []
            for item in seq:
                name = item["name"]
                if name in seen:
                    continue
                seen.add(name)
                result.append(item)
            return result

        inputs_unique = _unique_by_name(inputs)
        diff_inputs_names = {info["name"] for info in differentiable_inputs}
        diff_outputs_names = {info["name"] for info in differentiable_outputs}
        input_names = {info["name"] for info in inputs}
        output_names = {info["name"] for info in outputs}
        all_infos = _unique_by_name(inputs + outputs)
        info_by_name = {info["name"]: info for info in all_infos}

        local_declares: Dict[str, Declaration] = {}
        array_placeholders: Dict[str, List[str]] = {}
        placeholder_values: Dict[str, str] = {}
        unresolved_placeholders: List[str] = []
        extent_symbol_refs: Set[str] = set()

        def _ensure_decl(
            name: str, decl: Declaration, *, track_placeholders: bool = True
        ) -> None:
            if name not in local_declares:
                local_declares[name] = decl
                if track_placeholders:
                    rank = len(decl.dims_raw) if decl.dims_raw else 0
                    if rank > 0:
                        placeholders = [f"n_{name}_{i+1}" for i in range(rank)]
                        array_placeholders[name] = placeholders

        for info in all_infos:
            name = info["name"]
            base_decl = _clone_decl(
                info["decl"], name, force_allocatable=True, force_optional=False
            )
            _ensure_decl(name, base_decl, track_placeholders=True)

            if name in input_names:
                init_decl = _clone_decl(
                    info["decl"],
                    f"{name}_init",
                    force_allocatable=True,
                    force_optional=False,
                )
                _ensure_decl(
                    f"{name}_init",
                    init_decl,
                    track_placeholders=False,
                )

            if name in diff_inputs_names:
                ad_name = f"{name}{ad_suffix}"
                ad_decl = _clone_decl(
                    info["decl"],
                    ad_name,
                    force_allocatable=True,
                    force_optional=False,
                )
                _ensure_decl(ad_name, ad_decl, track_placeholders=False)
                u_decl = _clone_decl(
                    info["decl"],
                    f"{name}_u",
                    force_allocatable=True,
                    force_optional=False,
                )
                _ensure_decl(
                    f"{name}_u",
                    u_decl,
                    track_placeholders=False,
                )

            if name in output_names:
                base_val_decl = _clone_decl(
                    info["decl"],
                    f"{name}_base",
                    force_allocatable=True,
                    force_optional=False,
                )
                _ensure_decl(
                    f"{name}_base",
                    base_val_decl,
                    track_placeholders=False,
                )
                pert_decl = _clone_decl(
                    info["decl"],
                    f"{name}_pert",
                    force_allocatable=True,
                    force_optional=False,
                )
                _ensure_decl(
                    f"{name}_pert",
                    pert_decl,
                    track_placeholders=False,
                )

            if name in diff_outputs_names:
                deriv_decl = _clone_decl(
                    info["decl"],
                    f"{name}{ad_suffix}",
                    force_allocatable=info["rank"] > 0,
                    force_optional=False,
                )
                _ensure_decl(
                    f"{name}{ad_suffix}",
                    deriv_decl,
                    track_placeholders=False,
                )
                grad_decl = _clone_decl(
                    info["decl"],
                    f"{name}_grad",
                    force_allocatable=True,
                    force_optional=False,
                )
                _ensure_decl(
                    f"{name}_grad",
                    grad_decl,
                    track_placeholders=False,
                )
                v_decl = _clone_decl(
                    info["decl"],
                    f"{name}_v",
                    force_allocatable=True,
                    force_optional=False,
                )
                _ensure_decl(
                    f"{name}_v",
                    v_decl,
                    track_placeholders=False,
                )

        if array_placeholders:
            for base_name, placeholders in array_placeholders.items():
                extents: Optional[List[str]] = None
                for lookup in decl_lookup_candidates:
                    decls = lookup.get(base_name, [])
                    for decl in decls:
                        extents = _infer_extents_from_decl(decl)
                        if extents is not None:
                            break
                    if extents is not None:
                        break
                if extents is None:
                    base_decl = info_by_name.get(base_name, {}).get("decl")
                    if base_decl is not None:
                        extents = _infer_extents_from_decl(base_decl)
                if extents is None or len(extents) != len(placeholders):
                    unresolved_placeholders.extend(placeholders)
                    continue
                for placeholder, extent in zip(placeholders, extents):
                    placeholder_values[placeholder] = extent
                    extent_symbol_refs.update(_collect_extent_symbols(extent))

        if differentiable_inputs and differentiable_outputs and forward_available:
            delta_type = inputs_unique[0]["decl"].var_type.copy()
            scalar_real_decls.append(_render_scalar_decl("delta", delta_type))
            scalar_real_names.append("delta")
            fd_type = _select_var_type(
                [info_by_name[name] for name in diff_outputs_names],
                default=delta_type,
            )
            scalar_real_decls.append(_render_scalar_decl("fd_error", fd_type))
            scalar_real_names.append("fd_error")
            scalar_real_decls.append(_render_scalar_decl("fd_forward_val", fd_type))
            scalar_real_names.append("fd_forward_val")
            scalar_real_decls.append(_render_scalar_decl("ad_forward_val", fd_type))
            scalar_real_names.append("ad_forward_val")
        if forward_available and reverse_available:
            vtj_type = _select_var_type(
                [info_by_name[name] for name in diff_outputs_names],
                default=fd_type,
            )
            utj_default = delta_type if delta_type is not None else vtj_type
            utj_type = _select_var_type(
                [info_by_name[name] for name in diff_inputs_names],
                default=utj_default,
            )
            scalar_real_decls.extend(
                [
                    _render_scalar_decl("v_t_j_u", vtj_type),
                    _render_scalar_decl("u_t_j_t_v", utj_type),
                ]
            )
            scalar_real_names.extend(["v_t_j_u", "u_t_j_t_v"])

        if array_placeholders:
            if unresolved_placeholders:
                sub_lines.append("")
                sub_lines.append(
                    _indent(
                        "! TODO: provide array extents for validation work buffers.",
                        2,
                    )
                )
            for placeholders in array_placeholders.values():
                for placeholder in placeholders:
                    scalar_real_decls.append(
                        _render_scalar_decl(placeholder, VarType("integer"))
                    )
                    scalar_real_names.append(placeholder)

        local_symbols: Set[str] = set(local_declares.keys())
        local_symbols.update(placeholder_values.keys())
        for placeholders in array_placeholders.values():
            local_symbols.update(placeholders)
        local_symbols.update(scalar_real_names)
        local_symbols_lower = {name.lower() for name in local_symbols}
        local_symbols_lower.update(module_declared_names)

        if extent_symbol_refs:
            for symbol in extent_symbol_refs:
                symbol_lower = symbol.lower()
                if symbol_lower in local_symbols_lower:
                    continue
                uses_for_symbol = use_lookup.get(symbol_lower)
                if uses_for_symbol is None and len(fallback_uses) == 1:
                    uses_for_symbol = [(fallback_uses[0], None)]
                if uses_for_symbol:
                    for use, entry in uses_for_symbol:
                        _record_additional_use(use, entry)

        if scalar_real_decls:
            sub_lines.append("")
            sub_lines.extend(scalar_real_decls)

        if local_declares:
            sub_lines.append("")
            for decl in local_declares.values():
                rendered = decl.render(indent=2)[0].rstrip()
                sub_lines.append(rendered)

        if inputs_unique:
            sub_lines.append("")
            sub_lines.append(
                _indent("! TODO: assign initial values to input variables below.", 2)
            )

            def _default_assign_value(info: Dict[str, Any]) -> str:
                if info["decl"].var_type.is_real_type():
                    return "0.0"
                if info["decl"].var_type.is_integer_type():
                    return "0"
                return info["name"]

            for info in inputs_unique:
                if info["rank"] > 0:
                    continue
                assign_value = _default_assign_value(info)
                sub_lines.append(_indent(f"{info['name']} = {assign_value}", 2))

            for info in inputs_unique:
                if info["rank"] == 0:
                    continue
                base_name = info["name"]
                placeholders = array_placeholders.get(base_name, [])
                if placeholders:
                    for placeholder in placeholders:
                        value = placeholder_values.get(placeholder)
                        if value is not None:
                            sub_lines.append(_indent(f"{placeholder} = {value}", 2))
                    dims = ", ".join(placeholders)
                    sub_lines.append(_indent(f"allocate({base_name}({dims}))", 2))
                    assign_value = _default_assign_value(info)
                    sub_lines.append(_indent(f"{base_name} = {assign_value}", 2))

        pure_outputs = [
            info_by_name[name]
            for name in output_names - input_names
            if info_by_name[name]["rank"] > 0
        ]
        if pure_outputs:
            sub_lines.append("")
            sub_lines.append(_indent("! Allocate storage for output variables.", 2))
            for info in pure_outputs:
                placeholders = array_placeholders.get(info["name"], [])
                if placeholders:
                    for placeholder in placeholders:
                        value = placeholder_values.get(placeholder)
                        if value is not None:
                            sub_lines.append(_indent(f"{placeholder} = {value}", 2))
                    dims = ", ".join(placeholders)
                    sub_lines.append(_indent(f"allocate({info['name']}({dims}))", 2))
                    if info["name"] in diff_outputs_names:
                        sub_lines.append(
                            _indent(
                                f"allocate({info['name']}{ad_suffix}, mold={info['name']})",
                                2,
                            )
                        )
                assign_value = (
                    "0.0"
                    if info["decl"].var_type.is_real_type()
                    else (
                        "0" if info["decl"].var_type.is_integer_type() else info["name"]
                    )
                )
                if info["intent"] != "out":
                    sub_lines.append(_indent(f"{info['name']} = {assign_value}", 2))

        for name, decl in local_declares.items():
            rank = len(decl.dims_raw) if decl.dims_raw else 0
            if rank > 0 and name.endswith(
                ("_init", "_ad", "_u", "_grad", "_v", "_base", "_pert")
            ):
                base_name = name.split("_", 1)[0]
                if base_name == name:
                    continue
                if name.endswith(ad_suffix) and base_name not in input_names:
                    continue
                sub_lines.append(_indent(f"allocate({name}, mold={base_name})", 2))
            elif rank > 0 and name.endswith(ad_suffix):
                base_name = name[: -len(ad_suffix)]
                if base_name not in input_names:
                    continue
                sub_lines.append(_indent(f"allocate({name}, mold={base_name})", 2))

        if inputs_unique:
            sub_lines.append("")
            for info in inputs_unique:
                sub_lines.append(_indent(f"{info['name']}_init = {info['name']}", 2))

        first_input_info: Optional[Dict[str, Any]] = None
        if diff_inputs_names:
            for info in inputs_unique:
                if info["name"] in diff_inputs_names:
                    first_input_info = info
                    break
        elif inputs_unique:
            first_input_info = inputs_unique[0]

        def _emit_call(call_name: Optional[str], arg_list: List[str], indent: int):
            if call_name is None:
                return
            args_str = ", ".join(arg_list)
            sub_lines.append(_indent(f"call {call_name}({args_str})", indent))

        if input_names or output_names:
            sub_lines.append("")
            sub_lines.append(_indent("! Baseline evaluation", 2))
            if routine.kind == "subroutine":
                _emit_call(routine.name, list(routine.args), 2)
            else:
                call_args = ", ".join(routine.args)
                result_name = result_info["name"] if result_info else "result_value"
                sub_lines.append(
                    _indent(f"{result_name} = {routine.name}({call_args})", 2)
                )

            for name in output_names:
                sub_lines.append(_indent(f"{name}_base = {name}", 2))
            for info in inputs_unique:
                sub_lines.append(_indent(f"{info['name']} = {info['name']}_init", 2))

        if diff_inputs_names and diff_outputs_names and forward_available:
            sub_lines.append("")
            sub_lines.append(_indent("! Perturbed evaluation", 2))
            if first_input_info:
                ref_name = first_input_info["name"]
                rank = first_input_info["rank"]
                if rank > 0:
                    ref_expr = f"{ref_name}(1)"
                else:
                    ref_expr = ref_name
                sub_lines.append(_indent(f"delta = sqrt(epsilon({ref_expr}))", 2))
            for info in inputs_unique:
                base_name = info["name"]
                if base_name in diff_inputs_names:
                    sub_lines.append(
                        _indent(f"{base_name} = {base_name}_init + delta", 2)
                    )
                else:
                    sub_lines.append(_indent(f"{base_name} = {base_name}_init", 2))
            if routine.kind == "subroutine":
                _emit_call(routine.name, list(routine.args), 2)
            else:
                call_args = ", ".join(routine.args)
                result_name = result_info["name"] if result_info else "result_value"
                sub_lines.append(
                    _indent(f"{result_name} = {routine.name}({call_args})", 2)
                )
            for name in output_names:
                sub_lines.append(_indent(f"{name}_pert = {name}", 2))
            for info in inputs_unique:
                sub_lines.append(_indent(f"{info['name']} = {info['name']}_init", 2))

        if forward_available and diff_inputs_names and diff_outputs_names:
            sub_lines.append("")
            sub_lines.append(_indent("! Forward AD evaluation", 2))
            for info in inputs_unique:
                sub_lines.append(_indent(f"{info['name']} = {info['name']}_init", 2))
            for base_name in diff_inputs_names:
                sub_lines.append(_indent(f"{base_name}_u = 1.0", 2))
                sub_lines.append(_indent(f"{base_name}{ad_suffix} = {base_name}_u", 2))
            for name in diff_outputs_names:
                info = info_by_name[name]
                if info["intent"] != "in":
                    sub_lines.append(_indent(f"{name} = {name}_base", 2))
            for name in diff_outputs_names:
                sub_lines.append(_indent(f"{name}{ad_suffix} = 0.0", 2))
            sub_lines.append(_indent("fd_error = 0.0", 2))
            sub_lines.append(_indent("fd_forward_val = 0.0", 2))
            sub_lines.append(_indent("ad_forward_val = 0.0", 2))
            if routine_fwd is not None:
                _emit_call(routine_fwd.name, list(routine_fwd.args), 2)
            for name in diff_outputs_names:
                sub_lines.append(_indent(f"{name}_grad = {name}{ad_suffix}", 2))
            sub_lines.append("")
            for name in diff_outputs_names:
                info = info_by_name[name]
                expr_num = f"{name}_pert - {name}_base"
                fd_expr = f"({expr_num}) / delta"
                if info["rank"] == 0:
                    diff_expr = f"abs({fd_expr} - {name}_grad)"
                    fd_mag = f"abs({fd_expr})"
                    ad_mag = f"abs({name}_grad)"
                else:
                    diff_expr = f"maxval(abs({fd_expr} - {name}_grad))"
                    fd_mag = f"maxval(abs({fd_expr}))"
                    ad_mag = f"maxval(abs({name}_grad))"
                sub_lines.append(_indent(f"fd_error = max(fd_error, {diff_expr})", 2))
                sub_lines.append(
                    _indent(f"fd_forward_val = max(fd_forward_val, {fd_mag})", 2)
                )
                sub_lines.append(
                    _indent(
                        f"ad_forward_val = max(ad_forward_val, {ad_mag})",
                        2,
                    )
                )
            sub_lines.extend(
                _render_print(
                    2,
                    [
                        f"'Forward check ({routine.name}): max |FD - FWD| = '",
                        "fd_error",
                        "'  |FD| = '",
                        "fd_forward_val",
                        "'  |FWD| = '",
                        "ad_forward_val",
                    ],
                )
            )

        if (
            forward_available
            and reverse_available
            and diff_inputs_names
            and diff_outputs_names
        ):
            sub_lines.append("")
            sub_lines.append(_indent("! Reverse AD evaluation", 2))
            sub_lines.append(_indent("v_t_j_u = 0.0", 2))
            sub_lines.append(_indent("u_t_j_t_v = 0.0", 2))
            for name in diff_outputs_names:
                info = info_by_name[name]
                sub_lines.append(_indent(f"{name}_v = 1.0", 2))
                if info["rank"] == 0:
                    sub_lines.append(
                        _indent(
                            f"v_t_j_u = v_t_j_u + {name}_v * {name}_grad",
                            2,
                        )
                    )
                else:
                    sub_lines.append(
                        _indent(
                            f"v_t_j_u = v_t_j_u + sum({name}_v * {name}_grad)",
                            2,
                        )
                    )
            for name in diff_inputs_names:
                sub_lines.append(_indent(f"{name}{ad_suffix} = 0.0", 2))
            for name in diff_outputs_names:
                sub_lines.append(
                    _indent(
                        f"{name}{ad_suffix} = {name}_v",
                        2,
                    )
                )
            if routine_rev is not None:
                _emit_call(routine_rev.name, list(routine_rev.args), 2)
            for name in diff_inputs_names:
                info = info_by_name[name]
                if info["rank"] == 0:
                    sub_lines.append(
                        _indent(
                            f"u_t_j_t_v = u_t_j_t_v + {name}_u * {name}{ad_suffix}",
                            2,
                        )
                    )
                else:
                    sub_lines.append(
                        _indent(
                            f"u_t_j_t_v = u_t_j_t_v + sum({name}_u * {name}{ad_suffix})",
                            2,
                        )
                    )
            sub_lines.extend(
                _render_print(
                    2,
                    [
                        f"'Transpose check ({routine.name}): |v^TJu - u^TJ^Tv| = '",
                        "abs(v_t_j_u - u_t_j_t_v)",
                        "'  v^TJu = '",
                        "v_t_j_u",
                        "'  u^TJ^Tv = '",
                        "u_t_j_t_v",
                    ],
                )
            )

        sub_lines.append(_indent(f"end subroutine validate_{routine.name}", 1))
        if idx != len(routines) - 1:
            sub_lines.append("")
        lines.extend(sub_lines)

    if additional_uses:
        try:
            implicit_idx = lines.index("  implicit none")
        except ValueError:
            implicit_idx = 3
        use_lines = []
        for info in additional_uses.values():
            use = info["use"]
            entries = info["entries"]
            if entries is not None:
                if not entries:
                    continue
                use.only = list(entries.values())
            rendered = use.render(indent=1)[0].rstrip()
            use_lines.append(rendered)
        lines[implicit_idx:implicit_idx] = use_lines

    lines.append("")
    lines.append(f"end program {program_name}")
    return "\n".join(lines) + "\n"
