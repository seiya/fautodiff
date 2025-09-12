#!/usr/bin/env python3
"""Generate mpi.fadmod from mpi_ad.f90 using fautodiff.parser."""
from __future__ import annotations

import os
import sys

# Ensure the package can be imported when running from the source tree
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if PROJECT_ROOT not in sys.path:
    sys.path.insert(0, PROJECT_ROOT)

import json
import re
import tempfile
from pathlib import Path
from typing import Dict, List, Set

from fautodiff import parser
from fautodiff.code_tree import Declaration, Interface
from fautodiff.operators import VarType, OpVar, OpRange

_MODE_RE = re.compile(r"mpi_(.*?)(_fwd_rev_ad|_fwd_ad|_rev_ad)(?:_(.*))?$", re.I)


def _find_count_arg(arg: str, args: List[str]) -> str | None:
    arg_lower = arg.lower()
    for cand in args:
        cand_lower = cand.lower()
        if cand_lower.endswith("count") or cand_lower.endswith("counts"):
            base = cand_lower[:-5] if cand_lower.endswith("count") else cand_lower[:-6]
            base = base.rstrip("_")
            if base and arg_lower.startswith(base):
                return cand
    if "count" in args:
        return "count"
    return None


def _collect_routines(
    mod, routines: Dict[str, dict], assumed_rank: Set[str]
) -> Dict[str, dict]:
    for sub in mod.routines:
        name = sub.name
        if not name.startswith("mpi_"):
            continue
        m = _MODE_RE.match(name)
        if not m:
            continue
        base, mode, variant = m.groups()
        base_upper = "MPI_" + base.capitalize()
        variant_upper = base_upper + ("_" + variant if variant else "")
        decls: Dict[str, Declaration] = sub.decl_map or {}
        args = sub.args
        info = routines.setdefault(variant_upper, {"module": "mpi"})
        if mode == "_fwd_ad":
            mpi_args = [a for a in args if not a.endswith("_ad")]
            info["args"] = mpi_args
            intents = []
            dims = []
            types = []
            kinds = []
            for arg in mpi_args:
                arg_lower = arg.lower()
                intents.append(decls.get(arg).intent if decls.get(arg) else None)
                if arg_lower in assumed_rank:
                    dim = None
                elif decls.get(arg) and decls.get(arg).dims_raw:
                    dim = []
                    for d in decls.get(arg).dims_raw:
                        if d == "*":
                            count = _find_count_arg(arg, mpi_args)
                            dim.append(count if count else d)
                        else:
                            dim.append(d)
                else:
                    dim = None
                dims.append(dim)
                types.append(
                    decls.get(arg).var_type.typename.lower() if decls.get(arg) else None
                )
                raw_kind = decls.get(arg).var_type.kind if decls.get(arg) else None
                if raw_kind is not None:
                    kind_str = str(raw_kind).strip()
                    kind = int(kind_str) if kind_str.isdigit() else None
                else:
                    kind = None
                kinds.append(kind)
            info["intents"] = intents
            info["dims"] = dims
            info["type"] = types
            info["kind"] = kinds
        info[f"name{mode}"] = f"{base_upper}{mode}"
        info[f"args{mode}"] = args
        info[f"intents{mode}"] = [
            decls.get(a).intent if decls.get(a) else None for a in args
        ]
    return routines


def _interfaces_to_generics(mod) -> Dict[str, List[str]]:
    generics: Dict[str, List[str]] = {}
    for node in mod.decls.iter_children():
        if not isinstance(node, Interface):
            continue
        m = _MODE_RE.match(node.name)
        if not m:
            continue
        base, mode, variant = m.groups()
        base_upper = "MPI_" + base.capitalize()
        if mode == "_fwd_ad":
            name = base_upper
            lst = []
            for proc in node.module_procs:
                m = _MODE_RE.match(proc)
                if not m:
                    continue
                base, _, variant = m.groups()
                proc = "MPI_" + base.capitalize()
                proc = proc + ("_" + variant if variant else "")
                lst.append(proc)
            if lst:
                generics[name] = lst
    return generics


# MPI routines
routines_mpi: Dict[str, dict] = {
    "MPI_Comm_rank": {
        "args": ["comm", "rank", "ierror"],
        "intents": ["in", "out", "out"],
        "type": ["integer", "integer", "integer"],
    },
    "MPI_Comm_size": {
        "args": ["comm", "size", "ierror"],
        "intents": ["in", "out", "out"],
        "type": ["integer", "integer", "integer"],
    },
    "MPI_Init": {"skip": True},
    "MPI_Finalize": {"skip": True},
    "MPI_Win_fence": {
        "args": ["assert", "win", "ierr"],
        "intents": ["in", "in", "out"],
        "type": ["integer", "integer", "integer"],
    },
    "MPI_Win_create": {"skip": True},
    "MPI_Win_free": {"skip": True},
    "MPI_Type_commit": {"skip": True},
    "MPI_Type_free": {"skip": True},
    "MPI_Type_contiguous": {"skip": True},
    "MPI_Type_vector": {"skip": True},
    "MPI_Type_indexed": {"skip": True},
    "MPI_Type_create_hvector": {"skip": True},
    "MPI_Type_create_hindexed": {"skip": True},
    "MPI_Type_create_hindexed_block": {"skip": True},
    "MPI_Type_create_struct": {"skip": True},
    "MPI_Type_create_resized": {"skip": True},
    "MPI_Type_create_subarray": {"skip": True},
    "MPI_Type_create_darray": {"skip": True},
    "MPI_Group_compare": {"skip": True},
    "MPI_Group_range_incl": {"skip": True},
    "MPI_Group_range_excl": {"skip": True},
    "MPI_Cart_map": {"skip": True},
    "MPI_Graph_create": {"skip": True},
    "MPI_Graph_neighbors": {"skip": True},
    "MPI_Graph_neighbors_count": {"skip": True},
    "MPI_Graph_map": {"skip": True},
    "MPI_Topo_test": {"skip": True},
    "MPI_Add_error_class": {"skip": True},
    "MPI_Add_error_code": {"skip": True},
    "MPI_Add_error_string": {"skip": True},
    "MPI_Errhandler_create": {"skip": True},
    "MPI_Errhandler_get": {"skip": True},
    # MPI-IO routines (no AD support yet)
    "MPI_File_open": {"skip": True},
    "MPI_File_close": {"skip": True},
    "MPI_File_read": {"skip": True},
    "MPI_File_write": {"skip": True},
    "MPI_File_set_view": {"skip": True},
    "MPI_File_get_view": {"skip": True},
    "MPI_File_read_at": {"skip": True},
    "MPI_File_write_at": {"skip": True},
    "MPI_File_read_all": {"skip": True},
    "MPI_File_write_all": {"skip": True},
    "MPI_File_seek": {"skip": True},
    "MPI_File_get_position": {"skip": True},
    "MPI_File_get_size": {"skip": True},
    "MPI_File_set_size": {"skip": True},
}


# MPI constants
variables: Dict[str, dict] = {
    "MPI_SUCCESS": {},
    "MPI_COMM_WORLD": {},
    "MPI_COMM_SELF": {},
    "MPI_COMM_NULL": {},
    "MPI_PROC_NULL": {},
    "MPI_ANY_SOURCE": {},
    "MPI_ANY_TAG": {},
    "MPI_STATUS_SIZE": {},
    "MPI_STATUS_IGNORE": {"dims": ["MPI_STATUS_SIZE"]},
    "MPI_STATUSES_IGNORE": {"dims": ["MPI_STATUS_SIZE", ":"]},
    "MPI_IN_PLACE": {},
    "MPI_BOTTOM": {},
    "MPI_UNWEIGHTED": {},
    "MPI_WEIGHTS_EMPTY": {},
    "MPI_REQUEST_NULL": {},
    "MPI_SUM": {},
    "MPI_MAX": {},
    "MPI_MIN": {},
    "MPI_PROD": {},
    "MPI_MAXLOC": {},
    "MPI_MINLOC": {},
    "MPI_LAND": {},
    "MPI_LOR": {},
    "MPI_LXOR": {},
    "MPI_BAND": {},
    "MPI_BOR": {},
    "MPI_BXOR": {},
    "MPI_REPLACE": {},
    "MPI_INT": {},
    "MPI_INTEGER": {},
    "MPI_REAL": {},
    "MPI_DOUBLE_PRECISION": {},
    "MPI_COMPLEX": {},
    "MPI_DOUBLE_COMPLEX": {},
    "MPI_LOGICAL": {},
    "MPI_CHARACTER": {},
    "MPI_BYTE": {},
    "MPI_REAL8": {},
    "MPI_WIN_NULL": {},
    "MPI_GROUP_NULL": {},
    "MPI_UNDEFINED": {},
    "MPI_TAG_UB": {},
    "MPI_BSEND_OVERHEAD": {},
    "MPI_UNIVERSE_SIZE": {},
    "MPI_APPNUM": {},
    "MPI_MAX_PROCESSOR_NAME": {},
    "MPI_MAX_PORT_NAME": {},
    "MPI_MAX_LIBRARY_VERSION_STRING": {},
    "MPI_MAX_ERROR_STRING": {},
    "MPI_ERR_COMM": {},
    "MPI_ERR_GROUP": {},
    "MPI_ERR_TYPE": {},
    "MPI_ERR_RANK": {},
    "MPI_ERR_COUNT": {},
    "MPI_ERR_TAG": {},
    "MPI_ERR_ROOT": {},
    "MPI_ERR_TOPOLOGY": {},
    "MPI_ERR_OTHER": {},
    "MPI_ERR_IN_STATUS": {},
    "MPI_ERR_PENDING": {},
    # RMA constants
    "MPI_MODE_NOCHECK": {},
    "MPI_MODE_NOPRECEDE": {},
    "MPI_MODE_NOPUT": {},
    "MPI_MODE_NOSTORE": {},
    "MPI_MODE_NOSUCCEED": {},
    # Data distribution constants
    "MPI_DISTRIBUTE_BLOCK": {},
    "MPI_DISTRIBUTE_CYCLIC": {},
    "MPI_DISTRIBUTE_NONE": {},
    "MPI_DISTRIBUTE_DFLT_DARG": {},
    # Memory order constants
    "MPI_ORDER_C": {},
    "MPI_ORDER_FORTRAN": {},
    # MPI-IO constants
    "MPI_FILE_NULL": {},
    "MPI_INFO_NULL": {},
    "MPI_MAX_DATAREP_STRING": {},
    "MPI_MODE_RDONLY": {},
    "MPI_MODE_WRONLY": {},
    "MPI_MODE_RDWR": {},
    "MPI_MODE_CREATE": {},
    "MPI_MODE_EXCL": {},
    "MPI_MODE_DELETE_ON_CLOSE": {},
    "MPI_MODE_UNIQUE_OPEN": {},
    "MPI_MODE_APPEND": {},
    "MPI_MODE_SEQUENTIAL": {},
    "MPI_SEEK_SET": {},
    "MPI_SEEK_CUR": {},
    "MPI_SEEK_END": {},
    "MPI_DISPLACEMENT_CURRENT": {},
    "MPI_OFFSET_KIND": {},
}
common = {"typename": "integer", "parameter": True}
decl_map = {}
for name, v in variables.items():
    v.update(common)
    dims_val = v.get("dims")
    if dims_val is not None:
        dims_list = []
        for dim in dims_val:
            if dim is None:
                dims_list.append(None)
            elif ":" in dim:
                ds = dim.split(":")
                if ds[0] == "" and ds[1] == "":
                    dims_list.append(None)
                elif ds[0] == "":
                    dims_list.append(OpVar(ds[1]))
                else:
                    dims_list.append(OpRange([OpVar(ds[0]), OpVar(ds[1])]))
        dims = tuple(dims_list)
    else:
        dims = None
    decl_map[name] = Declaration(
        name=name, var_type=VarType("integer"), dims=dims, parameter=True
    )
# iso_c_binding declarations used in mpi_ad.f90
decl_map["c_null_ptr"] = Declaration(
    name="c_null_ptr", var_type=VarType("type(c_ptr)"), parameter=True
)


def main() -> None:
    here = Path(__file__).resolve().parent
    src_path = here / "mpi_ad.f90"
    src_text = src_path.read_text()

    assumed_rank: Set[str] = set()
    for m in re.finditer(r"::\s*([A-Za-z0-9_,\s]+)\(\s*\.\.\s*\)", src_text):
        names = m.group(1).split(",")
        for name in names:
            assumed_rank.add(name.strip().lower())

    processed = re.sub(r"\(\s*\.\.\s*\)", "(:)", src_text)
    with tempfile.NamedTemporaryFile("w", suffix=".f90") as tmp:
        tmp.write(processed)
        tmp.flush()
        mod = parser.parse_file(tmp.name, decl_map=decl_map)[0]
    routines = _collect_routines(mod, routines_mpi, assumed_rank)
    generics = _interfaces_to_generics(mod)
    data = {"version": 1, "routines": routines, "variables": variables}
    if generics:
        data["generics"] = generics
    print(json.dumps(data, indent=2, default=str))


if __name__ == "__main__":
    main()
