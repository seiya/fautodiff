#!/usr/bin/env python3
"""Generate mpi.fadmod from mpi_ad.f90 using fautodiff.parser."""
from __future__ import annotations

import os
import sys
# Ensure the package can be imported when running from the source tree
PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
if PROJECT_ROOT not in sys.path:
    sys.path.insert(0, PROJECT_ROOT)

import json
import re
import tempfile
from pathlib import Path
from typing import List, Dict

from fautodiff import parser
from fautodiff.code_tree import (
    Declaration,
    Interface,
)

_MODE_RE = re.compile(r"mpi_(.*?)(_fwd_rev_ad|_fwd_ad|_rev_ad)(?:_(.*))?$", re.I)

def _collect_routines(mod, routines: Dict[str, dict]) -> Dict[str, dict]:
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
                intents.append(decls.get(arg).intent if decls.get(arg) else None)
                if decls.get(arg) and decls.get(arg).dims:
                    dim = []
                    for d in decls.get(arg).dims:
                        if d == "*" and "count" in mpi_args:
                            dim.append("count")
                        else:
                            dim.append(d)
                else:
                    dim = None
                dims.append(dim)
                types.append(decls.get(arg).typename.lower() if decls.get(arg) else None)
                kinds.append(decls.get(arg).kind)
            info["intents"] = intents
            info["dims"] = dims
            info["type"] = types
            info["kind"] = kinds
        info[f"name{mode}"] = f"{base_upper}{mode}"
        info[f"args{mode}"] = args
        info[f"intents{mode}"] = [decls.get(a).intent if decls.get(a) else None for a in args]
    return routines


def _interfaces_to_generics(mod) -> Dict[str, List[str]]:
    generics: Dict[str, List[str]] = {}
    for node in mod.body.iter_children():
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
        "args": [
            "comm",
            "rank",
            "ierror"
        ],
        "intents": [
            "in",
            "out",
            "out"
        ],
        "type": [
            "integer",
            "integer",
            "integer"
        ]
    },
    "MPI_Comm_size": {
        "args": [
            "comm",
            "size",
            "ierror"
        ],
        "intents": [
            "in",
            "out",
            "out"
        ],
        "type": [
            "integer",
            "integer",
            "integer"
        ]
    }
}


# MPI constants
variables: Dict[str, dict] = {
    "MPI_COMM_WORLD": {},
    "MPI_STATUS_SIZE": {},
    "MPI_STATUS_IGNORE": {"dims": ["MPI_STATUS_SIZE"]},
    "MPI_STATUSES_IGNORE": {"dims": ["MPI_STATUS_SIZE", ":"]},
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
}
common = {"typename": "integer", "parameter": True}
decl_map = {}
for name, v in variables.items():
    v.update(common)
    dims = tuple(v.get("dims")) if "dims" in v else None
    decl_map[name] = Declaration(name=name, typename="integer", dims=dims, parameter=True)

def main() -> None:
    here = Path(__file__).resolve().parent
    src = str(here / "mpi_ad.f90")
    mod = parser.parse_file(src, search_dirs=[str(here)], decl_map=decl_map)[0]
    routines = _collect_routines(mod, routines_mpi)
    generics = _interfaces_to_generics(mod)
    data = {"routines": routines, "variables": variables, "generics": generics}
    print(json.dumps(data, indent=2))


if __name__ == "__main__":
    main()
