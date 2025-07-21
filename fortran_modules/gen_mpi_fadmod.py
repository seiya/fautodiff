#!/usr/bin/env python3
"""Generate mpi.fadmod from mpi_ad.f90 using fautodiff.parser."""
from __future__ import annotations

import json
import re
import tempfile
from pathlib import Path

from fautodiff import parser
from fautodiff.code_tree import Declaration

_INTERF_RE = re.compile(r"^\s*interface\s+(\w+)", re.I)
_END_INTERF_RE = re.compile(r"^\s*end\s+interface", re.I)
_TYPE_DEF_RE = re.compile(r"^\s*type\b(?!\()", re.I)
_END_TYPE_RE = re.compile(r"^\s*end\s*type", re.I)
_SUB_START_RE = re.compile(r"^\s*subroutine\s+(\w+)", re.I)
_END_SUB_RE = re.compile(r"^\s*end\s+subroutine", re.I)
_DECL_RE = re.compile(
    r"^\s*(use\b|implicit\b|integer\b|real\b|type\(|logical\b|character\b|complex\b|double\s+precision\b)",
    re.I,
)
_STAR_RE = re.compile(r"\(\s*\*\s*\)")
_MODE_RE = re.compile(r"mpi_(.*?)(_fwd_rev_ad|_fwd_ad|_rev_ad)(?:_(.*))?$", re.I)


def _preprocess(path: Path) -> tuple[Path, dict[str, list[str]]]:
    """Return path to a simplified source file and collected generics."""
    interfaces: dict[str, list[str]] = {}
    out_lines: list[str] = []
    buf: list[str] = []
    state = None
    sub_lines: list[str] = []
    with path.open() as f:
        for line in f:
            if state is None:
                m = _INTERF_RE.match(line)
                if m:
                    state = "interface"
                    name = m.group(1)
                    buf = []
                    continue
                if _TYPE_DEF_RE.match(line):
                    state = "type"
                    continue
                if _SUB_START_RE.match(line):
                    state = "sub"
                    sub_lines = [_STAR_RE.sub("(1)", line.rstrip())]
                    continue
                out_lines.append(_STAR_RE.sub("(1)", line.rstrip()))
            elif state == "interface":
                if _END_INTERF_RE.match(line):
                    procs = []
                    for l in buf:
                        m2 = re.search(r"module procedure\s+(\w+)", l, re.I)
                        if m2:
                            procs.append(m2.group(1))
                    interfaces[name] = procs
                    state = None
                else:
                    buf.append(line)
            elif state == "type":
                if _END_TYPE_RE.match(line):
                    state = None
            elif state == "sub":
                sub_lines.append(line.rstrip())
                if _END_SUB_RE.match(line):
                    processed = [sub_lines[0]]
                    i = 1
                    while i < len(sub_lines) - 1 and _DECL_RE.match(sub_lines[i]):
                        processed.append(_STAR_RE.sub("(1)", sub_lines[i]))
                        i += 1
                    processed.append("  ! body removed")
                    processed.append(sub_lines[-1])
                    out_lines.extend(processed)
                    state = None
    tmp = tempfile.NamedTemporaryFile("w", suffix=".f90", delete=False)
    tmp.write("\n".join(out_lines))
    tmp.close()
    return Path(tmp.name), interfaces


def _collect_routines(mod) -> dict[str, dict]:
    routines: dict[str, dict] = {}
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
        decls: dict[str, Declaration] = sub.decl_map or {}
        args = sub.args
        info = routines.setdefault(variant_upper, {"module": "mpi"})
        mpi_args = [a for a in args if not a.endswith("_ad")]
        if "args" not in info:
            info["args"] = mpi_args
            info["intents"] = [decls.get(a).intent if decls.get(a) else None for a in mpi_args]
            info["dims"] = [list(decls.get(a).dims) if decls.get(a) and decls.get(a).dims else None for a in mpi_args]
            info["type"] = [decls.get(a).typename.lower() if decls.get(a) else None for a in mpi_args]
            info["kind"] = [decls.get(a).kind for a in mpi_args]
        tag = {"_fwd_ad": "fwd_ad", "_rev_ad": "rev_ad", "_fwd_rev_ad": "fwd_rev_ad"}[mode]
        info[f"name_{tag}"] = name
        info[f"args_{tag}"] = args
        info[f"intents_{tag}"] = [decls.get(a).intent if decls.get(a) else None for a in args]
    return routines


def _interfaces_to_generics(ifaces: dict[str, list[str]]) -> dict[str, list[str]]:
    generics: dict[str, list[str]] = {}
    for name, procs in ifaces.items():
        m = _MODE_RE.match(name)
        if not m:
            continue
        base = m.group(1)
        base_upper = "MPI_" + base.capitalize()
        lst = []
        for p in procs:
            pm = _MODE_RE.match(p)
            if not pm:
                continue
            variant = pm.group(3)
            var_upper = base_upper + ("_" + variant if variant else "")
            lst.append(var_upper)
        if lst:
            generics[base_upper] = lst
    return generics


def main() -> None:
    here = Path(__file__).resolve().parent
    src = here / "mpi_ad.f90"
    tmp, interfaces = _preprocess(src)
    mod = parser.parse_file(str(tmp))[0]
    routines = _collect_routines(mod)
    generics = _interfaces_to_generics(interfaces)
    variables = {
        "MPI_COMM_WORLD": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_STATUS_SIZE": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_STATUS_IGNORE": {"typename": "integer", "dims": ["MPI_STATUS_SIZE"], "parameter": True, "constant": True, "access": "public"},
        "MPI_SUM": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_MAX": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_MIN": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_PROD": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_MAXLOC": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_MINLOC": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_LAND": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_LOR": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_LXOR": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_BAND": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_BOR": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_BXOR": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_REPLACE": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_INT": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_INTEGER": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_REAL": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_DOUBLE_PRECISION": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_COMPLEX": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_DOUBLE_COMPLEX": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_LOGICAL": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_CHARACTER": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_BYTE": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
        "MPI_REAL8": {"typename": "integer", "parameter": True, "constant": True, "access": "public"},
    }
    data = {"routines": routines, "variables": variables, "generics": generics}
    (here / "mpi.fadmod").write_text(json.dumps(data, indent=2))


if __name__ == "__main__":
    main()
