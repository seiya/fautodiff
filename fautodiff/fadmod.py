from __future__ import annotations

import json
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

from .code_tree import Declaration, Module
from .operators import Kind, OpInt, OpVar, VarType


class FadmodBase(ABC):
    """Base class for fadmod schema versions."""

    version: int

    def __init__(
        self,
        routines: Dict[str, Any],
        variables: List[OpVar],
        generics: Optional[Dict[str, Any]] = None,
        variables_raw: Optional[Dict[str, Any]] = None,
    ) -> None:
        self.routines = routines
        self.variables = variables
        self.generics = generics or {}
        self.variables_raw = variables_raw or {}

    @classmethod
    def load(cls, path: Union[str, Path]) -> "FadmodBase":
        """Load ``path`` and return an instance of the appropriate subclass."""

        try:
            data = json.loads(Path(path).read_text())
        except FileNotFoundError as exc:
            raise RuntimeError(f"fadmod file not found: {path}") from exc
        except Exception as exc:  # pragma: no cover - invalid JSON
            raise RuntimeError(f"invalid fadmod file: {exc}") from exc
        version = data.get("version", 1)
        for sub in cls.__subclasses__():
            if getattr(sub, "version", None) == version:
                return sub.from_dict(data)
        raise RuntimeError(f"unsupported fadmod version {version}")

    @classmethod
    @abstractmethod
    def from_dict(cls, data: Dict[str, Any]) -> "FadmodBase":
        """Create instance from parsed JSON data."""

    @abstractmethod
    def dump(self) -> Dict[str, Any]:
        """Return JSON-serialisable representation."""

    def write(self, path: Union[str, Path]) -> None:
        """Write fadmod information to ``path``."""

        Path(path).write_text(json.dumps(self.dump(), indent=2))


class FadmodV1(FadmodBase):
    """Implementation of fadmod schema version 1."""

    version = 1

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "FadmodV1":
        routines = data.get("routines", {})
        variables_raw = data.get("variables", {})
        generics = data.get("generics", {})
        variables: List[OpVar] = []
        for name, info in variables_raw.items():
            kind_name = info.get("kind")
            kind = Kind(OpInt(int(kind_name))) if kind_name is not None else None
            vt = VarType(info["typename"], kind=kind)
            variables.append(
                OpVar(
                    name=name,
                    var_type=vt,
                    dims=info.get("dims"),
                    is_constant=info.get("constant", False),
                    allocatable=info.get("allocatable", False),
                    pointer=info.get("pointer", False),
                )
            )
        return cls(routines, variables, generics, variables_raw)

    def dump(self) -> Dict[str, Any]:
        data = {
            "version": self.version,
            "routines": self.routines,
            "variables": self.variables_raw,
        }
        if self.generics:
            data["generics"] = self.generics
        return data

    @classmethod
    def from_module(cls, mod: Module, routine_map: Dict[str, Any]) -> "FadmodV1":
        routines_data: Dict[str, Any] = {}
        for r in mod.routines:
            info = routine_map.get(r.name)
            if info is None:
                continue
            skip = info.get("skip") or (
                info.get("name_fwd_ad") is None and info.get("name_rev_ad") is None
            )
            if skip:
                routines_data[r.name] = {"skip": True}
                continue
            info_copy = dict(info)
            info_copy["module"] = mod.name
            routines_data[r.name] = info_copy

        variables_data: Dict[str, Any] = {}
        if mod.decls is not None:
            for d in mod.decls.iter_children():
                if isinstance(d, Declaration):
                    info: Dict[str, Any] = {"typename": d.var_type.typename}
                    if d.var_type.kind is not None:
                        info["kind"] = d.var_type.kind.val
                    if d.dims is not None:
                        info["dims"] = list(d.dims)
                    if d.parameter:
                        info["parameter"] = True
                    if d.constant:
                        info["constant"] = True
                    if d.init_val is not None:
                        info["init_val"] = d.init_val
                    if d.access is not None:
                        info["access"] = d.access
                    if d.allocatable:
                        info["allocatable"] = True
                    if d.pointer:
                        info["pointer"] = True
                    variables_data[d.name] = info
        return cls(routines_data, [], {}, variables_data)
