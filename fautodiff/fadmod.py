from __future__ import annotations

import json
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Union

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

    @abstractmethod
    def variable_declarations(
        self, kind_resolver: Optional[Callable[[Any], Kind]] = None
    ) -> Dict[str, Declaration]:
        """Return mapping of variable names to :class:`Declaration` objects.

        ``kind_resolver`` converts ``kind`` entries from the fadmod file to a
        :class:`~fautodiff.operators.Kind` instance.  It is passed either a
        string or an operator and must return a ``Kind``.  Subclasses should
        use it when a ``kind`` is expressed symbolically.  When ``None`` the
        raw value is used directly.
        """


class FadmodV1(FadmodBase):
    """Implementation of fadmod schema version 1."""

    version = 1

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "FadmodV1":
        routines = data.get("routines", {})
        if not isinstance(routines, dict):
            raise RuntimeError("'routines' must be an object")
        variables_raw = data.get("variables", {})
        if not isinstance(variables_raw, dict):
            raise RuntimeError("'variables' must be an object")
        generics = data.get("generics", {})
        if generics is None:
            generics = {}
        if not isinstance(generics, dict):
            raise RuntimeError("'generics' must be an object")

        variables: List[OpVar] = []
        for name, info in variables_raw.items():
            if not isinstance(name, str):
                raise RuntimeError("variable names must be strings")
            if not isinstance(info, dict):
                raise RuntimeError(f"variable '{name}' must map to an object")
            typename = info.get("typename")
            if not isinstance(typename, str):
                raise RuntimeError(f"variable '{name}' missing 'typename' field")
            kind_val = info.get("kind")
            if kind_val is not None and not isinstance(kind_val, int):
                raise RuntimeError(f"variable '{name}' field 'kind' must be int")
            dims_val = info.get("dims")
            if dims_val is not None:
                if not isinstance(dims_val, list) or any(
                    not isinstance(d, str) for d in dims_val
                ):
                    raise RuntimeError(
                        f"variable '{name}' field 'dims' must be list of strings"
                    )
            for key in ["parameter", "constant", "allocatable", "pointer", "optional"]:
                if key in info and not isinstance(info[key], bool):
                    raise RuntimeError(
                        f"variable '{name}' field '{key}' must be boolean"
                    )
            if "access" in info and not isinstance(info["access"], str):
                raise RuntimeError(f"variable '{name}' field 'access' must be a string")
            if "init_val" in info and not isinstance(
                info["init_val"], (int, float, str)
            ):
                raise RuntimeError(
                    f"variable '{name}' field 'init_val' must be a scalar or string"
                )

            kind = Kind(OpInt(kind_val)) if kind_val is not None else None
            vt = VarType(typename, kind=kind)
            variables.append(
                OpVar(
                    name=name,
                    var_type=vt,
                    dims=tuple(dims_val) if dims_val is not None else None,
                    is_constant=info.get("constant", False),
                    allocatable=info.get("allocatable", False),
                    pointer=info.get("pointer", False),
                    optional=info.get("optional", False),
                )
            )
        return cls(routines, variables, generics, variables_raw)

    def variable_declarations(
        self, kind_resolver: Optional[Callable[[Any], Kind]] = None
    ) -> Dict[str, Declaration]:
        decls: Dict[str, Declaration] = {}
        for name, info in self.variables_raw.items():
            kind: Optional[Kind] = None
            kind_name = info.get("kind_name")
            if kind_name is not None and kind_resolver is not None:
                kind = kind_resolver(kind_name)
            elif info.get("kind") is not None:
                val = info["kind"]
                kind = Kind(OpInt(val), val=val)
            vt = VarType(info["typename"], kind=kind)
            decls[name] = Declaration(
                name,
                var_type=vt,
                dims=tuple(info["dims"]) if info.get("dims") is not None else None,
                intent=None,
                parameter=info.get("parameter", False),
                constant=info.get("constant", False),
                init_val=info.get("init_val"),
                access=info.get("access"),
                allocatable=info.get("allocatable", False),
                pointer=info.get("pointer", False),
                optional=info.get("optional", False),
            )
        return decls

    def dump(self) -> Dict[str, Any]:
        data = {
            "routines": self.routines,
            "variables": self.variables_raw,
        }
        if self.generics:
            data["generics"] = self.generics
        return data

    def write(self, path: Union[str, Path]) -> None:
        data = {"version": self.version}
        data.update(self.dump())
        Path(path).write_text(json.dumps(data, indent=2))

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
