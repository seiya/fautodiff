from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


@dataclass
class VarType:
    """Representation of a Fortran variable type."""

    typename: str
    kind: Optional[str] = None
    kind_val: Optional[str] = None
    kind_keyword: bool = False
    char_len: Optional[str] = None

    def copy(self) -> "VarType":
        return VarType(
            typename=self.typename,
            kind=self.kind,
            kind_val=self.kind_val,
            kind_keyword=self.kind_keyword,
            char_len=self.char_len,
        )

    def __str__(self) -> str:
        parts = []
        if self.kind is not None:
            if self.kind.isdigit():
                parts.append(self.kind)
            else:
                parts.append(f"kind={self.kind}" if self.kind_keyword else self.kind)
        if self.char_len is not None:
            parts.append(f"len={self.char_len}")
        if parts:
            return f"{self.typename}(" + ",".join(parts) + ")"
        return self.typename

    def is_real_type(self) -> bool:
        typename = self.typename.lower()
        return typename.startswith("real") or typename.startswith("double")

    def is_complex_type(self) -> bool:
        return self.typename.lower().startswith("complex")

    def is_integer_type(self) -> bool:
        return self.typename.lower().startswith("integer")

    def _binary_result(self, other: "VarType") -> "VarType":
        if other is None:
            return self.copy()
        if self.is_complex_type() or other.is_complex_type():
            kind = self.kind or other.kind
            return VarType("complex", kind=kind)
        if self.is_real_type() or other.is_real_type():
            kind = self.kind or other.kind
            return VarType("real", kind=kind)
        if self.is_integer_type() and other.is_integer_type():
            kind = self.kind or other.kind
            return VarType("integer", kind=kind)
        return self.copy()

    def __add__(self, other: "VarType") -> "VarType":
        return self._binary_result(other)

    __sub__ = __add__
    __mul__ = __add__
    __truediv__ = __add__
