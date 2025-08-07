"""Simple ordered dictionary used by the tests."""

from __future__ import annotations

from typing import Dict, Generic, Iterable, Iterator, List, Tuple, TypeVar

KT = TypeVar("KT")
VT = TypeVar("VT")


class VarDict(Generic[KT, VT]):
    """Ordered dictionary with a minimal API.

    This class mimics part of :class:`dict` while preserving insertion order.
    The previous implementation stored key/value pairs in a list which was
    inefficient for lookups.  Using a real dictionary keeps the behaviour
    unchanged but improves performance.
    """

    def __init__(self) -> None:
        self._data: Dict[KT, VT] = {}

    def __setitem__(self, key: KT, value: VT) -> None:
        """Set ``key`` to ``value`` preserving insertion order."""
        self._data[key] = value

    def __getitem__(self, key: KT) -> VT:
        return self._data[key]

    def __contains__(self, key: object) -> bool:
        return key in self._data

    def __delitem__(self, key: KT) -> None:
        del self._data[key]

    def keys(self) -> List[KT]:
        return list(self._data.keys())

    def values(self) -> List[VT]:
        return list(self._data.values())

    def items(self) -> List[Tuple[KT, VT]]:
        return list(self._data.items())

    def __len__(self) -> int:
        return len(self._data)

    def __iter__(self) -> Iterator[KT]:
        return iter(self._data)

    def __str__(self) -> str:
        return ", ".join(f"{k}=>{v}" for k, v in self._data.items())

    def remove(self, key: KT) -> None:
        self._data.pop(key)

    def copy(self) -> "VarDict[KT, VT]":
        obj = type(self)()
        obj._data = self._data.copy()
        return obj


# Backwards compatibility: old class name
Vardict = VarDict
