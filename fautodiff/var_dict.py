"""Simple ordered dictionary used by the tests."""

from __future__ import annotations

from typing import Dict, Generic, Iterable, Iterator, List, Tuple, TypeVar, Optional

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
        self._keys: List[KT] = []
        self._values: List[VT] = []

    def __setitem__(self, key: KT, value: VT) -> None:
        """Set ``key`` to ``value`` preserving insertion order."""
        if key in self._keys:
            index = self._keys.index(key)
            self._values[index] = value
        else:
            self._keys.append(key)
            self._values.append(value)

    def __getitem__(self, key: KT) -> VT:
        if key not in self._keys:
            raise KeyError(key)
        index = self._keys.index(key)
        return self._values[index]

    def get(self, key: KT, default = None) -> Optional[VT]:
        if key in self._keys:
            return self[key]
        return default

    def __contains__(self, key: object) -> bool:
        return key in self._keys

    def __delitem__(self, key: KT) -> None:
        if key not in self._keys:
            raise KeyError(key)
        index = self._keys.index(key)
        del self._keys[index]
        del self._values[index]

    def keys(self) -> List[KT]:
        return list(self._keys)

    def values(self) -> List[VT]:
        return list(self._values)

    def items(self) -> List[Tuple[KT, VT]]:
        return list(zip(self._keys, self._values))

    def __len__(self) -> int:
        return len(self._keys)

    def __iter__(self) -> Iterator[KT]:
        return iter(self._keys)

    def __str__(self) -> str:
        return ", ".join(f"{k}=>{v}" for k, v in self.items())

    def remove(self, key: KT) -> None:
        del self[key]

    def copy(self) -> "VarDict[KT, VT]":
        obj = type(self)()
        obj._keys = self._keys.copy()
        obj._values = self._values.copy()
        return obj

