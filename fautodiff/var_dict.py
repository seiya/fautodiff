"""Simple ordered dictionary used by the tests."""

from __future__ import annotations

class VarDict:
    """Ordered dictionary with a minimal API.

    This class mimics part of :class:`dict` while preserving insertion order.
    The previous implementation stored key/value pairs in a list which was
    inefficient for lookups.  Using a real dictionary keeps the behaviour
    unchanged but improves performance.
    """

    def __init__(self) -> None:
        self._data: dict = {}

    def __setitem__(self, key, value) -> None:
        """Set ``key`` to ``value`` preserving insertion order."""
        self._data[key] = value

    def __getitem__(self, key):
        return self._data[key]

    def __contains__(self, key) -> bool:
        return key in self._data

    def __delitem__(self, key) -> None:
        del self._data[key]

    def keys(self):
        return list(self._data.keys())

    def values(self):
        return list(self._data.values())

    def items(self):
        return list(self._data.items())

    def __len__(self):
        return len(self._data)

    def __iter__(self):
        return iter(self._data)

    def __str__(self):
        return ", ".join(f"{k}=>{v}" for k, v in self._data.items())

    def remove(self, key) -> None:
        self._data.pop(key)

    def copy(self) -> "VarDict":
        obj = type(self)()
        obj._data = self._data.copy()
        return obj


# Backwards compatibility: old class name
Vardict = VarDict
