"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
import copy

from .operators import (
    AryIndex,
    Operator,
    OpInt,
    OpVar,
    OpNeg,
    OpRange
)


@dataclass
class VarList:
    """List of variable."""

    vars: dict[str, List] = field(init=False, default=None)

    def __init__(self, vars: Optional[List[OpVar]] = None):
        super().__init__()
        self.vars = {}
        if vars is not None:
            for var in vars:
                self.push(var)

    def __contains__(self, item: OpVar):
        if not isinstance(item, OpVar):
            raise ValueError(f"Must be OpVar: {type(item)}")
        if not item.name in self.vars:
            return False
        index_list = self.vars[item.name]
        for index in index_list:
            if index is None:
                return True
            if index <= item.index or index >= item.index:
                return True
        return False

    def __str__(self) -> str:
        ary = []
        for name, index_list in sorted(self.vars.items()):
            for index in index_list:
                ary.append(str(OpVar(name, index=index)))
        return ", ".join(ary)

    def __len__(self) -> int:
        return len(self.vars)

    def __getitem__(self, key: str):
        return self.vars[key]

    def __iter__(self) -> iter:
        return iter(self.vars)

    def names(self) -> Set[str]:
        return set(self.vars.keys())

    @staticmethod
    def _get_int(op) -> bool:
        if isinstance(op, OpInt):
            return op.val
        if isinstance(op, OpNeg) and isinstance(op.args[0], OpInt):
            return - op.args[0].val
        return None

    @staticmethod
    def _dim_for_comparison(index1: AryIndex, index2: AryIndex) -> int:
        if not isinstance(index1, AryIndex) or not isinstance(index2, AryIndex):
            raise ValueError(f"Unexpected type: {type(index1)} {type(var.index2)}")

        if index1 == index2:
            raise ValueError("Must be different")

        diff_dim = None # dimension at which difference was found
        for i, dim1 in enumerate(index1):
            dim2 = index2[i]

            def _check_found():
                if diff_dim is not None:
                    return -1

            if dim1 == dim2:
                continue
            if dim1 is None or dim2 is None:
                _check_found()
                diff_dim = i
                continue
            v1 = VarList._get_int(dim1)
            v2 = VarList._get_int(dim2)
            if v1 is not None and v2 is not None:
                _check_found()
                diff_dim = i
                continue
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                return -1
            if v1 is None: # dim1 is OpRange
                i0 = VarList._get_int(dim1[0])
                i1 = VarList._get_int(dim1[1])
                i2 = VarList._get_int(dim1[2])
            if (v1 is None) and not ((i0 is not None and i1 is not None) and (dim1[2] is None or (i2 is not None and abs(i2)==1))):
                return -1
            if v2 is None: # dim2 is OpRange
                j0 = VarList._get_int(dim2[0])
                j1 = VarList._get_int(dim2[1])
                j2 = VarList._get_int(dim2[2])
            if (v2 is None) and not ((j0 is not None and j1 is not None) and (dim2[2] is None or (j2 is not None and abs(j2)==1))):
                return -1

            _check_found()
            diff_dim = i

        if diff_dim is None:
            raise RuntimeError(f"Unexpected error: {index1} {index2}")
        return diff_dim

    @staticmethod
    def _force_stride(var) -> OpVar:
        # force stride value of -1 to 1
        if isinstance(var.index, AryIndex):
            replaced = False
            index_new = []
            for idx in var.index:
                if isinstance(idx, OpRange) and VarList._get_int(idx[2]) == -1:
                    index_new.append(OpRange([idx[1],idx[0]]))
                    replaced = True
                else:
                    index_new.append(idx)
            if replaced:
                return OpVar(var.name, index=index_new, is_real=var.is_real)
        return var

    def push(self, var: OpVar, not_reorganize: bool = False) -> None:
        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")

        name = var.name

        if not name in self.vars:
            self.vars[name] = [var.index]
            return

        var = self._force_stride(var)

        found = False
        for index in self.vars[name]:
            if index is None or index == var.index:
                return
            if var.index is None:
                self.vars[name] = [None]
                return

            i = self._dim_for_comparison(index, var.index) # index and var.index is either OpInt or OpRange
            if i < 0:
                continue

            dim1 = index[i]
            dim2 = var.index[i]

            if dim1 is None or dim2 is None:
                index[i] = None # replaced
                found = True
                break

            v1 = self._get_int(dim1)
            v2 = self._get_int(dim2)
            if v1 is not None and v2 is not None:
                if abs(v1 - v2) == 1:
                    index[i] = OpRange(sorted([v1, v2])) # replaced
                    found = True
                    break
                continue

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
                j0 = self._get_int(dim2[0])
                j1 = self._get_int(dim2[1])
                if i1 < j0 or j1 < i0:
                    continue
                index[i] = OpRange([min(i0, j0), max(i1, j1)]) #  replaced
                found = True
                break

            if isinstance(dim1, OpRange):
                range = dim1
                v = v2
            else:
                range = dim2
                v = v1
            i0 = self._get_int(range[0])
            i1 = self._get_int(range[1])

            if v == i0 - 1:
                i0 -= 1
            if v == i1 + 1:
                i1 += 1
            if i0 <= v and v <= i1:
                index[i] = OpRange([i0, i1]) # replace
                found = True
                break

        if found:
            if not not_reorganize:
                self._reorganize(name)
        else:
            self.vars[name].append(var.index)

    def remove(self, var) -> None:
        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")

        name = var.name

        if not name in self.vars:
            raise ValueError(f"Not found: {name}")

        if var.index is None:
            del self.vars[name]
            return

        var = self._force_stride(var)

        for index in list(self.vars[name]):
            if index is None:
                continue

            if index == var.index:
                self.vars[name].remove(index)
                continue

            i = self._dim_for_comparison(index, var.index) # index and var.index is either OpInt or OpRange
            if i < 0:
                continue

            dim1 = index[i]
            dim2 = var.index[i]

            if dim2 is None:
                self.vars[name].remove(index)
                continue

            if dim1 is None:
                continue

            v1 = self._get_int(dim1)
            v2 = self._get_int(dim2)
            if v1 is not None and v2 is not None:
                continue

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
                j0 = self._get_int(dim2[0])
                j1 = self._get_int(dim2[1])
                if i1 < j0 or j1 < i0: # no overlap
                    continue
                if j0 <= i0 and i1 <= j1: # dim2 covers entire dim1
                    self.vars[name].remove(index)
                    continue
                if i0 < j0 and j1 < i1: # dim2 is sub of dim1
                    if i0 == j0-1:
                        index[i] = OpInt(i0) # replaced
                    else:
                        index[i] = OpRange([i0, j0-1]) # replaced
                    if j1+1 == i1:
                        index_new = index.copy()
                        index_new[i] = OpInt(i1)
                        self.vars[name].append(index_new) # added
                    else:
                        index_new = index.copy()
                        index_new[i] = OpRange([j1+1, i1])
                        self.vars[name].append(index_new) # added
                    continue
                if j0 <= i0 and j1 < i1:
                    if j1 + 1 == i1:
                        index[i] = OpInt(i1) # replaced
                    else:
                        index[i] = OpRange([j1+1, i1]) # replaced
                    continue
                if i0 < j0 and i1 <= j1:
                    if i0 == j0 - 1:
                        index[i] = OpInt(i0) # replaced
                    else:
                        index[i] = OpRange([i0, j0-1]) # replaced
                    continue
                continue

            if isinstance(dim1, OpRange): # dim2 is OpInt
                v = v2
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
                if v < i0 or i1 < v:
                    continue
                if v == i0:
                    if v + 1 == i1:
                        index[i] = OpInt(i1) # replaced
                    else:
                        index[i] = OpRange([v+1, i1]) # replaced
                    continue
                if v == i1:
                    if i0 == v - 1:
                        index[i] = OpInt(i0) # replaced
                    else:
                        index[i] = OpRange([i0, v-1]) # replaced
                    continue
                if i0  == v - 1:
                    index[i] = OpInt(i0) # replaced
                else:
                    index[i] = OpRange([i0, v-1]) # replaced
                if v + 1 == i1:
                    index_new = index.copy()
                    index_new[i] = OpInt(i1)
                    self.vars[name].append(index_new) # added
                else:
                    index_new = index.copy()
                    index_new[i] = OpRange([v+1, i1])
                    self.vars[name].append(index_new) # added
                continue

            if isinstance(dim2, OpRange): # dim1 is OpInt
                v = v1
                i0 = self._get_int(dim2[0])
                i1 = self._get_int(dim2[1])
                if v < i0 or i1 < v:
                    continue
                self.vars[name].remove(index)

        self._reorganize(name)

    def _reorganize(self, name) -> None:
        if not name in self.vars:
            raise ValueError(f"Not found: {name}")

        if len(self.vars[name])==1:
            return

        index_list = self.vars[name]
        self.vars[name] = []
        for index in index_list:
            self.push(OpVar(name, index=index), not_reorganize=True)
