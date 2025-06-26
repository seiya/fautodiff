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
            if index <= item.index or index >= item.index:
                return True
        return False

    def __str__(self) -> str:
        ary = []
        for name, index_list in self.vars.items():
            for index in index_list:
                ary.append(str(OpVar(name, index=index)))
        return ", ".join(ary)

    def __len__(self) -> int:
        return len(self.vars)

    def __getitem__(self, key: str):
        return self.vars[key]

    def __iter__(self) -> iter:
        return iter(self.vars)

    def names(self) -> Tuple[str]:
        return list(self.vars.keys())

    def _dim_for_comparison(self, index1: AryIndex, index2: AryIndex) -> int:
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
            if isinstance(dim1, OpInt) and isinstance(dim2, OpInt):
                _check_found()
                diff_dim = i
                continue
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                return -1
            if (not isinstance(dim1, OpInt)) or not (isinstance(dim1[0], OpInt) and isinstance(dim1[1], OpInt) and (dim1[2] is None or (isinstance(dim1[2], OpInt) and dim1[2].val)==1)):
                return -1
            if (not isinstance(dim2, OpInt)) or not (isinstance(dim2[0], OpInt) and isinstance(dim2[1], OpInt) and (dim2[2] is None or (isinstance(dim2[2], OpInt) and dim2[2].val)==1)):
                return -1

            _check_found()
            diff_dim = i

        if diff_dim is None:
            raise RuntimeError(f"Unexpected error: {index1} {index2}")
        return diff_dim

    def push(self, var: OpVar, not_reorganize: bool = False) -> None:
        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")

        name = var.name

        if not name in self.vars:
            self.vars[name] = [var.index]
            return

        if isinstance(var.index, AryIndex):
            replaced = False
            index_new = []
            for idx in var.index:
                if isinstance(idx, OpRange) and isinstance(idx[2], OpInt) and idx[3].val == -1:
                    index_new.append(OpRange([idx[0],idx[1]]))
                    replaced = True
                else:
                    index_new.append(idx)
            if replaced:
                var = OpVar(var.name, index=index_new, is_real=var.is_real)

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

            if isinstance(dim1, OpInt) and isinstance(dim2, OpInt):
                if abs(dim1.val - dim2.val) == 1:
                    index[i] = OpRange(sorted([dim1.val, dim2.val])) # replaced
                    found = True
                    break

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                if dim1[1].val == dim2[0].val:
                    index[i] = OpRange([dim1[0].val, dim2[1].val]) #  replaced
                    found = True
                    break
                if dim2[1].val == dim1[0].val:
                    index[i] = OpRange([dim2[0].val, dim1[1].val]) #  replaced
                    found = True
                    break

            if isinstance(dim1, OpRange):
                range = dim1
                v = dim2.val
            else:
                range = dim2
                v = dim1.val
            i0 = range[0].val
            i1 = range[1].val

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

            if isinstance(dim1, OpInt) and isinstance(dim2, OpInt):
                continue

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                i0, i1 = dim1[0], dim1[1]
                j0, j1 = dim2[0], dim2[1]
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
                        self.vars[name].append(OpInt(i1)) # added
                    else:
                        self.vars[name].append(OpRange([j1+1, i1])) # added
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
                v = dim2.val
                i0 = dim1[0].val
                i1 = dim1[1].val
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
                    self.vars[name].append(OpInt(i1)) # added
                else:
                    self.vars[name].append(OpRange([v+1, i1])) # added
                continue

            if isinstance(dim2, OpRange): # dim1 is OpInt
                v = dim1.val
                i0 = dim2[0].val
                i1 = dim2[1].val
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
            self.push(name, not_reorganize=True)
