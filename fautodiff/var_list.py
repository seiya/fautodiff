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
                self.push(var, not_reorganize=True)

    def deep_clone(self) -> VarList:
        return copy.deepcopy(self)

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
        for name in self.names():
            for index in self.vars[name]:
                yield OpVar(name, index=index)

    def names(self) -> List[str]:
        return sorted(self.vars.keys())

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
                    return -999

            if dim1 == dim2:
                continue
            if dim1 is None or dim2 is None:
                _check_found()
                diff_dim = i
                continue
            v1 = VarList._get_int(dim1)
            v2 = VarList._get_int(dim2)
            if isinstance(v1, int) and isinstance(v2, int):
                _check_found()
                diff_dim = i
                continue
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                return -999
            if isinstance(dim1, OpRange):
                i0 = VarList._get_int(dim1[0])
                i1 = VarList._get_int(dim1[1])
                i2 = VarList._get_int(dim1[2])
                if not (     (dim1[0] is None or isinstance(i0, int))
                         and (dim1[1] is None or isinstance(i1, int))
                         and (dim1[2] is None or (isinstance(i2, int) and abs(i2)==1))):
                    diff_dim = -i-1
                    continue
            elif not isinstance(v1, int):
                diff_dim = -i-1
                continue
            if isinstance(dim2, OpRange):
                j0 = VarList._get_int(dim2[0])
                j1 = VarList._get_int(dim2[1])
                j2 = VarList._get_int(dim2[2])
                if not (     (dim2[0] is None or isinstance(j0, int))
                         and (dim2[1] is None or isinstance(j1, int))
                         and (dim2[2] is None or (isinstance(j2, int) and abs(j2)==1))):
                    diff_dim = -i-1
                    continue
            elif not isinstance(v2, int):
                diff_dim = -i-1
                continue

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

    def merge(self, other) -> None:
        if not isinstance(other, VarList):
            raise ValueError("Must be VarList: {type(other)}")
        for var in other:
            self.push(var, not_reorganize=True)

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
                if (isinstance(i1, int) and isinstance(j0, int) and i1 < j0) or (isinstance(j1, int) and isinstance(j1, int) and j1 < i0): # no overlap
                    continue
                i0 = None if (i0 is None or j0 is None) else min(i0, j0)
                i1 = None if (i1 is None or j1 is None) else max(i1, j1)
                index[i] = OpRange([i0, j1]) #  replaced
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

            if i0 is not None and v == i0 - 1:
                i0 -= 1
            if i1 is not None and v == i1 + 1:
                i1 += 1
            if (i0 is None or i0 <= v) and (i1 is None or v <= i1):
                index[i] = OpRange([i0, i1]) # replace
                found = True
                break

        if found:
            if not not_reorganize:
                self._reorganize(name)
        else:
            self.vars[name].append(var.index) # added

    def remove(self, var) -> None:
        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")

        name = var.name

        if not name in self.vars:
            return

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
                if (isinstance(i1, int) and isinstance(j0, int) and i1 < j0) or (isinstance(j1, int) and isinstance(i0, int) and j1 < i0): # no overlap
                    continue
                if (j0 is None or (isinstance(i0, int) and j0 <= i0)) and (j1 is None or (isinstance(i1, int) and i1 <= j1)): # dim2 covers entire dim1
                    self.vars[name].remove(index)
                    continue
                if ((i0 is None and j0 is not None) or (isinstance(i0, int) and isinstance(j0, int) and i0 < j0)) and ((i1 is None and j1 is not None) or (isinstance(j1, int) and isinstance(i1, int) and j1 < i1)): # dim2 is sub of dim1
                    if i0 is not None and i0 == j0-1:
                        index[i] = OpInt(i0) # replaced
                    else:
                        index[i] = OpRange([i0, j0-1]) # replaced
                    if i1 is not None and j1+1 == i1:
                        index_new = index.copy()
                        index_new[i] = OpInt(i1)
                        self.vars[name].append(index_new) # added
                    else:
                        index_new = index.copy()
                        index_new[i] = OpRange([j1+1, i1])
                        self.vars[name].append(index_new) # added
                    continue
                if (j0 is None or (isinstance(i0, int) and j0 <= i0)) and ((i1 is None and j1 is not None) or (isinstance(j1, int) and isinstance(i1, int) and j1 < i1)):
                    if i1 is not None and j1 + 1 == i1:
                        index[i] = OpInt(i1) # replaced
                    else:
                        index[i] = OpRange([j1+1, i1]) # replaced
                    continue
                if ((i0 is None and j0 is not None) or (isinstance(i0, int) and isinstance(j0, int) and i0 < j0)) and (j1 is None or (isinstance(i1, int) and i1 <= j1)):
                    if i0 is not None and i0 == j0 - 1:
                        index[i] = OpInt(i0) # replaced
                    else:
                        index[i] = OpRange([i0, j0-1]) # replaced
                    continue
                continue

            if isinstance(dim1, OpRange): # dim2 is OpInt
                v = v2
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
                if (i0 is not None and v < i0) or (i1 is not None and i1 < v): # outside
                    continue
                if i0 is not None and v == i0:
                    if i1 is not None and v + 1 == i1:
                        index[i] = OpInt(i1) # replaced
                    else:
                        index[i] = OpRange([v+1, i1]) # replaced
                    continue
                if i1 is not None and v == i1:
                    if i0 is not None and i0 == v - 1:
                        index[i] = OpInt(i0) # replaced
                    else:
                        index[i] = OpRange([i0, v-1]) # replaced
                    continue
                if i0 is not None and i0  == v - 1:
                    index[i] = OpInt(i0) # replaced
                else:
                    index[i] = OpRange([i0, v-1]) # replaced
                if i1 is not None and v + 1 == i1:
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
                if (i0 is not None and v < i0) or (i0 is not None and i1 < v): # outsize
                    continue
                self.vars[name].remove(index)

        self._reorganize(name)

    def __and__(self, other) -> VarList:
        if not isinstance(other, VarList):
            raise ValueError(f"Must be VarList: {type(other)}")

        var_list = VarList()

        for name in other.names():
            if not name in self.vars:
                continue

            index_list = []
            for index2 in other[name]:

                for index1 in self.vars[name]:

                    if index1 == index2:
                        index_list.append(index1.copy())
                        continue
                    if index1 is None:
                        index_list.append(index2.copy())
                        continue
                    if index2 is None:
                        index_list.append(index1.copy())
                        continue

                    i = self._dim_for_comparison(index1, index2) # index1 and index2 is either OpInt or OpRange
                    if i == -999:
                        continue

                    index = index1.copy()

                    if i < 0:
                        i = -i-1
                        dim1 = index1[i]
                        dim2 = index2[i]
                        if isinstance(dim1, OpVar):
                            index[i] = dim1
                            index_list.append(index)
                            continue
                        if isinstance(dim2, OpVar):
                            index[i] = dim2
                            index_list.append(index)
                            continue
                        raise RuntimeError(f"Unexpected: {type(dim1)} {type(dim2)}")
                        continue

                    dim1 = index1[i]
                    dim2 = index2[i]
                    if dim1 is None:
                        index[i] = dim2
                        index_list.append(index)
                        continue
                    if dim2 is None:
                        index[i] = dim1
                        index_list.append(index)
                        continue

                    v1 = self._get_int(dim1)
                    v2 = self._get_int(dim2)
                    if v1 is not None and v2 is not None: # int
                        # different
                        continue

                    if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                        i0 = self._get_int(dim1[0])
                        i1 = self._get_int(dim1[1])
                        j0 = self._get_int(dim2[0])
                        j1 = self._get_int(dim2[1])
                        if (isinstance(i1, int) and isinstance(j0, int) and i1 < j0) or (isinstance(j1, int) and isinstance(j1, int) and j1 < i0): # no overlap
                            continue
                        if i0 is None:
                            i0 = j0
                        elif j0 is not None:
                            i0 = max(i0, j0)
                        if i1 is None:
                            i1 = j1
                        elif j1 is not None:
                            i1 = min(i1, j1)
                        index[i] = OpRange([i0, i1])
                        index_list.append(index)
                        continue

                    if isinstance(dim1, OpRange):
                        range = dim1
                        v = v2
                    else:
                        range = dim2
                        v = v1
                    i0 = self._get_int(range[0])
                    i1 = self._get_int(range[1])

                    if (i0 is not None and v < i0) or (i1 is not None and i1 < v): # outside
                        continue
                    index[i] = OpInt(v)
                    index_list.append(index)

            var_list.vars[name] = index_list
            var_list._reorganize(name)
        return var_list

    def _reorganize(self, name) -> None:
        if not name in self.vars:
            raise ValueError(f"Not found: {name}")

        if len(self.vars[name])==1:
            return

        index_list = self.vars[name]
        self.vars[name] = []
        for index in index_list:
            self.push(OpVar(name, index=index), not_reorganize=True)

    def update_index_upward(self, index_map: dict) -> None:
        for name in self.names():
            if name in index_map:
                do_index = index_map[name]
            else:
                continue
            for index in self.vars[name]:
                if index is None:
                    continue
                index[do_index] = None
            self._reorganize(name)

    def update_index_downward(self, index_map: dict, do_index_var: OpVar) -> None:
        for name in self.names():
            if name in index_map:
                do_index = index_map[name]
            else:
                continue
            for index in self.vars[name]:
                if index is None:
                    continue
                index[do_index] = do_index_var
            self._reorganize(name)

