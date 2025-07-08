"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar

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

    vars: Optional[dict[str, List]] = field(init=False, default=None)
    exclude: Optional[dict[str, List]] = field(init=False, default=None)

    def __init__(self, vars: Optional[List[OpVar]] = None):
        super().__init__()
        self.vars = {}
        self.exclude = {}
        if vars is not None:
            for var in vars:
                self.push(var, not_reorganize=True)

    def add_exclude(self, var: OpVar):
        name = var.name
        if not name in self.exclude:
            self.exclude[name] = []
        if not var.index in self.exclude[name]:
            self.exclude[name].append(var.index)

    def copy(self) -> VarList:
        var_list = VarList()
        for name in self.names():
            list = []
            for index in self.vars[name]:
                list.append(index)
            var_list.vars[name] = list
        for name in self.exclude:
            list = []
            for index in self.exclude[name]:
                list.append(index)
            var_list.exclude[name] = list
        return var_list

    def __contains__(self, item: OpVar):
        if not isinstance(item, OpVar):
            raise ValueError(f"Must be OpVar: {type(item)}")
        name = item.name
        if not name in self.vars:
            return False
        if name in self.exclude and item.index in self.exclude[name]:
            return False
        index_list = self.vars[name]
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
        res = ", ".join(ary)
        if self.exclude:
            ary = []
            for name, index_list in sorted(self.exclude.items()):
                for index in index_list:
                    ary.append(str(OpVar(name, index=index)))
            if len(ary) > 0:
                res = f"{res}, exclude: {', '.join(ary)}"
        return res

    def __len__(self) -> int:
        return len(self.vars)

    def __getitem__(self, key: str):
        return self.vars[key]

    def __iter__(self) -> Iterator[OpVar]:
        for name in self.names():
            for index in self.vars[name]:
                yield OpVar(name, index=index)

    def names(self) -> List[str]:
        names = []
        for name in sorted(self.vars.keys()):
            if len(self.vars[name]) > 0:
                names.append(name)
        return names

    @staticmethod
    def _get_int(op) -> Optional[int]:
        if isinstance(op, OpInt):
            return op.val
        if isinstance(op, OpNeg) and isinstance(op.args[0], OpInt):
            return - op.args[0].val
        return None

    def merge(self, other) -> None:
        if not isinstance(other, VarList):
            raise ValueError("Must be VarList: {type(other)}")
        for var in other:
            self.push(var, not_reorganize=True)
        for name in other.exclude:
            if not name in self.exclude:
                self.exclude[name] = []
            for index in other.exclude[name]:
                self.exclude[name].append(index)
        self._reorganize

    @staticmethod
    def _force_stride_one(var) -> OpVar:
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
                return OpVar(
                    var.name,
                    index=AryIndex(index_new),
                    kind=var.kind,
                    typename=var.typename,
                    ad_target=var.ad_target,
                    is_constant=var.is_constant,
                )
        return var

    def push(self, var: OpVar, not_reorganize: bool = False) -> None:
        if not isinstance(var, OpVar):
            raise ValueError(f"Must be OpVar: {type(var)}")

        name = var.name

        if name in self.exclude:
            for index in self.exclude[name]:
                if var.index >= index:
                    self.exclude[name].remove(index)

        if not name in self.vars:
            self.vars[name] = [var.index]
            return

        var = self._force_stride_one(var)

        found = False
        for pos, index in enumerate(self.vars[name]):
            if index is None or index == var.index:
                return
            if var.index is None:
                self.vars[name] = [None]
                return

            i = AryIndex.get_diff_dim(index, var.index)
            if i < 0:
                continue

            index = index.copy()
            self.vars[name][pos] = index

            dim1 = index[i]
            dim2 = var.index[i]

            if AryIndex.dim_is_entire(dim1) or AryIndex.dim_is_entire(dim2):
                index[i] = None # replaced
                found = True
                break

            if isinstance(dim1, OpRange):
                if not(dim1[2] is None or self._get_int(dim1[2]) == 1): # stride must be 1
                    continue
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
            if isinstance(dim2, OpRange):
                if not(dim2[2] is None or self._get_int(dim2[2]) == 1): # stride must be 1
                    continue
                j0 = self._get_int(dim2[0])
                j1 = self._get_int(dim2[1])

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange): # both are range
                if i0 is None or i1 is None or j0 is None or j1 is None: # either is non-integer
                    continue
                if (isinstance(i1, int) and isinstance(j0, int) and i1 < j0) or (isinstance(j1, int) and isinstance(j1, int) and j1 < i0): # no overlap
                    continue
                i0 = None if (i0 is None or j0 is None) else min(i0, j0)
                i1 = None if (i1 is None or j1 is None) else max(i1, j1)
                index[i] = OpRange([i0, j1]) #  replaced
                found = True
                break

            v1 = self._get_int(dim1)
            v2 = self._get_int(dim2)

            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)): # neither is range
                if v1 is None or v2 is None: # either is non-integer
                    continue
                if abs(v1 - v2) == 1: # merge
                    index[i] = OpRange(sorted([v1, v2])) # replaced
                    found = True
                    break
                continue

            # one is range and the other is not

            if isinstance(dim1, OpRange):
                range = dim1
                v = v2
            else:
                range = dim2
                v = v1
            i0 = self._get_int(range[0])
            i1 = self._get_int(range[1])

            if i0 is None or i1 is None or v is None: # either is not integer
                continue

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

        var = self._force_stride_one(var)

        for pos, index in enumerate(self.vars[name]):

            if index == var.index:
                self.vars[name].remove(index)
                continue

            if index is None:
                self.add_exclude(var)
                continue

            i = AryIndex.get_diff_dim(index, var.index) # index and var.index is either OpInt or OpRange
            if i is None:
                raise RuntimeError(f"Unexpected: {var} {self}")
 
            index = index.copy()
            self.vars[name][pos] = index

            dim1 = index[i]
            dim2 = var.index[i] if var.index is not None else None

            if AryIndex.dim_is_entire(dim1):
                self.add_exclude(var)
                continue

            if AryIndex.dim_is_entire(dim2):
                self.vars[name].remove(index) # remove all
                continue

            if isinstance(dim1, OpRange):
                if not(dim1[2] is None or self._get_int(dim1[2]) == 1): # stride must be 1
                    self.add_exclude(var)
                    continue
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
            if isinstance(dim2, OpRange):
                if not(dim2[2] is None or self._get_int(dim2[2]) == 1): # stride must be 1
                    continue
                j0 = self._get_int(dim2[0])
                j1 = self._get_int(dim2[1])

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange): # both are range
                if i0 is None or i1 is None or j0 is None or j1 is None: # either is non-integer
                    continue # do nothing (overlap cannot be judged)

                if (isinstance(i1, int) and isinstance(j0, int) and i1 < j0) or (isinstance(j1, int) and isinstance(j1, int) and j1 < i0): # no overlap
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
                raise RuntimeError(f"Unexpected: {dim1} {dim2}")
                continue

            v1 = self._get_int(dim1)
            v2 = self._get_int(dim2)

            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)): # neither is range
                # do nothing
                continue

            # one is range and the other is not

            if isinstance(dim1, OpRange):
                if v2 is None:
                    self.add_exclude(var)
                    continue

                if (i0 is not None and v2 < i0) or (i1 is not None and i1 < v2): # outside
                    continue
                if (i0 is not None and i0+1 < v2) or (i1 is not None and v2 < i1-1): # inside
                    self.add_exclude(var)
                    continue
                if i0 is not None and v2 == i0:
                    if i1 is not None and v2 + 1 == i1:
                        index[i] = OpInt(i1) # replaced
                    else:
                        index[i] = OpRange([v2+1, i1]) # replaced
                    continue
                if i1 is not None and v2 == i1:
                    if i0 is not None and i0 == v2 - 1:
                        index[i] = OpInt(i0) # replaced
                    else:
                        index[i] = OpRange([i0, v2-1]) # replaced
                    continue
                if i0 is not None and i0  == v2 - 1:
                    index[i] = OpInt(i0) # replaced
                else:
                    index[i] = OpRange([i0, v2-1]) # replaced
                if i1 is not None and v2 + 1 == i1:
                    index_new = index.copy()
                    index_new[i] = OpInt(i1)
                    self.vars[name].append(index_new) # added
                else:
                    index_new = index.copy()
                    index_new[i] = OpRange([v2+1, i1])
                    self.vars[name].append(index_new) # added
                continue

            if isinstance(dim2, OpRange):
                if isinstance(v1, int):
                    if (j0 is not None and v1 < j0) or (j1 is not None and j1 < v1): # outsize
                        continue
                self.vars[name].remove(index)

        self._reorganize(name)
        if not self[name]:
            del self.vars[name]


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
                        index_list.append(None if index1 is None else index1.copy())
                        continue
                    if index1 is None:
                        index_list.append(index2.copy())
                        continue
                    if index2 is None:
                        index_list.append(index1.copy())
                        continue

                    i = AryIndex.get_diff_dim(index1, index2)
                    if i < 0:
                        continue

                    index = index1.copy()

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

                    if isinstance(dim1, OpRange):
                        i0 = self._get_int(dim1[0])
                        i1 = self._get_int(dim1[1])
                        i2 = self._get_int(dim1[2])
                        if not(isinstance(i2, int) and abs(i2) == 1):
                            continue # assumes no overlap
                        if not(isinstance(dim2, OpInt) or isinstance(dim2, OpRange)):
                            index[i] = dim2
                            index_list.append(index)
                            continue
                    if isinstance(dim2, OpRange):
                        j0 = self._get_int(dim2[0])
                        j1 = self._get_int(dim2[1])
                        j2 = self._get_int(dim2[2])
                        if not(isinstance(j2, int) and abs(j2) == 1):
                            continue # assumes no overlap
                        if not(isinstance(dim1, OpInt) or isinstance(dim1, OpRange)):
                            index[i] = dim1
                            index_list.append(index)
                            continue

                    v1 = self._get_int(dim1)
                    v2 = self._get_int(dim2)
                    if not(isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                        # both are not range
                        if v1 is not None and v2 is not None: # both are int
                            continue # different
                        if v1 is None and v2 is None: # neither is int
                            continue # different
                        if v1 is None: # dim1 is int and dim2 is not int
                            index[i] = dim1
                            index_list.append(index)
                            continue
                        if v2 is None: # dim2 is int and dim1 is not int
                            index[i] = dim2
                            index_list.append(index)
                            continue

                    if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                        if not((dim1[0] is None or isinstance(i0, int)) and (dim1[1] is None or isinstance(i1, int)) and (dim2[0] is None or isinstance(j0, int)) and (dim2[1] is None or isinstance(j1, int))):
                                continue # assumue different
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
                        if not isinstance(dim2, OpRange):
                            raise RuntimeError(f"Unexpected: {type(dim2)}")
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
        if name in self.exclude:
            exclude = list(self.exclude[name])
        else:
            exclude = []
        for index in index_list:
            if index in exclude:
                exclude.remove(index)
            else:
                self.push(OpVar(name, index=index), not_reorganize=True)
        if exclude:
            self.exclude[name] = exclude
        elif name in self.exclude:
            del self.exclude[name]

    def update_index_upward(self, index_map: dict, range: OpRange) -> None:
        if range[2] is not None and isinstance(range[2], OpNeg):
            slice = - range[2]
            if isinstance(slice, OpInt) and slice.val == 1:
                slice = None
            range = OpRange([range[1], range[0], slice])
        for name in self.names():
            if name in index_map:
                do_index, _ = index_map[name]
            else:
                continue
            index_new = []
            for index in self.vars[name]:
                if index is not None and index[do_index] is not None and index[do_index] in range:
                    index = index.copy()
                    index[do_index] = range
                index_new.append(index)
            self.vars[name] = index_new
            self._reorganize(name)

    def update_index_downward(self, index_map: dict, do_index_var: OpVar) -> None:
        for name in self.names():
            if name in index_map:
                do_index, ndim = index_map[name]
            else:
                continue
            index_new = []
            for index in self.vars[name]:
                if index is None:
                    index = AryIndex([None] * ndim)
                else:
                    index = index.copy()
                index[do_index] = do_index_var
                index_new.append(index)
            self.vars[name] = index_new
            self._reorganize(name)
