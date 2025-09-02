"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterator, List, Optional

from .operators import AryIndex, OpInt, OpNeg, OpRange, OpVar


@dataclass
class VarList:
    """Maintain a set of variables and their index information.

    The class tracks for each variable name the used index patterns and
    dimensions.  It also records sub-indices that should be excluded from
    operations.  This information is used when generating derivative code to
    understand which elements of an array are accessed.
    """

    vars: dict[str, List[AryIndex]] = field(default_factory=dict)
    dims: dict[str, List[int]] = field(default_factory=dict)
    exclude: dict[str, List[AryIndex]] = field(default_factory=dict)

    def __init__(self, vars: Optional[List[OpVar]] = None):
        """Initialise the container.

        Parameters
        ----------
        vars:
            Optional initial list of variables to populate the set with.
        """

        super().__init__()
        self.vars = {}
        self.dims = {}
        self.exclude = {}

        # Add provided variables while skipping expensive reorganisations.
        if vars is not None:
            for var in vars:
                self.push(var, not_reorganize=True)

    def copy(self) -> VarList:
        """Return a deep copy of the variable list."""

        var_list = VarList()

        # Duplicate index information for each variable name.
        for name in self.names():
            var_list.vars[name] = list(self.vars[name])

        # Copy recorded dimension and exclusion information.
        for name, dims in self.dims.items():
            var_list.dims[name] = list(dims)
        for name, indices in self.exclude.items():
            var_list.exclude[name] = list(indices)
        return var_list

    def __contains__(self, item: OpVar) -> bool:
        """Return ``True`` if ``item`` is in the list and not excluded."""

        if not isinstance(item, OpVar):
            raise ValueError(f"Must be OpVar: {type(item)}")

        name = item.name_ext()
        index_item = item.concat_index()

        # Quick checks: variable present and not explicitly excluded.
        if name not in self.vars:
            return False
        if name in self.exclude and index_item in self.exclude[name]:
            return False

        # Compare against recorded index patterns.
        index_list = self.vars[name]
        for index in index_list:
            if index is None:
                return True
            if index <= index_item or index >= index_item:
                return True
        return False

    def __str__(self) -> str:
        """Human readable representation used for debugging.

        Render full-array coverage explicitly as ':' for each dimension so
        that strings like 'v(:,:)' appear even when the internal representation
        uses ``None`` for entire coverage.
        """

        def _format(name: str, index: Optional[AryIndex]) -> str:
            # For explicit indices, reconstruct nested variable for accurate printing
            if index is not None:
                var = self._get_var(name, index, self.dims.get(name, [0]))
                return str(var)
            # For full coverage (None), render ':' per dimension at each nesting level
            dims_counts = list(self.dims.get(name, [0]))
            parts = name.split("%")
            out = []
            for i, part in enumerate(parts):
                out.append(part)
                if i < len(dims_counts):
                    cnt = dims_counts[i]
                    if cnt and cnt > 0:
                        out.append("(" + ",".join([":"] * cnt) + ")")
                if i < len(parts) - 1:
                    out.append("%")
            return "".join(out)

        parts: List[str] = []
        for name in self.names():
            for index in self.vars[name]:
                parts.append(_format(name, index))
        res = ", ".join(parts)

        excl: List[str] = []
        for name in sorted(self.exclude.keys()):
            for index in self.exclude[name]:
                excl.append(_format(name, index))
        if excl:
            res = f"{res}, exclude: {', '.join(excl)}"
        return res

    def __len__(self) -> int:
        """Return number of variable names tracked."""

        return len(self.vars)

    def __getitem__(self, key: str):
        """Return index list for ``key``."""

        return self.vars[key]

    def _get_var(self, name: str, index: Optional[AryIndex], dims: List[int]) -> OpVar:
        """Construct an ``OpVar`` from stored data.

        Parameters
        ----------
        name, index, dims:
            Information retrieved from the internal dictionaries.

        Returns
        -------
        OpVar
            A variable object representing ``name`` with nested components and
            index data restored.
        """

        # Support for derived-type components separated by ``%``.
        pos = name.rfind("%")
        if pos >= 0:
            name_ref = name[:pos]
            index_ref = index[: -dims[-1]] if index is not None else None
            var_ref = self._get_var(name_ref, index_ref, dims[:-1])
            name = name[pos + 1 :]
            index = index[-dims[-1] :] if index is not None else None
        else:
            var_ref = None

        # Normalise dimensions: non-zero entries become ':' slices.
        if len(dims) > 0 and dims[-1] > 0:
            dims = tuple([":"] * dims[-1])
        else:
            dims = tuple()

        var = OpVar(name=name, index=index, dims=dims, ref_var=var_ref)
        return var

    def __iter__(self) -> Iterator[OpVar]:
        """Iterate over stored variables as ``OpVar`` objects."""

        for name in self.names():
            dims = self.dims[name]
            for index in self.vars[name]:
                yield self._get_var(name, index, dims)

    def iter_exclude(self) -> Iterator[OpVar]:
        """Iterate over indices that are explicitly excluded."""

        for name in sorted(self.exclude.keys()):
            dims = self.dims[name]
            for index in self.exclude[name]:
                yield self._get_var(name, index, dims)

    def names(self) -> List[str]:
        """Return a sorted list of variable names currently stored."""

        names = []
        for name in sorted(self.vars.keys()):
            if len(self.vars[name]) > 0:
                names.append(name)
        return names

    @staticmethod
    def _get_int(op) -> Optional[int]:
        """Return integer value of ``op`` if it represents a literal."""

        if isinstance(op, OpInt):
            return op.val
        if isinstance(op, OpNeg) and isinstance(op.args[0], OpInt):
            return -op.args[0].val
        return None

    def _update_dims(self, name: str, ndims: List[int]):
        """Update stored dimension information for ``name``."""

        if name not in self.dims:
            self.dims[name] = ndims
        else:
            ndims_self = self.dims[name]
            if len(ndims_self) != len(ndims):
                raise ValueError(
                    f"Different number of dimensions: {name} {ndims_self} {ndims}"
                )
            for i in range(len(ndims_self)):
                if ndims_self[i] != ndims[i]:
                    if ndims_self[i] > 0 and ndims[i] > 0:
                        raise ValueError(
                            f"Different number of dimensions: {name} {i} {ndims_self} {ndims}"
                        )
                    if ndims_self[i] == 0:
                        self.dims[name][i] = ndims[i]

    def merge(self, other: VarList) -> None:
        """Merge variables from ``other`` into this list."""

        if not isinstance(other, VarList):
            raise ValueError(f"Must be VarList: {type(other)}")

        processed: set[str] = set()

        # Copy variables and record which names were touched.
        for var in other:
            self.push(var, not_reorganize=True)
            processed.add(var.name_ext())

        # Merge excluded indices as well.
        for name in other.exclude:
            if name not in self.exclude:
                self.exclude[name] = []
            for index in other.exclude[name]:
                if index not in self.exclude[name]:
                    self.exclude[name].append(index)
            if len(self.exclude[name]) > 100:
                raise RuntimeError(name)
            processed.add(name)

        # Reorganise merged names and update dimension information.
        for name in processed:
            if name in self.vars:
                self._reorganize(name)
            self._update_dims(name, other.dims[name])

    @staticmethod
    def _force_stride_one(var) -> OpVar:
        """Return a copy of ``var`` with negative strides normalised."""

        # force stride value of -1 to 1
        index = var.index
        replaced = False
        if isinstance(var.index, AryIndex):
            index_new = []
            for idx in var.index:
                if isinstance(idx, OpRange) and VarList._get_int(idx[2]) == -1:
                    index_new.append(OpRange([idx[1], idx[0]]))
                    replaced = True
                else:
                    index_new.append(idx)
            if replaced:
                index = AryIndex(index_new)

        # Recursively normalise any referenced variable.
        ref_var = var.ref_var
        if ref_var is not None:
            ref_var = VarList._force_stride_one(ref_var)

            if replaced or ref_var is not var.ref_var:
                return OpVar(
                    var.name,
                    index=index,
                    var_type=var.var_type.copy() if var.var_type else None,
                    ad_target=var.ad_target,
                    is_constant=var.is_constant,
                    ref_var=ref_var,
                )
        return var

    def push(self, var: OpVar, not_reorganize: bool = False) -> None:
        """Add ``var`` to the list, merging overlapping indices."""

        if not isinstance(var, OpVar):
            raise ValueError(f"Must be OpVar: {type(var)}")

        name = var.name_ext()
        var_index = var.concat_index()

        # Record dimensionality for new variables or update existing info.
        if name not in self.vars:
            self.dims[name] = var.ndims_ext()
        else:
            self._update_dims(name, var.ndims_ext())

        # If ``var`` references the entire array (all dimensions are ``:``)
        # then record a single ``None`` entry representing the full variable.
        # Using ``None`` ensures fast membership checks (treat as always-covered)
        # and avoids conservative failures when bounds include symbolic values.
        if isinstance(var_index, AryIndex) and all(
            AryIndex.dim_is_entire(dim) for dim in var_index
        ):
            self.vars[name] = [None]
            if name in self.exclude:
                del self.exclude[name]
            return

        # Remove exclusions that are now explicitly included.
        if name in self.exclude:
            for index in list(self.exclude[name]):
                if var_index >= index:
                    self.exclude[name].remove(index)

        # First occurrence just records the index as-is.
        if name not in self.vars:
            self.vars[name] = [var_index]
            return

        var = self._force_stride_one(var)

        found = False
        for pos, index in enumerate(self.vars[name]):
            if index is None or index == var_index:
                return
            if var_index is None:
                self.vars[name] = [None]
                return

            i = AryIndex.get_diff_dim(index, var_index)
            if i < 0:
                continue

            index = index.copy()
            self.vars[name][pos] = index

            dim1 = index[i]
            dim2 = var_index[i]

            # Entire dimension covered by either index -> store as entire.
            if AryIndex.dim_is_entire(dim1) or AryIndex.dim_is_entire(dim2):
                index[i] = None  # replaced
                found = True
                break

            if isinstance(dim1, OpRange):
                if not (dim1[2] is None or self._get_int(dim1[2]) == 1):
                    continue  # stride must be 1
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
            if isinstance(dim2, OpRange):
                if not (dim2[2] is None or self._get_int(dim2[2]) == 1):
                    continue  # stride must be 1
                j0 = self._get_int(dim2[0])
                j1 = self._get_int(dim2[1])

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                # both are range
                if (
                    i0 is None or i1 is None or j0 is None or j1 is None
                ):  # either is non-integer
                    continue
                if (isinstance(i1, int) and isinstance(j0, int) and i1 < j0) or (
                    isinstance(j1, int) and isinstance(i0, int) and j1 < i0
                ):
                    continue  # no overlap
                i0 = None if (i0 is None or j0 is None) else min(i0, j0)
                i1 = None if (i1 is None or j1 is None) else max(i1, j1)
                index[i] = OpRange([i0, j1])  # replaced
                found = True
                break

            v1 = self._get_int(dim1)
            v2 = self._get_int(dim2)

            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                # neither is range
                if v1 is None or v2 is None:  # either is non-integer
                    continue
                if abs(v1 - v2) == 1:  # merge
                    index[i] = OpRange(sorted([v1, v2]))  # replaced
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

            if i0 is None or i1 is None or v is None:  # either is not integer
                continue

            if i0 is not None and v == i0 - 1:
                i0 -= 1
            if i1 is not None and v == i1 + 1:
                i1 += 1
            if (i0 is None or i0 <= v) and (i1 is None or v <= i1):
                index[i] = OpRange([i0, i1])  # replace
                found = True
                break

        if found:
            if not not_reorganize:
                self._reorganize(name)
        else:
            self.vars[name].append(var_index)  # added

    def remove(self, var) -> None:
        """Remove ``var`` from the list, splitting ranges as needed."""

        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")

        name = var.name_ext()
        var_index = var.concat_index()

        if name not in self.vars:
            return

        if var_index is None:
            del self.vars[name]
            return

        var = self._force_stride_one(var)

        for pos, index in enumerate(self.vars[name]):
            if index == var_index:
                self.vars[name].remove(index)
                continue
            if index is None:
                self.add_exclude(var)
                continue

            i = AryIndex.get_diff_dim(index, var_index)
            if i < 0:
                continue

            index = index.copy()
            self.vars[name][pos] = index

            dim1 = index[i]
            dim2 = var_index[i] if var_index is not None else None

            if AryIndex.dim_is_entire(dim1):
                self.add_exclude(var)
                continue

            if AryIndex.dim_is_entire(dim2):
                self.vars[name].remove(index)  # remove all
                continue

            if isinstance(dim1, OpRange):
                if not (dim1[2] is None or self._get_int(dim1[2]) == 1):
                    self.add_exclude(var)
                    continue
                i0 = self._get_int(dim1[0])
                i1 = self._get_int(dim1[1])
            if isinstance(dim2, OpRange):
                if not (dim2[2] is None or self._get_int(dim2[2]) == 1):
                    continue
                j0 = self._get_int(dim2[0])
                j1 = self._get_int(dim2[1])

            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                if (
                    (dim1[0] is not None and i0 is None)
                    or (dim1[1] is not None and i1 is None)
                    or (dim2[0] is not None and j0 is None)
                    or (dim2[1] is not None and j1 is None)
                ):
                    continue  # either is variable, cannot judge overlap
                if (i1 is not None and j0 is not None and i1 < j0) or (
                    j1 is not None and i0 is not None and j1 < i0
                ):
                    continue  # no overlap
                if (j0 is None or (i0 is not None and j0 <= i0)) and (
                    j1 is None or (i1 is not None and i1 <= j1)
                ):
                    self.vars[name].remove(index)
                    continue
                if (
                    j0 is not None
                    and j1 is not None
                    and (i0 is None or (i0 is not None and i0 < j0))
                    and (i1 is None or (i1 is not None and j1 < i1))
                ):
                    if i0 is not None and i0 == j0 - 1:
                        index[i] = OpInt(i0)  # replaced
                    else:
                        index[i] = OpRange([dim1[0], j0 - 1])  # replaced
                    if i1 is not None and j1 + 1 == i1:
                        index_new = index.copy()
                        index_new[i] = OpInt(i1)
                        self.vars[name].append(index_new)  # added
                    else:
                        index_new = index.copy()
                        index_new[i] = OpRange([j1 + 1, dim1[1]])
                        self.vars[name].append(index_new)  # added
                    continue
                if (j0 is None or (i0 is not None and j0 <= i0)) and (
                    j1 is not None and ((i1 is None) or (i1 is not None and j1 < i1))
                ):
                    if i1 is not None and j1 + 1 == i1:
                        index[i] = OpInt(i1)  # replaced
                    else:
                        index[i] = OpRange([j1 + 1, dim1[1]])  # replaced
                    continue
                if (
                    j0 is not None
                    and ((i0 is None) or (i0 is not None and i0 < j0))
                    and (j1 is None or (i1 is not None and i1 <= j1))
                ):
                    if i0 is not None and i0 == j0 - 1:
                        index[i] = OpInt(i0)  # replaced
                    else:
                        index[i] = OpRange([dim1[0], j0 - 1])  # replaced
                    continue
                raise RuntimeError(f"Unexpected: {dim1} {dim2}")
                continue

            v1 = self._get_int(dim1)
            v2 = self._get_int(dim2)

            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                continue  # neither is range

            # one is range and the other is not

            if isinstance(dim1, OpRange):
                if v2 is None:
                    self.add_exclude(var)
                    continue

                if (i0 is not None and v2 < i0) or (i1 is not None and i1 < v2):
                    continue  # outside
                if (i0 is not None and i0 + 1 < v2) or (i1 is not None and v2 < i1 - 1):
                    self.add_exclude(var)
                    continue
                if i0 is not None and v2 == i0:
                    if i1 is not None and v2 + 1 == i1:
                        index[i] = OpInt(i1)  # replaced
                    else:
                        index[i] = OpRange([v2 + 1, dim1[1]])  # replaced
                    continue
                if i1 is not None and v2 == i1:
                    if i0 is not None and i0 == v2 - 1:
                        index[i] = OpInt(i0)  # replaced
                    else:
                        index[i] = OpRange([dim1[0], v2 - 1])  # replaced
                    continue
                if i0 is not None and i0 == v2 - 1:
                    index[i] = OpInt(i0)  # replaced
                else:
                    index[i] = OpRange([dim1[0], v2 - 1])  # replaced
                if i1 is not None and v2 + 1 == i1:
                    index_new = index.copy()
                    index_new[i] = OpInt(i1)
                    self.vars[name].append(index_new)  # added
                else:
                    index_new = index.copy()
                    index_new[i] = OpRange([v2 + 1, dim1[1]])
                    self.vars[name].append(index_new)  # added
                continue

            if isinstance(dim2, OpRange):
                if isinstance(v1, int):
                    if (j0 is not None and v1 < j0) or (j1 is not None and j1 < v1):
                        continue  # outside
                self.vars[name].remove(index)

        self._reorganize(name)
        if not self[name]:
            del self.vars[name]

    def add_exclude(self, var: OpVar):
        """Mark ``var`` as excluded from the list."""

        name = var.name_ext()
        var_index = var.concat_index()
        self._update_dims(name, var.ndims_ext())
        if name not in self.exclude:
            self.exclude[name] = []
        if var_index not in self.exclude[name]:
            self.exclude[name].append(var_index)

    def __and__(self, other: VarList) -> VarList:
        """Return intersection of this list with ``other``."""

        if not isinstance(other, VarList):
            raise ValueError(f"Must be VarList: {type(other)}")

        var_list = VarList()

        for name in other.names():
            if not name in self.vars:
                continue

            var_list.dims[name] = self.dims[name]
            var_list._update_dims(name, other.dims[name])

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
                        if not (isinstance(i2, int) and abs(i2) == 1):
                            continue  # assumes no overlap
                        if not (isinstance(dim2, OpInt) or isinstance(dim2, OpRange)):
                            index[i] = dim2
                            index_list.append(index)
                            continue
                    if isinstance(dim2, OpRange):
                        j0 = self._get_int(dim2[0])
                        j1 = self._get_int(dim2[1])
                        j2 = self._get_int(dim2[2])
                        if not (isinstance(j2, int) and abs(j2) == 1):
                            continue  # assumes no overlap
                        if not (isinstance(dim1, OpInt) or isinstance(dim1, OpRange)):
                            index[i] = dim1
                            index_list.append(index)
                            continue

                    v1 = self._get_int(dim1)
                    v2 = self._get_int(dim2)
                    if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                        # both are not range
                        if v1 is not None and v2 is not None:  # both are int
                            continue  # different
                        if v1 is None and v2 is None:  # neither is int
                            continue  # different
                        if v1 is None:  # dim1 is int and dim2 is not int
                            index[i] = dim1
                            index_list.append(index)
                            continue
                        if v2 is None:  # dim2 is int and dim1 is not int
                            index[i] = dim2
                            index_list.append(index)
                            continue

                    if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                        if not (
                            (dim1[0] is None or isinstance(i0, int))
                            and (dim1[1] is None or isinstance(i1, int))
                            and (dim2[0] is None or isinstance(j0, int))
                            and (dim2[1] is None or isinstance(j1, int))
                        ):
                            continue  # assumue different
                        if (
                            isinstance(i1, int) and isinstance(j0, int) and i1 < j0
                        ) or (isinstance(j1, int) and isinstance(i0, int) and j1 < i0):
                            continue  # no overlap
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

                    if (i0 is not None and v < i0) or (
                        i1 is not None and i1 < v
                    ):  # outside
                        continue
                    index[i] = OpInt(v)
                    index_list.append(index)

            var_list.vars[name] = index_list
            var_list._reorganize(name)
        return var_list

    def _reorganize(self, name) -> None:
        """Normalise and merge indices for ``name`` after modifications."""

        if name not in self.vars:
            raise ValueError(f"Not found: {name}")

        if len(self.vars[name]) == 1:
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
                # Preserve full coverage without clearing excludes
                if index is None:
                    if None not in self.vars[name]:
                        self.vars[name].append(None)
                else:
                    var = self._get_var(name, index, self.dims[name])
                    self.push(var, not_reorganize=True)
        if exclude:
            self.exclude[name] = exclude
        elif name in self.exclude:
            del self.exclude[name]

    def update_index_upward(self, index_map: dict, range: OpRange) -> None:
        """Expand indices covered by ``range`` for variables in ``index_map``."""

        if range[2] is not None and isinstance(range[2], OpNeg):
            slice = -range[2]
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
                if (
                    index is not None
                    and index[do_index] is not None
                    and index[do_index] in range
                ):
                    index = index.copy()
                    index[do_index] = range
                index_new.append(index)
            self.vars[name] = index_new
            self._reorganize(name)

    def update_index_downward(self, index_map: dict, do_index_var: OpVar) -> None:
        """Replace index ``do_index`` with ``do_index_var`` for given names."""

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
