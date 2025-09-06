"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Iterator, List, Tuple, Optional, Union

from .operators import AryIndex, OpInt, OpNeg, OpRange, OpVar, Operator
from .var_dict import VarDict


@dataclass
class VarList:
    """Maintain a set of variables and their index information.

    The class tracks for each variable name the used index patterns and
    dimensions.  It also records sub-indices that should be excluded from
    operations.  This information is used when generating derivative code to
    understand which elements of an array are accessed.
    """

    vars: dict[str, List[AryIndex|None]] = field(default_factory=dict)
    dims: dict[str, List[int]] = field(default_factory=dict)
    exclude: dict[str, List[AryIndex]] = field(default_factory=dict)
    _context: List[Tuple[OpVar, List[OpVar], OpRange]] = field(default_factory=list)

    def __init__(self, vars: Optional[List[OpVar]] = None, context: Optional[List[Tuple[OpVar, List[OpVar], OpRange]]] = None):
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
        self._context = context if context is not None else []

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
        var_list._context = list(self._context)
        return var_list

    def copy_context(self) -> VarList:
        """Return a new variable list with the context copied from self"""
        return VarList(context = self._context)

    def push_context(self, context: Tuple[OpVar, List[OpVar], OpRange]) -> None:
        if not isinstance(context[0], OpVar):
            raise ValueError(f"Must be OpVar: {type(context[0])}")
        if not isinstance(context[1], list):
            raise ValueError(f"Must be list of OpVar: {type(context[1])}")
        if not isinstance(context[2], OpRange):
            raise ValueError(f"Must be OpRange: {type(context[2])}")
        range = context[2]
        if isinstance(range[2], OpNeg):
            range = OpRange([range[1], range[0], range[2].args[0]])
        if range[2] == OpInt(1):
            range = OpRange([range[0], range[1]])
        self._context.append((context[0], context[1], range))

    def pop_context(self) -> None:
        if len(self._context) == 0:
            raise RuntimeError(f"Context is empty")
        base_var, vars, range = self._context.pop()
        for name in list(self.vars):
            vl = VarList()
            vl.vars[name] = self.vars[name]
            vl.dims[name] = self.dims[name]
            if name in self.exclude:
                for index in self.exclude[name]:
                    index_new = None
                    skip = False
                    for j, dim in enumerate(index):
                        if dim is None:
                            continue
                        dim_new = dim.replace_with(base_var, range)
                        if dim_new is not dim:
                            if index_new is None:
                                index_new = index.copy()
                            index_new[j] = dim_new
                        else:
                            for var in vars:
                                if var in dim.collect_vars(without_index=True):
                                    skip = True
                                    break
                            if skip:
                                break
                    if skip:
                        continue
                    idx = index if index_new is None else index_new
                    dims = self.dims[name] if name in self.dims else None
                    v = self._get_var(name, index=idx, dims=dims)
                    vl.remove(v)
                if name in vl.exclude and vl.exclude[name]:
                    self.exclude[name] = vl.exclude[name]
                else:
                    del self.exclude[name]

            if not name in vl.vars:
                del self.vars[name]
                if name in self.dims:
                    del self.dims[name]
                if name in self.exclude:
                    del self.exclude[name]
                continue

            index_list: List[Optional[AryIndex]] = []
            for index in vl.vars[name]:
                if index is None:
                    index_list.append(None)
                    continue
                index_new: Optional[AryIndex] = None
                for j, dim in enumerate(index):
                    if dim is None:
                        continue
                    dim_new = dim.replace_with(base_var, range)
                    if dim_new is not dim:
                        if index_new is None:
                            index_new = index.copy()
                        index_new[j] = dim_new
                    else:
                        for var in vars:
                            dim_new = dim.replace_with(var, OpRange([None, None]))
                            if dim_new is not dim:
                                if index_new is None:
                                    index_new = index.copy()
                                index_new[j] = dim_new
                                break
                if index_new is None:
                    index_list.append(index)
                else:
                    index_list.append(index_new)
            self.vars[name] = index_list
            self._reorganize(name)

    def __contains__(self, item: OpVar) -> bool:
        """Return ``True`` if ``item`` is partially in the list."""

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
            # Stored entry that represents full coverage always contains
            if index is None:
                return True
            # Query asks for full coverage but we only have partial indices
            if index_item is None:
                return True
            if AryIndex.check_overlap(index_item, index):
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
                dims = self.dims.get(name, [0])
                var = self._get_var(name, index, dims)
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

    def _get_var(self, name: str, index: Optional[Union[AryIndex,List[Operator]]], dims: List[int]) -> OpVar:
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
            if index is not None:
                index_ref = index[: -dims[-1]] if dims[-1] > 0 else index
            else:
                index_ref = None
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

    def _get_op(self, op: Optional[Union[Operator, int]]) -> Optional[Operator]:
        """Resolve variables from context and normalise integers to OpInt."""
        if isinstance(op, int):
            return OpInt(op)
        if op is None:
            return None
        for var, range in self._context.items():
            op = op.replace_with(var, range)
        return op

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

        # Merge context
        for i, cont in enumerate(other._context):
            if (len(self._context) <= i
                or self._context[i][0] != cont[0]
                or self._context[i][2] != cont[2]):
                print(self._context)
                print(other._context)
                raise RuntimeError("context is not consistent")
            for var in cont[1]:
                if var not in self._context[i][1]:
                    self._context[i][1].append(var)

    @staticmethod
    def _force_stride_one(var) -> OpVar:
        """Return a copy of ``var`` with negative strides normalised."""

        # force stride value of -1 to 1
        index = var.index
        replaced = False
        if isinstance(var.index, AryIndex):
            index_new = []
            for idx in var.index:
                if isinstance(idx, OpRange) and AryIndex._get_int(idx[2]) == -1:
                    index_new.append(OpRange([idx[1], idx[0]]))
                    replaced = True
                else:
                    index_new.append(idx)
            if replaced:
                var = var.change_index(AryIndex(index_new))

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
        for pos, index in enumerate(list(self.vars[name])):
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

            # both are OpRange
            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                i0, i1 = dim1[0], dim1[1]
                j0, j1 = dim2[0], dim2[1]

                # Check for overlapping
                k0 = None
                k1 = None
                if dim1.strict_in(j0):
                    k0 = i0
                elif dim2.strict_in(i0):
                    k0 = j0
                if dim1.strict_in(j1):
                    k1 = i1
                elif dim2.strict_in(i1):
                    k1 = j1
                if k0 is not None and k1 is not None:
                    index[i] = OpRange([k0, k1])
                    found = True
                    break

                # Check for adjacency: e.g., (:k) and (k+1:)
                if i1 is not None and j0 is not None:
                    diff = j0 - i1
                    if isinstance(diff, OpInt) and diff.val == 1:
                        index[i] = OpRange([i0, j1])
                        found = True
                        break
                if j1 is not None and i0 is not None:
                    diff = i0 - j1
                    if isinstance(diff, OpInt) and diff.val == 1:
                        index[i] = OpRange([j0, i1])
                        found = True
                        break
                continue


            # both are scalar
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                diff = AryIndex._get_int(dim1 - dim2)
                if isinstance(diff, int):
                    if diff == 1:
                        index[i] = OpRange([dim2, dim1])
                        found = True
                        break
                    elif diff == -1:
                        index[i] = OpRange([dim1, dim2])
                        found = True
                        break
                continue

            # one is range and the other is not
            if isinstance(dim1, OpRange) and not isinstance(dim2, OpRange):
                rng, scalar = dim1, dim2
            elif not isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                rng, scalar = dim2, dim1
            else: # unexpected
                raise RuntimeError(f"Unexpected: {type(dim1)} {type(dim2)}")
            i0, i1 = rng[0], rng[1]
            # Check adjacency at start
            if i0 is not None:
                diff = i0 - scalar
                if isinstance(diff, OpInt) and diff.val == 1:
                    index[i] = OpRange([scalar, i1])
                    found = True
                    break
            # Check adjacency at end
            if i1 is not None:
                diff = scalar - i1
                if isinstance(diff, OpInt) and diff.val == 1:
                    index[i] = OpRange([i0, scalar])
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

        if name not in self.vars:
            return

        var = self._force_stride_one(var) # Normalise negative strides before removal
        var_index = var.concat_index()

        if var_index is None or all(dim is None for dim in var_index.dims):
            del self.vars[name]
            return

        added = False
        index_list: List[AryIndex] = []
        for index in self.vars[name]:
            if index == var_index:
                # removed
                continue
            if index is None:
                index_list.append(index)
                if not added:
                    self.add_exclude(var)
                    added = True
                continue

            i = AryIndex.get_diff_dim(index, var_index)
            # If indices differ in multiple dimensions, add to the exclude list
            if i < 0 and index != var_index:
                index_list.append(index)
                if not added:
                    self.add_exclude(var)
                    added = True
                continue

            index = index.copy()

            dim1 = index[i]
            dim2 = var_index[i] if var_index is not None else None

            if AryIndex.dim_is_entire(dim1):
                index_list.append(index)
                if not added:
                    self.add_exclude(var)
                    added = True
                continue

            if AryIndex.dim_is_entire(dim2):
                # removed
                continue

            # both are OpRange
            if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                i0, i1 = dim1[0], dim1[1]
                j0, j1 = dim2[0], dim2[1]

                if i0 is None:
                    diff0 = 0 if j0 is None else 999
                else:
                    diff0 = AryIndex._get_int(j0 - i0) if j0 is not None else -999
                if i1 is None:
                    diff1 = 0 if j1 is None else 999
                else:
                    diff1 = AryIndex._get_int(i1 - j1) if j1 is not None else -999

                if diff0 is None or diff1 is None:
                    index_list.append(index)
                    if not added:
                        self.add_exclude(var)
                        added = True
                    continue

                if diff0 > 0:
                    if diff0 == 1:
                        index[i] = i0
                    else:
                        index[i] = OpRange([i0, j0-1])
                    index_list.append(index)
                    if diff1 > 0:
                        index_new = index.copy()
                        if diff1 == 1:
                            index_new[i] = i1
                        else:
                            index_new[i] = OpRange([j1+1, i1])
                        index_list.append(index_new)
                else: # diff0 <= 0 (= j0 <= i0 or j0 is None)
                    if diff1 == 1:
                        index[i] = i1
                    elif diff1 > 1:
                        index[i] = OpRange([j1+1, i1])
                    else: # diff1 <= 0 (= i1 <= j1 of j1 is None)
                        # removed
                        continue
                    index_list.append(index)
                continue

            # both are scalar
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                index_list.append(index)
                if not added:
                    self.add_exclude(var)
                    added = True
                continue

            # dim1 is range and dim2 is scalar
            if isinstance(dim1, OpRange):
                if not (dim1[2] is None or AryIndex._get_int(dim1[2]) == 1):
                    index_list.append(index)
                    if not added:
                        self.add_exclude(var)
                        added = True
                    continue
                if dim1.strict_in(dim2):
                    index_list.append(index)
                    if dim2 == dim1[0]:
                        index[i] = dim1[1] if dim1[1] == dim1[0] + 1 else OpRange([dim1[0]+1, dim1[1]])
                    elif dim2 == dim1[1]:
                        index[i] = dim1[0] if dim1[0] == dim1[1] - 1 else OpRange([dim1[0], dim1[1]-1])
                    else:
                        index[i] = dim1[0] if dim1[0] == dim2 - 1 else OpRange([dim1[0], dim2-1])
                        index_new = index.copy()
                        index_new[i] = dim1[1] if dim1[1] == dim2 + 1 else OpRange([dim2+1, dim1[1]])
                        index_list.append(index_new)
                    continue
                d0 = AryIndex._get_int(dim1[0] - dim2)
                d1 = AryIndex._get_int(dim2 - dim1[1])
                index_list.append(index)
                if (isinstance(d0, int) and d0 > 0) or (isinstance(d1, int) and d1 > 0): # outside of the range
                    continue # do not change
                if not added:
                    self.add_exclude(var)
                    added = True
                continue

            # dim2 is range and dim1 is scalar
            if isinstance(dim2, OpRange):
                if not (dim2[2] is None or AryIndex._get_int(dim2[2]) == 1):
                    index_list.append(index)
                    continue
                if dim2.strict_in(dim1):
                    # removed
                    continue
                index_list.append(index)
                if not added:
                    self.add_exclude(var)
                    added = True
                continue

        self.vars[name] = index_list
        self._reorganize(name)
        if not self.vars[name]:
            del self.vars[name]
            if name in self.dims:
                del self.dims[name]
            if name in self.exclude:
                del self.exclude[name]

    def add_exclude(self, var: OpVar):
        """Mark ``var`` as excluded from the list."""

        name = var.name_ext()
        self._update_dims(name, var.ndims_ext())
        if name not in self.exclude:
            self.exclude[name] = []
        var_index = var.concat_index()
        if var_index is None:
            raise ValueError("index must not be None for exclude")
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
                        i0 = AryIndex._get_int(dim1[0])
                        i1 = AryIndex._get_int(dim1[1])
                        i2 = AryIndex._get_int(dim1[2]) if dim1[2] is not None else 1
                        if not (isinstance(i2, int) and abs(i2) == 1):
                            continue  # assumes no overlap
                        if not (isinstance(dim2, OpInt) or isinstance(dim2, OpRange)):
                            index[i] = dim2
                            index_list.append(index)
                            continue

                    if isinstance(dim2, OpRange):
                        j0 = AryIndex._get_int(dim2[0])
                        j1 = AryIndex._get_int(dim2[1])
                        j2 = AryIndex._get_int(dim2[2]) if dim2[2] is not None else 1
                        if not (isinstance(j2, int) and abs(j2) == 1):
                            continue  # assumes no overlap
                        if not (isinstance(dim1, OpInt) or isinstance(dim1, OpRange)):
                            index[i] = dim1
                            index_list.append(index)
                            continue

                    v1 = AryIndex._get_int(dim1)
                    v2 = AryIndex._get_int(dim2)
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
                    i0 = AryIndex._get_int(range[0])
                    i1 = AryIndex._get_int(range[1])

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

        index_list = self.vars[name]
        self.vars[name] = []
        if name in self.exclude:
            exclude = list(self.exclude[name])
        else:
            exclude = []
        for index in index_list:
            if index in exclude:
                if index is not None:
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
