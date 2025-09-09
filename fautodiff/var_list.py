"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Iterator, List, Tuple, Optional, Union, Iterable


from .operators import AryIndex, OpInt, OpNeg, OpRange, OpVar, Operator
from .var_dict import VarDict


@dataclass
class IndexList:
    """Hold indices, excluded indices and dimension info for a variable name."""

    indices: List[Optional[AryIndex]] = field(default_factory=list)
    exclude: List[AryIndex] = field(default_factory=list)
    dims: List[int] = field(default_factory=list)

    def copy(self) -> "IndexList":
        return IndexList(indices=list(self.indices), exclude=list(self.exclude), dims=list(self.dims))

    def __len__(self) -> int:
        return len(self.indices)

    def __iter__(self) -> Iterator[Optional[AryIndex]]:
        return iter(self.indices)

    def __getitem__(self, index: int) -> Optional[AryIndex]:
        return self.indices[index]

    def __str__(self) -> str:
        """Human readable representation used for debugging.

        Render full-array coverage explicitly as ':' for each dimension so
        that strings like '(:,:)' appear even when the internal representation
        uses ``None`` for entire coverage.
        """

        res = ", ".join([f"({idx})" for idx in self.indices])

        excl = [f"({idx})" for idx in self.exclude]
        if excl:
            res = f"{res}, exclude: {', '.join(excl)}"
        return res

    # --- Convenience API for managing internal data ---
    def set_indices(self, indices: List[Optional[AryIndex]]) -> None:
        self.indices = list(indices)

    def append_index(self, index: Optional[AryIndex]) -> None:
        self.indices.append(index)

    def has_indices(self) -> bool:
        return bool(self.indices)

    def set_exclude(self, exclude: List[AryIndex]) -> None:
        self.exclude = list(exclude)

    def add_exclude(self, index: AryIndex) -> None:
        if index not in self.exclude:
            self.exclude.append(index)

    def clear_exclude(self) -> None:
        self.exclude = []

    def set_dims(self, dims: List[int]) -> None:
        self.dims = list(dims)

    # --- Context handling ---
    def _replace_index(self, index: AryIndex, base_var: OpVar, range: OpRange, vars: List[OpVar], exclude: bool = False) -> Optional[AryIndex]:
        """Return transformed index for context exit.

        - Replace occurrences of base_var in dims by the given range.
        - If not replaced, replace occurrences of any var in vars by full slice (:).
        Returns a possibly new AryIndex (or the original) or None if it should be skipped.
        """
        index_new = None
        for j, dim in enumerate(index):
            if dim is None:
                continue
            dim_new = dim.replace_with(base_var, range)
            if dim_new is not dim:
                if index_new is None:
                    index_new = index.copy()
                index_new[j] = dim_new
            elif not exclude:
                for v in vars:
                    dim_new2 = dim.replace_with(v, OpRange([None, None]))
                    if dim_new2 is not dim:
                        if index_new is None:
                            index_new = index.copy()
                        index_new[j] = dim_new2
                        break
        return index if index_new is None else index_new

    def remove_index(self, var_index: AryIndex) -> None:
        """Remove coverage specified by var_index from self.indices.

        This is a simplified removal: exact matches and full-coverage cases are removed;
        partial overlaps are kept and marked by adding the removed index to exclude.
        """
        if not self.indices:
            return
        # Full coverage removal clears indices
        if var_index is None or all(dim is None for dim in var_index):
            self.indices = []
            return
        new_list: List[Optional[AryIndex]] = []
        added_exclude = False
        for idx in self.indices:
            if idx is None:
                # Previously entire array; keep and record exclusion
                new_list.append(idx)
                if not added_exclude:
                    self.add_exclude(var_index)
                    added_exclude = True
                continue
            if idx == var_index:
                # exact removal
                continue
            # For now, keep partial overlaps and mark exclude
            new_list.append(idx)
            if not added_exclude:
                self.add_exclude(var_index)
                added_exclude = True
        self.indices = new_list

    def exit_context(self, context: Tuple[OpVar, List[OpVar], OpRange]) -> None:
        """Adjust indices/excludes when exiting a loop context.

        Transforms exclude indices by replacing the loop index with the loop range,
        then removes those covered regions from the tracked indices.
        """
        base_var, vars, range = context
        # Normalize negative stride ranges to forward direction (e.g., n:1:-1 -> 1:n)
        rng = range
        try:
            if rng[2] is not None and isinstance(rng[2], OpNeg):
                step = -rng[2]
                if isinstance(step, OpInt) and step.val == 1:
                    step = None
                rng = OpRange([rng[1], rng[0], step])
            if rng[2] == OpInt(1):
                rng = OpRange([rng[0], rng[1]])
        except Exception:
            # In case range is shorter (len<3), fall back to original
            rng = range
        idx_list = IndexList()
        idx_list.indices = list(self.indices)
        idx_list.dims = list(self.dims)
        for ex in list(self.exclude):
            if ex is None:
                raise RuntimeError("Unexpected")
            ex_new = self._replace_index(ex, base_var, rng, vars, exclude=True)
            # Use full-featured removal to shrink covered regions instead of just marking excludes
            try:
                idx_list.remove_var(OpVar("__tmp__", index=ex_new))
            except Exception:
                # Fallback to simplified removal if OpVar creation or removal fails
                idx_list.remove_index(ex_new)
        if idx_list.exclude:
            self.exclude = idx_list.exclude
        else:
            self.clear_exclude()

        # Adopt indices possibly modified by removals above
        if not idx_list.indices:
            self.indices = []
            self.dims = []
            self.clear_exclude()
        else:
            self.indices = list(idx_list.indices)

        indices: List[Optional[AryIndex]] = []
        for idx in self.indices:
            if idx is None:
                indices.append(None)
                continue
            idx_new = self._replace_index(idx, base_var, rng, vars)
            if idx_new is None:
                indices.append(idx)
            else:
                indices.append(idx_new)
        self.indices = indices
        self.reorganize()

    # --- High-level helpers used by VarList ---
    def make_opvar(self, full_name: str, index: Optional[Union[AryIndex, List[Operator]]]) -> OpVar:
        """Construct an OpVar for this name with given index using stored dims.
        Supports nested derived-type components split by '%'.
        """
        dims = list(self.dims) if self.dims else [0]
        # Support for derived-type components
        pos = full_name.rfind("%")
        if pos >= 0:
            name_ref = full_name[:pos]
            if index is not None:
                if dims[-1] > 0:
                    index_ref = index[: -dims[-1]]
                    index_leaf = index[-dims[-1] :]
                else:
                    index_ref = index
                    index_leaf = None
            else:
                index_ref = None
                index_leaf = None
            ref_il = IndexList(dims=dims[:-1])
            var_ref = ref_il.make_opvar(name_ref, index_ref)
            name = full_name[pos + 1 :]
            index = index_leaf
        else:
            var_ref = None
            name = full_name
        # Normalise dims for this level
        if len(dims) > 0 and dims[-1] > 0:
            dims_t = tuple([":"] * dims[-1])
        else:
            dims_t = tuple()
        return OpVar(name=name, index=index, dims=dims_t, ref_var=var_ref)

    def contains(self, index_item: Optional[AryIndex]) -> bool:
        """Return True if index_item overlaps tracked indices, excluding exclusions."""
        if index_item in self.exclude:
            return False
        for index in self.indices:
            if index is None:
                return True
            if index_item is None:
                return True
            if AryIndex.check_overlap(index_item, index):
                return True
        return False

    def format_strings(self, full_name: str) -> List[str]:
        """Return string representations for all tracked indices of this name."""
        out: List[str] = []
        for index in self.indices:
            if index is not None:
                out.append(str(self.make_opvar(full_name, index)))
            else:
                # full coverage: render ':' slices according to dims
                dims_counts = list(self.dims or [0])
                parts = full_name.split("%")
                buf: List[str] = []
                for i, part in enumerate(parts):
                    buf.append(part)
                    if i < len(dims_counts):
                        cnt = dims_counts[i]
                        if cnt and cnt > 0:
                            buf.append("(" + ",".join([":"] * cnt) + ")")
                    if i < len(parts) - 1:
                        buf.append("%")
                out.append("".join(buf))
        return out

    def format_exclude_strings(self, full_name: str) -> List[str]:
        return [str(self.make_opvar(full_name, idx)) for idx in self.exclude]

    def iter_opvars(self, full_name: str) -> Iterator[OpVar]:
        for index in self.indices:
            yield self.make_opvar(full_name, index)

    # --- Algorithms migrated from VarList for per-name operations ---
    def update_dims_compat(self, ndims: List[int]) -> None:
        """Update dims by merging with ndims using legacy compatibility rules."""
        if not self.dims:
            self.dims = list(ndims)
            return
        if len(self.dims) != len(ndims):
            raise ValueError(f"Different number of dimensions: {self.dims} {ndims}")
        for i in range(len(self.dims)):
            if self.dims[i] != ndims[i]:
                if self.dims[i] > 0 and ndims[i] > 0:
                    raise ValueError(
                        f"Different number of dimensions: {i} {self.dims} {ndims}"
                    )
                if self.dims[i] == 0:
                    self.dims[i] = ndims[i]

    def merge_from(self, other: "IndexList") -> None:
        """Merge indices, excludes, and dims from ``other`` into ``self``."""
        # merge indices (preserve existing ordering where possible)
        for idx in other.indices:
            if idx not in self.indices:
                self.indices.append(idx if idx is None else idx.copy())
        # merge exclude
        for ex in other.exclude:
            if ex not in self.exclude:
                self.exclude.append(ex if ex is None else ex.copy())
        # update dims compatibly
        self.update_dims_compat(other.dims)

    def reorganize(self) -> None:
        """Normalize and merge indices until convergence.

        This replays all stored non-None indices through ``push`` with
        ``not_reorganize=True`` repeatedly until neither ``indices`` nor
        ``exclude`` change.
        """
        while True:
            prev_indices = list(self.indices)
            prev_exclude = list(self.exclude)

            index_list = list(self.indices)
            self.indices = []
            exclude = list(self.exclude)
            for index in index_list:
                if index in exclude:
                    if index is not None and index in exclude:
                        exclude.remove(index)
                else:
                    if index is None:
                        if None not in self.indices:
                            self.indices.append(None)
                    else:
                        # Replay through push without further reorganize.
                        self.push(index, True)
            self.exclude = exclude

            if self.indices == prev_indices and self.exclude == prev_exclude:
                break

    def update_index_upward(self, do_index: int, rng: OpRange) -> None:
        new_list: List[Optional[AryIndex]] = []
        for index in self.indices:
            if (
                index is not None
                and index[do_index] is not None
                and index[do_index] in rng
            ):
                idx = index.copy()
                idx[do_index] = rng
                new_list.append(idx)
            else:
                new_list.append(index)
        self.indices = new_list

    def update_index_downward(self, do_index: int, ndim: int, do_index_var: OpVar) -> None:
        new_list: List[Optional[AryIndex]] = []
        for index in self.indices:
            if index is None:
                idx = AryIndex([None] * ndim)
            else:
                idx = index.copy()
            idx[do_index] = do_index_var
            new_list.append(idx)
        self.indices = new_list

    def push(self, var_index: AryIndex, not_reorganize: bool = False) -> None:
        """Push a raw AryIndex into this IndexList, merging/normalizing indices.

        Parameters
        - full_name: canonical name key (e.g., "v" or "s%a") for reorganize replay
        - var_index: index to insert (AryIndex). Entire coverage is represented by
          an AryIndex whose dims are all entire (":") and will be stored as None.
        - not_reorganize: if True, skip final reorganize pass
        - push_cb: callback to VarList.push used by reorganize to replay inserts
        """
        # Entire array coverage -> store as single None and clear excludes
        if isinstance(var_index, AryIndex) and all(
            AryIndex.dim_is_entire(dim) for dim in var_index
        ):
            self.indices = [None]
            self.clear_exclude()
            return

        # Remove exclusions that are now explicitly included
        for ex in list(self.exclude):
            if var_index >= ex:
                self.exclude.remove(ex)

        # First occurrence just records the index as-is.
        if not self.indices:
            self.indices = [var_index]
            return

        found = False
        for pos, index in enumerate(list(self.indices)):
            if index is None or index == var_index:
                return
            if var_index is None:
                self.indices = [None]
                return

            i = AryIndex.get_diff_dim(index, var_index)
            if i < 0:
                continue

            # work on a local copy and write back
            work = index.copy()
            self.indices[pos] = work

            dim1 = work[i]
            dim2 = var_index[i]

            # Entire dimension covered by either index -> store as entire.
            if AryIndex.dim_is_entire(dim1) or AryIndex.dim_is_entire(dim2):
                work[i] = None
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
                    work[i] = OpRange([k0, k1])
                    found = True
                    break

                # Check for adjacency: e.g., (:k) and (k+1:)
                if i1 is not None and j0 is not None:
                    diff = j0 - i1
                    if isinstance(diff, OpInt) and diff.val == 1:
                        work[i] = OpRange([i0, j1])
                        found = True
                        break
                if j1 is not None and i0 is not None:
                    diff = i0 - j1
                    if isinstance(diff, OpInt) and diff.val == 1:
                        work[i] = OpRange([j0, i1])
                        found = True
                        break
                continue

            # both are scalar
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                diff = AryIndex._get_int(dim1 - dim2)
                if isinstance(diff, int):
                    if diff == 1:
                        work[i] = OpRange([dim2, dim1])
                        found = True
                        break
                    elif diff == -1:
                        work[i] = OpRange([dim1, dim2])
                        found = True
                        break
                continue

            # one is range and the other is not
            if isinstance(dim1, OpRange) and not isinstance(dim2, OpRange):
                rng, scalar = dim1, dim2
            elif not isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                rng, scalar = dim2, dim1
            else:  # unexpected
                raise RuntimeError(f"Unexpected: {type(dim1)} {type(dim2)}")
            i0, i1 = rng[0], rng[1]
            # Check adjacency at start
            if i0 is not None:
                diff = i0 - scalar
                if isinstance(diff, OpInt) and diff.val == 1:
                    work[i] = OpRange([scalar, i1])
                    found = True
                    break
            # Check adjacency at end
            if i1 is not None:
                diff = scalar - i1
                if isinstance(diff, OpInt) and diff.val == 1:
                    work[i] = OpRange([i0, scalar])
                    found = True
                    break

        if not found:
            self.indices.append(var_index)

    @staticmethod
    def _force_stride_one(var: OpVar) -> OpVar:
        """Return a copy of ``var`` with negative strides normalised."""
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
            ref_var = IndexList._force_stride_one(ref_var)

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

    def push_var(self, var: OpVar, not_reorganize: bool = False) -> None:
        """Push a var (OpVar) into this IndexList using merge logic.

        Parameters
        - var: OpVar to push (negative strides normalized)
        - not_reorganize: if True, skip the final reorganize
        - push_cb: callback that accepts AryIndex for reorganize replay
        """
        if not isinstance(var, OpVar):
            raise ValueError(f"Must be OpVar: {type(var)}")

        var = IndexList._force_stride_one(var)
        self.push(var.concat_index(), not_reorganize)

    def remove_var(self, var: OpVar) -> None:
        """Remove ``var`` index coverage from this IndexList.

        This is a per-name refactoring of VarList.remove.
        """
        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")
        var = IndexList._force_stride_one(var)
        var_index = var.concat_index()
        if var_index is None or (hasattr(var_index, "dims") and all(dim is None for dim in var_index.dims)):
            # Removing entire variable clears indices
            self.indices = []
            return

        added = False
        index_list: List[AryIndex] = []
        for index in self.indices:
            if index == var_index:
                # removed exactly
                continue
            if index is None:
                index_list.append(index)
                if not added:
                    self.add_exclude(var_index)
                    added = True
                continue

            i = AryIndex.get_diff_dim(index, var_index)
            # If indices differ in multiple dimensions, add to the exclude list
            if i < 0 and index != var_index:
                index_list.append(index)
                if not added:
                    self.add_exclude(var_index)
                    added = True
                continue

            work = index.copy()
            dim1 = work[i]
            dim2 = var_index[i] if var_index is not None else None

            if AryIndex.dim_is_entire(dim1):
                index_list.append(work)
                if not added:
                    self.add_exclude(var_index)
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
                    index_list.append(work)
                    if not added:
                        self.add_exclude(var_index)
                        added = True
                    continue

                if diff0 > 0:
                    if diff0 == 1:
                        work[i] = i0
                    else:
                        work[i] = OpRange([i0, j0 - 1])
                    index_list.append(work)
                    if diff1 > 0:
                        index_new = work.copy()
                        if diff1 == 1:
                            index_new[i] = i1
                        else:
                            index_new[i] = OpRange([j1 + 1, i1])
                        index_list.append(index_new)
                else:  # diff0 <= 0 (= j0 <= i0 or j0 is None)
                    if diff1 == 1:
                        work[i] = i1
                    elif diff1 > 1:
                        work[i] = OpRange([j1 + 1, i1])
                    else:  # diff1 <= 0 (= i1 <= j1 of j1 is None)
                        # removed
                        continue
                    index_list.append(work)
                continue

            # both are scalar
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                index_list.append(work)
                if not added:
                    self.add_exclude(var_index)
                    added = True
                continue

            # dim1 is range and dim2 is scalar
            if isinstance(dim1, OpRange):
                if not (dim1[2] is None or AryIndex._get_int(dim1[2]) == 1):
                    index_list.append(work)
                    if not added:
                        self.add_exclude(var_index)
                        added = True
                    continue
                if dim1.strict_in(dim2):
                    index_list.append(work)
                    if dim2 == dim1[0]:
                        work[i] = dim1[1] if dim1[1] == dim1[0] + 1 else OpRange([dim1[0] + 1, dim1[1]])
                    elif dim2 == dim1[1]:
                        work[i] = dim1[0] if dim1[0] == dim1[1] - 1 else OpRange([dim1[0], dim1[1] - 1])
                    else:
                        work[i] = dim1[0] if dim1[0] == dim2 - 1 else OpRange([dim1[0], dim2 - 1])
                        index_new = work.copy()
                        index_new[i] = dim1[1] if dim1[1] == dim2 + 1 else OpRange([dim2 + 1, dim1[1]])
                        index_list.append(index_new)
                    continue
                d0 = AryIndex._get_int(dim1[0] - dim2)
                d1 = AryIndex._get_int(dim2 - dim1[1])
                index_list.append(work)
                if (isinstance(d0, int) and d0 > 0) or (isinstance(d1, int) and d1 > 0):
                    continue  # do not change
                if not added:
                    self.add_exclude(var_index)
                    added = True
                continue

            # dim2 is range and dim1 is scalar
            if isinstance(dim2, OpRange):
                if not (dim2[2] is None or AryIndex._get_int(dim2[2]) == 1):
                    index_list.append(work)
                    continue
                if dim2.strict_in(dim1):
                    # removed
                    continue
                index_list.append(work)
                if not added:
                    self.add_exclude(var_index)
                    added = True
                continue

        self.indices = index_list

    def intersect_with(self, other: "IndexList") -> "IndexList":
        """Return a new IndexList representing the intersection with ``other``."""
        out = IndexList(dims=list(self.dims))
        # Merge dims conservatively
        out.update_dims_compat(other.dims if other.dims else self.dims)
        index_list: List[Optional[AryIndex]] = []
        for index2 in other.indices:
            for index1 in self.indices:
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
                    i0 = AryIndex._get_int(dim1[0])
                    i1 = AryIndex._get_int(dim1[1])
                    j0 = AryIndex._get_int(dim2[0])
                    j1 = AryIndex._get_int(dim2[1])
                    if not (
                        (dim1[0] is None or isinstance(i0, int))
                        and (dim1[1] is None or isinstance(i1, int))
                        and (dim2[0] is None or isinstance(j0, int))
                        and (dim2[1] is None or isinstance(j1, int))
                    ):
                        continue  # assume different
                    if (isinstance(i1, int) and isinstance(j0, int) and i1 < j0) or (
                        isinstance(j1, int) and isinstance(i0, int) and j1 < i0
                    ):
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
                    range_ = dim1
                    v = v2
                else:
                    if not isinstance(dim2, OpRange):
                        raise RuntimeError(f"Unexpected: {type(dim2)}")
                    range_ = dim2
                    v = v1
                i0 = AryIndex._get_int(range_[0])
                i1 = AryIndex._get_int(range_[1])
                if (i0 is not None and v < i0) or (i1 is not None and i1 < v):
                    continue
                index[i] = OpInt(v)
                index_list.append(index)

        out.indices = index_list
        return out





@dataclass
class VarList:
    """Maintain a set of variables and their index information.

    The class tracks for each variable name the used index patterns and
    dimensions.  It also records sub-indices that should be excluded from
    operations.  This information is used when generating derivative code to
    understand which elements of an array are accessed.
    """

    _store: Dict[str, IndexList] = field(default_factory=dict)
    _context: List[Tuple[OpVar, List[OpVar], OpRange]] = field(default_factory=list)

    def __init__(self, vars: Optional[List[OpVar]] = None, context: Optional[List[Tuple[OpVar, List[OpVar], OpRange]]] = None):
        """Initialise the container.

        Parameters
        ----------
        vars:
            Optional initial list of variables to populate the set with.
        """

        super().__init__()
        # Internal unified store (name -> IndexList)
        self._store = {}
        self._context = context if context is not None else []

        # Add provided variables while skipping expensive reorganisations.
        if vars is not None:
            for var in vars:
                self.push(var, not_reorganize=True)

    def copy(self) -> VarList:
        """Return a deep copy of the variable list."""

        var_list = VarList()

        # Deep copy the store
        for name in self.names():
            var_list._store[name] = self._store[name].copy()
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
            raise RuntimeError("Context is empty")
        base_var, vars, range = self._context.pop()
        for name in list(self._store.keys()):
            il = self._store[name]
            il.exit_context((base_var, vars, range))
            # drop empty entries
            if not il.indices:
                del self._store[name]

    def __contains__(self, item: OpVar) -> bool:
        """Return True if item is (partially) covered by the tracked indices."""
        if not isinstance(item, OpVar):
            raise ValueError(f"Must be OpVar: {type(item)}")
        name = item.name_ext()
        if not self.has_name(name):
            return False
        index_item = item.concat_index()
        return self._store[name].contains(index_item)

    def __str__(self) -> str:
        """Human readable representation used for debugging.

        Render full-array coverage explicitly as ':' for each dimension so
        that strings like 'v(:,:)' appear even when the internal representation
        uses ``None`` for entire coverage.
        """

        parts: List[str] = []
        for name in self.names():
            parts.extend(self._store[name].format_strings(name))
        res = ", ".join(parts)

        excl_parts: List[str] = []
        for name in sorted(self._store.keys()):
            excl_parts.extend(self._store[name].format_exclude_strings(name))
        if excl_parts:
            res = f"{res}, exclude: {', '.join(excl_parts)}"
        return res

    def __len__(self) -> int:
        """Return number of variable names tracked."""

        return len(self.names())

    def __getitem__(self, name: str) -> IndexList:
        """Return index list for ``name``."""

        return self._store[name]

    def __setitem__(self, name: str, item: IndexList) -> None:
        if not isinstance(item, IndexList):
            raise ValueError(f"item must be IndexList: {type(item)}")
        self._store[name] = item

    def __iter__(self) -> Iterator[OpVar]:
        """Iterate over stored variables as ``OpVar`` objects."""
        for name in self.names():
            yield from self._store[name].iter_opvars(name)

    # --- New public API over unified store ---
    def names(self) -> List[str]:
        """Return a sorted list of variable names currently stored."""
        out: List[str] = []
        for name in sorted(self._store.keys()):
            il = self._store[name]
            if il.indices:
                out.append(name)
        return out

    def has_name(self, name: str) -> bool:
        il = self._store.get(name)
        return bool(il and il.indices)

    def dims(self, name: str) -> List[int]:
        return self._store[name].dims

    def remove_name(self, name: str) -> None:
        if name in self._store:
            del self._store[name]

    def get_vars(self, name: str) -> List[OpVar]:
        return [var for var in self._store[name].iter_opvars(name)]

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

        if name not in self._store:
            self._store[name] = IndexList()
        il = self._store[name]
        if not il.get_dims():
            il.set_dims(ndims)
        else:
            il.update_dims_compat(ndims)

    def merge(self, other: VarList) -> None:
        """Merge variables from ``other`` into this list."""

        if not isinstance(other, VarList):
            raise ValueError(f"Must be VarList: {type(other)}")

        # Per-name merge via IndexList.merge_from + reorganize
        for name in other._store.keys():
            src = other._store[name]
            if name not in self._store:
                self._store[name] = IndexList()
            dst = self._store[name]
            dst.merge_from(src)
            # Normalise per-name store
            dst.reorganize()

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

    # _force_stride_one moved to IndexList

    def push(self, var: OpVar, not_reorganize: bool = False) -> None:
        """Add ``var`` to the list, merging overlapping indices."""

        if not isinstance(var, OpVar):
            raise ValueError(f"Must be OpVar: {type(var)}")

        name = var.name_ext()
        # Update dims info
        if name not in self._store:
            self._store[name] = IndexList()
        self._store[name].update_dims_compat(var.ndims_ext()) if self._store[name].dims else self._store[name].set_dims(var.ndims_ext())
        # Delegate push to IndexList
        il = self._store[name]
        il.push_var(var, not_reorganize)

    def remove(self, var) -> None:
        """Remove ``var`` from the list, splitting ranges as needed."""

        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")

        name = var.name_ext()
        if name not in self._store:
            return
        self._store[name].remove_var(var)
        # cleanup if empty
        if not self._store[name].indices:
            del self._store[name]

    def add_exclude(self, var: OpVar):
        """Mark ``var`` as excluded from the list."""

        name = var.name_ext()
        if name not in self._store:
            self._store[name] = IndexList()
        self._store[name].update_dims_compat(var.ndims_ext()) if self._store[name].dims else self._store[name].set_dims(var.ndims_ext())
        var_index = var.concat_index()
        if var_index is None:
            raise ValueError("index must not be None for exclude")
        il = self._store[name]
        if var_index not in il.exclude:
            il.exclude.append(var_index)

    def __and__(self, other: VarList) -> VarList:
        """Return intersection of this list with ``other``."""

        if not isinstance(other, VarList):
            raise ValueError(f"Must be VarList: {type(other)}")

        var_list = VarList()

        for name in other.names():
            if not self.has_name(name):
                continue
            left = self._store[name]
            right = other._store[name]
            inter = left.intersect_with(right)
            if inter.indices:
                var_list._store[name] = inter
        return var_list

    def update_index_upward(self, index_map: dict, range: OpRange) -> None:
        """Expand indices covered by ``range`` for variables in ``index_map``."""

        if range[2] is not None and isinstance(range[2], OpNeg):
            slice = -range[2]
            if isinstance(slice, OpInt) and slice.val == 1:
                slice = None
            range = OpRange([range[1], range[0], slice])
        for name in self.names():
            if name not in index_map:
                continue
            do_index, _ = index_map[name]
            il = self._store[name]
            # normalise negative stride
            rng = range
            if rng[2] is not None and isinstance(rng[2], OpNeg):
                slice_ = -rng[2]
                if isinstance(slice_, OpInt) and slice_.val == 1:
                    slice_ = None
                rng = OpRange([rng[1], rng[0], slice_])
            il.update_index_upward(do_index, rng)
            il.reorganize()

    def update_index_downward(self, index_map: dict, do_index_var: OpVar) -> None:
        """Replace index ``do_index`` with ``do_index_var`` for given names."""

        for name in self.names():
            if name not in index_map:
                continue
            do_index, ndim = index_map[name]
            il = self._store[name]
            il.update_index_downward(do_index, ndim, do_index_var)
            il.reorganize()
