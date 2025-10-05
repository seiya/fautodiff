"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Iterable, Iterator, List, Optional, Set, Tuple, Union

from .operators import AryIndex, Operator, OpInt, OpNeg, OpRange, OpVar, OpTrue, OpFalse, OpLogic, OpNot
from .var_dict import VarDict


@dataclass
class IndexList:
    """Hold indices, excluded indices and dimension info for a variable name."""

    indices: List[Optional[AryIndex]] = field(default_factory=list)
    exclude: List[AryIndex] = field(default_factory=list)
    dims: List[int] = field(default_factory=list)
    # Shape information per array dimension. For 1D array x(n), shape is (OpRange(1:n), ...).
    # None means unknown/not set.
    shape: Optional[Tuple[Optional[OpRange], ...]] = None

    def __post_init__(self) -> None:
        self._apply_shape()

    def copy(self) -> "IndexList":
        return IndexList(
            indices=list(self.indices),
            exclude=list(self.exclude),
            dims=list(self.dims),
            shape=(tuple(self.shape) if self.shape is not None else None),
        )

    def __len__(self) -> int:
        return len(self.indices)

    def __iter__(self) -> Iterator[Optional[AryIndex]]:
        return iter(self.indices)

    def __getitem__(self, index: int) -> Optional[AryIndex]:
        return self.indices[index]

    def __contains__(self, item: Optional[AryIndex]) -> bool:
        return self.contains(item)

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
        self._apply_shape()

    def append_index(self, index: Optional[AryIndex]) -> None:
        index = self._apply_shape_index(index)
        self.indices.append(index)

    def has_indices(self) -> bool:
        return bool(self.indices)

    def set_exclude(self, exclude: List[AryIndex]) -> None:
        self.exclude = list(exclude)

    def add_exclude(self, index: AryIndex, not_reorganize: bool = False) -> None:
        if index not in self.exclude:
            self.exclude.append(index)
            if not not_reorganize:
                self.reorganize()

    def clear_exclude(self) -> None:
        self.exclude = []

    def set_dims(self, dims: List[int]) -> None:
        self.dims = list(dims)

    def set_shape(self, shape: Tuple[Optional[OpRange], ...]) -> None:
        self.shape = tuple(shape)
        self._apply_shape()

    # --- Shape helpers ---
    def _shape_as_index(self) -> Optional[AryIndex]:
        if self.shape is None:
            return None
        return AryIndex(list(self.shape))

    def _apply_shape_index(self, index: AryIndex | None) -> AryIndex | None:
        if self.shape is None or index is None:
            return index

        index = index.ascending()

        new_index: List[Operator | None] = []
        replaced = False
        for j, dim in enumerate(index):
            if (dim is not None and dim == self.shape[j]) or (
                isinstance(dim, OpRange) and dim[0] is None and dim[1] is None
            ):
                new_index.append(None)
                replaced = True
                continue
            if isinstance(dim, OpRange) and self.shape[j] is not None:
                shape = self.shape[j]
                d0, d1, d2 = dim
                if dim[0] is None and shape[0] is not None:
                    replaced = True
                    d0 = shape[0]
                if dim[1] is None and shape[1] is not None:
                    replaced = True
                    d1 = shape[1]
                if replaced:
                    dim = (
                        OpRange([d0, d1, d2])
                        if d0 is not None and d0 is not OpInt(1)
                        else OpRange([d0, d1])
                    )
            new_index.append(dim)
        if replaced:
            if all(dim is None for dim in new_index):
                return None
            return AryIndex(new_index)
        return index

    def _apply_shape(self) -> None:
        if self.shape is None:
            return
        for i, index in enumerate(self.indices):
            new_index = self._apply_shape_index(index)
            if new_index is not index:
                self.indices[i] = new_index

    @staticmethod
    def _is_entire_index(index: Optional[AryIndex]) -> bool:
        if index is None:
            return True
        return all(AryIndex.dim_is_entire(dim) for dim in index)

    def _is_full_shape_index(self, index: Optional[AryIndex]) -> bool:
        """Return True if ``index`` matches the known shape exactly.

        Only returns True when shape information is available and all shape
        dimensions are explicitly specified (non-None) and equal to ``index``.
        """
        if self.shape is None or index is None:
            return False
        # If shape exists and lengths differ, raise error as requested
        if len(index) != len(self.shape):
            raise ValueError(
                f"index length {len(index)} does not match shape length {len(self.shape)}"
            )

        for dim_idx, shp in zip(index, self.shape):
            if shp is None:
                return False
            if dim_idx != shp:
                return False
        return True

    def _overlaps_shape(self, index: Optional[AryIndex]) -> bool:
        """Return True if the given index overlaps the shape domain.

        If no shape is defined, return True.
        """
        if self.shape is None:
            return True
        if index is None:
            return True
        try:
            if len(index) != len(self.shape):
                # Dimension mismatch -> conservatively assume no overlap
                return False
        except Exception:
            return False

        for d, s in zip(index, self.shape):
            if s is None:
                # Unknown bound -> cannot restrict
                continue
            if d is None:
                continue
            # If d is a range, check mutual overlap with shape range.
            if isinstance(d, OpRange):
                # s contains d OR d contains s implies overlap
                try:
                    if d in s or s in d:
                        continue
                except Exception:
                    # Fallback to conservative overlap
                    return True
                return False
            # Scalar/other operator: ensure it's inside shape range
            try:
                if d in s:
                    continue
            except Exception:
                # Conservative
                return True
            return False
        return True

    # --- Shared context application helper ---
    def _apply_context_index(
        self,
        idx: Optional[AryIndex],
        context: Optional[List[Tuple[OpVar, List[OpVar], OpRange]]],
        for_exclude: bool = False,
    ) -> Optional[AryIndex]:
        """Return a new AryIndex with VarList-style context applied.

        - ``context``: list of (base_var, vars, range) tuples.
        - ``for_exclude``: when True, do NOT apply slicing (":") for variables
          in the ``vars`` lists; only replace ``base_var`` with its ``range``.
          When False (indices), also replace occurrences of vars with
          ``OpRange([None, None])`` if the base variable replacement does not
          occur for that dimension.
        """
        if idx is None or context is None:
            return idx

        mappings: List[Tuple[OpVar, Operator]] = []
        slice_vars: List[OpVar] = []
        for base_var, vars_list, rng in context:
            rng = rng.ascending()
            mappings.append((base_var, rng))
            # Collect vars to be sliced for indices
            if vars_list:
                for v in vars_list:
                    if v not in slice_vars:
                        slice_vars.append(v)

        dims_new: List[Optional[Operator]] = []
        for n, dim in enumerate(idx):
            d = dim
            if d is not None:
                replaced = False
                # First apply base_var replacements
                for var, rng in mappings:
                    d_new = d.replace_with(var, rng)
                    if d_new is not d:
                        d = d_new
                        # trimming out-of-bounds ranges to shape if known
                        if self.shape and self.shape[n] is not None and isinstance(d, OpRange):
                            i0 = (self.shape[n][0] - d[0]).get_int()
                            if i0 is not None and i0 > 0:
                                d.args[0] = self.shape[n][0]
                            i1 = (d[1] - self.shape[n][1]).get_int()
                            if i1 is not None and i1 > 0:
                                d.args[1] = self.shape[n][1]
                        replaced = True
                # If not exclude and base_var not replaced, slice other vars
                if not for_exclude and not replaced and slice_vars:
                    for v in slice_vars:
                        d_new2 = d.replace_with(v, OpRange([None, None]))
                        if d_new2 is not d:
                            d = d_new2
                            break
            dims_new.append(d)
        return AryIndex(dims_new)

    # --- Context handling ---
    def _replace_index(
        self,
        index: AryIndex,
        base_var: OpVar,
        range: OpRange,
        vars: List[OpVar],
        exclude: bool = False,
    ) -> Optional[AryIndex]:
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

    def remove(
        self, var_index: Optional[AryIndex], not_reorganize: bool = False
    ) -> None:
        """Remove ``var_index`` coverage from this IndexList.

        This method performs full-featured removal, splitting ranges and updating
        excludes as needed. It accepts an ``AryIndex`` describing the region to
        remove. Entire coverage (None or all-entire dims) clears indices.
        """
        if not self.indices:
            return

        if not (var_index is None or isinstance(var_index, AryIndex)):
            raise ValueError(
                f"var_index must be None or AryIndex, not {type(var_index)}"
            )

        # Normalization
        var_index = self._apply_shape_index(var_index)
        if var_index is not None:
            var_index = var_index.ascending()

        # If shape is known and var_index is concrete, enforce it is within shape
        if self.shape is not None and var_index is not None:
            shape_idx = self._shape_as_index()
            if shape_idx is not None and shape_idx.check_outside(var_index):
                raise ValueError(
                    f"remove index outside shape: {self.shape} {var_index}"
                )

        # Removing entire variable clears indices in non-full-coverage cases
        if IndexList._is_entire_index(var_index) or self._is_full_shape_index(
            var_index
        ):
            self.indices = []
            return
        # Now var_index is not None

        added = False
        index_list: List[AryIndex | None] = []
        for index in self.indices:
            # Avoid treating entire-dimension coverage (e.g., (:)) as exactly equal
            # to a proper subrange. Only skip when both sides are structurally equal
            # and the stored index is not an entire-dimension placeholder.
            if index == var_index and not (
                IndexList._is_entire_index(index) and not IndexList._is_entire_index(var_index)
            ):
                # removed exactly
                continue

            if index is None and self.shape is None:
                index_list.append(None)
                if not added:
                    self.add_exclude(var_index, not_reorganize=True)
                    added = True
                continue

            i = AryIndex.get_diff_dim(index, var_index)
            # If indices differ in multiple dimensions, add to the exclude list
            if i < 0 and index != var_index:
                index_list.append(index)
                if not added:
                    self.add_exclude(var_index, not_reorganize=True)
                    added = True
                continue

            if index is None:
                work = AryIndex([None] * len(self.shape))
                # For full coverage, use the concrete shape on the differing dim
                if self.shape is not None:
                    work[i] = self.shape[i]
            else:
                work = index.copy()
                # Only expand to shape when the stored index covers the entire
                # dimension (None or full slice). Do NOT expand scalars or
                # proper subranges, otherwise we might create spurious coverage.
                if self.shape is not None:
                    dim_orig = index[i]
                    if AryIndex.dim_is_entire(dim_orig):
                        work[i] = self.shape[i]

            dim1 = work[i]
            dim2 = var_index[i]

            if AryIndex.dim_is_entire(dim1):
                index_list.append(index)
                if not added:
                    self.add_exclude(var_index, not_reorganize=True)
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
                    # could be partially overlapped
                    index_list.append(index)
                    if not added:
                        self.add_exclude(var_index, not_reorganize=True)
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

            # both are scalar: if different, no overlap -> keep index; add exclude
            # only when the removal index is symbolic (cannot prove no-overlap globally).
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                index_list.append(index)
                # Add exclude for symbolic removals (e.g., OpVar), but avoid
                # adding for concrete scalars (e.g., removing 2 while only 1 is stored).
                if not isinstance(dim2, OpInt):
                    if not added:
                        self.add_exclude(var_index, not_reorganize=True)
                        added = True
                continue

            # dim1 is range and dim2 is scalar
            if isinstance(dim1, OpRange):
                if not (dim1[2] is None or AryIndex._get_int(dim1[2]) == 1):
                    index_list.append(index)
                    if not added:
                        self.add_exclude(var_index, not_reorganize=True)
                        added = True
                    continue
                if dim1.strict_in(dim2):
                    index_list.append(work)  # work will be modified
                    if dim2 == dim1[0]:
                        work[i] = (
                            dim1[1]
                            if dim1[1] == dim1[0] + 1
                            else OpRange([dim1[0] + 1, dim1[1]])
                        )
                    elif dim2 == dim1[1]:
                        work[i] = (
                            dim1[0]
                            if dim1[0] == dim1[1] - 1
                            else OpRange([dim1[0], dim1[1] - 1])
                        )
                    else:
                        work[i] = (
                            dim1[0]
                            if dim1[0] == dim2 - 1
                            else OpRange([dim1[0], dim2 - 1])
                        )
                        index_new = work.copy()
                        index_new[i] = (
                            dim1[1]
                            if dim1[1] == dim2 + 1
                            else OpRange([dim2 + 1, dim1[1]])
                        )
                        index_list.append(index_new)
                    continue
                d0 = AryIndex._get_int(dim1[0] - dim2)
                d1 = AryIndex._get_int(dim2 - dim1[1])
                index_list.append(index)
                if (isinstance(d0, int) and d0 > 0) or (isinstance(d1, int) and d1 > 0):
                    continue  # do not change
                if not added:
                    self.add_exclude(var_index, not_reorganize=True)
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
                    self.add_exclude(var_index, not_reorganize=True)
                    added = True
                continue

        self.indices = index_list
        if not not_reorganize:
            self.reorganize()

    def exit_context(self, context: Tuple[OpVar, List[OpVar], OpRange]) -> None:
        """Adjust indices/excludes when exiting a loop context.

        Transforms exclude indices by replacing the loop index with the loop range,
        then removes those covered regions from the tracked indices. Any portion
        of an exclude not overlapping current indices is kept as a residual
        exclude (e.g., exclude i with i=1:n and indices covering 1 -> residual 2:n).
        """
        base_var, vars, range = context
        # Normalize negative stride ranges to forward direction (e.g., n:1:-1 -> 1:n)
        rng = range.ascending()

        idx_list = IndexList()
        idx_list.indices = list(self.indices)
        idx_list.dims = list(self.dims)
        # Propagate shape so removals can treat full-shape ranges as entire
        if self.shape is not None:
            idx_list.set_shape(self.shape)
        # Build VarList-style context list for base_var replacement
        ctx = [(base_var, vars, rng)]

        # Compute residual excludes after subtracting what overlaps current indices
        new_excludes: List[AryIndex] = []
        for ex in list(self.exclude):
            if ex is None:
                raise RuntimeError("Unexpected")
            # Replace base_var using shared context applicator; excludes do not
            # require the fallback ':' behaviour for other vars.
            ex_new = self._apply_context_index(ex, ctx, for_exclude=True)

            # Residual of ex_new w.r.t. current indices
            if ex_new is not None:
                residual = IndexList(indices=[ex_new])
                for idx in idx_list.indices:
                    if idx is None:
                        remove = False
                        if self._is_full_shape_index(ex_new):
                            remove = True
                        elif self.shape is not None:
                            remove = True
                            for d, s in zip(ex_new, self.shape):
                                if s is None or d == s:
                                    continue
                                if not s.strict_in(d):
                                    remove = False
                                    break
                        if remove:
                             residual.indices = []
                        break
                    residual.remove(idx)
                    if not residual.indices:
                        break
                for r in residual.indices:
                    if r is not None and r not in new_excludes:
                        new_excludes.append(r)

            # Remove the covered regions from indices
            idx_list.remove(ex_new)

        # Adopt indices possibly modified by removals above
        if not idx_list.indices:
            self.indices = []
        else:
            self.indices = list(idx_list.indices)

        # Update excludes based on computed residuals
        if new_excludes:
            self.exclude = new_excludes
        else:
            self.clear_exclude()

        indices: List[Optional[AryIndex]] = []
        for idx in self.indices:
            if idx is None:
                indices.append(None)
            else:
                indices.append(
                    self._apply_context_index(idx, ctx, for_exclude=False)
                )
        self.indices = indices
        self.reorganize()

    # --- High-level helpers used by VarList ---
    def make_opvar(
        self, full_name: str, index: Optional[Union[AryIndex, List[Operator]]]
    ) -> OpVar:
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
            dims_t = tuple([None] * dims[-1])
        else:
            dims_t = tuple()
        return OpVar(name=name, index=index, dims=dims_t, ref_var=var_ref)

    def contains(
        self,
        index_item: Optional[AryIndex],
        context: Optional[List[Tuple[OpVar, List[OpVar], OpRange]]] = None,
    ) -> bool:
        """Return True if ``index_item`` is covered by indices minus excludes.

        Rules:
        - For indices, any overlap is sufficient to return True.
        - For excludes, we must only return False when the query is fully covered
          by the (context-applied) excludes. If excludes only partially overlap
          the query, remove those parts and then check the remainder against the
          stored indices.
        """
        if not(index_item is None or isinstance(index_item, AryIndex)):
            raise ValueError(
                f"index_item must be None or AryIndex, not {type(index_item)}"
            )
        # Exact exclude match short-circuits in the trivial no-context case,
        # but do not treat an entire-dimension query like AryIndex([None]) as
        # exactly equal to a partial exclude range.
        if (
            context is None
            and index_item is not None
            and index_item in self.exclude
            and not IndexList._is_entire_index(index_item)
        ):
            return False

        index_item = self._apply_shape_index(index_item)

        # Apply context to the queried index and all excludes
        q = self._apply_context_index(index_item, context, for_exclude=False)
        # If shape is known, queries fully outside the shape domain are not contained
        if self.shape is not None and q is not None and not self._overlaps_shape(q):
            return False
        excludes_ctx: List[Optional[AryIndex]] = [
            self._apply_context_index(ex, context, for_exclude=True)
            for ex in self.exclude
        ]

        # Compute remaining segments of q after subtracting the excludes exactly once
        remaining_q: List[Optional[AryIndex]]
        if q is None:
            # Entire-domain query: if shape is known, scope to shape for more precise handling
            if self.shape is not None:
                q0 = self._shape_as_index()
            else:
                q0 = None
            if q0 is None:
                # Only fully removed if any exclude is entire-domain
                if any(ex is None for ex in excludes_ctx):
                    remaining_q = []
                else:
                    remaining_q = [None]
            else:
                tmp_q = IndexList(indices=[q0])
                for ex_ in excludes_ctx:
                    if ex_ is None:
                        tmp_q.indices = []
                        break
                    tmp_q.remove(ex_, not_reorganize=True)
                    if not tmp_q.indices:
                        break
                remaining_q = list(tmp_q.indices)
        else:
            tmp_q = IndexList(indices=[q])
            for ex_ in excludes_ctx:
                if ex_ is None:
                    tmp_q.indices = []
                    break
                tmp_q.remove(ex_, not_reorganize=True)
                if not tmp_q.indices:
                    break
            remaining_q = list(tmp_q.indices)

        # If nothing remains after removing excludes, not contained
        if not remaining_q:
            return False

        # Now test whether any stored index overlaps the remaining query
        for index in self.indices:
            idx = self._apply_context_index(index, context, for_exclude=False)
            if idx is None:
                # Entire coverage; we already ensured q is not fully excluded
                return True
            # Check against each remaining segment of q
            for rq in remaining_q:
                # rq can be None only when q is None; idx is not None here so that's a hit
                if rq is None:
                    return True
                if AryIndex.check_overlap(rq, idx):
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
            print(self.__dict__)
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
        # update shape
        if self.shape is None and other.shape is not None:
            self.set_shape(other.shape)
        # merge indices (preserve existing ordering where possible)
        for idx in other.indices:
            self.push(idx)
        # merge exclude (intersect with existing excludes)
        exclude_new = IndexList(list(self.exclude)).intersect_with(IndexList(list(other.exclude))).indices
        self.exclude = exclude_new
        self.reorganize()

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
                index = self._apply_shape_index(index)
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

    def push(self, var_index: Optional[AryIndex], not_reorganize: bool = False) -> None:
        """Push a raw AryIndex into this IndexList, merging/normalizing indices.

        Parameters
        - full_name: canonical name key (e.g., "v" or "s%a") for reorganize replay
        - var_index: index to insert (AryIndex). Entire coverage is represented by
          an AryIndex whose dims are all entire (":") and will be stored as None.
        - not_reorganize: if True, skip final reorganize pass
        - push_cb: callback to VarList.push used by reorganize to replay inserts
        """
        var_index = self._apply_shape_index(var_index)
        # Entire array coverage: when shape known, use shape value explicitly
        if IndexList._is_entire_index(var_index) or self._is_full_shape_index(
            var_index
        ):
            self.indices = [None]
            self.clear_exclude()
            return

        # If shape is known and var_index is concrete, enforce it is within shape
        if self.shape is not None and isinstance(var_index, AryIndex):
            shape_idx = self._shape_as_index()
            if shape_idx is not None and shape_idx.check_outside(var_index):
                raise ValueError(f"push index outside shape: {self.shape} {var_index}")

        # Remove or refine exclusions that intersect the newly included region
        # - If the new index fully covers an exclude, drop it.
        # - If we currently have full-coverage indices (None present), subtract
        #   the new index from the exclude to keep residuals only.
        if self.exclude:
            # Drop fully covered excludes
            for ex in list(self.exclude):
                if var_index >= ex:
                    self.exclude.remove(ex)

            # Refine partial excludes by subtracting the newly included region
            refined: List[AryIndex] = []
            for ex in list(self.exclude):
                residual = IndexList(indices=[ex])
                residual.remove(var_index, not_reorganize=True)

                # If unchanged due to symbolic bounds, try trimming exact endpoint hits
                if residual.indices == [ex]:
                    # Only handle 1D simple cases here
                    if isinstance(ex[0], OpRange) and not isinstance(
                        var_index[0], OpRange
                    ):
                        r = ex[0]
                        v = var_index[0]
                        # If v equals lower bound: (i0:i1) - i0 -> (i0+1:i1)
                        if str(v) == str(r[0]):
                            new_lb = r[0] + OpInt(1)
                            residual.indices = [AryIndex([OpRange([new_lb, r[1]])])]
                        # If v equals upper bound: (i0:i1) - i1 -> (i0:i1-1)
                        elif str(v) == str(r[1]):
                            new_ub = r[1] - OpInt(1)
                            residual.indices = [AryIndex([OpRange([r[0], new_ub])])]

                for r in residual.indices:
                    if r is not None and r not in refined:
                        refined.append(r)
            self.exclude = refined

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
            # If scalar lies within range (including endpoints), nothing to change
            if rng.strict_in(scalar):
                work[i] = rng
                found = True
                break
            diff0 = (i0 - scalar).get_int() if i0 is not None else 999
            diff1 = (scalar - i1).get_int() if i1 is not None else -999
            # Check inside
            if diff0 is not None and diff1 is not None and diff0 < 0 and diff1 < 0:
                work[i] = rng
                found = True
                break
            # Check adjacency at start
            if diff0 is not None and diff0 == 1:
                work[i] = OpRange([scalar, i1])
                found = True
                break
            # Check adjacency at end
            if diff1 is not None and diff1 == 1:
                work[i] = OpRange([i0, scalar])
                found = True
                break

        if not found:
            self.indices.append(var_index)

        if not not_reorganize:
            self.reorganize()

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
                    is_read_only=var.is_read_only,
                    ref_var=ref_var,
                )
        return var

    # --- Shape extraction/validation from OpVar ---
    def _shape_candidate_from_var(
        self, var: OpVar
    ) -> Optional[List[Optional[OpRange]]]:
        """Try to build a shape candidate from ``var``'s declared dims, not its slice.

        - Uses ``var.get_dims()`` which may return symbolic size variables per dimension.
        - For each dimension, synthesize an OpRange([1, size_var]) when size var is known,
          or None when unknown (":" placeholder).
        - Returns None when dims are not available.
        """
        dims = var.concat_dims()
        if dims is None:
            return None
        cand: List[Optional[OpRange]] = []
        for d in dims:
            if d is None:
                cand.append(None)
                continue
            if isinstance(d, OpRange):
                if d[0] is None and d[1] is None:
                    cand.append(None)
                else:
                    lb = d[0] if d[0] is not None else OpInt(1)
                    ub = d[1]
                    cand.append(OpRange([lb, ub]))
            else:
                cand.append(OpRange([OpInt(1), d]))
        # If all dims are unknown/":" placeholders, do not set shape
        if all(x is None for x in cand):
            return None
        return cand

    def _ensure_or_set_shape_from_var(self, var: OpVar) -> None:
        """Set ``shape`` from ``var`` if unknown; otherwise verify consistency.

        - If a candidate shape can be extracted from the var's index, use it to
          set ``self.shape`` when currently None, or compare for equality when
          already set.
        - If no candidate can be extracted, but ``self.shape`` exists and the
          var's index length differs from the shape length, raise an error.
        """
        cand = self._shape_candidate_from_var(var)
        if cand is None:
            if self.shape is not None:
                idx = var.concat_index()
                if idx is not None and len(idx) != len(self.shape):
                    raise ValueError(
                        f"inconsistent shape length: expected {len(self.shape)}, got {len(idx)}"
                    )
            return
        if self.shape is None:
            self.shape = tuple(cand)
            return
        if len(cand) != len(self.shape):
            raise ValueError(
                f"inconsistent shape length: expected {len(self.shape)}, got {len(cand)}"
            )
        shape_new = None
        for n, s1 in enumerate(self.shape):
            s2 = cand[n]
            if s1 != s2:
                if s1 is None:
                    if shape_new is None:
                        shape_new = list(self.shape)
                    shape_new[n] = s2
                    continue
                if isinstance(s1, OpRange) and isinstance(s2, OpRange) and s1[0] == s2[0]:
                    if s1[1] is None:
                        if shape_new is None:
                            shape_new = list(self.shape)
                        shape_new[n] = s2
                        continue
                    if s2[1] is None:
                        continue
                raise ValueError(f"shape mismatch: expected {self.shape}, got {cand}")
        if shape_new is not None:
            self.shape = tuple(shape_new)

    def push_var(self, var: OpVar, not_reorganize: bool = False) -> None:
        """Push a var (OpVar) into this IndexList using merge logic.

        Parameters
        - var: OpVar to push (negative strides normalized)
        - not_reorganize: if True, skip final reorganize pass
        """
        if not isinstance(var, OpVar):
            raise ValueError(f"Must be OpVar: {type(var)}")

        # Set or validate shape based on the original var (before stride normalization)
        self._ensure_or_set_shape_from_var(var)
        var = IndexList._force_stride_one(var)
        self.push(var.concat_index(), not_reorganize=not_reorganize)

    def remove_var(self, var: OpVar) -> None:
        """Remove ``var`` index coverage from this IndexList.

        Wrapper around ``remove`` that accepts an ``OpVar`` and normalises
        negative strides before delegating.
        """
        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")
        # Set or validate shape based on the original var (before stride normalization)
        self._ensure_or_set_shape_from_var(var)
        var = IndexList._force_stride_one(var)
        self.remove(var.concat_index())

    def intersect_with(self, other: "IndexList") -> "IndexList":
        """Return a new IndexList representing the intersection with ``other``."""
        out = IndexList(dims=list(self.dims))
        # Merge dims conservatively
        out.update_dims_compat(other.dims if other.dims else self.dims)
        # Set shape
        if self.shape is not None:
            out.set_shape(self.shape)
        elif other.shape is not None:
            out.set_shape(other.shape)

        indices = [out._apply_shape_index(idx) for idx in self.indices]
        index_list: List[Optional[AryIndex]] = []
        for index2 in other.indices:
            index2 = out._apply_shape_index(index2)
            for index1 in indices:
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

    def __init__(
        self,
        vars: Optional[List[OpVar]] = None,
        context: Optional[List[Tuple[OpVar, List[OpVar], OpRange]]] = None,
    ):
        """Initialise the container.

        Parameters
        ----------
        vars:
            Optional initial list of variables to populate the set with.
        context:
            Optional context information.
        """

        super().__init__()
        # Internal unified store (name -> IndexList)
        self._store: Dict[str, IndexList] = {}
        self._context: List[Tuple[OpVar, List[OpVar], OpRange]] = context if context is not None else []
        self._guarded: Dict[str, Dict[str, IndexList]] = {}
        self._current_guard: str | None = None
        self._guards: List[str] = []
        self._guard_dict: Dict[str, Operator] = {}

        # Add provided variables while skipping expensive reorganisations.
        if vars is not None:
            for var in vars:
                self.push(var, not_reorganize=True)

    def copy(self) -> VarList:
        """Return a deep copy of the variable list."""

        var_list = VarList()

        # Deep copy the store
        for name in self._store:
            var_list._store[name] = self._store[name].copy()
        for name in self._guarded:
            if name not in var_list._guarded:
                var_list._guarded[name] = {}
            for gname, guarded in self._guarded[name].items():
                var_list._guarded[name][gname] = guarded.copy()
        var_list._current_guard = self._current_guard
        var_list._guards = list(self._guards)
        for gname in self._guard_dict:
            var_list._guard_dict[gname] = self._guard_dict[gname].deep_clone()
        var_list._context = list(self._context)
        return var_list

    def copy_context(self) -> VarList:
        """Return a new variable list with the context copied from self"""
        new_list = VarList(context=self._context)
        new_list._current_guard = self._current_guard
        new_list._guards = list(self._guards)
        for gname in self._guard_dict:
            new_list._guard_dict[gname] = self._guard_dict[gname].deep_clone()
        return new_list

    def clone_var(self, src: str, dst: str) -> None:
        found = False
        if src in self._store:
            self._store[dst] = self._store[src].copy()
            found = True
        if src in self._guarded:
            if dst not in self._guarded:
                self._guarded[dst] = {}
            for gname, guraded in self._guarded[src].items():
                self._guarded[dst][gname] = guraded.copy()
            found = True
        if not found:
            raise ValueError(f"Variable not found: {src}")

    def push_context(self, context: Tuple[OpVar, List[OpVar], OpRange]) -> None:
        if not isinstance(context[0], OpVar):
            raise ValueError(f"Must be OpVar: {type(context[0])}")
        if not isinstance(context[1], list):
            raise ValueError(f"Must be list of OpVar: {type(context[1])}")
        if not isinstance(context[2], OpRange):
            raise ValueError(f"Must be OpRange: {type(context[2])}")
        range = context[2].ascending()
        self._context.append((context[0], context[1], range))

    def pop_context(self) -> None:
        if len(self._context) == 0:
            raise RuntimeError("Context is empty")
        base_var, vars, range = self._context.pop()
        for _, il in self._store.items():
            il.exit_context((base_var, vars, range))

        def _remove_var(guard: Operator, vars: List[OpVar]) -> Operator | None:
            if isinstance(guard, (OpTrue, OpFalse)):
                return guard
            if isinstance(guard, OpNot):
                arg = _remove_var(guard.args[0], vars)
                if arg is None:
                    return None
                if arg is not guard.args[0]:
                    return OpNot([arg])
                return guard
            if not isinstance(guard, OpLogic):
                op_vars = guard.collect_vars()
                for var in vars:
                    if var in op_vars:
                        return None
                return guard
            arg0 = _remove_var(guard.args[0], vars)
            arg1 = _remove_var(guard.args[1], vars)
            if arg0 is None and arg1 is None:
                raise RuntimeError(f"Unexpected guard: {guard}")
            if arg0 is None:
                return arg1
            if arg1 is None:
                return arg0
            if arg0 is not guard.args[0] or arg1 is not guard.args[1]:
                return OpLogic(guard.op, [arg0, arg1])
            return guard
        all_vars = [base_var] + vars
        to_remove: List[str] = []
        for name in self._guarded:
            for gname in list(self._guarded[name].keys()):
                guarded = self._guarded[name][gname]
                guarded.exit_context((base_var, vars, range))
                guard = self._guard_dict[gname]
                guard_new = _remove_var(guard, all_vars)
                if guard_new is not guard: # changed
                    # print("del in pop_content 1", name, gname)
                    del self._guarded[name][gname]
                    if guard_new is not None:
                        gname_new = str(guard_new)
                        self._guarded[name][gname_new] = guarded
                        if gname_new not in self._guard_dict:
                            self._guard_dict[gname_new] = guard_new
                    if gname not in to_remove:
                        to_remove.append(gname)
                else:
                    self._guarded[name][gname] = guarded
        for gname in to_remove:
            del self._guard_dict[gname]

        # drop empty entries
        for name in list(self._store.keys()):
            if not self._store[name].indices:
                del self._store[name]

    def __contains__(self, item: OpVar) -> bool:
        return self.contains_with_context(item, without_context=True)

    def contains_with_context(
        self,
        item: OpVar,
        without_context: bool = False,
    ) -> bool:
        """Explicit membership check with optional VarList-style context.

        Pass a list of (base_var, vars, range) tuples to apply loop context.
        When context is None, behaves the same as the default "in" check.
        """
        if not isinstance(item, OpVar):
            raise ValueError(f"Must be OpVar: {type(item)}")
        name = item.name_ext()
        il = self.get_list(name)
        if il is None:
            return False
        index_item = item.concat_index()
        context = None if without_context else self._context
        return il.contains(index_item, context=context)

    def __str__(self) -> str:
        """Human readable representation used for debugging.

        Render full-array coverage explicitly as ':' for each dimension so
        that strings like 'v(:,:)' appear even when the internal representation
        uses ``None`` for entire coverage.
        """
        parts: List[str] = []
        excl_parts: List[str] = []
        for name in sorted(self._store.keys()):
            il = self._store[name]
            if il.indices:
                parts.extend(il.format_strings(name))
            if il.exclude:
                excl_parts.extend(il.format_exclude_strings(name))
        res = ", ".join(parts)
        if excl_parts:
            res = f"{res}, exclude: {', '.join(excl_parts)}"

        guard: Dict[str, Tuple[List[str], List[str]]] = {}
        for name in sorted(self._guarded.keys()):
            for gname, guarded in self._guarded[name].items():
                if gname not in guard:
                    guard[gname] = ([], [])
                if guarded.indices:
                    guard[gname][0].extend(guarded.format_strings(name))
                if guarded.exclude:
                    guard[gname][1].extend(guarded.format_exclude_strings(name))
        res_total = ["[" + res + "]"]
        for gname in sorted(guard.keys()):
            parts = guard[gname][0]
            excl_parts = guard[gname][1]
            res2 = ", ".join(parts)
            if excl_parts:
                res2 = f"{res2}, exclude: {', '.join(excl_parts)}"
            res2 = "[" + res2 + " @ " + str(gname) + "]"
            res_total.append(res2)
        return " + ".join(res_total)

    def __len__(self) -> int:
        """Return number of variable names tracked."""
        return len(self.names())

    def get_list(self, name: str) -> IndexList | None:
        """Return index list for ``name``."""
        if self._current_guard is not None:
            if name in self._guarded and self._current_guard in self._guarded[name]:
                return self._guarded[name][self._current_guard]
        res: IndexList | None = None
        if name in self._store:
            res = self._store[name].copy()
        if name in self._guarded:
            for guarded in self._guarded[name].values():
                if res is None:
                    res = guarded.copy()
                else:
                    res.merge_from(guarded)
        return res

    def __iter__(self) -> Iterator[OpVar]:
        """Iterate over stored variables as ``OpVar`` objects."""
        for name in self.names():
            il = self.get_list(name)
            if il is not None:
                yield from il.iter_opvars(name)

    def get_vars(self, name: str) -> List[OpVar]:
        """Return a list of stored variables for ``name``."""
        il = self.get_list(name)
        if il is None:
            return []
        return [var for var in il.iter_opvars(name)]

    def names(self) -> List[str]:
        """Return a sorted list of variable names currently stored."""
        names: List[str] = []
        names = [name for name in self._store]
        names.extend([name for name in self._guarded if name not in names])
        names_new: list[str] = []
        for name in names:
            il = self.get_list(name)
            if il is not None and il.indices:
                names_new.append(name)
        return sorted(names_new)

    def has_name(self, name: str) -> bool:
        il = self.get_list(name)
        if il is not None and il.indices:
            return True
        return False

    def dims(self, name: str) -> List[int]:
        return self._store[name].dims

    def remove_name(self, name: str) -> None:
        if name in self._store:
            del self._store[name]
        if name in self._guarded:
            # print("del in remove_name", name)
            del self._guarded[name]

    def merge(self, other: VarList) -> None:
        """Merge variables from ``other`` into this list."""

        if not isinstance(other, VarList):
            raise ValueError(f"other must be VarList: {type(other)}")

        # Per-name merge via IndexList.merge_from + reorganize
        for name in other._store:
            src = other._store[name]
            if name not in self._store:
                self._store[name] = IndexList(dims=list(src.dims))
            dst = self._store[name]
            dst.merge_from(src)
        for name in other._guarded:
            if name in self._guarded:
                for gname, guarded in other._guarded[name].items():
                    if gname not in self._guarded[name]:
                        self._guarded[name][gname] = guarded.copy()
                        if gname not in self._guard_dict:
                            self._guard_dict[gname] = other._guard_dict[gname].deep_clone()
                    else:
                        self._guarded[name][gname].merge_from(guarded)
            else:
                self._guarded[name] = {}
                for gname, guarded in other._guarded[name].items():
                    self._guarded[name][gname] = guarded.copy()
                    if gname not in self._guard_dict:
                        self._guard_dict[gname] = other._guard_dict[gname].deep_clone()

        # Merge current_guard
        if self._current_guard is not None:
            if other._current_guard is not None and self._current_guard != other._current_guard:
                raise RuntimeError("current_guard is not consistent")
        elif other._current_guard is not None:
            gname = other._current_guard
            self._current_guard = gname
            if gname not in self._guard_dict:
                self._guard_dict[gname] = other._guard_dict[gname].deep_clone()

        # Merge context
        for i, cont in enumerate(other._context):
            if (
                len(self._context) <= i
                or self._context[i][0] != cont[0]
                or self._context[i][2] != cont[2]
            ):
                print(self._context)
                print(other._context)
                raise RuntimeError("context is not consistent")
            for var in cont[1]:
                if var not in self._context[i][1]:
                    self._context[i][1].append(var)

    def enter_guard(self, guard: Operator) -> None:
        self._guards.append(self._current_guard)
        if self._current_guard is not None:
            guard = OpLogic(".and.", [self._guard_dict[self._current_guard], guard])
        self._current_guard = str(guard)
        if self._current_guard not in self._guard_dict:
            self._guard_dict[self._current_guard] = guard.deep_clone()

    def leave_guard(self) -> None:
        self._current_guard = self._guards.pop()

    # _force_stride_one moved to IndexList
    def push(self, var: OpVar, not_reorganize: bool = False) -> None:
        """Add ``var`` to the list, merging overlapping indices."""

        if not isinstance(var, OpVar):
            raise ValueError(f"Must be OpVar: {type(var)}")
        if var.name == var.macro_name:
            return

        name = var.name_ext()
        dims = var.ndims_ext()
        index = var.concat_index()

        # Update dims and shape info
        if name not in self._store:
            self._store[name] = IndexList(dims=list(dims))
        else:
            if self._store[name].dims:
                self._store[name].update_dims_compat(dims)
            else:
                self._store[name].set_dims(dims)
        self._store[name]._ensure_or_set_shape_from_var(var)

        # Delegate push to IndexList
        if self._current_guard is None:
            self._store[name].push_var(var, not_reorganize=not_reorganize)
            if name in self._guarded:
                for gname, guarded in self._guarded[name].items():
                    guarded.push_var(var, not_reorganize=not_reorganize)
            return
        found = False
        if name not in self._guarded:
            self._guarded[name] = {}
        else:
            for gname in list(self._guarded[name].keys()):
                guarded = self._guarded[name][gname]
                if gname == self._current_guard:
                    guarded.push_var(var, not_reorganize=not_reorganize)
                    found = True
                    continue
                if not guarded or guarded.contains(index, self._context):
                    if guarded:
                        self._store[name].merge_from(guarded)
                    # print("del in push", name, gname)
                    del self._guarded[name][gname]
        if not found:
            guarded = self._store[name].copy()
            guarded.push_var(var, not_reorganize=not_reorganize)
            self._guarded[name][self._current_guard] = guarded

    def remove(self, var) -> None:
        """Remove ``var`` from the list, splitting ranges as needed."""

        if not isinstance(var, OpVar):
            raise ValueError("Must be OpVar")
        if var.name == var.macro_name:
            return

        name = var.name_ext()
        index = var.concat_index()

        if name not in self._store and name not in self._guarded:
            return

        # Delegate remove to IndexList
        if self._current_guard is None:
            if name in self._store:
                self._store[name].remove_var(var)
            if name in self._guarded:
                for guarded in self._guarded[name].values():
                    guarded.remove_var(var)
        else:
            if name not in self._guarded:
                self._guarded[name] = {}
            found = False
            for gname in list(self._guarded[name].keys()):
                guarded = self._guarded[name][gname]
                if gname == self._current_guard:
                    guarded.remove_var(var)
                    found = True
                    continue
                if guarded.contains(index, self._context):
                    if name in self._store:
                        self._store[name].merge_from(guarded)
                    else:
                        self._store[name] = guarded
                    # print("del in remove 1", name, gname)
                    del self._guarded[name][gname]
            if not found:
                guarded = self._store[name].copy()
                guarded.remove_var(var)
                self._guarded[name][self._current_guard] = guarded

        # cleanup if empty
        if name in self._guarded and not self._guarded[name]:
            # print("del in remove 2", name)
            del self._guarded[name]
        if name in self._store and not self._store[name].indices and name not in self._guarded:
            del self._store[name]

    def __and__(self, other: VarList) -> VarList:
        """Return intersection of this list with ``other``."""

        if not isinstance(other, VarList):
            raise ValueError(f"Must be VarList: {type(other)}")

        var_list = VarList()

        for name in other.names():
            il_self = self.get_list(name)
            if il_self is None or not il_self.indices:
                continue
            il_other = other.get_list(name)
            if il_other is None or not il_other.indices:
                continue
            inter = il_self.intersect_with(il_other)
            if inter.indices:
                var_list._store[name] = inter
        return var_list
