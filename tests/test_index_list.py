import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.operators import (
    AryIndex,
    Operator,
    OpInt,
    OpNeg,
    OpRange,
    OpVar,
    VarType,
)
from fautodiff.var_list import IndexList, VarList


def v(name, *idx_dims) -> OpVar:
    if idx_dims:
        dims = []
        for d in idx_dims:
            if d is None:
                dims.append(None)
            elif isinstance(d, int):
                dims.append(OpInt(d))
            elif isinstance(d, tuple):

                def to_op(item):
                    if isinstance(item, Operator):
                        return item
                    if item is None:
                        return None
                    if isinstance(item, int):
                        return OpInt(item)
                    raise ValueError(f"invalid item in tuple for idx dim: {item}")

                start = to_op(d[0])
                end = to_op(d[1])
                if len(d) > 2:
                    step = to_op(d[2])
                else:
                    step = None
                dims.append(OpRange([start, end, step]))
            elif isinstance(d, Operator):
                dims.append(d)
            else:
                raise ValueError(f"invalid idx dim: {d}")
        index = AryIndex(dims)
    else:
        index = None

    var_type = VarType("real")
    var = OpVar(name, index=index, var_type=var_type)
    if index:
        var.dims = tuple([None] * len(index))
        var.dims_raw = tuple([":"] * len(index))
    return var


class TestIndexList(unittest.TestCase):
    def test_var_in_None(self):
        il = IndexList()
        i = AryIndex([OpVar("i")])
        il.push(None)
        self.assertTrue(il.contains(i))

    def test_remove_var(self):
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpInt(1)]))
        il.push(AryIndex([i]))
        il.remove(AryIndex([i]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "1")
        self.assertEqual(len(il.exclude), 1)
        self.assertEqual(str(il.exclude[0]), "i")

    def test_push_in_range(self):
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([i - 1, i + 1])]))
        il.push(AryIndex([i]))
        self.assertEqual(str(il), "(i - 1:i + 1)")

    def test_push_wider_range(self):
        il = IndexList([AryIndex([OpInt(1)])])
        il.push(AryIndex([OpRange([OpInt(1),OpInt(2)])]))
        self.assertEqual(str(il), "(1:2)")

    def test_push_merge(self):
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([i - 2, i - 1])]))
        il.push(AryIndex([OpRange([i + 1, i + 2])]))
        il.push(AryIndex([i]))
        self.assertEqual(str(il), "(i - 2:i + 2)")

    def test_merge_wider_range(self):
        il1 = IndexList([AryIndex([OpInt(1)])])
        il2 = IndexList([AryIndex([OpRange([OpInt(1),OpInt(2)])])])
        il1.merge_from(il2)
        self.assertEqual(str(il1), "(1:2)")

    def test_push_merge_adjacent_int_and_range(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))
        il.push(AryIndex([OpInt(11)]))  # adjacent scalar
        # Contains endpoints of merged range
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpInt(11)])))

    def test_remove_int_from_range(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(2)])]))
        il.remove(AryIndex([OpInt(2)]))
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertFalse(il.contains(AryIndex([OpInt(2)])))

    def test_remove_subslice_splits(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))
        il.remove(AryIndex([OpRange([OpInt(3), OpInt(7)])]))
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpInt(10)])))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))

    def test_contains_scalar_vs_range(self):
        il = IndexList()
        il.push(AryIndex([OpInt(5)]))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)])])))

    def test_intersection(self):
        il1 = IndexList()
        il1.push(AryIndex([OpRange([OpInt(1), OpInt(4), OpInt(1)])]))
        il2 = IndexList()
        il2.push(AryIndex([OpRange([OpInt(3), OpInt(5), OpInt(1)])]))
        inter = il1.intersect_with(il2)
        self.assertTrue(any(isinstance(idx[0], OpRange) for idx in inter.indices))
        # intersection should contain 3 and 4
        self.assertTrue(inter.contains(AryIndex([OpInt(3)])))
        self.assertTrue(inter.contains(AryIndex([OpInt(4)])))
        self.assertFalse(inter.contains(AryIndex([OpInt(2)])))

    def test_negative_stride_normalized_in_push(self):
        i0 = OpInt(1)
        i5 = OpInt(5)
        il = IndexList()
        il.push_var(OpVar("S", index=AryIndex([OpRange([i5, i0, OpInt(-1)])])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(5)])])))

    def test_negative_stride_coverage(self):
        il = IndexList()
        il.push_var(v("A", (10, 1, -1)))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(2), OpInt(8)])])))
        self.assertFalse(il.contains(AryIndex([OpInt(11)])))

    def test_remove_with_negative_stride(self):
        il = IndexList()
        il.push_var(v("A", (1, 10)))
        il.remove_var(v("A", (5, 1, -1)))
        self.assertFalse(il.contains(AryIndex([OpInt(3)])))
        self.assertTrue(il.contains(AryIndex([OpInt(6)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(6), OpInt(10)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_adjacent_integers(self):
        il = IndexList()
        il.push(AryIndex([OpInt(1)]))
        il.push(AryIndex([OpInt(3)]))
        il.push(AryIndex([OpInt(2)]))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(3)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_adjacent_ranges(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(5)])]))
        il.push(AryIndex([OpRange([OpInt(6), OpInt(10)])]))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_adjacent_ranges_scalar(self):
        i = OpVar("i")
        il = IndexList()
        il.push(AryIndex([OpRange([i - OpInt(1), i + OpInt(1)])]))
        il.push(AryIndex([i - OpInt(2)]))
        self.assertTrue(il.contains(AryIndex([OpRange([i - OpInt(2), i + OpInt(1)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_overlapping_ranges(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(7)])]))
        il.push(AryIndex([OpRange([OpInt(5), OpInt(12)])]))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(12)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_integer_into_range(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(2), OpInt(5)])]))
        il.push(AryIndex([OpInt(1)]))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(5)])])))
        il.push(AryIndex([OpInt(6)]))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(6)])])))

    def test_remove_from_range(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))
        il.remove(AryIndex([OpInt(5)]))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpInt(4)])))
        self.assertTrue(il.contains(AryIndex([OpInt(6)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(4)])])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(6), OpInt(10)])])))
        self.assertEqual(len(il.indices), 2)

    def test_remove_sub_range(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))
        il.remove(AryIndex([OpRange([OpInt(4), OpInt(6)])]))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpInt(3)])))
        self.assertTrue(il.contains(AryIndex([OpInt(7)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(3)])])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(7), OpInt(10)])])))

    def test_remove_from_ends_of_range(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))
        il.remove(AryIndex([OpInt(1)]))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(2), OpInt(10)])])))
        il.remove(AryIndex([OpInt(10)]))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(2), OpInt(9)])])))

    def test_exclude_from_full_coverage(self):
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpInt(5)]))
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpInt(10)])))
        # exclude list should contain 5
        self.assertTrue(any(str(idx[0]) == "5" for idx in il.exclude))

    def test_push_overwrites_exclude(self):
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpInt(5)]))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        # now include it back
        il.push(AryIndex([OpInt(5)]))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))
        # and exclude should no longer contain that exact index
        self.assertFalse(any(str(idx[0]) == "5" for idx in il.exclude))

    def test_multi_dimensional(self):
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(5)]), OpInt(1)]))
        il.push(AryIndex([OpRange([OpInt(6), OpInt(10)]), OpInt(1)]))
        # contains combined in first dim, fixed 1 in second dim
        self.assertTrue(
            il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)]), OpInt(1)]))
        )
        # different second dim should not be contained
        self.assertFalse(
            il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)]), OpInt(2)]))
        )

    def test_exit_context_and_remove(self):
        il = IndexList()
        il.dims = [2]

        i = OpVar("i")
        n = OpVar("n")
        il.push(AryIndex([OpRange([OpInt(1), OpInt(2)]), i]))
        il.exit_context((i, [i], OpRange([OpInt(1), n])))
        il.remove(AryIndex([OpInt(1), i]))
        il.exit_context((i, [i], OpRange([OpInt(1), n])))
        self.assertEqual([str(idx) for idx in il.indices], ["2,1:n"])
        self.assertEqual(il.exclude, [])

    def test_contains_with_context_scalar_substitution(self):
        # Stored numeric range; query contains symbolic i. Context should decide membership.
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))

        # Without context: conservative overlap assumed
        self.assertTrue(il.contains(AryIndex([i])))

        # With i=11 (outside): not contained
        ctx_out = [(i, [], OpRange([OpInt(11), OpInt(11)]))]
        self.assertFalse(il.contains(AryIndex([i]), context=ctx_out))

        # With i=5 (inside): contained
        ctx_in = [(i, [], OpRange([OpInt(5), OpInt(5)]))]
        self.assertTrue(il.contains(AryIndex([i]), context=ctx_in))

    def test_contains_with_context_resolves_symbolic_dim(self):
        # Second dimension is symbolic j; it should match only with the same j from context
        il = IndexList()
        j = OpVar("j")
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)]), j]))

        # Query a concrete index in second dim = 1
        idx_item = AryIndex([OpInt(5), OpInt(1)])

        # With context j=1 -> contained
        ctx1 = [(j, [], OpRange([OpInt(1), OpInt(1)]))]
        self.assertTrue(il.contains(idx_item, context=ctx1))

        # With context j=2 -> not contained
        ctx2 = [(j, [], OpRange([OpInt(2), OpInt(2)]))]
        self.assertFalse(il.contains(idx_item, context=ctx2))

    def test_contains_with_context_range_substitution(self):
        # Stored range depends on loop var i; providing i as a range should expand bounds accordingly
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([i - OpInt(1), i + OpInt(1)])]))

        # Context: i iterates from 2 to 4, effective coverage becomes 1:5
        ctx = [(i, [], OpRange([OpInt(2), OpInt(4)]))]

        self.assertTrue(il.contains(AryIndex([OpInt(1)]), context=ctx))
        self.assertTrue(il.contains(AryIndex([OpInt(5)]), context=ctx))
        self.assertFalse(il.contains(AryIndex([OpInt(0)]), context=ctx))
        # Range fully inside should also be contained
        self.assertTrue(
            il.contains(AryIndex([OpRange([OpInt(3), OpInt(4)])]), context=ctx)
        )

    def test_exclude_check_uses_context(self):
        # Entire coverage with an exclude that depends on a symbol; context should exclude concretized index
        il = IndexList()
        # cover entire 1D array
        il.push(None)
        i = OpVar("i")
        il.add_exclude(AryIndex([i]))

        # Without context, not excluded
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))

        # With context i=5, excluded
        ctx = [(i, [], OpRange([OpInt(5), OpInt(5)]))]
        self.assertFalse(il.contains(AryIndex([OpInt(5)]), context=ctx))

    def test_contains_with_context_exclude_partial_overlap(self):
        # indices: 1:10, exclude provided via context covers 3:5 only.
        # query 2:7 should still be contained because 2 and 6:7 remain.
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))
        il.add_exclude(AryIndex([i]))

        q = AryIndex([OpRange([OpInt(2), OpInt(7)])])
        # Without context, conservative overlap -> True
        self.assertTrue(il.contains(q))

        # With context i=3:5, only part of q is excluded; remainder overlaps indices
        ctx = [(i, [], OpRange([OpInt(3), OpInt(5)]))]
        self.assertTrue(il.contains(q, context=ctx))

        # Fully excluded subrange should not be contained
        q2 = AryIndex([OpRange([OpInt(3), OpInt(5)])])
        self.assertFalse(il.contains(q2, context=ctx))

        # Scalar inside the excluded region should not be contained
        self.assertFalse(il.contains(AryIndex([OpInt(4)]), context=ctx))

    def test_apply_context_index_slices_vars_only_for_indices(self):
        # _apply_context_index should slice vars for indices but not for excludes
        i = OpVar("i")
        j = OpVar("j")
        idx = AryIndex([i, j])
        il = IndexList([idx])
        ctx = [(i, [j], OpRange([OpInt(1), OpInt(3)]))]

        # For indices (for_exclude=False): i -> 1:3, j -> :
        idx_for_indices = il._apply_context_index(idx, ctx, for_exclude=False)
        self.assertEqual(str(idx_for_indices), "1:3,:")

        # For excludes (for_exclude=True): i -> 1:3, j unchanged
        idx_for_exclude = il._apply_context_index(idx, ctx, for_exclude=True)
        self.assertEqual(str(idx_for_exclude), "1:3,j")

    def test_exit_context_converts_exclude_to_residual(self):
        # Push 1, exclude i, then exit context with i=1:n -> exclude should become 2:n
        il = IndexList()
        il.push(AryIndex([OpInt(1)]))
        i = OpVar("i")
        n = OpVar("n")
        il.remove(AryIndex([i]))
        il.exit_context((i, [], OpRange([OpInt(1), n])))
        # Remaining exclude should be 2:n
        self.assertEqual(len(il.exclude), 1)
        self.assertTrue(str(il.exclude[0]) == "2:n")

    def test_full_coverage_remove_range_then_push_endpoint_updates_exclude(self):
        # Start with full coverage, remove 1:n, then push n -> exclude becomes 1:n-1
        il = IndexList()
        il.push(None)
        n = OpVar("n")
        il.remove(AryIndex([OpRange([OpInt(1), n])]))
        # Sanity: exclude initially 1:n
        self.assertEqual(len(il.exclude), 1)
        self.assertEqual(str(il.exclude[0]), "1:n")
        # Push endpoint n
        il.push(AryIndex([n]))
        # Expect exclude to be a single range 1:n - 1
        self.assertEqual(len(il.exclude), 1)
        self.assertEqual(str(il.exclude[0]), "1:n - 1")

    def test_full_coverage_remove_range_then_push_lower_endpoint_updates_exclude(self):
        # Start with 0, remove 1:n, then push 1 -> exclude becomes 2:n
        il = IndexList()
        il.push(AryIndex([OpInt(0)]))
        n = OpVar("n")
        il.remove(AryIndex([OpRange([OpInt(1), n])]))
        # Push lower endpoint 1
        il.push(AryIndex([OpInt(1)]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "0:1")
        self.assertEqual(len(il.exclude), 1)
        self.assertEqual(str(il.exclude[0]), "2:n")

    def test_contains_none_respects_partial_exclude(self):
        # With full coverage and a partial exclude, contains(None) should be True
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpRange([OpInt(1), OpInt(3)])]))
        # Entire query not fully excluded -> True
        self.assertTrue(il.contains(None))
        # Query inside excluded region -> False
        self.assertFalse(il.contains(AryIndex([OpInt(2)])))
        # Query outside excluded region -> True
        self.assertTrue(il.contains(AryIndex([OpInt(4)])))

    def test_remove_2d_subslice_and_contains(self):
        # In 2D, removing a subrange on one dim should split coverage accordingly
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)]), OpInt(1)]))
        il.remove(AryIndex([OpRange([OpInt(3), OpInt(5)]), OpInt(1)]))
        # Removed center band in first dim; should not contain (4,1), but contain (2,1) and (6,1)
        self.assertFalse(il.contains(AryIndex([OpInt(4), OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpInt(2), OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpInt(6), OpInt(1)])))

    def test_push_range_then_remove_range_with_exit_context(self):
        # Push numeric range, remove symbolic range, concretize via exit_context
        il = IndexList()
        il.push(AryIndex([OpRange([OpInt(1), OpInt(10)])]))
        i = OpVar("i")
        # Remove range i:i+2 (symbolic for now)
        il.remove(AryIndex([OpRange([i, i + OpInt(2)])]))
        # After concretizing i=3..4, removed region becomes 3:6
        il.exit_context((i, [], OpRange([OpInt(3), OpInt(4)])))
        # Check membership around the removed band
        self.assertTrue(il.contains(AryIndex([OpInt(2)])))
        self.assertFalse(il.contains(AryIndex([OpInt(3)])))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        self.assertFalse(il.contains(AryIndex([OpInt(6)])))
        self.assertTrue(il.contains(AryIndex([OpInt(7)])))
        # Holes are realized in indices; exclude list should be empty
        self.assertEqual(il.exclude, [])

    def test_push_symbolic_range_then_remove_symbolic_range_with_exit_context(self):
        # Push i-1:i+1 with i in [2:4] -> coverage 1:5
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([i - OpInt(1), i + OpInt(1)])]))
        il.exit_context((i, [], OpRange([OpInt(2), OpInt(4)])))
        # Remove j-1:j+1 with j=3 -> removes 2:4
        j = OpVar("j")
        il.remove(AryIndex([OpRange([j - OpInt(1), j + OpInt(1)])]))
        il.exit_context((j, [], OpRange([OpInt(3), OpInt(3)])))
        # Expect only endpoints 1 and 5 to remain
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertFalse(il.contains(AryIndex([OpInt(3)])))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))
        self.assertEqual(il.exclude, [])

    def test_push_symbolic_range_then_push_endpoint_keeps_single_range(self):
        # Push (n-1:n) then push n; should keep a single range (n-1:n)
        il = IndexList()
        n = OpVar("n")
        il.push(AryIndex([OpRange([n - OpInt(1), n])]))
        il.push(AryIndex([n]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "n - 1:n")

    def test_shape_full_then_remove_scalar_exit_context_clears_all(self):
        # shape = (1:n), push None (full), remove i, exit_context with i=1:n -> everything cleared
        il = IndexList()
        n = OpVar("n")
        i = OpVar("i")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(None)
        il.remove(AryIndex([i]))
        il.exit_context((i, [], OpRange([OpInt(1), n])))
        self.assertEqual(il.indices, [])
        self.assertEqual(il.exclude, [])

    # ---- shape-aware behavior ----
    def test_shape_full_range_treated_as_full_coverage(self):
        # If shape says 1:n, pushing exactly 1:n should be treated as full coverage
        il = IndexList()
        n = OpVar("n")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(AryIndex([OpRange([OpInt(1), n])]))
        # full coverage stored as None
        self.assertEqual(il.indices, [None])

    def test_shape_contains_limits_domain(self):
        # With full coverage but shape 1:n, queries outside [1,n] are not contained
        il = IndexList()
        n = OpVar("n")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(None)  # full coverage
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([n])))
        # 0 is outside lower bound
        self.assertFalse(il.contains(AryIndex([OpInt(0)])))
        # n+1 is outside upper bound
        self.assertFalse(il.contains(AryIndex([n + OpInt(1)])))

    def test_shape_remove_full_shape_from_full_coverage_clears_indices(self):
        # Start with full coverage, remove exactly the full shape -> indices cleared
        il = IndexList()
        n = OpVar("n")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(None)
        il.remove(AryIndex([OpRange([OpInt(1), n])]))
        self.assertEqual(il.indices, [])
        self.assertEqual(il.exclude, [])

    def test_shape_2d_limits(self):
        # 2D shape limits containment checks in both dimensions
        il = IndexList()
        n = OpVar("n")
        m = OpVar("m")
        il.set_shape((OpRange([OpInt(1), n]), OpRange([OpInt(1), m])))
        il.push(None)
        # Inside
        self.assertTrue(il.contains(AryIndex([OpInt(1), OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([n, m])))
        # Outside in first dim
        self.assertFalse(il.contains(AryIndex([OpInt(0), OpInt(1)])))
        # Outside in second dim
        self.assertFalse(il.contains(AryIndex([OpInt(1), m + OpInt(1)])))

    def test_shape_2d_full_then_remove_pair_then_exit_keeps_partial_exclude_then_clears(
        self,
    ):
        # shape = (1:n, 1:m), push None (full), remove (i,j)
        # After exit_context i=1:n, exclude should contain (1:n,j)
        # After exit_context j=1:m, everything clears
        il = IndexList()
        n = OpVar("n")
        m = OpVar("m")
        i = OpVar("i")
        j = OpVar("j")
        il.set_shape((OpRange([OpInt(1), n]), OpRange([OpInt(1), m])))
        il.push(None)
        il.remove(AryIndex([i, j]))
        il.exit_context((i, [], OpRange([OpInt(1), n])))
        # exclude should contain (1:n,j)
        self.assertTrue(any(str(e) == "1:n,j" for e in il.exclude))
        il.exit_context((j, [], OpRange([OpInt(1), m])))
        self.assertEqual(il.indices, [])
        self.assertEqual(il.exclude, [])

    def test_shape_push_none_then_remove_endpoint_static(self):
        # shape=(1:2), push None -> coverage = 1:2, remove 2 -> indices=1, exclude=[]
        il = IndexList()
        il.set_shape((OpRange([OpInt(1), OpInt(2)]),))
        il.push(None)
        il.remove(AryIndex([OpInt(2)]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "1")
        self.assertEqual(il.exclude, [])

    def test_shape_push_none_then_remove_endpoint_static_2d(self):
        # shape=(1:2, 1:n), push None -> coverage = 1:2, remove (2,:) -> indices=(1,:), exclude=[]
        il = IndexList()
        il.set_shape(
            (
                OpRange([OpInt(1), OpInt(2)]),
                OpRange([OpInt(1), OpVar("n")]),
            )
        )
        il.push(None)
        il.remove(AryIndex([OpInt(2), None]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "1,:")
        self.assertEqual(il.exclude, [])

    def test_shape_push_full1d_then_remove_endpoint_static_2d(self):
        # shape=(1:2, 1:n), push (:,i), remove (2,i) -> indices=(1,i), exclude=[]
        il = IndexList()
        il.set_shape(
            (
                OpRange([OpInt(1), OpInt(2)]),
                OpRange([OpInt(1), OpVar("n")]),
            )
        )
        il.push(AryIndex([None, OpVar("i")]))
        il.remove(AryIndex([OpInt(2), OpVar("i")]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "1,i")
        self.assertEqual(il.exclude, [])

    def test_shape_push_none_then_remove_outside_raises(self):
        # shape=(1:2), push None, remove 3 -> error
        il = IndexList()
        il.set_shape((OpRange([OpInt(1), OpInt(2)]),))
        il.push(None)
        with self.assertRaises(ValueError):
            il.remove(AryIndex([OpInt(3)]))

    def test_shape_push_none_then_remove_first_symbolic(self):
        # shape=(1:n), push None, remove 1 -> indices = 2:n, exclude = []
        il = IndexList()
        n = OpVar("n")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(None)
        il.remove(AryIndex([OpInt(1)]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "2:n")
        self.assertEqual(il.exclude, [])

    def test_shape_push_none_remove_symbolic_then_exit_context_keeps_ends(self):
        # shape=(1:n), push None, remove i, exit_context i=2:n-1
        # -> indices should be [1, n], exclude should be empty
        il = IndexList()
        n = OpVar("n")
        i = OpVar("i")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(None)
        il.remove(AryIndex([i]))
        il.exit_context((i, [], OpRange([OpInt(2), n - OpInt(1)])))
        self.assertEqual(len(il.indices), 2)
        self.assertEqual(set(str(idx) for idx in il.indices), {"1", "n"})
        self.assertEqual(il.exclude, [])

    def test_shape_push_ends_then_remove_middle_contains_none(self):
        # shape=(1:n), (1) and (n), exclude (2:n-1) -> contains(None) should be True
        il = IndexList()
        n = OpVar("n")
        i = OpVar("i")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(AryIndex([OpInt(1)]))
        il.push(AryIndex([n]))
        il.exclude = [AryIndex([OpRange([OpInt(2), n-1])])]
        # Coverage remains (1) (n); contains([None]) should be True
        self.assertTrue(il.contains(AryIndex([None])))

    def test_shape_push_none_then_remove_middle_symbolic_exclude(self):
        # shape=(1:n), push None, remove 2 -> indices = 1:n, exclude = (2)
        il = IndexList()
        n = OpVar("n")
        il.set_shape((OpRange([OpInt(1), n]),))
        il.push(None)
        il.remove(AryIndex([OpInt(2)]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(il.indices[0], None)
        self.assertEqual(len(il.exclude), 1)
        self.assertEqual(str(il.exclude[0]), "2")

    def test_shape_push_first_then_remove_other_keeps_indices_only(self):
        # shape=(1:4), push (1), remove (2) -> indices=(1), exclude=[]
        il = IndexList()
        il.set_shape((OpRange([OpInt(1), OpInt(4)]),))
        il.push(AryIndex([OpInt(1)]))
        il.remove(AryIndex([OpInt(2)]))
        self.assertEqual(len(il.indices), 1)
        self.assertEqual(str(il.indices[0]), "1")
        self.assertEqual(il.exclude, [])

    def test_push_scalar_lower_endpoint_trims_exclude(self):
        # Guard removes a lower slice; re-adding endpoint should trim the hole.
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpRange([OpInt(1), OpInt(7)])]))
        self.assertEqual([str(ex) for ex in il.exclude], ["1:7"])

        il.push(AryIndex([OpInt(1)]))
        self.assertEqual([str(ex) for ex in il.exclude], ["2:7"])

    def test_push_scalar_upper_endpoint_trims_exclude(self):
        # Guard removes an upper slice; re-adding endpoint should trim from the top.
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpRange([OpInt(1), OpInt(7)])]))
        self.assertEqual([str(ex) for ex in il.exclude], ["1:7"])

        il.push(AryIndex([OpInt(7)]))
        self.assertEqual([str(ex) for ex in il.exclude], ["1:6"])

    def test_push_scalar_middle_splits_exclude(self):
        # Guard leaves a band hole; re-adding a middle scalar should split it.
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpRange([OpInt(1), OpInt(7)])]))

        il.push(AryIndex([OpInt(4)]))
        self.assertEqual([str(ex) for ex in il.exclude], ["1:3", "5:7"])

    def test_push_range_eliminates_matching_exclude(self):
        # Guard carved out a range; pushing the same range should clear the guard hole.
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpRange([OpInt(1), OpInt(7)])]))
        self.assertEqual([str(ex) for ex in il.exclude], ["1:7"])

        il.push(AryIndex([OpRange([OpInt(1), OpInt(7)])]))
        self.assertEqual(il.exclude, [])

    def test_remove_range_keeps_full_coverage_entry(self):
        # Removing inside a guard should keep the full-coverage entry but record the gap.
        il = IndexList()
        il.push(None)
        il.remove(AryIndex([OpRange([OpInt(3), OpInt(6)])]))

        self.assertEqual(il.indices, [None])
        self.assertEqual([str(ex) for ex in il.exclude], ["3:6"])
        self.assertFalse(il.contains(AryIndex([OpInt(4)])))
        self.assertTrue(il.contains(AryIndex([OpInt(2)])))


if __name__ == "__main__":
    unittest.main()
