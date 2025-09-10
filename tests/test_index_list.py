import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.operators import AryIndex, OpInt, OpNeg, OpRange, OpVar, VarType, Operator
from fautodiff.var_list import IndexList


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
        var.dims = (":",) * len(index)
    return var


def push(il: IndexList, name: str, var: OpVar, not_reorganize: bool = False):
    # Use self-callback for reorganize replay (AryIndex based)
    il.push_var(var, not_reorganize)
    if not not_reorganize:
        il.reorganize()


class TestIndexList(unittest.TestCase):
    def test_exit_context_and_remove(self):
        il = IndexList()
        il.dims = [2]

        i = OpVar("i")
        n = OpVar("n")
        il.push(AryIndex([OpRange([OpInt(1), OpInt(2)]), i]))
        il.exit_context((i, [i], OpRange([OpInt(1), n])))
        il.remove_index(AryIndex([OpInt(1), i]))
        il.exit_context((i, [i], OpRange([OpInt(1), n])))
        self.assertEqual([str(idx) for idx in il.indices], ["2,1:n"])
        self.assertEqual(il.exclude, [])

    def test_push_in_range(self):
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([i-1,i+1])]))
        il.push(AryIndex([i]))
        self.assertEqual(str(il), "(i - 1:i + 1)")

    def test_push_merge(self):
        il = IndexList()
        i = OpVar("i")
        il.push(AryIndex([OpRange([i-2,i-1])]))
        il.push(AryIndex([OpRange([i+1,i+2])]))
        il.push(AryIndex([i]))
        self.assertEqual(str(il), "(i - 2:i + 2)")

    def test_push_merge_adjacent_int_and_range(self):
        il = IndexList()
        push(il, "B", v("B", (1, 10)))
        push(il, "B", v("B", 11))  # adjacent scalar
        # Contains endpoints of merged range
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpInt(11)])))

    def test_remove_int_from_range(self):
        il = IndexList()
        push(il, "C", v("C", (1, 2)))
        il.remove_var(v("C", 2))
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertFalse(il.contains(AryIndex([OpInt(2)])))

    def test_remove_subslice_splits(self):
        il = IndexList()
        push(il, "C", v("C", (1, 10)))
        il.remove_var(v("C", (3, 7)))
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpInt(10)])))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))

    def test_contains_scalar_vs_range(self):
        il = IndexList()
        push(il, "E", v("E", 5))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)])])))

    def test_intersection(self):
        il1 = IndexList()
        push(il1, "v", OpVar("v", index=AryIndex([OpRange([OpInt(1), OpInt(4), OpInt(1)])])))
        il2 = IndexList()
        push(il2, "v", OpVar("v", index=AryIndex([OpRange([OpInt(3), OpInt(5), OpInt(1)])])))
        inter = il1.intersect_with(il2)
        self.assertTrue(any(isinstance(idx[0], OpRange) for idx in inter.indices))
        # intersection should contain 3 and 4
        self.assertTrue(inter.contains(AryIndex([OpInt(3)])))
        self.assertTrue(inter.contains(AryIndex([OpInt(4)])))
        self.assertFalse(inter.contains(AryIndex([OpInt(2)])))

    def test_update_index_upward(self):
        il = IndexList()
        push(il, "a", OpVar("a", index=AryIndex([OpRange([OpInt(1), OpInt(3)])])))
        il.update_index_upward(0, OpRange([OpInt(1), OpInt(5)]))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))

    def test_update_index_downward(self):
        il = IndexList(indices=[AryIndex([OpInt(1)])])
        i = OpVar("i")
        il.update_index_downward(0, 1, i)
        self.assertTrue(all(str(idx[0]) == "i" for idx in il.indices))

    def test_negative_stride_normalized_in_push(self):
        i0 = OpInt(1)
        i5 = OpInt(5)
        il = IndexList()
        push(il, "S", OpVar("S", index=AryIndex([OpRange([i5, i0, OpInt(-1)])])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(5)])])))

    def test_negative_stride_coverage(self):
        il = IndexList()
        push(il, "A", v("A", (10, 1, -1)))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(2), OpInt(8)])])))
        self.assertFalse(il.contains(AryIndex([OpInt(11)])))

    def test_remove_with_negative_stride(self):
        il = IndexList()
        push(il, "A", v("A", (1, 10)))
        il.remove_var(v("A", (5, 1, -1)))
        self.assertFalse(il.contains(AryIndex([OpInt(3)])))
        self.assertTrue(il.contains(AryIndex([OpInt(6)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(6), OpInt(10)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_adjacent_integers(self):
        il = IndexList()
        push(il, "A", v("A", 1))
        push(il, "A", v("A", 3))
        push(il, "A", v("A", 2))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(3)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_adjacent_ranges(self):
        il = IndexList()
        push(il, "A", v("A", (1, 5)))
        push(il, "A", v("A", (6, 10)))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_overlapping_ranges(self):
        il = IndexList()
        push(il, "A", v("A", (1, 7)))
        push(il, "A", v("A", (5, 12)))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(12)])])))
        self.assertEqual(len(il.indices), 1)

    def test_merge_integer_into_range(self):
        il = IndexList()
        push(il, "A", v("A", (2, 5)))
        push(il, "A", v("A", 1))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(5)])])))
        push(il, "A", v("A", 6))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(6)])])))

    def test_remove_from_range(self):
        il = IndexList()
        push(il, "A", v("A", (1, 10)))
        il.remove_var(v("A", 5))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpInt(4)])))
        self.assertTrue(il.contains(AryIndex([OpInt(6)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(4)])])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(6), OpInt(10)])])))
        self.assertEqual(len(il.indices), 2)

    def test_remove_sub_range(self):
        il = IndexList()
        push(il, "A", v("A", (1, 10)))
        il.remove_var(v("A", (4, 6)))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpInt(3)])))
        self.assertTrue(il.contains(AryIndex([OpInt(7)])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(3)])])))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(7), OpInt(10)])])))

    def test_remove_from_ends_of_range(self):
        il = IndexList()
        push(il, "A", v("A", (1, 10)))
        il.remove_var(v("A", 1))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(2), OpInt(10)])])))
        il.remove_var(v("A", 10))
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(2), OpInt(9)])])))

    def test_exclude_from_full_coverage(self):
        il = IndexList()
        push(il, "A", v("A", None))
        il.remove_var(v("A", 5))
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        self.assertTrue(il.contains(AryIndex([OpInt(10)])))
        # exclude list should contain 5
        self.assertTrue(any(str(idx[0]) == "5" for idx in il.exclude))

    def test_push_overwrites_exclude(self):
        il = IndexList()
        push(il, "A", v("A", None))
        il.remove_var(v("A", 5))
        self.assertFalse(il.contains(AryIndex([OpInt(5)])))
        # now include it back
        push(il, "A", v("A", 5))
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))
        # and exclude should no longer contain that exact index
        self.assertFalse(any(str(idx[0]) == "5" for idx in il.exclude))

    def test_multi_dimensional(self):
        il = IndexList()
        push(il, "A", v("A", (1, 5), 1))
        push(il, "A", v("A", (6, 10), 1))
        # contains combined in first dim, fixed 1 in second dim
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)]), OpInt(1)])))
        # different second dim should not be contained
        self.assertFalse(il.contains(AryIndex([OpRange([OpInt(1), OpInt(10)]), OpInt(2)])))

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
        self.assertTrue(il.contains(AryIndex([OpRange([OpInt(3), OpInt(4)])]), context=ctx))

    def test_exclude_check_uses_context(self):
        # Entire coverage with an exclude that depends on a symbol; context should exclude concretized index
        il = IndexList()
        # cover entire 1D array
        push(il, "A", v("A", None))
        i = OpVar("i")
        il.add_exclude(AryIndex([i]))

        # Without context, not excluded
        self.assertTrue(il.contains(AryIndex([OpInt(5)])))

        # With context i=5, excluded
        ctx = [(i, [], OpRange([OpInt(5), OpInt(5)]))]
        self.assertFalse(il.contains(AryIndex([OpInt(5)]), context=ctx))

    def test_apply_context_index_slices_vars_only_for_indices(self):
        # _apply_context_index should slice vars for indices but not for excludes
        i = OpVar("i")
        j = OpVar("j")
        idx = AryIndex([i, j])
        ctx = [(i, [j], OpRange([OpInt(1), OpInt(3)]))]

        # For indices (for_exclude=False): i -> 1:3, j -> :
        idx_for_indices = IndexList._apply_context_index(idx, ctx, for_exclude=False)
        self.assertEqual(str(idx_for_indices), "1:3,:")

        # For excludes (for_exclude=True): i -> 1:3, j unchanged
        idx_for_exclude = IndexList._apply_context_index(idx, ctx, for_exclude=True)
        self.assertEqual(str(idx_for_exclude), "1:3,j")


if __name__ == "__main__":
    unittest.main()
