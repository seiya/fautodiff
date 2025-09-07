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


if __name__ == "__main__":
    unittest.main()
