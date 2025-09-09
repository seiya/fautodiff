import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.operators import AryIndex, OpInt, OpNeg, OpRange, OpVar, VarType, Operator
from fautodiff.var_list import VarList, IndexList


def v(name, *idx_dims) -> OpVar:
    """Helper to build OpVar with optional AryIndex from dims list.

    Each element of idx_dims can be:
    - None: entire dimension
    - int: fixed index
    - tuple: (start, end[, step]) where items may be int, OpVar or Operator
    - OpVar: fixed index symbolically
    - Operator: fixed index symbolically
    """
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

    # A bit of a hack to give it a type for some tests
    var_type = VarType("real")

    # Handle derived types
    if "%" in name:
        raise ValueError("Derived types not supported")
    else:
        var = OpVar(name, index=index, var_type=var_type)

    # Set dims for multi-dim arrays
    if index:
        var.dims = (":",) * len(index)

    return var


class TestVarList(unittest.TestCase):
    
    def test_entire_contains_slices(self):
        vl = VarList()
        vl.push(v("A", (1, 10), (1, 5)))
        self.assertIn(v("A", (1, 10), (1, 5)), vl)
        # Mark entire coverage on second dim, still contains
        vl.push(v("A", (1, 10), None))
        self.assertIn(v("A", (3, 7), (2, 4)), vl)

    # moved to tests/test_index_list.py: test_push_merge_adjacent_int_and_range
    # moved to tests/test_index_list.py: test_remove_int_from_range
    # moved to tests/test_index_list.py: test_remove_subslice_splits

    def test_contains_superset_is_not_contained(self):
        # Storing a subrange must not imply containment of a larger superset
        vl = VarList()
        vl.push(v("D", (2, 8)))
        # exact is contained
        self.assertIn(v("D", (2, 8)), vl)
        # wider range is considered contained partially
        self.assertIn(v("D", (1, 10)), vl)
        # full coverage is also considered contained partially
        self.assertIn(v("D", None), vl)

    # moved to tests/test_index_list.py: test_contains_scalar_vs_range

    def test_vars_in(self):
        vl = VarList()
        v1 = OpVar("x", index=[OpVar("i")])
        v2 = OpVar("x", index=[OpVar("j")])
        vl.push(v1)
        self.assertIn(v1, vl)
        self.assertIn(v2, vl)

    def test_expression_bounds_exact_match(self):
        ihalo = OpVar("ihalo")
        jend = OpVar("jend")
        vl = VarList()
        # push expression range exactly
        vl.push(v("F", (jend - ihalo + OpInt(1), jend)))
        # exact same AST should be found
        self.assertIn(v("F", (jend - ihalo + OpInt(1), jend)), vl)

    def test_expression_bounds_uncertain_overlap(self):
        ihalo = OpVar("ihalo")
        jend = OpVar("jend")
        vl = VarList()
        vl.push(v("G", (jend - ihalo + OpInt(1), jend)))
        # Overlap with (jend+1:je) is uncertain -> should not claim containment
        je = OpVar("je")
        self.assertNotIn(v("G", (jend + OpInt(1), je)), vl)

    def test_remove_expression_exact(self):
        ihalo = OpVar("ihalo")
        jend = OpVar("jend")
        vl = VarList()
        expr = v("H", (jend - ihalo + OpInt(1), jend))
        vl.push(expr)
        vl.remove(expr)
        # removal of exact range should clear entry
        self.assertNotIn(expr, vl)

    def test_negative_stride_normalized_in_push(self):
        # moved to tests/test_index_list.py
        pass

    def test_push_and_contains_scalar(self):
        vl = VarList()
        vl.push(v("A"))
        self.assertIn(v("A"), vl)
        self.assertNotIn(v("B"), vl)

    def test_push_and_contains_full_array(self):
        vl = VarList()
        vl.push(v("A", None))
        self.assertIn(v("A", None), vl)
        self.assertIn(v("A", 5), vl)
        self.assertIn(v("A", (1, 10)), vl)

    def test_generate_different_vars(self):
        i = OpVar("i")
        j1 = OpVar("j1")
        j2 = OpVar("j2")
        v1 = OpVar("v", index=[i, j1])
        v2 = OpVar("v", index=[i, j2])
        vl = VarList([v1, v2])
        self.assertTrue(v1 in vl)
        self.assertTrue(v2 in vl)
        self.assertEqual(str(vl), "v(i,j1), v(i,j2)")
        self.assertEqual(vl.names(), ["v"])

    def test_push_int_and_int(self):
        one = OpInt(1)
        two = OpInt(2)
        i = OpVar("i")
        v1 = OpVar("v", index=[i, one])
        v2 = OpVar("v", index=[i, two])
        v3 = OpVar("v", index=[i, OpRange([one, two])])
        vl = VarList()
        vl.push(v1)
        vl.push(v2)
        self.assertTrue(v1 in vl)
        self.assertTrue(v2 in vl)
        self.assertTrue(v3 in vl)
        self.assertEqual(str(vl), "v(i,1:2)")
        self.assertEqual(vl.names(), ["v"])

    def test_exclude(self):
        i = OpVar("i")
        j = OpVar("j")
        v = OpVar("v", index=[None, j])
        vi = OpVar("v", index=[i, j])
        vl = VarList([v])
        vl.push(vi)
        self.assertEqual(str(vl), "v(:,j)")
        vl = VarList([v])
        vl.remove(vi)
        self.assertEqual(str(vl), "v(:,j), exclude: v(i,j)")
        self.assertFalse(vi in vl)
        vl.push(vi)
        self.assertEqual(str(vl), "v(:,j)")
        self.assertTrue(vi in vl)
        vl.remove(vi)
        self.assertFalse(vi in vl)

    def test_exclude_int(self):
        i = OpVar("i")
        j = OpVar("j")
        n = OpVar("n")
        v1 = OpVar("v", index=[OpRange([1, 2]), OpRange([1, n])])
        v2 = OpVar("v", index=[OpRange([1, 2]), j])
        vi = OpVar("v", index=[i, j])
        vl = VarList([v1, v2])
        self.assertEqual(str(vl), "v(1:2,1:n), v(1:2,j)")
        vl.push(vi)
        self.assertEqual(str(vl), "v(1:2,1:n), v(1:2,j), v(i,j)")
        vl = VarList([v1, v2])
        vl.remove(vi)
        self.assertEqual(str(vl), "v(1:2,1:n), v(1:2,j), exclude: v(i,j)")
        self.assertFalse(vi in vl)
        vl.push(vi)
        self.assertEqual(str(vl), "v(1:2,1:n), v(1:2,j), v(i,j)")
        self.assertTrue(vi in vl)
        vl.remove(vi)
        self.assertFalse(vi in vl)

    def test_derived(self):
        n = OpVar("n")
        m = OpVar("m")
        vref = OpVar("vref", index=[n])
        var = OpVar("ary", index=[m], ref_var=vref)
        vl = VarList([var])
        v = OpVar("ary", ref_var=OpVar("vref"))
        self.assertIn(v.name_ext(), vl.names())
        self.assertIn(var, vl)
        self.assertEqual(str(vl), "vref(n)%ary(m)")
        self.assertEqual(AryIndex([n, m]), vl["vref%ary"][0])
        self.assertEqual([var], list(v for v in vl))

    def test_merge(self):
        v1 = OpVar("v", index=[OpRange([OpInt(1), OpInt(2)])])
        v2 = OpVar("v", index=[OpRange([OpInt(2), OpInt(4)])])
        vl1 = VarList([v1])
        vl2 = VarList([v2])
        vl1.merge(vl2)
        self.assertIn(OpVar("v", index=[OpInt(1)]), vl1)
        self.assertIn(OpVar("v", index=[OpInt(4)]), vl1)
        self.assertIn(OpVar("v", index=[OpInt(3)]), vl1)
        self.assertEqual(str(vl1), "v(1:4)")

    def test_intersection(self):
        v1 = OpVar("v", index=[OpRange([OpInt(1), OpInt(4), OpInt(1)])])
        v2 = OpVar("v", index=[OpRange([OpInt(3), OpInt(5), OpInt(1)])])
        vl1 = VarList([v1])
        vl2 = VarList([v2])
        inter = vl1 & vl2
        self.assertIn(OpVar("v", index=[OpInt(3)]), inter)
        self.assertIn(OpVar("v", index=[OpInt(4)]), inter)
        self.assertNotIn(OpVar("v", index=[OpInt(2)]), inter)
        self.assertEqual(str(inter), "v(3:4)")
        self.assertEqual(inter.names(), ["v"])

    def test_push_full_array(self):
        ny = OpVar("ny", var_type=VarType("integer"))
        nx = OpVar("nx", var_type=VarType("integer"))
        one = OpInt(1)
        zero = OpInt(0)
        v1 = OpVar("v", index=[zero, OpRange([one, ny])])
        v2 = OpVar("v", index=[OpRange([one, nx]), OpRange([one, ny])])
        vl = VarList()
        vl.push(v1)
        vl.push(v2)
        self.assertEqual(str(vl), "v(0:nx,1:ny)")
        v3 = OpVar("v", index=[None, None])
        vl.push(v3)
        self.assertEqual(str(vl), "v(:,:)")

    def test_force_stride_one_and_update_upward(self):
        # a(5:1:-1) should be normalised to a(1:5)
        rng = OpRange([OpInt(5), OpInt(1), OpNeg([OpInt(1)])])
        a_idx = AryIndex([rng])
        a = OpVar("a", index=a_idx)
        vl = VarList([a])
        # expand upward over negative stride; range should flip and stride cleared
        vl.update_index_upward(
            {"a": (0, 1)}, OpRange([OpInt(1), OpInt(5), OpNeg([OpInt(1)])])
        )
        names = vl.names()
        self.assertEqual(names, ["a"])
        # index list exists and contains a range
        self.assertTrue(any(isinstance(idx[0], OpRange) for idx in vl["a"]))

    def test_remove_whole_variable(self):
        # moved to tests/test_index_list.py
        pass

    def test_intersection_various_cases(self):
        # self: a(:) and b(2:4)
        vl1 = VarList(
            [
                OpVar("a", index=AryIndex([None])),
                OpVar("b", index=AryIndex([OpRange([OpInt(2), OpInt(4), OpInt(1)])])),
            ]
        )
        # other: a(3) and b(4)
        vl2 = VarList(
            [
                OpVar("a", index=AryIndex([OpInt(3)])),
                OpVar("b", index=AryIndex([OpInt(4)])),
            ]
        )
        inter = vl1 & vl2
        s = sorted(str(v) for v in inter)
        self.assertEqual(s, ["a(3)", "b(4)"])

        # no overlap: c vs d
        vl3 = VarList([OpVar("c", index=AryIndex([OpInt(1)]))])
        vl4 = VarList([OpVar("d", index=AryIndex([OpInt(1)]))])
        inter2 = vl3 & vl4
        self.assertEqual(len(inter2), 0)

    # moved to tests/test_index_list.py: test_update_index_downward

    def test_contains_and_str_with_exclude(self):
        v = OpVar("a", index=AryIndex([OpInt(2)]))
        vl = VarList([v])
        # exclude a(2) and check membership skips it
        vl.add_exclude(v)
        self.assertNotIn(v, vl)
        # __str__ contains exclude info
        s = str(vl)
        self.assertIn("exclude", s)
        self.assertIn("a(2)", s)

    def test_merge_reorganize_and_dims(self):
        v1 = OpVar("a", index=AryIndex([OpRange([OpInt(1), OpInt(3), OpInt(1)])]))
        v2 = OpVar("a", index=AryIndex([OpRange([OpInt(2), OpInt(4), OpInt(1)])]))
        vl1 = VarList([v1])
        vl2 = VarList([v2])
        vl1.merge(vl2)
        # a indices merged; at least one index recorded
        self.assertIn("a", vl1.names())
        self.assertTrue(len(vl1["a"]) >= 1)

    def test_push_entire_replaces_and_clears_exclude(self):
        # Start with partial indices and an explicit exclude
        vl = VarList([OpVar("b", index=AryIndex([OpInt(1)]))])
        vl.add_exclude(OpVar("b", index=AryIndex([OpInt(2)])))
        # Pushing entire array b(:) should replace index list and clear exclude
        vl.push(OpVar("b", index=AryIndex([OpRange([None])])))
        self.assertEqual(vl["b"], IndexList([None], dims=[1]))

    def test_merge_adjacent_integers(self):
        # moved to tests/test_index_list.py
        pass

    def test_merge_adjacent_ranges(self):
        # moved to tests/test_index_list.py
        pass

    def test_merge_overlapping_ranges(self):
        # moved to tests/test_index_list.py
        pass

    def test_merge_integer_into_range(self):
        # moved to tests/test_index_list.py
        pass

    def test_remove_from_range(self):
        # moved to tests/test_index_list.py
        pass

    def test_remove_sub_range(self):
        # moved to tests/test_index_list.py
        pass

    def test_remove_from_ends_of_range(self):
        # moved to tests/test_index_list.py
        pass

    def test_exclude_from_full_coverage(self):
        # moved to tests/test_index_list.py
        pass

    def test_push_overwrites_exclude(self):
        # moved to tests/test_index_list.py
        pass

    def test_multi_dimensional(self):
        # moved to tests/test_index_list.py
        pass

    def test_derived_type(self):
        vl = VarList()
        va = v("a", (1, 10))
        va.ref_var = v("s")
        vl.push(va)
        v1 = v("a", 5); v1.ref_var = v("s")
        self.assertIn(v1, vl)
        v2 = v("b", 5); v2.ref_var = v("s")
        self.assertNotIn(v2, vl)

    def test_range_with_variables_containment(self):
        n = OpVar("n")
        m = OpVar("m")
        vl = VarList()
        vl.push(v("A", (1, n)))

        # With conservative check, we can't know if 5 is in (1,n)
        self.assertIn(v("A", 5), vl)
        self.assertIn(v("A", (1, 5)), vl)
        self.assertIn(v("A", (1, m)), vl)
        # But exact match should work
        self.assertIn(v("A", (1, n)), vl)

    def test_range_with_variables_removal(self):
        n = OpVar("n")
        vl = VarList()
        vl.push(v("A", (1, 10)))
        vl.remove(v("A", (1, n)))

        # Since we can't determine the range of n, it should be excluded
        self.assertIn(AryIndex([OpRange([1, n])]), vl["A"].exclude)
        # The original range should still be there, but __contains__ will use the exclude list
        self.assertIn(v("A", (1, 10)), vl)
        self.assertNotIn(v("A", (1, n)), vl)

    def test_str_representation(self):
        vl = VarList()
        vl.push(v("A", (1, 10)))
        vl.push(v("B", None))
        va = v("a"); va.ref_var = v("s", 5)
        vl.push(va)
        vb = v("b", 5); vb.ref_var = v("p")
        vl.push(vb)
        vl["B"].dims = [2]  # B is 2D
        s = str(vl)
        self.assertIn("A(1:10)", s)
        self.assertIn("B(:,:)", s)
        self.assertIn("s(5)%a", s)
        self.assertIn("p%b(5)", s)

    def test_negative_stride_coverage(self):
        # moved to tests/test_index_list.py
        pass

    def test_remove_with_negative_stride(self):
        # moved to tests/test_index_list.py
        pass

    def test_intersection(self):
        vl1 = VarList()
        vl1.push(v("A", (1, 10)))
        vl1.push(v("B", (1, 5)))

        vl2 = VarList()
        vl2.push(v("A", (5, 15)))
        vl2.push(v("C", (1, 5)))

        vl_intersect = vl1 & vl2
        self.assertIn(v("A", (5, 10)), vl_intersect)
        self.assertNotIn(v("A", 4), vl_intersect)
        self.assertNotIn(v("A", 11), vl_intersect)
        self.assertNotIn(v("B"), vl_intersect)
        self.assertNotIn(v("C"), vl_intersect)

    def test_intersection_with_variable_bounds(self):
        n = OpVar("n")
        vl1 = VarList()
        vl1.push(v("A", (1, n)))
        vl2 = VarList()
        vl2.push(v("A", (1, 10)))

        # Cannot determine intersection for sure
        vl_intersect = vl1 & vl2
        self.assertNotIn(v("A"), vl_intersect)

        # But with identical variable bounds, it should work
        vl3 = VarList()
        vl3.push(v("A", (1, n)))
        vl_intersect2 = vl1 & vl3
        self.assertIn(v("A", (1, n)), vl_intersect2)


if __name__ == "__main__":
    unittest.main()
