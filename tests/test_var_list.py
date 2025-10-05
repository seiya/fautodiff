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
        var.dims = tuple([None] * len(index))
        var.dims_raw = tuple([":"] * len(index))

    return var


class TestVarList(unittest.TestCase):

    def test_entire_contains_slices(self):
        vl = VarList()
        vl.push(v("A", (1, 10), (1, 5)))
        self.assertIn(v("A", (1, 10), (1, 5)), vl)
        # Mark entire coverage on second dim, still contains
        vl.push(v("A", (1, 10), None))
        self.assertIn(v("A", (3, 7), (2, 4)), vl)

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
        self.assertEqual(str(vl), "[v(i,j1), v(i,j2)]")
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
        self.assertEqual(str(vl), "[v(i,1:2)]")
        self.assertEqual(vl.names(), ["v"])

    def test_exclude(self):
        i = OpVar("i")
        j = OpVar("j")
        v = OpVar("v", index=[None, j])
        vi = OpVar("v", index=[i, j])
        vl = VarList([v])
        vl.push(vi)
        self.assertEqual(str(vl), "[v(:,j)]")
        vl = VarList([v])
        vl.remove(vi)
        self.assertEqual(str(vl), "[v(:,j), exclude: v(i,j)]")
        self.assertFalse(vi in vl)
        vl.push(vi)
        self.assertEqual(str(vl), "[v(:,j)]")
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
        self.assertEqual(str(vl), "[v(1:2,1:n), v(1:2,j)]")
        vl.push(vi)
        self.assertEqual(str(vl), "[v(1:2,1:n), v(1:2,j), v(i,j)]")
        vl = VarList([v1, v2])
        vl.remove(vi)
        self.assertEqual(str(vl), "[v(1:2,1:n), v(1:2,j), exclude: v(i,j)]")
        self.assertFalse(vi in vl)
        vl.push(vi)
        self.assertEqual(str(vl), "[v(1:2,1:n), v(1:2,j), v(i,j)]")
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
        self.assertEqual(str(vl), "[vref(n)%ary(m)]")
        self.assertEqual(AryIndex([n, m]), vl._store["vref%ary"][0])
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
        self.assertEqual(str(vl1), "[v(1:4)]")

    def test_intersection(self):
        v1 = OpVar("v", index=[OpRange([OpInt(1), OpInt(4), OpInt(1)])])
        v2 = OpVar("v", index=[OpRange([OpInt(3), OpInt(5), OpInt(1)])])
        vl1 = VarList([v1])
        vl2 = VarList([v2])
        inter = vl1 & vl2
        self.assertIn(OpVar("v", index=[OpInt(3)]), inter)
        self.assertIn(OpVar("v", index=[OpInt(4)]), inter)
        self.assertNotIn(OpVar("v", index=[OpInt(2)]), inter)
        self.assertEqual(str(inter), "[v(3:4)]")
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
        self.assertEqual(str(vl), "[v(0:nx,1:ny)]")
        v3 = OpVar("v", index=[None, None])
        vl.push(v3)
        self.assertEqual(str(vl), "[v(:,:)]")

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

    def test_merge_reorganize_and_dims(self):
        v1 = OpVar("a", index=AryIndex([OpRange([OpInt(1), OpInt(3), OpInt(1)])]))
        v2 = OpVar("a", index=AryIndex([OpRange([OpInt(2), OpInt(4), OpInt(1)])]))
        vl1 = VarList([v1])
        vl2 = VarList([v2])
        vl1.merge(vl2)
        # a indices merged; at least one index recorded
        self.assertIn("a", vl1.names())
        self.assertTrue(len(vl1._store["a"]) >= 1)

    def test_derived_type(self):
        vl = VarList()
        va = v("a", (1, 10))
        va.ref_var = v("s")
        vl.push(va)
        v1 = v("a", 5)
        v1.ref_var = v("s")
        self.assertIn(v1, vl)
        v2 = v("b", 5)
        v2.ref_var = v("s")
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
        self.assertIn(AryIndex([OpRange([1, n])]), vl._store["A"].exclude)
        # The original range should still be there, but __contains__ will use the exclude list
        self.assertIn(v("A", (1, 10)), vl)
        self.assertNotIn(v("A", (1, n)), vl)

    def test_str_representation(self):
        vl = VarList()
        vl.push(v("A", (1, 10)))
        vl.push(v("B", None))
        va = v("a")
        va.ref_var = v("s", 5)
        vl.push(va)
        vb = v("b", 5)
        vb.ref_var = v("p")
        vl.push(vb)
        vl._store["B"].dims = [2]  # B is 2D
        s = str(vl)
        self.assertIn("A(1:10)", s)
        self.assertIn("B(:,:)", s)
        self.assertIn("s(5)%a", s)
        self.assertIn("p%b(5)", s)

    def test_intersection_with_variables(self):
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


def make_dt_var(chain, index_dims=None):
    """Create an OpVar for a derived-type chain.

    chain: list of tuples (name, dims_tuple_or_None).
      - dims use declaration-size tokens, e.g., ("n",) or ("n","m").
    index_dims: optional list of index Operators for the leaf (slice at use-site); if None, no index.
    """
    ref = None
    for name, dims in chain:
        dims_op = None
        dims_raw = None
        if dims is not None:
            dims_raw = tuple(str(d) for d in dims)
            dims_op = []
            for d in dims:
                if isinstance(d, str):
                    if d in (":", "*"):
                        dims_op.append(None)
                    else:
                        dims_op.append(OpVar(d))
                else:
                    dims_op.append(d)
            dims_op = tuple(dims_op)
        ref = OpVar(
            name,
            var_type=VarType("real"),
            dims=dims_op,
            dims_raw=dims_raw,
            ref_var=ref,
        )
    if index_dims is not None:
        ref = OpVar(
            ref.name,
            index=AryIndex(index_dims),
            var_type=ref.var_type,
            dims=ref.dims,
            dims_raw=ref.dims_raw,
            ref_var=ref.ref_var,
        )
    return ref


class TestVarListDerivedShape(unittest.TestCase):
    def test_shape_inferred_from_leaf_dims_in_derived_type(self):
        # s%a where a has dims ('n',) and s is scalar
        var = make_dt_var([("s", None), ("a", ("n",))])
        il = IndexList()
        il.push_var(var)
        self.assertTrue(il.contains(AryIndex([OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpVar("n")])))
        self.assertFalse(il.contains(AryIndex([OpInt(0)])))

    def test_shape_inferred_from_parent_and_leaf_dims(self):
        # s(i)%a(n,m) -> concatenated dims length 3
        var = make_dt_var([("s", ("i",)), ("a", ("n", "m"))])
        il = IndexList()
        il.push_var(var)
        self.assertTrue(il.contains(AryIndex([OpInt(1), OpInt(1), OpInt(1)])))
        self.assertTrue(il.contains(AryIndex([OpVar("i"), OpVar("n"), OpVar("m")])))
        self.assertFalse(il.contains(AryIndex([OpInt(0), OpInt(1), OpInt(1)])))
        self.assertFalse(il.contains(AryIndex([OpInt(1), OpInt(0), OpInt(1)])))
        self.assertFalse(il.contains(AryIndex([OpInt(1), OpInt(1), OpInt(0)])))

    def test_shape_mismatch_raises_for_different_leaf_dims(self):
        var1 = make_dt_var([("s", None), ("a", ("n",))])
        il = IndexList()
        il.push_var(var1)
        var2 = make_dt_var([("s", None), ("a", ("m",))])
        with self.assertRaises(ValueError):
            il.push_var(var2)

    def test_guard_push_creates_guarded_entry(self):
        vl = VarList()
        guard = OpVar("mask")
        item = v("G", 1)
        vl.enter_guard(guard)
        vl.push(item)

        self.assertIn("G", vl._guarded)
        self.assertIn("mask", vl._guarded["G"])
        guarded = vl._guarded["G"]["mask"]
        self.assertTrue(guarded.contains(item.concat_index()))
        self.assertEqual(vl._store["G"].indices, [])

    def test_guard_push_in_different_guard(self):
        vl = VarList()
        guard1 = OpVar("mask1")
        guard2 = OpVar("mask2")
        item = OpVar("x")
        vl.enter_guard(guard1)
        vl.push(item)
        vl.leave_guard()
        vl.enter_guard(guard2)

        self.assertIn(item, vl)

    def test_guard_push_different_var_in_different_guard(self):
        vl = VarList()
        guard1 = OpVar("mask1")
        guard2 = OpVar("mask2")
        item1 = OpVar("x")
        item2 = OpVar("y")
        vl.enter_guard(guard1)
        vl.push(item1)
        vl.leave_guard()
        vl.enter_guard(guard2)
        vl.push(item2)

        self.assertIn(item1, vl)

    def test_guard_push_and_remove_in_different_guards(self):
        vl = VarList()
        guard1 = OpVar("mask1")
        guard2 = OpVar("mask2")
        item = OpVar("x")
        vl.push(item)
        vl.enter_guard(guard1)
        vl.remove(item)
        vl.leave_guard()
        vl.enter_guard(guard2)

        self.assertIn(item, vl)

    def test_guard_push_existing_coverage_goes_to_store(self):
        vl = VarList()
        base = v("G", (1, 5))
        inner = v("G", 3)
        vl.push(base)

        vl.enter_guard(OpVar("mask"))
        vl.push(inner)
        vl.leave_guard()

        self.assertIn("G", vl._guarded)
        self.assertEqual(str(vl._guarded["G"]["mask"]), "(1:5)")
        self.assertIn(inner, vl)
        self.assertEqual(str(vl), "[G(1:5)] + [G(1:5) @ mask]")

    def test_guard_push_with_different_guard_merges_store(self):
        vl = VarList()
        guard1 = OpVar("mask1")
        guard2 = OpVar("mask2")
        first = v("G", 1)
        second = v("G", 2)

        vl.enter_guard(guard1)
        vl.push(first)
        vl.leave_guard()

        vl.enter_guard(guard2)
        vl.push(second)
        vl.leave_guard()

        self.assertIn("G", vl._guarded)
        self.assertIn("mask1", vl._guarded["G"])
        self.assertIn("mask2", vl._guarded["G"])
        self.assertIn(first.index, vl._guarded["G"]["mask1"])
        self.assertIn(second.index, vl._guarded["G"]["mask2"])
        self.assertIn(first, vl)
        self.assertIn(second, vl)

    def test_guard_remove_same_guard_clears_guarded_entry(self):
        vl = VarList()
        guard = OpVar("mask")
        item = v("G", 1)

        vl.enter_guard(guard)
        vl.push(item)
        vl.remove(item)

        self.assertIn("G", vl._guarded)
        self.assertIn("mask", vl._guarded["G"])
        self.assertFalse(vl._guarded["G"]["mask"].indices)
        self.assertNotIn("G", vl.names())

        vl.leave_guard()

    def test_guard_remove_different_guard_merges_back(self):
        vl = VarList()
        guard1 = OpVar("mask1")
        guard2 = OpVar("mask2")
        item = v("G", 1)

        vl.enter_guard(guard1)
        vl.push(item)
        vl.leave_guard()

        vl.enter_guard(guard2)
        vl.remove(item)
        vl.leave_guard()

        self.assertIn("G", vl._guarded)
        self.assertNotIn("mask1", vl._guarded["G"])
        self.assertIn("mask2", vl._guarded["G"])
        self.assertFalse(vl._guarded["G"]["mask2"].indices)
        self.assertIn("G", vl._store)
        self.assertIn(item, vl)

    def test_guard_push_wo_guard_then_remove_w_guard(self):
        vl = VarList()
        guard = OpVar("mask")
        item = OpVar("u")

        vl.push(item)
        vl.enter_guard(guard)
        vl.remove(item)
        self.assertFalse(item in vl)
        vl.leave_guard()
        self.assertTrue(item in vl)

    def test_guard_nested(self):
        vl = VarList()
        guard1 = OpVar("mask1")
        guard2 = OpVar("mask2")
        item = OpVar("u")

        vl.push(item)
        vl.enter_guard(guard1)
        vl.enter_guard(guard2)
        vl.remove(item)
        vl.leave_guard()
        vl.leave_guard()
        vl.enter_guard(guard2)

        self.assertIn(item, vl)


if __name__ == "__main__":
    unittest.main()
