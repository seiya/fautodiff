import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.operators import AryIndex, OpInt, OpNeg, OpRange, OpVar
from fautodiff.var_list import VarList
from fautodiff.var_type import VarType


class TestVarList(unittest.TestCase):
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
        self.assertIn("vref%ary", vl.names())
        self.assertIn(var, vl)
        self.assertEqual(str(vl), "vref(n)%ary(m)")
        self.assertEqual(AryIndex([n, m]), vl.vars["vref%ary"][0])
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
        self.assertEqual(str(vl), "v(0,1:ny), v(1:nx,1:ny)")
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
        # start with a(1:5)
        a = OpVar("a", index=AryIndex([OpRange([OpInt(1), OpInt(5)])]))
        vl = VarList([a])
        # remove entire variable -> entry disappears
        vl.remove(OpVar("a"))
        self.assertEqual(len(vl), 0)

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

    def test_update_index_downward(self):
        # names mapping: a uses index 0 and has 1 dim
        vl = VarList(
            [
                OpVar("a", index=AryIndex([OpRange([OpInt(1), OpInt(3)])])),
            ]
        )
        i = OpVar("i")
        vl.update_index_downward({"a": (0, 1)}, i)
        # now indices must be list of explicit i
        for idx in vl["a"]:
            self.assertEqual(str(idx[0]), "i")

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
        self.assertIn("a", vl1.vars)
        self.assertTrue(len(vl1["a"]) >= 1)

    def test_push_entire_replaces_and_clears_exclude(self):
        # Start with partial indices and an explicit exclude
        vl = VarList([OpVar("b", index=AryIndex([OpInt(1)]))])
        vl.add_exclude(OpVar("b", index=AryIndex([OpInt(2)])))
        # Pushing entire array b(:) should replace index list and clear exclude
        vl.push(OpVar("b", index=AryIndex([OpRange([None])])))
        self.assertEqual(vl["b"], [AryIndex([OpRange([None])])])
        self.assertNotIn("b", vl.exclude)


if __name__ == "__main__":
    unittest.main()
