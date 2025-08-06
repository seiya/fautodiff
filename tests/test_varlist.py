import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.operators import AryIndex, OpInt, OpRange, OpVar
from fautodiff.var_list import VarList


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


if __name__ == "__main__":
    unittest.main()
