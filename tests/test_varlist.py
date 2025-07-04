import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.var_list import VarList
from fautodiff.operators import (
    OpInt,
    OpVar,
    OpRange
)


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
        v3 = OpVar("v", index=[i, OpRange([one,two])])
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
        v = OpVar("v", index=[None,j])
        vi = OpVar("v", index=[i,j])
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
        v1 = OpVar("v", index=[OpRange([1, 2]),OpRange([1, n])])
        v2 = OpVar("v", index=[OpRange([1, 2]),j])
        vi = OpVar("v", index=[i,j])
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


if __name__ == '__main__':
    unittest.main()
