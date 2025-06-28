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



if __name__ == '__main__':
    unittest.main()
