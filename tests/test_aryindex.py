import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.operators import (
    AryIndex,
    OpVar,
    OpInt,
    OpRange,
)


class TestAryIndex(unittest.TestCase):
    def test_equality_and_str(self):
        i = OpVar('i')
        idx1 = AryIndex([OpInt(1), None, i])
        idx2 = AryIndex([1, OpRange(), i])
        self.assertEqual(idx1.list(), ['1', ':', 'i'])
        self.assertEqual(str(idx1), '1,:,i')
        self.assertEqual(idx1, idx2)
        idx3 = AryIndex([1, None, OpVar('j')])
        self.assertNotEqual(idx1, idx3)

    def test_comparisons(self):
        i = OpVar('i')
        idx = AryIndex([i, None])
        full = AryIndex([None, None])
        self.assertTrue(idx <= full)
        self.assertTrue(full >= idx)
        self.assertFalse(full <= idx)

    def test_comparisons_with_int_range(self):
        i = OpVar('i')
        one = OpInt(1)
        two = OpInt(2)
        three = OpInt(3)
        four = OpInt(4)
        five = OpInt(5)
        idx1 = AryIndex([three, i])
        idx2 = AryIndex([OpRange([one,two]), None])
        idx3 = AryIndex([four, None])
        idx4 = AryIndex([OpRange([three,four]), None])
        ref = AryIndex([OpRange([two,four]), None])
        self.assertTrue(AryIndex([three, i]) <= ref)
        self.assertFalse(AryIndex([five, i]) <= ref)
        self.assertTrue(AryIndex([OpRange([two,three]), None]) <= ref)
        self.assertFalse(AryIndex([OpRange([one,three]), None]) <= ref)
        self.assertFalse(AryIndex([OpRange([three,five]), None]) <= ref)

    def test_collect_vars_and_flags(self):
        i = OpVar('i')
        j = OpVar('j')
        idx = AryIndex([i, None, j])
        vars = idx.collect_vars()
        self.assertEqual(vars, [i, j])
        self.assertTrue(idx.is_partial_access())
        self.assertTrue(idx.is_depended_on(i))
        self.assertFalse(idx.is_depended_on(OpVar('k')))


if __name__ == '__main__':
    unittest.main()
