import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import operators


class TestAryIndex(unittest.TestCase):
    def test_equality_and_str(self):
        i = operators.OpVar('i')
        idx1 = operators.AryIndex([operators.OpInt(1), None, i])
        idx2 = operators.AryIndex([1, operators.OpRange(), i])
        self.assertEqual(idx1.list(), ['1', ':', 'i'])
        self.assertEqual(str(idx1), '1,:,i')
        self.assertEqual(idx1, idx2)
        idx3 = operators.AryIndex([1, None, operators.OpVar('j')])
        self.assertNotEqual(idx1, idx3)

    def test_comparisons(self):
        i = operators.OpVar('i')
        idx = operators.AryIndex([i, None])
        full = operators.AryIndex([None, None])
        self.assertTrue(idx <= full)
        self.assertTrue(full >= idx)
        self.assertFalse(full <= idx)

    def test_collect_vars_and_flags(self):
        i = operators.OpVar('i')
        j = operators.OpVar('j')
        idx = operators.AryIndex([i, None, j])
        vars = idx.collect_vars()
        self.assertEqual(vars, [i, j])
        self.assertTrue(idx.is_partial_access())
        self.assertTrue(idx.is_depended_on(i))
        self.assertFalse(idx.is_depended_on(operators.OpVar('k')))


if __name__ == '__main__':
    unittest.main()
