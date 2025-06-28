import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.var_dict import Vardict


class TestVardict(unittest.TestCase):
    def test_basic_operations(self):
        vd = Vardict()
        vd['a'] = 1
        vd['b'] = 2
        self.assertEqual(vd['a'], 1)
        self.assertEqual(vd['b'], 2)
        self.assertIn('a', vd)
        self.assertIn('b', vd)
        self.assertEqual(len(vd), 2)
        self.assertEqual(set(vd.keys()), {'a', 'b'})
        self.assertEqual(set(vd.values()), {1, 2})
        self.assertEqual(set(vd.items()), {('a', 1), ('b', 2)})
        collected = [k for k in vd]
        self.assertEqual(set(collected), {'a', 'b'})

        copy = vd.copy()
        self.assertEqual(set(copy.items()), set(vd.items()))

        del vd['a']
        self.assertNotIn('a', vd)
        with self.assertRaises(KeyError):
            _ = vd['a']

        vd.remove('b')
        self.assertEqual(len(vd), 0)


if __name__ == '__main__':
    unittest.main()
