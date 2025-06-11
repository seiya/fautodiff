import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator

class TestGenerator(unittest.TestCase):
    def test_simple_math(self):
        examples = Path('examples')
        generated = generator.generate_ad(str(examples / 'simple_math.f90'))
        expected = (examples / 'simple_math_ad.f90').read_text()
        self.assertEqual(generated, expected)

if __name__ == '__main__':
    unittest.main()
