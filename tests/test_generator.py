import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator

class TestGenerator(unittest.TestCase):
    def test_examples(self):
        examples = Path('examples')
        for src in sorted(examples.glob('*.f90')):
            if src.name.endswith('_ad.f90'):
                continue
            generated = generator.generate_ad(str(src), warn=False)
            expected = src.with_name(src.stem + '_ad.f90').read_text()
            self.assertEqual(generated, expected, msg=f"Mismatch for {src.name}")

if __name__ == '__main__':
    unittest.main()
