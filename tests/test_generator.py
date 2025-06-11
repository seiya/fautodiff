import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator

class TestGenerator(unittest.TestCase):
    def test_examples(self):
        """Generate AD code for every file in ``examples``."""
        examples = Path("examples")
        for src in sorted(examples.glob("*.f90")):
            if src.name.endswith("_ad.f90"):
                continue
            with self.subTest(src=src.name):
                generated = generator.generate_ad(str(src))
                expected_file = src.with_name(f"{src.stem}_ad.f90")
                if expected_file.exists():
                    expected = expected_file.read_text()
                    self.assertEqual(generated, expected)
                else:
                    self.assertTrue(generated.startswith(f"module {src.stem}_ad"))

if __name__ == '__main__':
    unittest.main()
