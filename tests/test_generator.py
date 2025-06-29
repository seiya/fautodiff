import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator
from fautodiff import code_tree


class TestGenerator(unittest.TestCase):
    """Tests auto-diff generation for each example file."""


def _make_example_test(src: Path):
    def test(self):
        code_tree.Node.reset()
        generated = generator.generate_ad(str(src), warn=False)
        expected = src.with_name(src.stem + "_ad.f90").read_text()
        self.assertEqual(generated, expected, msg=f"Mismatch for {src.name}")

    return test


examples_dir = Path("examples")
for _src in sorted(examples_dir.glob("*.f90")):
    if _src.name.endswith("_ad.f90"):
        continue
    test_name = f"test_{_src.stem}"
    setattr(TestGenerator, test_name, _make_example_test(_src))


if __name__ == '__main__':
    unittest.main()
