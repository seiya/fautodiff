import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import parser


class TestParser(unittest.TestCase):
    def test_find_subroutines(self):
        base = Path(__file__).resolve().parents[1]
        src = base / "examples" / "simple_math.f90"
        modules = parser.parse_file(str(src))
        names = parser.find_subroutines(modules)
        self.assertIn("add_numbers", names)
        self.assertIn("multiply_numbers", names)


if __name__ == "__main__":
    unittest.main()
