import sys
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator, parser


class TestMacroBranching(unittest.TestCase):
    def test_macro_conditional_definition(self):
        code_tree.Node.reset()
        src = """
#ifdef CASE1
#define FOO y = x + 1
#else
#define FOO y = x - 1
#endif
module test
contains
  subroutine sub(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
    FOO
  end subroutine sub
end module test
"""
        modules = parser.parse_src(src)
        with patch("fautodiff.generator.parser.parse_file", return_value=modules):
            generated = generator.generate_ad("macro.f90", warn=False)
        lines = generated.splitlines()
        stripped = [l.strip() for l in lines]
        sub_start = stripped.index("subroutine sub_fwd_ad(x, x_ad, y, y_ad)")
        sub_end = stripped.index("end subroutine sub_fwd_ad")
        block = stripped[sub_start:sub_end]
        self.assertIn("#ifdef CASE1", block)
        self.assertIn("y_ad = x_ad ! y = x + 1", block)
        self.assertIn("#else", block)
        self.assertIn("y_ad = x_ad ! y = x - 1", block)


if __name__ == "__main__":
    unittest.main()
