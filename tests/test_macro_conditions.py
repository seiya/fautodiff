import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator


class TestMacroConditions(unittest.TestCase):
    def test_conditional_macros(self):
        code_tree.Node.reset()
        src = Path(__file__).resolve().parents[1] / "examples" / "conditional_macro.F90"
        generated = generator.generate_ad(str(src), warn=False)
        lines = generated.splitlines()
        stripped = [l.strip() for l in lines]

        module_idx = stripped.index("module conditional_macro_ad")
        top_lines = stripped[:module_idx]

        # top level macro definitions and undef handling
        self.assertIn("#define BASE 1", top_lines)
        self.assertIn("#define TEMP BASE", top_lines)
        self.assertIn("#define DOUBLE TEMP", top_lines)
        self.assertIn("#undef TEMP", top_lines)
        self.assertIn("#ifndef OMIT", top_lines)
        self.assertIn("#define COND 5", top_lines)
        self.assertIn("#endif", top_lines)

        # conditional block preserved in subroutine
        sub_start = stripped.index("subroutine foo_fwd_ad(x, x_ad, y, y_ad)")
        sub_end = stripped.index("end subroutine foo_fwd_ad")
        block = stripped[sub_start:sub_end]
        self.assertIn("z = x + BASE", block)
        self.assertIn("z = z + DOUBLE", block)
        self.assertIn("#ifdef COND", block)
        self.assertIn("y = z + COND", block)


if __name__ == "__main__":
    unittest.main()
