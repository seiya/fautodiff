import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator, parser


class TestMacroMultiStmt(unittest.TestCase):
    def test_multi_statement_macro(self):
        code_tree.Node.reset()
        src = Path(__file__).resolve().parents[1] / "examples" / "macro_multistmt.F90"
        generated = generator.generate_ad(str(src), warn=False)
        lines = generated.splitlines()
        stripped = [l.strip() for l in lines]

        # macro definition moved to top
        self.assertEqual(stripped[0], "#define DO_TWO x  = x + 1; y = y * 2")
        # expanded statements are present
        self.assertTrue(any("x = x + 1" in l for l in stripped))
        self.assertTrue(any("y = y * 2" in l for l in stripped))
        # generated code should parse without errors
        parser.parse_src(generated)


if __name__ == "__main__":
    unittest.main()
