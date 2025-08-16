import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator


class TestMacroPreserve(unittest.TestCase):
    def test_macro_re_emitted(self):
        code_tree.Node.reset()
        src = Path(__file__).resolve().parents[1] / "examples" / "macro_sample.F90"
        generated = generator.generate_ad(str(src), warn=False)
        lines = generated.splitlines()
        stripped = [l.strip() for l in lines]

        # macro before module definition
        self.assertEqual(stripped[0], "#define CONST 1")

        # macro inside module variable declarations
        mod_start = stripped.index("module macro_sample_ad")
        mod_contains = stripped.index("contains")
        self.assertIn("#define CONST_MOD 2", stripped[mod_start + 1 : mod_contains])

        # macro inside subroutine variable declarations
        sub_start = stripped.index("subroutine foo_fwd_ad(x, x_ad)")
        sub_end = stripped.index("end subroutine foo_fwd_ad")
        self.assertIn("#define CONST_SUB 3", stripped[sub_start + 1 : sub_end])

        # defined constants participate in computation without expansion
        self.assertTrue(
            any("CONST + CONST_MOD + CONST_SUB" in l for l in stripped),
            msg="defined constants not preserved in computation",
        )

    def test_macro_args_parenthesized(self):
        code_tree.Node.reset()
        src = Path(__file__).resolve().parents[1] / "examples" / "macro_args.F90"
        generated = generator.generate_ad(str(src), warn=False)
        lines = generated.splitlines()
        stripped = [l.strip() for l in lines]

        self.assertIn("#define SQR(x) ((x) * (x))", stripped)
        self.assertIn("#define MUL(x, y) ((x) * (y))", stripped)
        self.assertIn("b = SQR(a + 1.0) + MUL(a, a - 1.0)", stripped)
        self.assertTrue(
            any(
                "b_ad = a_ad * (a + 1.0 + a + 1.0 + a - 1.0 + a)" in l for l in stripped
            ),
            msg="function-like macros not expanded correctly",
        )


if __name__ == "__main__":
    unittest.main()
