import io
import sys
import textwrap
import unittest
from contextlib import redirect_stderr
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


class TestMacroMultiStmt(unittest.TestCase):
    def test_multi_statement_macro(self):
        code_tree.Node.reset()
        src = Path(__file__).resolve().parents[1] / "examples" / "macro_multistmt.F90"
        generated = generator.generate_ad(str(src), warn=False)
        lines = generated.splitlines()
        stripped = [l.strip() for l in lines]

        # conditional macro block moved to top
        self.assertEqual(stripped[0], "#ifdef INC")
        self.assertEqual(stripped[1], "#define DO_TWO x  = x + 1; y = y * 2")
        self.assertEqual(stripped[2], "#else")
        self.assertEqual(stripped[3], "#define DO_TWO x  = x - 1; y = y * 2")
        self.assertEqual(stripped[4], "#endif")
        # expanded statements are present
        self.assertTrue(any("x = x + 1" in l for l in stripped))
        self.assertTrue(any("y = y * 2" in l for l in stripped))
        # generated code should parse without errors
        parser.parse_src(generated)


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


class TestMacroRename(unittest.TestCase):
    def test_token_with_ad_suffix_renamed(self):
        src = "!$CPP #define BAD foo_ad\n"
        parser._extract_macros(src)
        expanded = parser._expand_macros("BAD\n")
        line = expanded.strip()
        self.assertEqual(line, "foo_ad_macro ! foo_ad -> foo_ad_macro")
        self.assertTrue(any("foo_ad" in m for m in parser.macro_warnings))


class TestMacroWarnings(unittest.TestCase):
    def test_token_paste_and_variadic_macro_warn(self):
        code_tree.Node.reset()
        src = textwrap.dedent(
            """
            #define CONCAT(a, b) a ## b
            #define VARIADIC(...) (__VA_ARGS__)
            program foo
            end program foo
            """
        )
        modules = parser.parse_src(src)
        with patch("fautodiff.generator.parser.parse_file", return_value=modules):
            buf = io.StringIO()
            with redirect_stderr(buf):
                generated = generator.generate_ad("macro.f90")
        msgs = buf.getvalue().splitlines()
        self.assertTrue(any("CONCAT" in m for m in msgs))
        self.assertTrue(any("VARIADIC" in m for m in msgs))
        lines = [l.strip() for l in generated.splitlines()]
        self.assertIn("#define CONCAT(a, b) a ## b", lines)
        self.assertIn("#define VARIADIC(...) (__VA_ARGS__)", lines)

    def test_line_continuation_macro_warn(self):
        src = "#define BIG(a) a + \\\n+  a\n"
        injected = parser._inject_cpp_lines(src)
        parser._extract_macros(injected)
        self.assertTrue(any("BIG" in m for m in parser.macro_warnings))
        self.assertNotIn("BIG", parser.macro_table.func)
        self.assertIn("#define BIG(a) a + \\", parser.file_cpp_lines)


if __name__ == "__main__":
    unittest.main()
