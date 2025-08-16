import io
import sys
import textwrap
import unittest
from contextlib import redirect_stderr
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator, parser


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
        src = "#define BIG(a) a + \\\n  a\n"
        injected = parser._inject_cpp_lines(src)
        parser._extract_macros(injected)
        self.assertTrue(any("BIG" in m for m in parser.macro_warnings))
        self.assertNotIn("BIG", parser.macro_table.func)
        self.assertIn("#define BIG(a) a + \\", parser.file_cpp_lines)


if __name__ == "__main__":
    unittest.main()
