import textwrap
import unittest

from fautodiff import parser


class TestMacroRename(unittest.TestCase):
    def test_token_with_ad_suffix_renamed(self):
        src = "!$CPP #define BAD foo_ad\n"
        parser._extract_macros(src)
        expanded = parser._expand_macros("BAD\n")
        line = expanded.strip()
        self.assertEqual(line, "foo_ad_macro ! foo_ad -> foo_ad_macro")
        self.assertTrue(any("foo_ad" in m for m in parser.macro_warnings))


if __name__ == "__main__":
    unittest.main()
