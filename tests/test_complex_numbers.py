import sys
import textwrap
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator, operators, parser


class TestComplexNumbers(unittest.TestCase):
    def test_parse_complex_literal(self):
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo()
                complex :: z
                z = (1.0, 2.0)
              end subroutine foo
            end module test
            """
        )
        mod = parser.parse_src(src)[0]
        stmt = mod.routines[0].content.first()
        self.assertIsInstance(stmt, code_tree.Assignment)
        self.assertIsInstance(stmt.rhs, operators.OpComplex)

    def test_generate_complex_mul(self):
        code_tree.Node.reset()
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(x, y, z)
                complex :: x, y, z
                z = x * y
              end subroutine foo
            end module test
            """
        )
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "test.f90"
            path.write_text(src)
            generated = generator.generate_ad(str(path), warn=False)
        self.assertIn("complex :: x_ad", generated)
        self.assertIn("z_ad = x_ad * y + y_ad * x", generated)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()

