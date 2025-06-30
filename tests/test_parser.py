import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import parser, code_tree, operators
from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.two.utils import walk


class TestParser(unittest.TestCase):
    def test_find_subroutines(self):
        base = Path(__file__).resolve().parents[1]
        src = base / "examples" / "simple_math.f90"
        modules = parser.parse_file(str(src))
        names = parser.find_subroutines(modules)
        self.assertIn("add_numbers", names)
        self.assertIn("multiply_numbers", names)

    def test_parse_file_module_names(self):
        base = Path(__file__).resolve().parents[1]
        src = base / "examples" / "simple_math.f90"
        modules = parser.parse_file(str(src))
        self.assertEqual(len(modules), 1)
        mod = modules[0]
        self.assertEqual(mod.name, "simple_math")
        routine_names = [r.name for r in mod.routines]
        self.assertIn("add_numbers", routine_names)
        self.assertIn("multiply_numbers", routine_names)

    def test_parse_call_stmt(self):
        src = """
subroutine wrapper(x)
  integer :: x
  call foo(x)
end subroutine wrapper
"""
        reader = FortranStringReader(src)
        factory = ParserFactory().create(std="f2008")
        ast = factory(reader)
        sub = walk(ast, Fortran2003.Subroutine_Subprogram)[0]
        routine = parser._parse_routine(sub, "<string>")
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.name, "foo")

    def test_parse_call_stmt_in_function(self):
        src = """
function wrapper(x) result(y)
  integer :: x, y
  call foo(x)
  y = x
end function wrapper
"""
        reader = FortranStringReader(src)
        factory = ParserFactory().create(std="f2008")
        ast = factory(reader)
        func = walk(ast, Fortran2003.Function_Subprogram)[0]
        routine = parser._parse_routine(func, "<string>")
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.name, "foo")

    def test_parse_function_call_assignment(self):
        src = """
subroutine wrapper(x, y)
  integer :: x, y
  y = foo(x)
end subroutine wrapper
"""
        reader = FortranStringReader(src)
        factory = ParserFactory().create(std="f2008")
        ast = factory(reader)
        sub = walk(ast, Fortran2003.Subroutine_Subprogram)[0]
        routine = parser._parse_routine(sub, "<string>")
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.Assignment)
        self.assertIsInstance(stmt.rhs, operators.OpFunc)
        self.assertEqual(stmt.rhs.name, "foo")

    def test_parse_function_call_assignment_in_function(self):
        src = """
function wrapper(x) result(y)
  integer :: x, y
  y = foo(x)
end function wrapper
"""
        reader = FortranStringReader(src)
        factory = ParserFactory().create(std="f2008")
        ast = factory(reader)
        func = walk(ast, Fortran2003.Function_Subprogram)[0]
        routine = parser._parse_routine(func, "<string>")
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.Assignment)
        self.assertIsInstance(stmt.rhs, operators.OpFunc)
        self.assertEqual(stmt.rhs.name, "foo")



if __name__ == "__main__":
    unittest.main()
