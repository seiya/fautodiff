import sys
from pathlib import Path
import unittest
import textwrap

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import parser, code_tree, operators
from fautodiff.code_tree import Block, render_program


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
        src = textwrap.dedent("""\
        module test
        contains
          subroutine wrapper(x)
            integer :: x
            call foo(x)
          end subroutine wrapper
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.name, "foo")

    def test_parse_call_stmt_keyword_args(self):
        src = textwrap.dedent("""\
        module test
        contains
          subroutine wrapper()
            call foo(a=1, b=2)
          end subroutine wrapper
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.arg_keys, ["a", "b"])
        self.assertEqual(render_program(Block([stmt])), "call foo(a=1, b=2)\n")

    def test_parse_call_stmt_in_function(self):
        src = textwrap.dedent("""\
        module test
        contains
          function wrapper(x) result(y)
            integer :: x, y
            call foo(x)
            y = x
          end function wrapper
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.name, "foo")

    def test_parse_function_call_assignment(self):
        src = textwrap.dedent("""\
        module test
        contains
          subroutine wrapper(x, y)
            integer :: x, y
            y = foo(x)
          end subroutine wrapper
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.Assignment)
        self.assertIsInstance(stmt.rhs, operators.OpFuncUser)
        self.assertEqual(stmt.rhs.name, "foo")

    def test_parse_function_call_assignment_in_function(self):
        src = textwrap.dedent("""\
        module test
        contains
          function wrapper(x) result(y)
            integer :: x, y
            y = foo(x)
          end function wrapper
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.Assignment)
        self.assertIsInstance(stmt.rhs, operators.OpFuncUser)
        self.assertEqual(stmt.rhs.name, "foo")

    def test_parse_real_kind_8(self):
        src = textwrap.dedent("""\
        module test
        contains
          subroutine foo(x)
            real(kind=8) :: x
          end subroutine foo
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.kind, "8")

    def test_parse_real_kind_rp(self):
        src = textwrap.dedent("""\
        module test
        integer, parameter :: RP = kind(1.0d0)
        contains
          subroutine foo(x)
            real(kind=RP) :: x
          end subroutine foo
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.kind, "RP")

    def test_module_level_decls(self):
        src = textwrap.dedent(
            """
            module test
              integer, parameter :: RP = kind(1.0d0)
            contains
              subroutine foo(x)
                real(kind=RP) :: x
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        decl = module.decls.find_by_name("RP")
        self.assertIsNotNone(decl)
        self.assertTrue(decl.parameter)
        self.assertEqual(decl.typename, "integer")
        self.assertEqual(decl.access, "public")

    def test_module_decl_with_intent_error(self):
        src = textwrap.dedent(
            """
            module test
              real, intent(in) :: x
            contains
              subroutine foo()
              end subroutine foo
            end module test
            """
        )
        with self.assertRaises(RuntimeError):
            parser.parse_src(src)

    def test_module_default_private(self):
        src = textwrap.dedent(
            """
            module test
              private
              integer :: x
            contains
              subroutine foo()
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        decl = module.decls.find_by_name("x")
        self.assertEqual(decl.access, "private")

    def test_module_public_override(self):
        src = textwrap.dedent(
            """
            module test
              private
              integer :: x
              public :: x
            contains
              subroutine foo()
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        decl = module.decls.find_by_name("x")
        self.assertEqual(decl.access, "public")

    def test_parse_double_precision(self):
        src = textwrap.dedent("""\
        module test
        contains
          subroutine foo(x)
            double precision :: x
          end subroutine foo
        end module test
        """)
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.typename.lower(), "double precision")
        self.assertIsNone(decl.kind)

    def test_parse_directives_constant_args(self):
        src = textwrap.dedent(
            """
            module test
            contains
            !$FAD CONSTANT_ARGS: x, y
              subroutine foo(x, y, z)
                real, intent(in) :: x, y
                real, intent(out) :: z
                z = x + y
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        self.assertIn("CONSTANT_ARGS", routine.directives)
        self.assertEqual(routine.directives["CONSTANT_ARGS"], ["x", "y"])
        decl_x = routine.decls.find_by_name("x")
        decl_y = routine.decls.find_by_name("y")
        self.assertTrue(decl_x.constant)
        self.assertTrue(decl_y.constant)
        var = routine.get_var("x")
        self.assertTrue(var.is_constant)

    def test_parse_file_directive_constant_args(self):
        base = Path(__file__).resolve().parents[1]
        src = base / "examples" / "directives.f90"
        modules = parser.parse_file(str(src))
        routine = modules[0].routines[0]
        self.assertIn("CONSTANT_ARGS", routine.directives)
        self.assertEqual(routine.directives["CONSTANT_ARGS"], ["z"])
        decl = routine.decls.find_by_name("z")
        self.assertTrue(decl.constant)
        var = routine.get_var("z")
        self.assertTrue(var.is_constant)

    def test_parse_directive_skip(self):
        src = textwrap.dedent(
            """
            module test
            contains
            !$FAD SKIP
              subroutine foo(x)
                real :: x
                x = x + 1.0
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        self.assertIn("SKIP", routine.directives)



if __name__ == "__main__":
    unittest.main()
