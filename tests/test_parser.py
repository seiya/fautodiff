import sys
import textwrap
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator, operators, parser
from fautodiff.code_tree import Block, DoLoop, OmpDirective, render_program


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
        src = textwrap.dedent(
            """\
        module test
        contains
          subroutine wrapper(x)
            integer :: x
            call foo(x)
          end subroutine wrapper
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.name, "foo")

    def test_parse_call_stmt_keyword_args(self):
        src = textwrap.dedent(
            """\
        module test
        contains
          subroutine wrapper()
            call foo(a=1, b=2)
          end subroutine wrapper
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.arg_keys, ["a", "b"])
        self.assertEqual(render_program(Block([stmt])), "call foo(a=1, b=2)\n")

    def test_parse_call_stmt_in_function(self):
        src = textwrap.dedent(
            """\
        module test
        contains
          function wrapper(x) result(y)
            integer :: x, y
            call foo(x)
            y = x
          end function wrapper
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.CallStatement)
        self.assertEqual(stmt.name, "foo")

    def test_parse_function_call_assignment(self):
        src = textwrap.dedent(
            """\
        module test
        contains
          subroutine wrapper(x, y)
            integer :: x, y
            y = foo(x)
          end subroutine wrapper
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.Assignment)
        self.assertIsInstance(stmt.rhs, operators.OpFuncUser)
        self.assertEqual(stmt.rhs.name, "foo")

    def test_command_argument_count(self):
        src = textwrap.dedent(
            """\
            module t
            contains
              subroutine foo()
                if (command_argument_count() > 0) then
                end if
              end subroutine foo
            end module t
            """
        )
        mod = parser.parse_src(src)[0]
        r = mod.routines[0]
        cond = r.content.first().cond_blocks[0][0]
        self.assertIsInstance(cond.args[0], operators.OpFunc)
        self.assertEqual(cond.args[0].var_type.typename.lower(), "integer")

    def test_parse_function_call_assignment_in_function(self):
        src = textwrap.dedent(
            """\
        module test
        contains
          function wrapper(x) result(y)
            integer :: x, y
            y = foo(x)
          end function wrapper
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.Assignment)
        self.assertIsInstance(stmt.rhs, operators.OpFuncUser)
        self.assertEqual(stmt.rhs.name, "foo")

    def test_parse_real_kind_8(self):
        src = textwrap.dedent(
            """\
        module test
        contains
          subroutine foo(x)
            real(kind=8) :: x
          end subroutine foo
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.var_type.kind, "8")

    def test_parse_real_kind_rp(self):
        src = textwrap.dedent(
            """\
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
        routine = module.routines[0]
        decl = routine.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.var_type.kind, "RP")

    def test_parse_dimension(self):
        src = textwrap.dedent(
            """\
        module test
        contains
          subroutine foo(n, x, y)
            integer :: n
            real, dimension(:) :: x
            real, dimension(n) :: y
          end subroutine foo
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.dims, (":",))
        decl = routine.decls.find_by_name("y")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.dims, ("n",))

    def test_parse_assumed_size(self):
        src = textwrap.dedent(
            """\
        module test
        contains
          subroutine foo(n, a, b)
            integer :: n
            real :: a(*)
            real :: b(n, *)
          end subroutine foo
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("a")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.dims, ("*",))
        decl = routine.decls.find_by_name("b")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.dims, ("n", "*"))

    def test_parse_public(self):
        src = textwrap.dedent(
            """\
        module test
          private
          real, public :: x
          real, private :: y
          real :: z
        contains
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        decl = module.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.access, "public")
        decl = module.decls.find_by_name("y")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.access, "private")
        decl = module.decls.find_by_name("z")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.access, "private")

    def test_routine_decl_map(self):
        src = Path(__file__).resolve().parents[1] / "examples" / "real_kind.f90"
        module = parser.parse_file(str(src))[0]
        routine = next(r for r in module.routines if r.name == "scale_rp")
        self.assertTrue(routine.is_declared("RP"))
        decl = routine.decl_map.get("RP")
        self.assertIsNotNone(decl)
        var = routine.get_var("RP")
        self.assertIsNotNone(var)
        self.assertTrue(var.is_constant)

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
        self.assertEqual(decl.var_type.typename, "integer")
        self.assertEqual(decl.access, "public")

    def test_module_vars_default_diff(self):
        src = textwrap.dedent(
            """
            module test
              real :: c
            contains
              subroutine foo(x)
                real, intent(in) :: x
                c = c + x
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        decl = module.decls.find_by_name("c")
        self.assertIsNotNone(decl)
        self.assertFalse(decl.constant)
        var = module.routines[0].get_var("c")
        self.assertTrue(var.ad_target)

    def test_parse_example_module_vars(self):
        base = Path(__file__).resolve().parents[1]
        src = base / "examples" / "module_vars.f90"
        modules = parser.parse_file(str(src))
        self.assertEqual(len(modules), 1)
        mod = modules[0]
        decl = mod.decls.find_by_name("c")
        self.assertIsNotNone(decl)
        self.assertEqual(mod.routines[0].name, "inc_and_use")

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
        src = textwrap.dedent(
            """\
        module test
        contains
          subroutine foo(x)
            double precision :: x
          end subroutine foo
        end module test
        """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("x")
        self.assertIsNotNone(decl)
        self.assertEqual(decl.var_type.typename.lower(), "double precision")
        self.assertIsNone(decl.var_type.kind)

    def test_parse_directives_constant_vars(self):
        src = textwrap.dedent(
            """
            module test
            contains
            !$FAD CONSTANT_VARS: x, y
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
        self.assertIn("CONSTANT_VARS", routine.directives)
        self.assertEqual(routine.directives["CONSTANT_VARS"], ["x", "y"])
        decl_x = routine.decls.find_by_name("x")
        decl_y = routine.decls.find_by_name("y")
        self.assertTrue(decl_x.constant)
        self.assertTrue(decl_y.constant)
        var = routine.get_var("x")
        self.assertTrue(var.is_constant)

    def test_parse_file_directive_constant_vars(self):
        base = Path(__file__).resolve().parents[1]
        src = base / "examples" / "directives.f90"
        modules = parser.parse_file(str(src))
        routine = modules[0].routines[0]
        self.assertIn("CONSTANT_VARS", routine.directives)
        self.assertEqual(routine.directives["CONSTANT_VARS"], ["z"])
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

    def test_parse_directive_constant_module_vars(self):
        src = textwrap.dedent(
            """
            module test
              real :: c
              !$FAD CONSTANT_VARS: c
            contains
              subroutine foo(x)
                real, intent(inout) :: x
                c = c + x
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        self.assertIn("CONSTANT_VARS", module.directives)
        self.assertEqual(module.directives["CONSTANT_VARS"], ["c"])
        decl = routine.decl_map.get("c")
        self.assertTrue(decl.constant)
        var = routine.get_var("c")
        self.assertFalse(var.ad_target)

    def test_decl_map_from_fadmod(self):
        code_tree.Node.reset()
        from tempfile import TemporaryDirectory

        moda_src = textwrap.dedent(
            """
            module moda
              implicit none
              integer, parameter :: K = 3
            contains
              subroutine dummy()
              end subroutine dummy
            end module moda
            """
        )

        modb_src = textwrap.dedent(
            """
            module modb
              use moda
              implicit none
            contains
              subroutine foo(x)
                integer, intent(out) :: x
                x = K
              end subroutine foo
            end module modb
            """
        )

        with TemporaryDirectory() as tmp:
            moda = Path(tmp) / "moda.f90"
            moda.write_text(moda_src)
            generator.generate_ad(str(moda), warn=False, fadmod_dir=tmp)
            modb = Path(tmp) / "modb.f90"
            modb.write_text(modb_src)

            modules = parser.parse_file(str(modb), search_dirs=[tmp])
            routine = modules[0].routines[0]

        self.assertTrue(routine.is_declared("K"))
        decl = routine.decl_map.get("K")
        self.assertIsNotNone(decl)
        self.assertTrue(decl.parameter)

    def test_parse_allocate_deallocate(self):
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(n)
                integer, intent(in) :: n
                integer, allocatable :: a(:)
                allocate(a(n))
                deallocate(a)
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmts = list(routine.content.iter_children())
        self.assertIsInstance(stmts[0], code_tree.Allocate)
        self.assertEqual(stmts[0].vars[0].name, "a")
        self.assertEqual(str(stmts[0].vars[0].index), "n")
        self.assertIsInstance(stmts[1], code_tree.Deallocate)
        self.assertEqual(stmts[1].vars[0].name, "a")

    def test_declared_in_attribute(self):
        src = textwrap.dedent(
            """
            module moda
              real :: a
            contains
              subroutine dummy()
              end subroutine dummy
            end module moda

            module modb
              use moda
            contains
              subroutine foo(x)
                real, intent(in) :: x
                a = a + x
              end subroutine foo
            end module modb
            """
        )
        modules = parser.parse_src(src)
        moda = modules[0]
        modb = modules[1]
        routine = modb.routines[0]

        decl_x = routine.decls.find_by_name("x")
        self.assertEqual(decl_x.declared_in, "routine")

        decl_a_module = moda.decls.find_by_name("a")
        self.assertEqual(decl_a_module.declared_in, "module")

        decl_a_use = routine.decl_map.get("a")
        self.assertEqual(decl_a_use.declared_in, "use")

        var_a = routine.get_var("a")
        self.assertEqual(var_a.declared_in, "use")

    def test_use_module_defined_later(self):
        src = textwrap.dedent(
            """
            module modb
              use moda
              implicit none
            contains
              subroutine foo(x)
                integer, intent(out) :: x
                x = K
              end subroutine foo
            end module modb

            module moda
              implicit none
              integer, parameter :: K = 3
            contains
              subroutine dummy()
              end subroutine dummy
            end module moda
            """
        )
        modules = parser.parse_src(src)
        routine = modules[0].routines[0]
        self.assertTrue(routine.is_declared("K"))
        decl = routine.decl_map.get("K")
        self.assertIsNotNone(decl)
        self.assertTrue(decl.parameter)

    def test_parse_pointer_decl(self):
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo()
                real, pointer :: p(:)
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("p")
        self.assertIsNotNone(decl)
        self.assertTrue(decl.pointer)
        self.assertEqual(render_program(Block([decl])).strip(), "real, pointer :: p(:)")

    def test_parse_pointer_assignment(self):
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(p, q)
                real, pointer :: p
                real :: q
                p => q
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.PointerAssignment)
        self.assertEqual(render_program(Block([stmt])), "p => q\n")

    def test_parse_optional_argument(self):
        src = textwrap.dedent(
            """
            module optmod
            contains
              subroutine foo(x, y)
                real, intent(inout) :: x
                real, intent(in), optional :: y
                if (present(y)) then
                  x = x + y
                end if
              end subroutine foo
            end module optmod
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("y")
        self.assertTrue(decl.optional)
        self.assertEqual(
            render_program(Block([decl])).strip(),
            "real, intent(in), optional  :: y",
        )

    def test_new_decl_and_type_attrs(self):
        src = textwrap.dedent(
            """
            module test
              type, abstract, bind(C) :: t
              end type t
              type :: seq_t
                sequence
                integer :: i
              end type seq_t
            contains
              subroutine foo(a, b, c, d)
                real, save :: a
                integer, value :: b
                real, volatile :: c
                real, asynchronous :: d
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decls = routine.decls
        self.assertTrue(decls.find_by_name("a").save)
        self.assertTrue(decls.find_by_name("b").value)
        self.assertTrue(decls.find_by_name("c").volatile)
        self.assertTrue(decls.find_by_name("d").asynchronous)
        type_defs = [
            d for d in module.decls.iter_children() if isinstance(d, code_tree.TypeDef)
        ]
        t = next(d for d in type_defs if d.name == "t")
        self.assertTrue(t.abstract)
        self.assertEqual(t.bind, "C")
        seq_t = next(d for d in type_defs if d.name == "seq_t")
        self.assertTrue(seq_t.sequence)

    def test_parse_optional_argument(self):
        src = textwrap.dedent(
            """
            module optmod
            contains
              subroutine foo(x, y)
                real, intent(inout) :: x
                real, intent(in), optional :: y
                if (present(y)) then
                  x = x + y
                end if
              end subroutine foo
            end module optmod
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decl = routine.decls.find_by_name("y")
        self.assertTrue(decl.optional)
        self.assertEqual(
            render_program(Block([decl])).strip(),
            "real, intent(in), optional  :: y",
        )

    def test_new_decl_and_type_attrs(self):
        src = textwrap.dedent(
            """
            module test
              type, abstract, bind(C) :: t
              end type t
              type :: seq_t
                sequence
                integer :: i
              end type seq_t
            contains
              subroutine foo(a, b, c, d)
                real, save :: a
                integer, value :: b
                real, volatile :: c
                real, asynchronous :: d
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        decls = routine.decls
        self.assertTrue(decls.find_by_name("a").save)
        self.assertTrue(decls.find_by_name("b").value)
        self.assertTrue(decls.find_by_name("c").volatile)
        self.assertTrue(decls.find_by_name("d").asynchronous)
        type_defs = [
            d for d in module.decls.iter_children() if isinstance(d, code_tree.TypeDef)
        ]
        t = next(d for d in type_defs if d.name == "t")
        self.assertTrue(t.abstract)
        self.assertEqual(t.bind, "C")
        seq_t = next(d for d in type_defs if d.name == "seq_t")
        self.assertTrue(seq_t.sequence)

    def test_use_missing_fadmod_raises(self):
        # Using an external module with search_dirs provided but no fadmod file
        # should raise an error when declarations are requested.
        src = textwrap.dedent(
            """
            module modb
            contains
              subroutine foo(x)
                use moda
                integer, intent(out) :: x
                x = 0
              end subroutine foo
            end module modb
            """
        )
        with self.assertRaises(RuntimeError):
            parser.parse_src(src, search_dirs=["."])

    def test_routine_decl_with_access_error(self):
        # Routine-level variable with access spec should raise
        src = textwrap.dedent(
            """
            module m
            contains
              subroutine s()
                integer, public :: x
              end subroutine s
            end module m
            """
        )
        with self.assertRaises(RuntimeError):
            parser.parse_src(src)


class TestParserOmp(unittest.TestCase):

    def test_parse_omp_do(self):
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(n)
                integer :: n, i
                integer :: a(n)
!$omp parallel do private(i)
                do i = 1, n
                  a(i) = i
                end do
!$omp end parallel do
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.OmpDirective)
        self.assertEqual(stmt.directive.lower(), "parallel do")
        self.assertIsInstance(stmt.body, code_tree.DoLoop)

    def test_parse_omp_do_no_end(self):
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(n)
                integer :: n, i
                integer :: a(n)
!$omp parallel do private(i)
                do i = 1, n
                  a(i) = i
                end do
                a(1) = 0
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        self.assertEqual(len(routine.content), 2)
        stmt = routine.content[0]
        self.assertIsInstance(stmt, code_tree.OmpDirective)
        self.assertEqual(stmt.directive.lower(), "parallel do")
        self.assertIsInstance(stmt.body, code_tree.DoLoop)
        self.assertIsInstance(routine.content[1], code_tree.Assignment)

    def test_parse_omp_parallel_block(self):
        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo()
!$omp parallel
                call bar()
!$omp end parallel
              end subroutine foo
            end module test
            """
        )
        module = parser.parse_src(src)[0]
        routine = module.routines[0]
        stmt = routine.content.first()
        self.assertIsInstance(stmt, code_tree.OmpDirective)
        self.assertEqual(stmt.directive.lower(), "parallel")
        self.assertIsInstance(stmt.body, code_tree.Block)

    def test_omp_standalone_barrier(self):
        src = textwrap.dedent(
            """
            module t
            contains
              subroutine foo()
                integer :: x
!$omp barrier
                x = 1
              end subroutine foo
            end module t
            """
        )
        mod = parser.parse_src(src)[0]
        r = mod.routines[0]
        # OmpDirective should be present
        self.assertTrue(any(isinstance(n, OmpDirective) for n in r.content))
        txt = render_program(r.content)
        self.assertIn("!$omp barrier", txt)

    def test_omp_do_follows_stmt(self):
        src = textwrap.dedent(
            """
            module t
            contains
              subroutine foo(n)
                integer :: n, i, x
!$omp do
                do i = 1, n
                  x = i
                end do
              end subroutine foo
            end module t
            """
        )
        mod = parser.parse_src(src)[0]
        r = mod.routines[0]
        omp_nodes = [n for n in r.content if isinstance(n, OmpDirective)]
        self.assertTrue(omp_nodes)
        self.assertIsInstance(omp_nodes[0].body, DoLoop)


class TestPreprocessorParsing(unittest.TestCase):
    def test_cpp_lines_do_not_break_parsing(self):
        # Ensure the parser tolerates C-preprocessor lines in the body.
        src = textwrap.dedent(
            """
            module t
            contains
              subroutine foo()
            #if defined(A)
                call bar()
            #else
                call qux()
            #endif
              end subroutine foo
            end module t
            """
        )
        mod = parser.parse_src(src)[0]
        r = mod.routines[0]
        # Calls should still be parsed
        body = render_program(r.content)
        self.assertIn("call bar()", body)
        self.assertIn("call qux()", body)

    def test_cpp_conditional_inside_statement(self):
        src = textwrap.dedent(
            """
            module t
            contains
              subroutine foo(x, y, z)
                real :: x, y, z
                x = y &
            #ifdef Z
                  + z &
            #endif
                  + 1.0
              end subroutine foo
            end module t
            """
        )
        mod = parser.parse_src(src)[0]
        r = mod.routines[0]
        body = render_program(r.content)
        self.assertIn("#ifdef Z", body)
        self.assertIn("x = y + z + 1.0", body)
        self.assertIn("#else", body)
        self.assertIn("x = y + 1.0", body)


if __name__ == "__main__":
    unittest.main()
