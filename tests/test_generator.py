import json
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator


class TestGenerator(unittest.TestCase):
    """Tests auto-diff generation for each example file."""

    def test_store_vars_use_stack(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/store_vars.f90", warn=False)
        lines = generated.splitlines()
        self.assertIn("use fautodiff_stack", lines[2])
        idx_use = next(
            i for i, l in enumerate(lines) if "use fautodiff_stack" in l
        )
        idx_imp = next(i for i, l in enumerate(lines) if "implicit none" in l)
        self.assertLess(idx_use, idx_imp)

    def test_cross_mod_a_writes_fadmod(self):
        code_tree.Node.reset()
        src = Path("examples/cross_mod_a.f90")
        fadmod = Path("cross_mod_a.fadmod")
        if fadmod.exists():
            fadmod.unlink()
        generated = generator.generate_ad(str(src), warn=False)
        expected = src.with_name("cross_mod_a_ad.f90").read_text()
        self.assertEqual(generated, expected)
        self.assertTrue(fadmod.exists())

    def test_cross_mod_b_loads_fadmod(self):
        code_tree.Node.reset()
        # ensure fadmod exists in current directory
        generator.generate_ad("examples/cross_mod_a.f90", warn=False)
        generated = generator.generate_ad(
            "examples/cross_mod_b.f90",
            warn=False,
        )
        expected = Path("examples/cross_mod_b_ad.f90").read_text()
        self.assertEqual(generated, expected)

    def test_missing_fadmod_raises(self):
        code_tree.Node.reset()
        fadmod = Path("cross_mod_a.fadmod")
        if fadmod.exists():
            fadmod.unlink()
        with self.assertRaises(RuntimeError):
            generator.generate_ad(
                "examples/cross_mod_b.f90",
                warn=False,
            )

    def test_fadmod_function_intents_length(self):
        code_tree.Node.reset()
        fadmod = Path("simple_math.fadmod")
        if fadmod.exists():
            fadmod.unlink()
        generator.generate_ad("examples/simple_math.f90", warn=False)
        routines, _, _ = generator._load_fadmods(["simple_math"], ["."])
        info = routines.get("add_numbers")
        self.assertIsNotNone(info)
        self.assertEqual(len(info["args"]), len(info["intents"]))
        self.assertEqual(info["intents"][-1], "out")

    def test_call_module_vars(self):
        code_tree.Node.reset()
        generator.generate_ad("examples/module_vars.f90", warn=False)
        generated = generator.generate_ad(
            "examples/call_module_vars.f90",
            warn=False,
        )
        expected = Path("examples/call_module_vars_ad.f90").read_text()
        self.assertEqual(generated, expected)

    def test_keyword_arg_call(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/keyword_args.f90", warn=False)
        expected = Path("examples/keyword_args_ad.f90").read_text()
        self.assertEqual(generated, expected)

    def test_constant_vars_directive(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = Path(tmp) / "const.f90"
            src.write_text(
                textwrap.dedent(
                    """
                    module test
                    contains
                    !$FAD CONSTANT_VARS: a
                      subroutine foo(a, b)
                        real, intent(in) :: a
                        real, intent(inout) :: b
                        b = a + b
                      end subroutine foo
                    end module test
                    """
                )
            )
            generated = generator.generate_ad(str(src), warn=False)
            self.assertNotIn("a_ad", generated)

    def test_module_variable_diff_by_default(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = Path(tmp) / "modvar.f90"
            src.write_text(
                textwrap.dedent(
                    """
                    module test
                      real :: c
                    contains
                      subroutine foo(x)
                        real, intent(inout) :: x
                        c = c + x
                      end subroutine foo
                    end module test
                    """
                )
            )
            generated = generator.generate_ad(str(src), warn=False)
            self.assertIn("c_ad", generated)

    def test_constant_module_var_directive(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = Path(tmp) / "modvar.f90"
            src.write_text(
                textwrap.dedent(
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
            )
            generated = generator.generate_ad(str(src), warn=False)
            self.assertNotIn("c_ad", generated)

    def test_module_vars_example_no_diff(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/module_vars.f90", warn=False)
        self.assertNotIn("c_ad", generated)

    def test_module_vars_example_fadmod(self):
        code_tree.Node.reset()
        fadmod = Path("module_vars.fadmod")
        if fadmod.exists():
            fadmod.unlink()
        generator.generate_ad("examples/module_vars.f90", warn=False)
        data = json.loads(fadmod.read_text())
        variables = data.get("variables", {})
        self.assertIn("c", variables)

    def test_block_construct(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/block_construct.f90", warn=False)
        expected = Path("examples/block_construct_ad.f90").read_text()
        self.assertEqual(generated, expected)

    def test_fadmod_variable_defaults(self):
        code_tree.Node.reset()
        fadmod = Path("module_vars.fadmod")
        if fadmod.exists():
            fadmod.unlink()
        generator.generate_ad("examples/module_vars.f90", warn=False)
        data = json.loads(fadmod.read_text())
        var_a = data.get("variables", {}).get("a")
        self.assertIsNotNone(var_a)
        self.assertIn("typename", var_a)
        self.assertNotIn("parameter", var_a)
        self.assertNotIn("constant", var_a)
        self.assertNotIn("dims", var_a)
        self.assertNotIn("kind", var_a)
        self.assertNotIn("init_val", var_a)
        self.assertNotIn("allocatable", var_a)
        self.assertNotIn("pointer", var_a)

    def test_module_vars_directive(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = Path(tmp) / "modvar.f90"
            src.write_text(
                textwrap.dedent(
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
            )
            generated = generator.generate_ad(str(src), warn=False)
            self.assertNotIn("c_ad", generated)

    def test_fadmod_includes_skip(self):
        code_tree.Node.reset()
        fadmod = Path("directives.fadmod")
        if fadmod.exists():
            fadmod.unlink()
        generator.generate_ad("examples/directives.f90", warn=False)
        data = json.loads(fadmod.read_text())
        routines = data.get("routines", {})
        self.assertIn("skip_me", routines)
        self.assertTrue(routines["skip_me"].get("skip"))

    def test_skip_call_skips_derivatives(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
            contains
            !$FAD SKIP
              subroutine foo(x, y)
                real, intent(in) :: x
                real, intent(out) :: y
                y = x + 1.0
              end subroutine foo
              subroutine bar(x, z)
                real, intent(in) :: x
                real, intent(out) :: z
                real :: y
                call foo(x, y)
                z = y * x
              end subroutine bar
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "test.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(str(src_path), warn=False, fadmod_dir=tmp)
            fadmod = json.loads((Path(tmp) / "test.fadmod").read_text())
            routines = fadmod.get("routines", {})

        self.assertNotIn("foo_fwd_ad", generated)
        self.assertNotIn("foo_rev_ad", generated)
        self.assertIn("bar_fwd_ad", generated)
        self.assertIn("bar_rev_ad", generated)
        self.assertIn("call foo(x, y)", generated)
        self.assertIn("foo", routines)
        self.assertTrue(routines["foo"].get("skip"))

    def test_mod_grad_var_prevents_skip(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
              real :: a
              real :: a_ad
            contains
              subroutine foo(x)
                real, intent(in) :: x
                a_ad = a_ad + x
              end subroutine foo
              subroutine bar(x)
                real, intent(in) :: x
                a = a + x
              end subroutine bar
              subroutine baz(x)
                real, intent(in) :: x
              end subroutine baz
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "test.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(str(src_path), warn=False, fadmod_dir=tmp)
            fadmod = json.loads((Path(tmp) / "test.fadmod").read_text())
            routines = fadmod.get("routines", {})

        self.assertIn("foo_fwd_ad", generated)
        self.assertIn("foo_rev_ad", generated)
        self.assertIn("bar_fwd_ad", generated)
        self.assertNotIn("baz_fwd_ad", generated)
        self.assertFalse(routines["foo"].get("skip"))
        self.assertFalse(routines["bar"].get("skip"))

    def test_deallocate_mod_grad_var_prevents_skip(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
              real, allocatable :: a(:)
            contains
              subroutine init(n)
                integer, intent(in) :: n
                allocate(a(n))
              end subroutine init
              subroutine fin()
                if (allocated(a)) then
                  deallocate(a)
                end if
              end subroutine fin
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "t.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(str(src_path), warn=False, fadmod_dir=tmp)

        self.assertIn("subroutine fin_fwd_ad", generated)
        self.assertIn("subroutine fin_rev_ad", generated)

    def test_optional_argument(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

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

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "opt.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(str(src_path), warn=False)
            self.assertIn("real, intent(in), optional  :: y", generated)

    def test_preserve_new_attributes(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
              real, save :: a
              type, abstract, bind(C) :: t
              end type t
              type :: seq_t
                sequence
                integer :: i
              end type seq_t
            contains
              subroutine foo(b, c, d)
                integer, value :: b
                real, volatile :: c
                real, asynchronous :: d
                a = c + b
                c = c + d
              end subroutine foo
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "attrs.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(str(src_path), warn=False)
            self.assertIn("real, save :: a", generated)
            self.assertIn("integer, value :: b", generated)
            self.assertIn("real, volatile :: c", generated)
            self.assertIn("real, asynchronous :: d", generated)
            self.assertIn("type, abstract, bind(C) :: t_ad", generated)
            self.assertIn("type :: seq_ad_t", generated)
            self.assertIn("sequence", generated)

    def test_omp_directive_clause_forward(self):
        """Ensure OpenMP directives are kept and `_ad` variables added."""
        code_tree.Node.reset()
        import textwrap
        src = textwrap.dedent(
            """
            module t
            contains
              subroutine s(n, a, x)
                integer, intent(in) :: n
                real, intent(in) :: a(n)
                real, intent(out) :: x
                integer :: i

                x = 0.0
!$omp parallel do reduction(+:x)
                do i = 1, n
                  x = x + a(i)
                end do
!$omp end parallel do
              end subroutine s
            end module t
            """
        )
        import fautodiff.parser as parser
        from unittest.mock import patch
        modules = parser.parse_src(src)
        with patch('fautodiff.generator.parser.parse_file', return_value=modules):
            generated = generator.generate_ad("omp.f90", warn=False)
        self.assertIn("!$omp parallel do reduction(+:x, x_ad)", generated)

    def test_save_variable_treated_like_inout(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(x)
                real, intent(in) :: x
                real, save :: s
                s = s + x
              end subroutine foo
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "save.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(str(src_path), warn=False)
            self.assertIn("real, save :: s", generated)
            self.assertIn("real, save :: s_ad", generated)
            self.assertIn("x_ad = s_ad", generated)
            self.assertNotIn("s_ad = 0", generated)

    def test_persistent_mpi_wrappers(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module pmpi
              use mpi
            contains
              subroutine foo(buf, r, comm)
                real :: buf(1)
                integer :: r, comm, req, ierr
                call MPI_Send_init(buf, 1, MPI_REAL, r, 0, comm, req, ierr)
                call MPI_Start(req, ierr)
                call MPI_Wait(req, MPI_STATUS_IGNORE, ierr)
              end subroutine foo
            end module pmpi
            """
        )

        fadmod = Path(__file__).resolve().parents[1] / 'fortran_modules' / 'mpi.fadmod'
        data = json.loads(fadmod.read_text())
        routines = data.get('routines', {})
        generics = data.get('generics', {})
        self.assertIn('MPI_Start', routines)
        self.assertEqual(routines['MPI_Start']['name_fwd_ad'], 'MPI_Start_fwd_ad')
        self.assertIn('MPI_Wait', routines)
        self.assertEqual(routines['MPI_Wait']['name_fwd_ad'], 'MPI_Wait_fwd_ad')
        self.assertIn('MPI_Send_init_r4', routines)
        self.assertEqual(routines['MPI_Send_init_r4']['name_fwd_rev_ad'], 'MPI_Send_init_fwd_rev_ad')
        self.assertIn('MPI_Recv_init', generics)
        for routine in generics['MPI_Recv_init']:
            self.assertIn(routine, routines)

    def test_mpi_example(self):
        code_tree.Node.reset()
        generated = generator.generate_ad(
            'examples/mpi_example.f90', warn=False, search_dirs=['fortran_modules']
        )
        expected = Path('examples/mpi_example_ad.f90').read_text()
        self.assertEqual(generated, expected)

    def test_where_forall(self):
        code_tree.Node.reset()
        src = Path('examples/where_forall.f90')
        generated = generator.generate_ad(str(src), warn=False)
        expected = src.with_name('where_forall_ad.f90').read_text()
        self.assertEqual(generated, expected)


def _make_example_test(src: Path):
    def test(self):
        code_tree.Node.reset()
        generated = generator.generate_ad(str(src), warn=False)
        expected = src.with_name(src.stem + "_ad.f90").read_text()
        self.assertEqual(generated, expected, msg=f"Mismatch for {src.name}")

    return test


examples_dir = Path("examples")
for _src in sorted(examples_dir.glob("*.f90")):
    if _src.name.endswith("_ad.f90") or _src.stem in {"cross_mod_a", "cross_mod_b", "call_module_vars", "pointer_arrays", "mpi_example", "omp_loops"}:
        continue
    test_name = f"test_{_src.stem}"
    setattr(TestGenerator, test_name, _make_example_test(_src))

class TestPointerArrays(unittest.TestCase):
    def test_pointer_arrays(self):
        code_tree.Node.reset()
        src = Path("examples/pointer_arrays.f90")
        generated = generator.generate_ad(str(src), warn=False)
        expected = src.with_name("pointer_arrays_ad.f90").read_text()
        self.assertEqual(generated, expected)


if __name__ == "__main__":
    unittest.main()
