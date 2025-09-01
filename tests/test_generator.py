import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, fadmod, generator


class TestGenerator(unittest.TestCase):
    """Tests auto-diff generation for each example file."""

    def test_store_vars_use_stack(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/store_vars.f90", warn=False)
        lines = generated.splitlines()
        self.assertIn("use fautodiff_stack", lines[2])
        idx_use = next(i for i, l in enumerate(lines) if "use fautodiff_stack" in l)
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
        fadmod_path = Path("simple_math.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        generator.generate_ad("examples/simple_math.f90", warn=False)
        fm = fadmod.FadmodBase.load(fadmod_path)
        info = fm.routines.get("add_numbers")
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

    def test_dependency_groups(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        import fautodiff.parser as parser

        src = textwrap.dedent(
            """
            module cyc
            contains
              subroutine a(x, y)
                real, intent(in) :: x
                real, intent(out) :: y
                call b(x, y)
              end subroutine a
              subroutine b(x, y)
                real, intent(in) :: x
                real, intent(out) :: y
                call a(x, y)
              end subroutine b
              subroutine c(x, y)
                real, intent(in) :: x
                real, intent(out) :: y
                y = x
              end subroutine c
            end module cyc
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "cyc.f90"
            src_path.write_text(src)
            modules = parser.parse_file(str(src_path))
        groups, _ = generator._dependency_groups(modules[0].routines)
        group_sets = [set(g) for g in groups]
        self.assertIn({"a", "b"}, group_sets)
        self.assertIn({"c"}, group_sets)

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

    def test_module_vars_example_fadmod(self):
        code_tree.Node.reset()
        fadmod_path = Path("module_vars.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        generator.generate_ad("examples/module_vars.f90", warn=False)
        fm = fadmod.FadmodBase.load(fadmod_path)
        self.assertIn("c", fm.variables_raw)

    def test_fadmod_variable_defaults(self):
        code_tree.Node.reset()
        fadmod_path = Path("module_vars.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        generator.generate_ad("examples/module_vars.f90", warn=False)
        fm = fadmod.FadmodBase.load(fadmod_path)
        var_a = fm.variables_raw.get("a")
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

    def test_local_var_shadows_module_var(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = Path(tmp) / "shadow.f90"
            src.write_text(
                textwrap.dedent(
                    """
                    module m
                      implicit none
                      real, allocatable :: a(:)
                    contains
                      subroutine foo(n, x)
                        integer, intent(in) :: n
                        real, intent(inout) :: x
                        real, allocatable :: a(:)
                        allocate(a(n))
                        a = x
                        x = sum(a)
                        deallocate(a)
                      end subroutine foo
                    end module m
                    """
                )
            )
            generated = generator.generate_ad(str(src), warn=False)
            self.assertIn("allocate(a(n))", generated)
            self.assertIn("deallocate(a)", generated)
            self.assertIn("allocate(a_ad(n))", generated)
        self.assertIn("deallocate(a_ad)", generated)

    def test_forward_allocate_also_allocates_derivative(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module m
            contains
              subroutine foo(n, x)
                integer, intent(in) :: n
                real, intent(inout) :: x
                real, allocatable :: a(:)
                allocate(a(n))
                a = x
                x = sum(a)
                deallocate(a)
              end subroutine foo
            end module m
            """
        )
        with TemporaryDirectory() as tmp:
            p = Path(tmp) / "m.f90"
            p.write_text(src)
            generated = generator.generate_ad(str(p), warn=False, mode="forward")
        self.assertIn("allocate(a_ad(n))", generated)
        self.assertIn("deallocate(a_ad)", generated)

    def test_fadmod_includes_skip(self):
        code_tree.Node.reset()
        fadmod_path = Path("directives.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        generator.generate_ad("examples/directives.f90", warn=False)
        fm = fadmod.FadmodBase.load(fadmod_path)
        routines = fm.routines
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
            fm = fadmod.FadmodBase.load(Path(tmp) / "test.fadmod")
            routines = fm.routines

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
            fm = fadmod.FadmodBase.load(Path(tmp) / "test.fadmod")
            routines = fm.routines

        self.assertIn("foo_fwd_ad", generated)
        self.assertIn("foo_rev_ad", generated)
        self.assertIn("bar_fwd_ad", generated)
        self.assertNotIn("baz_fwd_ad", generated)
        self.assertFalse(routines["foo"].get("skip"))
        self.assertFalse(routines["bar"].get("skip"))

    def test_macro_multistmt_example(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/macro_multistmt.F90", warn=False)
        expected = Path("examples/macro_multistmt_ad.F90").read_text()
        self.assertEqual(generated, expected)

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
        from unittest.mock import patch

        import fautodiff.parser as parser

        modules = parser.parse_src(src)
        with patch("fautodiff.generator.parser.parse_file", return_value=modules):
            generated = generator.generate_ad("omp.f90", warn=False)
        self.assertIn("!$omp parallel do reduction(+:x, x_ad)", generated)

    def test_omp_workshare_directive(self):
        code_tree.Node.reset()
        import textwrap

        src = textwrap.dedent(
            """
            module t
            contains
              subroutine s(n, x, y)
                integer, intent(in) :: n
                real, intent(in) :: x(n)
                real, intent(out) :: y(n)
                !$omp parallel workshare
                y = x
                !$omp end parallel workshare
              end subroutine s
            end module t
            """
        )
        from unittest.mock import patch

        import fautodiff.parser as parser

        modules = parser.parse_src(src)
        with patch("fautodiff.generator.parser.parse_file", return_value=modules):
            generated = generator.generate_ad("omp_ws.f90", warn=False)
        self.assertIn("!$omp parallel workshare", generated)
        self.assertIn("!$omp end parallel workshare", generated)

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

    def test_save_assignment_allocatable(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(n, x, y, z)
                integer, intent(in) :: n
                real, intent(in) :: x(n), y
                real, intent(out) :: z
                real, allocatable :: htmp(:)
                integer :: i
                allocate(htmp(n))
                htmp = x
                z = 0.0
                do i = 1, n
                  z = z + htmp(i) * y
                end do
                htmp = x**2
                do i = 1, n
                  z = z + htmp(i) * y
                end do
                deallocate(htmp)
              end subroutine foo
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "save_alloc.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(str(src_path), warn=False)
            lines = generated.splitlines()
            save_decl = next(l for l in lines if "htmp_save" in l)
            self.assertRegex(save_decl, r"allocatable :: .*\(:\)")
            self.assertNotIn("lbound", save_decl)
            self.assertNotIn("ubound", save_decl)
            self.assertNotRegex(
                generated, r"if \(\.not\. allocated\(htmp_save_\d+_ad\)\)"
            )
            self.assertRegex(generated, r"allocate\(htmp_save_\d+_ad, mold=htmp\)")
            self.assertNotIn("if (.not. allocated(htmp))", generated)
            self.assertNotRegex(generated, r"allocate\(htmp, mold=htmp_save_\d+_ad\)")

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

        fadmod_path = (
            Path(__file__).resolve().parents[1] / "fortran_modules" / "mpi.fadmod"
        )
        fm = fadmod.FadmodBase.load(fadmod_path)
        routines = fm.routines
        generics = fm.generics
        self.assertIn("MPI_Start", routines)
        self.assertEqual(routines["MPI_Start"]["name_fwd_ad"], "MPI_Start_fwd_ad")
        self.assertIn("MPI_Wait", routines)
        self.assertEqual(routines["MPI_Wait"]["name_fwd_ad"], "MPI_Wait_fwd_ad")
        self.assertIn("MPI_Send_init_r4", routines)
        self.assertEqual(
            routines["MPI_Send_init_r4"]["name_fwd_rev_ad"], "MPI_Send_init_fwd_rev_ad"
        )
        self.assertIn("MPI_Recv_init", generics)
        for routine in generics["MPI_Recv_init"]:
            self.assertIn(routine, routines)

    def test_mpi_send_multidim(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module m
              use mpi
            contains
              subroutine foo(a)
                real :: a(2,3)
                integer :: ierr
                call MPI_Send(a, 6, MPI_REAL, 0, 0, MPI_COMM_WORLD, ierr)
              end subroutine foo
            end module m
            """
        )

        with TemporaryDirectory() as tmp:
            src_path = Path(tmp) / "m.f90"
            src_path.write_text(src)
            generated = generator.generate_ad(
                str(src_path), warn=False, search_dirs=["fortran_modules"]
            )
        self.assertIn("MPI_Send_rev_ad", generated)

    def test_reverse_dealloc_guard_deduplicated(self):
        code_tree.Node.reset()
        import re
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module m
              real, allocatable :: z(:)
            contains
              subroutine init()
                allocate(z(3))
              end subroutine init
              subroutine foo()
                if (allocated(z)) then
                  deallocate(z)
                end if
              end subroutine foo
            end module m
            """
        )

        with TemporaryDirectory() as tmp:
            p = Path(tmp) / "m.f90"
            p.write_text(src)
            generated = generator.generate_ad(str(p), warn=False, mode="reverse")
            # No nested duplicate guard of the same condition inside the block
            self.assertIsNone(
                re.search(
                    r"if \.not\. allocated\(z_ad\) then[\s\S]*?if \.not\. allocated\(z_ad\) then",
                    generated,
                )
            )
            # At least one guard should exist for safety
            guards = re.findall(r"if \(\.not\. allocated\(z_ad\)\) then", generated)
            self.assertGreaterEqual(len(guards), 1)
            # Ensure allocation of z_ad with mold=z exists
            self.assertRegex(generated, r"allocate\(z_ad(?:\([^)]*\))?, mold=z\)")

    def test_reverse_fwblock_ad_alloc_near_primal(self):
        """In reverse mode, allocate AD arrays right before primal allocate.

        Ensure sizes computed in fw_block (e.g., m = n + 1) appear before the
        AD allocation, and that the AD allocation is placed immediately before
        the corresponding primal allocate, not at the routine top.
        """
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module m
            contains
              subroutine foo(n, x, y)
                integer, intent(in) :: n
                real, intent(in) :: x
                real, intent(out) :: y
                real, allocatable :: a(:)
                integer :: m
                m = n + 1
                allocate(a(m))
                a = x
                y = sum(a)
                deallocate(a)
              end subroutine foo
            end module m
            """
        )

        with TemporaryDirectory() as tmp:
            p = Path(tmp) / "m.f90"
            p.write_text(src)
            generated = generator.generate_ad(str(p), warn=False, mode="reverse")
        lines = generated.splitlines()
        # Find indices of key lines
        import re as _re

        idx_assign = next(
            i for i, l in enumerate(lines) if _re.search(r"m\s*=\s*n\s*\+\s*1", l)
        )
        idx_alloc_ad = next(i for i, l in enumerate(lines) if "allocate(a_ad(" in l)
        # Order: compute size -> allocate a_ad
        self.assertLess(idx_assign, idx_alloc_ad)


def _make_example_test(src: Path):
    def test(self):
        code_tree.Node.reset()
        generated = generator.generate_ad(
            str(src), warn=False, search_dirs=[".", "examples", "fortran_modules"]
        )
        expected = src.with_name(src.stem + "_ad" + src.suffix).read_text()
        self.assertEqual(generated, expected, msg=f"Mismatch for {src.name}")

    return test


examples_dir = Path("examples")
srcs = list(examples_dir.glob("*.f90")) + list(examples_dir.glob("*.F90"))
for _src in sorted(srcs):
    if _src.stem.endswith("_ad"):
        continue
    test_name = f"test_{_src.stem}"
    if hasattr(TestGenerator, test_name):
        continue
    setattr(TestGenerator, test_name, _make_example_test(_src))


if __name__ == "__main__":
    unittest.main()
