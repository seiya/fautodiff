import sys
import unittest
import textwrap
from contextlib import redirect_stderr
from io import StringIO
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, fadmod, generator


def gen(path, **kwargs):
    p = Path(path)
    return generator.generate_ad(p.read_text(), str(p), **kwargs)


class TestGenerator(unittest.TestCase):
    """Tests auto-diff generation for each example file."""

    def test_store_vars_use_stack(self):
        code_tree.Node.reset()
        generated = gen("examples/store_vars.f90", warn=False)
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
        generated = gen(src, warn=False)
        expected = src.with_name("cross_mod_a_ad.f90").read_text()
        self.assertEqual(generated, expected)
        self.assertTrue(fadmod.exists())

    def test_cross_mod_b_loads_fadmod(self):
        code_tree.Node.reset()
        # ensure fadmod exists in current directory
        gen("examples/cross_mod_a.f90", warn=False)
        generated = gen(
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
            gen(
                "examples/cross_mod_b.f90",
                warn=False,
            )

    def test_fadmod_function_intents_length(self):
        code_tree.Node.reset()
        fadmod_path = Path("simple_math.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        gen("examples/simple_math.f90", warn=False)
        fm = fadmod.FadmodBase.load(fadmod_path)
        info = fm.routines.get("add_numbers")
        self.assertIsNotNone(info)
        self.assertEqual(len(info["args"]), len(info["intents"]))
        self.assertEqual(info["intents"][-1], "out")

    def test_call_module_vars(self):
        code_tree.Node.reset()
        gen("examples/module_vars.f90", warn=False)
        generated = gen(
            "examples/call_module_vars.f90",
            warn=False,
        )
        expected = Path("examples/call_module_vars_ad.f90").read_text()
        self.assertEqual(generated, expected)

    def test_dependency_groups(self):
        code_tree.Node.reset()
        import textwrap

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

        modules = parser.parse_src(src)
        groups, _ = generator._dependency_groups(modules[0].routines)
        group_sets = [set(g) for g in groups]
        self.assertIn({"a", "b"}, group_sets)
        self.assertIn({"c"}, group_sets)

    def test_constant_vars_directive(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = textwrap.dedent(
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "const.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertNotIn("a_ad", generated)

    def test_module_variable_diff_by_default(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = textwrap.dedent(
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "modvar.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertIn("c_ad", generated)

    def test_constant_module_var_directive(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "modvar.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertNotIn("c_ad", generated)

    def test_module_vars_example_fadmod(self):
        code_tree.Node.reset()
        fadmod_path = Path("module_vars.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        gen("examples/module_vars.f90", warn=False)
        fm = fadmod.FadmodBase.load(fadmod_path)
        self.assertIn("c", fm.variables_raw)

    def test_reverse_shadowed_local_gradient(self):
        code_tree.Node.reset()
        src = textwrap.dedent(
            """
            module shadow_mod
              real :: z
            contains
              subroutine foo(x, y)
                real, intent(in) :: x
                real, intent(out) :: y
                real :: z
                z = x + 1.0
                block
                  real :: z
                  z = x + 2.0
                  y = z
                end block
                y = y + z
              end subroutine foo
            end module shadow_mod
            """
        )
        generated = generator.generate_ad(
            src, "shadow.f90", warn=False, mode="reverse"
        )
        self.assertIn("z_ad = y_ad ! y = y + z", generated)
        self.assertNotIn("z_ad = y_ad + z_ad ! y = y + z", generated)

    def test_fadmod_variable_defaults(self):
        code_tree.Node.reset()
        fadmod_path = Path("module_vars.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        gen("examples/module_vars.f90", warn=False)
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "modvar.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertNotIn("c_ad", generated)

    def test_local_var_shadows_module_var(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            src = textwrap.dedent(
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "shadow.f90"), warn=False, fadmod_dir=tmp
            )
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
            generated = generator.generate_ad(
                src,
                str(Path(tmp) / "m.f90"),
                warn=False,
                mode="forward",
                fadmod_dir=tmp,
            )
        self.assertIn("allocate(a_ad(n))", generated)
        self.assertIn("deallocate(a_ad)", generated)

    def test_fadmod_includes_skip(self):
        code_tree.Node.reset()
        fadmod_path = Path("directives.fadmod")
        if fadmod_path.exists():
            fadmod_path.unlink()
        gen("examples/directives.f90", warn=False)
        fm = fadmod.FadmodBase.load(fadmod_path)
        routines = fm.routines
        self.assertIn("skip_me", routines)
        self.assertTrue(routines["skip_me"].get("skip"))

    def test_ignore_fad_directives(self):
        code_tree.Node.reset()
        from tempfile import TemporaryDirectory

        with TemporaryDirectory() as tmp:
            generated = gen(
                "examples/directives.f90",
                warn=False,
                fadmod_dir=tmp,
                ignore_fad=True,
            )

        self.assertIn("skip_me_rev_ad", generated)
        self.assertIn(
            "subroutine add_const_fwd_ad(x, x_ad, y, y_ad, z, z_ad)", generated
        )

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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "test.f90"), warn=False, fadmod_dir=tmp
            )
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "test.f90"), warn=False, fadmod_dir=tmp
            )
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
        generated = gen("examples/macro_multistmt.F90", warn=False)
        expected = Path("examples/macro_multistmt_ad.F90").read_text()
        self.assertEqual(generated, expected)

    def test_warning_for_assumed_intent(self):
        code_tree.Node.reset()
        from tempfile import TemporaryDirectory

        src = Path("examples/macro_multistmt.F90").read_text()
        buf = StringIO()
        with TemporaryDirectory() as tmp, redirect_stderr(buf):
            generator.generate_ad(
                src,
                "examples/macro_multistmt.F90",
                warn=True,
                search_dirs=[".", "examples", "fortran_modules"],
                fadmod_dir=tmp,
            )
        stderr = buf.getvalue()
        expected_msg = (
            "macro_multistmt.F90:8: foo - Assumed intent(inout) for arguments x, y because no INTENT attribute was specified"
        )
        self.assertIn(expected_msg, stderr)

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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "t.f90"), warn=False, fadmod_dir=tmp
            )

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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "opt.f90"), warn=False, fadmod_dir=tmp
            )
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "attrs.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertIn("real, save :: a", generated)
            self.assertIn("integer, intent(inout), value :: b", generated)
            self.assertIn("real, intent(inout), volatile :: c", generated)
            self.assertIn("real, intent(inout), asynchronous :: d", generated)
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
        generated = generator.generate_ad(src, "omp.f90", warn=False)
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
        generated = generator.generate_ad(src, "omp_ws.f90", warn=False)
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "save.f90"), warn=False, fadmod_dir=tmp
            )
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
            generated = generator.generate_ad(
                src, str(Path(tmp) / "tmp.f90"), warn=False, fadmod_dir=tmp
            )
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

    def test_no_save_for_uninitialized_local(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(x, y)
                real, intent(in) :: x
                real, intent(out) :: y
                real :: w
                w = x
                y = w * 2.0
              end subroutine foo
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            generated = generator.generate_ad(
                src, str(Path(tmp) / "tmp.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertNotRegex(generated, r"w_save_\d+_ad")

    def test_no_save_for_intent_out_argument(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(x, y)
                real, intent(in) :: x
                real, intent(out) :: y
                y = y + x
              end subroutine foo
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            generated = generator.generate_ad(
                src, str(Path(tmp) / "tmp.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertNotRegex(generated, r"y_save_\d+_ad")

    def test_save_kept_for_inout_argument(self):
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module test
            contains
              subroutine foo(n, x)
                integer, intent(in) :: n
                real, intent(inout) :: x(n)
                integer :: i
                x(1) = x(1) * x(2) * 0.5
                do i = 2, n - 1
                  x(i) = x(i) * (x(i + 1) - x(i - 1)) * 0.5
                end do
                x(n) = - x(n) * x(n - 1) * 0.5
              end subroutine foo
            end module test
            """
        )

        with TemporaryDirectory() as tmp:
            generated = generator.generate_ad(
                src, str(Path(tmp) / "tmp.f90"), warn=False, fadmod_dir=tmp
            )
            self.assertRegex(generated, r"x_save_\d+_ad = x\(1\)")
            self.assertRegex(generated, r"x\(1\) = x_save_\d+_ad")

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

    def test_init_non_ad_target_mirror_vars(self):
        """Initialize non-AD-target "_ad" mirrors.

        Example: integer MPI request arrays are not AD targets. When
        wrappers require ``array_of_requests_ad``, the generator should insert
        ``array_of_requests_ad = <rhs>`` immediately before the original
        initialization ``array_of_requests = <rhs>``.
        """
        code_tree.Node.reset()
        import textwrap
        from tempfile import TemporaryDirectory

        src = textwrap.dedent(
            """
            module m
              use mpi
            contains
              subroutine foo(count, x, y)
                integer, intent(in) :: count
                real, intent(in) :: x
                real, intent(out) :: y
                integer :: array_of_requests(count)
                integer :: ierr, i
                do i = 1, count
                  array_of_requests(i) = MPI_REQUEST_NULL
                end do
                y = x
                call MPI_Startall(count, array_of_requests, ierr)
                call MPI_Waitall(count, array_of_requests, MPI_STATUSES_IGNORE, ierr)
              end subroutine foo
            end module m
            """
        )

        with TemporaryDirectory() as tmp:
            generated = generator.generate_ad(
                src,
                str(Path(tmp) / "m.f90"),
                warn=False,
                mode="reverse",
                search_dirs=["fortran_modules"],
                fadmod_dir=tmp,
            )

        # Declaration for the mirror must exist and be integer with same shape
        self.assertIn("integer :: array_of_requests_ad(count)", generated)

        # The initialization for the mirror must appear in the generated code
        self.assertIn("array_of_requests_ad(i) = MPI_REQUEST_NULL", generated)

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
            generated = generator.generate_ad(
                src,
                str(Path(tmp) / "m.f90"),
                warn=False,
                search_dirs=["fortran_modules"],
                fadmod_dir=tmp,
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
            generated = generator.generate_ad(
                src,
                str(Path(tmp) / "m.f90"),
                warn=False,
                mode="reverse",
                fadmod_dir=tmp,
            )
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
            generated = generator.generate_ad(
                src,
                str(Path(tmp) / "m.f90"),
                warn=False,
                mode="reverse",
                fadmod_dir=tmp,
            )
        lines = generated.splitlines()
        # Find indices of key lines
        import re as _re

        idx_assign = next(
            i for i, l in enumerate(lines) if _re.search(r"m\s*=\s*n\s*\+\s*1", l)
        )
        idx_alloc_ad = next(i for i, l in enumerate(lines) if "allocate(a_ad(" in l)
        # Order: compute size -> allocate a_ad
        self.assertLess(idx_assign, idx_alloc_ad)

    def test_reverse_accumulate_on_conditional_slice(self):
        # Ensure reverse AD accumulates into intent(inout) gradients inside branches
        code_tree.Node.reset()
        import textwrap

        src = textwrap.dedent(
            """
            module halo
            contains
              subroutine exchange(istart, iend, jstart, jend, ihalo, nbr, field)
                integer, intent(in) :: istart, iend, jstart, jend, ihalo, nbr
                real, intent(inout) :: field(:,:)
                real, allocatable :: send(:,:)
                integer :: nx

                nx = iend - istart + 1
                allocate(send(nx, ihalo))
                if (nbr /= -1) then
                  send(:,:) = field(istart:iend, jend-ihalo+1:jend)
                end if
                if (allocated(send)) then
                  deallocate(send)
                end if
              end subroutine exchange
            end module halo
            """
        )

        generated = generator.generate_ad(src, "halo.f90", warn=False, mode="reverse")

        # Accumulation into field_ad slice inside the conditional (ignore whitespace)
        import re as _re

        pat = (
            r"field_ad\(istart:iend\s*,\s*jend\s*-\s*ihalo\s*\+\s*1\s*:\s*jend\)"
            r"\s*=\s*send_ad\(\s*:\s*,\s*:\s*\)\s*(?:&\s*\n\s*)?\+\s*field_ad"
            r"\(istart:iend\s*,\s*jend\s*-\s*ihalo\s*\+\s*1\s*:\s*jend\)"
        )
        self.assertIsNotNone(_re.search(pat, generated))

    def test_reverse_slice_clear_minimal(self):
        """Minimal reproducer: ensure slice clears are kept in reverse mode.

        Checks that repeated overwrites of a slice of htmp generate
        corresponding htmp_ad slice clears and they are not pruned.
        """
        code_tree.Node.reset()
        import re
        import textwrap
        from tempfile import TemporaryDirectory

        # Compact minimal program: overwrite a slice three times, then use it
        src = textwrap.dedent(
            """
            module m
            contains
              subroutine s(n, dt, h, k1, k2, k3, y)
                integer, intent(in) :: n
                real, intent(in) :: dt
                real, intent(in) :: h(n), k1(n), k2(n), k3(n)
                real, intent(out) :: y
                real :: htmp(n)

                htmp(:) = h(:) + dt*k1(:)
                htmp(:) = h(:) + dt*k2(:)
                htmp(:) = h(:) + dt*k3(:)
                y = sum(htmp)
              end subroutine s
            end module m
            """
        )

        with TemporaryDirectory() as tmp:
            generated = generator.generate_ad(
                src,
                str(Path(tmp) / "mini.f90"),
                warn=False,
                search_dirs=[tmp],
                write_fadmod=False,
                mode="reverse",
                fadmod_dir=tmp,
            )

        self.assertIn("subroutine s_rev_ad", generated)
        clears = re.findall(r"htmp_ad\s*\(.*?\)\s*=\s*0\.0", generated)
        # In reverse, we clear after the last two overwrites when traversing
        # statements backward; the earliest overwrite need not clear again.
        self.assertGreaterEqual(
            len(clears),
            2,
            f"Expected >=2 htmp_ad slice clears, found {len(clears)}.\n\n{generated}",
        )


def _make_example_test(src: Path):
    def test(self):
        code_tree.Node.reset()
        generated = gen(
            src, warn=False, search_dirs=[".", "examples", "fortran_modules"]
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
