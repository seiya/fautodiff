import sys
from pathlib import Path
import unittest
import json

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator
from fautodiff import code_tree


class TestGenerator(unittest.TestCase):
    """Tests auto-diff generation for each example file."""

    def test_store_vars_use_data_storage(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/store_vars.f90", warn=False)
        lines = generated.splitlines()
        self.assertIn("use fautodiff_data_storage", lines[2])
        idx_use = next(i for i, l in enumerate(lines) if "use fautodiff_data_storage" in l)
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
            search_dirs=["."],
        )
        expected = Path("examples/cross_mod_b_ad.f90").read_text()
        self.assertEqual(generated, expected)

    def test_keyword_arg_call(self):
        code_tree.Node.reset()
        generated = generator.generate_ad("examples/keyword_args.f90", warn=False)
        expected = Path("examples/keyword_args_ad.f90").read_text()
        self.assertEqual(generated, expected)


    def test_constant_args_directive(self):
        code_tree.Node.reset()
        from tempfile import TemporaryDirectory
        import textwrap

        with TemporaryDirectory() as tmp:
            src = Path(tmp) / "const.f90"
            src.write_text(
                textwrap.dedent(
                    """
                    module test
                    contains
                    !$FAD CONSTANT_ARGS: a
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

    def test_fadmod_includes_skip(self):
        code_tree.Node.reset()
        fadmod = Path("directives.fadmod")
        if fadmod.exists():
            fadmod.unlink()
        generator.generate_ad("examples/directives.f90", warn=False)
        data = json.loads(fadmod.read_text())
        self.assertIn("skip_me", data)
        self.assertTrue(data["skip_me"].get("skip"))

    def test_skip_call_skips_derivatives(self):
        code_tree.Node.reset()
        from tempfile import TemporaryDirectory
        import textwrap

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

        self.assertNotIn("foo_fwd_ad", generated)
        self.assertNotIn("foo_rev_ad", generated)
        self.assertIn("bar_fwd_ad", generated)
        self.assertIn("bar_rev_ad", generated)
        self.assertIn("call foo(x, y)", generated)
        self.assertIn("foo", fadmod)
        self.assertTrue(fadmod["foo"].get("skip"))


def _make_example_test(src: Path):
    def test(self):
        code_tree.Node.reset()
        generated = generator.generate_ad(str(src), warn=False)
        expected = src.with_name(src.stem + "_ad.f90").read_text()
        self.assertEqual(generated, expected, msg=f"Mismatch for {src.name}")

    return test


examples_dir = Path("examples")
for _src in sorted(examples_dir.glob("*.f90")):
    if _src.name.endswith("_ad.f90") or _src.stem in {"cross_mod_a", "cross_mod_b"}:
        continue
    test_name = f"test_{_src.stem}"
    setattr(TestGenerator, test_name, _make_example_test(_src))


if __name__ == '__main__':
    unittest.main()
