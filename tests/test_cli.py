import io
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import cli

ROOT = Path(__file__).resolve().parents[1]


class TestCLI(unittest.TestCase):
    def run_cli(self, *args):
        return subprocess.run(
            [sys.executable, "-m", "fautodiff.cli", *args],
            capture_output=True,
            text=True,
            cwd=ROOT,
        )

    def test_cli_success(self):
        src = ROOT / "examples" / "store_vars.f90"
        result = self.run_cli(str(src), "--no-fadmod", "--no-warn")
        self.assertEqual(result.returncode, 0)
        self.assertIn("module store_vars_ad", result.stdout)

    def test_cli_invalid_input(self):
        result = self.run_cli("nonexistent.f90")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("Error", result.stderr)
        self.assertEqual(result.stdout, "")


class TestCLIUnit(unittest.TestCase):
    def test_stdout_output_when_no_o(self):
        src = Path(__file__).resolve().parents[1] / "examples" / "simple_math.f90"
        argv = ["fautodiff", str(src), "--no-fadmod", "--no-warn"]
        buf = io.StringIO()
        with patch.object(sys, "argv", argv), redirect_stdout(buf):
            cli.main()
        out = buf.getvalue()
        self.assertIn("module simple_math_ad", out)

    def test_writes_output_file(self):
        src = Path(__file__).resolve().parents[1] / "examples" / "simple_math.f90"
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp) / "simple_math_ad.f90"
            argv = ["fautodiff", str(src), "-o", str(out), "--no-fadmod", "--no-warn"]
            with patch.object(sys, "argv", argv):
                cli.main()
            text = out.read_text()
            self.assertIn("module simple_math_ad", text)

    def test_mode_forward_and_reverse(self):
        src = Path(__file__).resolve().parents[1] / "examples" / "simple_math.f90"
        # forward only
        argv = [
            "fautodiff",
            str(src),
            "--no-fadmod",
            "--no-warn",
            "--mode",
            "forward",
        ]
        buf = io.StringIO()
        with patch.object(sys, "argv", argv), redirect_stdout(buf):
            cli.main()
        out = buf.getvalue()
        self.assertIn("_fwd_ad", out)
        self.assertNotIn("_rev_ad", out)

        # reverse only
        argv[-1] = "reverse"
        buf = io.StringIO()
        with patch.object(sys, "argv", argv), redirect_stdout(buf):
            cli.main()
        out = buf.getvalue()
        self.assertIn("_rev_ad", out)
        self.assertNotIn("_fwd_ad", out)

    def test_flag_and_search_dirs_propagation(self):
        # ensure flags propagate to generator.generate_ad
        called = {}

        def fake_generate_ad(
            input, output, *, warn, search_dirs, write_fadmod, fadmod_dir, mode
        ):
            called.update(
                dict(
                    input=input,
                    output=output,
                    warn=warn,
                    search_dirs=search_dirs,
                    write_fadmod=write_fadmod,
                    fadmod_dir=fadmod_dir,
                    mode=mode,
                )
            )
            return "module dummy_ad\nend module dummy_ad\n"

        with patch("fautodiff.cli.generator.generate_ad", side_effect=fake_generate_ad):
            argv = [
                "fautodiff",
                "dummy.f90",
                "-I",
                "foo",
                "-I",
                "bar",
                "--no-fadmod",
                "--no-warn",
                "--mode",
                "both",
            ]
            buf = io.StringIO()
            with patch.object(sys, "argv", argv), redirect_stdout(buf):
                cli.main()
        # '.' should be appended to search_dirs
        self.assertEqual(called["search_dirs"], ["foo", "bar", "."])
        self.assertFalse(called["warn"])  # --no-warn
        self.assertFalse(called["write_fadmod"])  # --no-fadmod
        self.assertIsNone(called["fadmod_dir"])  # default
        self.assertEqual(called["mode"], "both")


if __name__ == "__main__":
    unittest.main()
