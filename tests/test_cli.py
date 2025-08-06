import subprocess
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

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


if __name__ == "__main__":
    unittest.main()
