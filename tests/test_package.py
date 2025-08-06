import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


class TestPackaging(unittest.TestCase):
    def test_install_and_import(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            subprocess.check_call(
                [sys.executable, "-m", "pip", "install", ".", "--target", tmpdir],
                cwd=ROOT,
            )
            env = os.environ.copy()
            env["PYTHONPATH"] = tmpdir
            subprocess.check_call(
                [sys.executable, "-c", "import fautodiff"],
                env=env,
            )


if __name__ == "__main__":
    unittest.main()
