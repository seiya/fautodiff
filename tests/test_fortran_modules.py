import subprocess
import sys
from pathlib import Path
import unittest

ROOT = Path(__file__).resolve().parents[1]
FMOD_DIR = ROOT / "fortran_modules"


class TestFortranModuleGenerators(unittest.TestCase):
    def test_generated_files_are_up_to_date(self):
        for gen in sorted(FMOD_DIR.glob("gen_*.py")):
            base = gen.name[len("gen_"):-3]
            if base.endswith("_fadmod"):
                out = FMOD_DIR / f"{base[:-7]}.fadmod"
            else:
                out = FMOD_DIR / f"{base}.f90"
            with self.subTest(generator=gen.name):
                result = subprocess.check_output(
                    [sys.executable, str(gen)], cwd=ROOT, text=True
                )
                expected = out.read_text()
                self.assertEqual(result, expected)


if __name__ == "__main__":
    unittest.main()
