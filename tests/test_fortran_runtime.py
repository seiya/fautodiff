import sys
import shutil
import subprocess
import tempfile
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator, code_tree


class TestFortranRuntime(unittest.TestCase):
    compiler = shutil.which('gfortran')

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_add_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_add.f90'
            exe = tmp / 'run_add.exe'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe)], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 10.0, places=5)
            self.assertAlmostEqual(values[1], 2.0, places=5)
            self.assertAlmostEqual(values[2], 1.0, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_multiply_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_multiply.f90'
            exe = tmp / 'run_multiply.exe'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe)], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 26.0, places=5)
            self.assertAlmostEqual(values[1], 13.0, places=5)
            self.assertAlmostEqual(values[2], 6.0, places=5)


if __name__ == '__main__':
    unittest.main()
