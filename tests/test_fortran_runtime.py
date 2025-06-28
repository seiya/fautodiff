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
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_simple_math.f90'
            exe = tmp / 'run_add.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), "add_numbers"], stdout=subprocess.PIPE, text=True, check=True)
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
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_simple_math.f90'
            exe = tmp / 'run_multiply.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), "multiply_numbers"], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 26.0, places=5)
            self.assertAlmostEqual(values[1], 13.0, places=5)
            self.assertAlmostEqual(values[2], 6.0, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_subtract_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_simple_math.f90'
            exe = tmp / 'run_subtract.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), "subtract_numbers"], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 4.0, places=5)
            self.assertAlmostEqual(values[1], -1.0, places=5)
            self.assertAlmostEqual(values[2], 2.0, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_divide_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_simple_math.f90'
            exe = tmp / 'run_divide.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), "divide_numbers"], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 2.2222222222222223, places=5)
            self.assertAlmostEqual(values[1], 1.1111111111111112, places=5)
            self.assertAlmostEqual(values[2], -0.04938271604938271, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_power_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_simple_math.f90'
            exe = tmp / 'run_power.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), "power_numbers"], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 263580.875, places=5)
            self.assertAlmostEqual(values[1], 2360520.0, places=5)
            self.assertAlmostEqual(values[2], 911601.625, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_arrays_elementwise_add(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'arrays.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'arrays_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_arrays.f90'
            exe = tmp / 'run_arrays.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), 'elementwise_add'], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 9.0, places=5)
            self.assertAlmostEqual(values[1], 1.0, places=5)
            self.assertAlmostEqual(values[2], 2.0, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_control_flow_if_example(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'control_flow.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'control_flow_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_control_flow.f90'
            exe = tmp / 'run_cf.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), 'if_example'], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 1.0, places=5)
            self.assertAlmostEqual(values[1], 1.0, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_intrinsic_casting(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'intrinsic_func.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'intrinsic_func_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_intrinsic_func.f90'
            exe = tmp / 'run_intrinsic.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), 'casting'], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 8.5, places=5)
            self.assertAlmostEqual(values[1], 1.0, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_save_vars_simple(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'save_vars.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'save_vars_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_save_vars.f90'
            exe = tmp / 'run_save_vars.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe), 'simple'], stdout=subprocess.PIPE, text=True, check=True)
            values = [float(v) for v in run.stdout.strip().split()]
            self.assertAlmostEqual(values[0], 32.0, places=5)
            self.assertAlmostEqual(values[1], 34.0, places=5)
            self.assertAlmostEqual(values[2], 1.0, places=5)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_data_storage_push_pop(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'fortran_modules' / 'data_storage.f90'
        driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_data_storage.f90'
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = tmp / 'run_ds.out'
            cmd = [self.compiler, str(src), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            run = subprocess.run([str(exe)], stdout=subprocess.PIPE, text=True, check=True)
            self.assertEqual(run.stdout.strip(), 'OK')


if __name__ == '__main__':
    unittest.main()
