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
            subprocess.run([str(exe), "add_numbers"], check=True)

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
            subprocess.run([str(exe), "multiply_numbers"], check=True)

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
            subprocess.run([str(exe), "subtract_numbers"], check=True)

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
            subprocess.run([str(exe), "divide_numbers"], check=True)

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
            subprocess.run([str(exe), "power_numbers"], check=True)

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
            subprocess.run([str(exe), 'elementwise_add'], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_arrays_multidimension_fd(self):
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
            subprocess.run([str(exe), 'multidimension'], check=True)

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
            subprocess.run([str(exe), 'if_example'], check=True)

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
            subprocess.run([str(exe), 'casting'], check=True)

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
            subprocess.run([str(exe), 'simple'], check=True)


if __name__ == '__main__':
    unittest.main()
