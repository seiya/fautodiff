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

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_cross_mod_call_inc(self):
        base = Path(__file__).resolve().parents[1]
        src_a = base / 'examples' / 'cross_mod_a.f90'
        src_b = base / 'examples' / 'cross_mod_b.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            tmp_a = tmp / 'cross_mod_a.f90'
            tmp_b = tmp / 'cross_mod_b.f90'
            tmp_a.write_text(src_a.read_text())
            tmp_b.write_text(src_b.read_text())
            ad_code_a = generator.generate_ad(str(tmp_a), warn=False)
            ad_path_a = tmp / 'cross_mod_a_ad.f90'
            ad_path_a.write_text(ad_code_a)
            ad_code_b = generator.generate_ad(str(tmp_b), warn=False, search_dirs=[str(tmp)])
            ad_path_b = tmp / 'cross_mod_b_ad.f90'
            ad_path_b.write_text(ad_code_b)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_cross_mod.f90'
            exe = tmp / 'run_cross_mod.out'
            cmd = [self.compiler, str(tmp_a), str(ad_path_a), str(tmp_b), str(ad_path_b), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            subprocess.run([str(exe)], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_call_example(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'call_example.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'call_example_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_call_example.f90'
            exe = tmp / 'run_call_example.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            for tname in ['call_subroutine', 'call_fucntion', 'arg_operation', 'arg_function']:
                subprocess.run([str(exe), tname], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_real_kind(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'real_kind.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'real_kind_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_real_kind.f90'
            exe = tmp / 'run_real_kind.out'
            cmd = [self.compiler, str(src), str(ad_path), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            for tname in ['scale_8', 'scale_rp', 'scale_dp']:
                subprocess.run([str(exe), tname], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_store_vars(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'store_vars.f90'
        ds = base / 'fortran_modules' / 'data_storage.f90'
        code_tree.Node.reset()
        ad_code = generator.generate_ad(str(src), warn=False)
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_path = tmp / 'store_vars_ad.f90'
            ad_path.write_text(ad_code)
            driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_store_vars.f90'
            exe = tmp / 'run_store_vars.out'
            cmd = [self.compiler, str(src), str(ad_path), str(ds), str(driver), '-o', str(exe)]
            subprocess.check_call(cmd)
            subprocess.run([str(exe)], check=True)


if __name__ == '__main__':
    unittest.main()
