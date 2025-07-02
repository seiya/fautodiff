import os
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

    def _build(self, tmp: Path, target: str) -> Path:
        """Compile runtime driver using the Makefile."""
        makefile = Path(__file__).resolve().parent / 'fortran_runtime' / 'Makefile'
        env = os.environ.copy()
        env['VPATH'] = str(tmp)
        subprocess.check_call(
            [
                'make',
                '-C', str(makefile.parent),
                '-f', str(makefile),
                f'OUTDIR={tmp}',
                target,
            ],
            env=env,
        )
        exe_src = makefile.parent / target
        exe_dst = tmp / target
        shutil.move(str(exe_src), exe_dst)
        return exe_dst

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_add_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_simple_math')
            subprocess.run([str(exe), "add_numbers"], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_multiply_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_simple_math')
            subprocess.run([str(exe), "multiply_numbers"], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_subtract_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_simple_math')
            subprocess.run([str(exe), "subtract_numbers"], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_divide_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_simple_math')
            subprocess.run([str(exe), "divide_numbers"], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_power_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_simple_math')
            subprocess.run([str(exe), "power_numbers"], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_arrays_elementwise_add(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'arrays.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'arrays_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_arrays')
            subprocess.run([str(exe), 'elementwise_add'], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_arrays_multidimension_fd(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'arrays.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'arrays_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_arrays')
            subprocess.run([str(exe), 'multidimension'], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_control_flow_if_example(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'control_flow.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'control_flow_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_control_flow')
            subprocess.run([str(exe), 'if_example'], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_intrinsic_casting(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'intrinsic_func.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'intrinsic_func_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_intrinsic_func')
            subprocess.run([str(exe), 'casting'], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_save_vars_simple(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'save_vars.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'save_vars_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_save_vars')
            subprocess.run([str(exe), 'simple'], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_data_storage_push_pop(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'fortran_modules' / 'data_storage.f90'
        driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_data_storage.f90'
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, 'run_data_storage')
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
            ad_code_a = generator.generate_ad(
                str(tmp_a), warn=False, fadmod_dir=str(tmp)
            )
            ad_path_a = tmp / 'cross_mod_a_ad.f90'
            ad_path_a.write_text(ad_code_a)
            ad_code_b = generator.generate_ad(
                str(tmp_b), warn=False, search_dirs=[str(tmp)], fadmod_dir=str(tmp)
            )
            ad_path_b = tmp / 'cross_mod_b_ad.f90'
            ad_path_b.write_text(ad_code_b)
            exe = self._build(tmp, 'run_cross_mod')
            subprocess.run([str(exe)], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_call_example(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'call_example.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'call_example_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_call_example')
            for tname in ['call_subroutine', 'call_fucntion', 'arg_operation', 'arg_function']:
                subprocess.run([str(exe), tname], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_real_kind(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'real_kind.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'real_kind_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_real_kind')
            for tname in ['scale_8', 'scale_rp', 'scale_dp']:
                subprocess.run([str(exe), tname], check=True)

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_store_vars(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'store_vars.f90'
        ds = base / 'fortran_modules' / 'data_storage.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(str(src), warn=False, fadmod_dir=str(tmp))
            ad_path = tmp / 'store_vars_ad.f90'
            ad_path.write_text(ad_code)
            exe = self._build(tmp, 'run_store_vars')
            subprocess.run([str(exe)], check=True)


if __name__ == '__main__':
    unittest.main()
