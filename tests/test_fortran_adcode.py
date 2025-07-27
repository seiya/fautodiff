import os
import sys
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import List, Optional
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator, code_tree


class TestFortranADCode(unittest.TestCase):
    compiler = shutil.which('gfortran')
    mpirun = shutil.which('mpirun')

    def _build(self, tmp: Path, target: str) -> Path:
        """Compile runtime driver using the Makefile."""
        makefile = Path(__file__).resolve().parent / 'fortran_runtime' / 'Makefile'
        env = os.environ.copy()
        exe_dst = tmp / target
        subprocess.check_call(
            [
                'make',
                '-C', str(makefile.parent),
                '-f', str(makefile),
                f'OUTDIR={tmp}',
                exe_dst
            ],
            env=env,
        )
        return exe_dst

    def _run_test(self, name: str, sub_names: List[str], deps: Optional[List[str]] = None, use_mpi: bool = False):
        base = Path(__file__).resolve().parents[1]
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, f'run_{name}.out')
            for sub_name in sub_names:
                # Allow runtime failures to keep coverage high
                if use_mpi:
                    cmd = [self.mpirun or 'mpirun', '-np', '2']
                    if os.geteuid() == 0:
                        cmd.append('--allow-run-as-root')
                    cmd += [str(exe), sub_name]
                else:
                    cmd = [str(exe), sub_name]
                subprocess.run(cmd, check=True)


    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_simple_math(self):
        self._run_test('simple_math', ['add_numbers', 'multiply_numbers', 'subtract_numbers', 'divide_numbers', 'power_numbers'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_arrays(self):
        self._run_test('arrays', ['elementwise_add', 'dot_product', 'multidimension',
                                  'scale_array', 'indirect', 'stencil'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_control_flow(self):
        self._run_test('control_flow', ['if_example', 'do_example', 'select_example',
                                       'do_while_example'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_intrinsic_func(self):
        self._run_test('intrinsic_func', ['casting', 'math', 'non_diff', 'special'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_save_vars(self):
        self._run_test('save_vars', ['simple', 'if_example', 'array_private',
                                     'array', 'local_array', 'stencil_array'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_cross_mod_call_inc(self):
        self._run_test('cross_mod', ['call_inc', 'incval', 'call_inc_kw'], deps=['cross_mod_a', 'cross_mod_b'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_call_example(self):
        self._run_test('call_example', ['call_subroutine', 'call_fucntion', 'arg_operation', 'arg_function',
                                       'foo', 'bar'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_real_kind(self):
        self._run_test('real_kind', ['scale_8', 'scale_rp', 'scale_dp'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_store_vars(self):
        self._run_test('store_vars', ['do_with_recurrent_scalar'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_directives(self):
        self._run_test('directives', ['add_const', 'worker'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_parameter_var(self):
        self._run_test('parameter_var', ['compute_area'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_module_vars(self):
        self._run_test('module_vars', ['inc_and_use'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_call_module_vars(self):
        self._run_test('call_module_vars', ['call_inc_and_use'], deps=['module_vars', 'call_module_vars'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_allocate(self):
        self._run_test('allocate_vars', ['allocate_and_sum', 'module_vars'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_exit_cycle(self):
        self._run_test('exit_cycle', ['do_exit_cycle', 'while_exit_cycle'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_pointer_arrays(self):
        self._run_test('pointer_arrays', ['pointer_example'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_derived_alloc(self):
        self._run_test('derived_alloc', ['derived_alloc'])

    mpifort = shutil.which('mpifort')

    @unittest.skipIf(compiler is None or mpirun is None or mpifort is None,
                     'MPI compiler not available')
    def test_mpi_example(self):
        self._run_test('mpi_example', ['sum_reduce'], use_mpi=True)



if __name__ == '__main__':
    unittest.main()
