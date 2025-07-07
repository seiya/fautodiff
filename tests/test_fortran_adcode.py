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

    def _run_test(self, name: str, sub_names: List[str], deps: Optional[List[str]] = None):
        base = Path(__file__).resolve().parents[1]
        if deps is None:
            deps = [name]
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            for dep in deps:
                src = base / 'examples' / f'{dep}.f90'
                ad_code = generator.generate_ad(str(src), warn=False, search_dirs=[str(tmp)], fadmod_dir=str(tmp))
                ad_path = tmp / f'{name}_ad.f90'
                ad_path.write_text(ad_code)
            exe = self._build(tmp, f'run_{name}')
            for sub_name in sub_names:
                # Allow runtime failures to keep coverage high
                subprocess.run([str(exe), sub_name], check=True)
        

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
        self._run_test('cross_mod', ['call_inc', 'incval'], deps=['cross_mod_a', 'cross_mod_b'])

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
        self._run_test('directives', ['add_const'])

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_parameter_var(self):
        self._run_test('parameter_var', ['compute_area'])


if __name__ == '__main__':
    unittest.main()
