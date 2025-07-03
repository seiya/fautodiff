import os
import sys
import shutil
import subprocess
import tempfile
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator, code_tree


class TestForwardRuntime(unittest.TestCase):
    compiler = shutil.which('gfortran')

    def _build(self, tmp: Path, driver: Path, *sources: Path) -> Path:
        """Compile a simple driver with gfortran."""
        exe = tmp / driver.stem
        cmd = [self.compiler, '-O2', '-ffree-line-length-none', '-J', str(tmp)]
        cmd.extend(str(s) for s in sources)
        cmd.append(str(driver))
        cmd.extend(['-o', str(exe)])
        subprocess.check_call(cmd, cwd=tmp)
        return exe

    @unittest.skipIf(compiler is None, 'gfortran compiler not available')
    def test_add_numbers(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'examples' / 'simple_math.f90'
        code_tree.Node.reset()
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            ad_code = generator.generate_ad(
                str(src), mode='forward', warn=False, fadmod_dir=str(tmp)
            )
            ad_path = tmp / 'simple_math_ad.f90'
            ad_path.write_text(ad_code)
            driver = tmp / 'run_add_numbers.f90'
            driver.write_text(
                '\n'.join([
                    'program run_add_numbers',
                    '  use simple_math',
                    '  use simple_math_ad',
                    '  implicit none',
                    '  real, parameter :: tol = 1.0e-5',
                    '  real :: a, b, c, c_ad',
                    '  real :: exp_c',
                    '  a = 2.0',
                    '  b = 3.0',
                    '  c = add_numbers(a, b)',
                    '  call add_numbers_fwd_ad(a, 1.0, b, 0.0, c_ad)',
                    '  if (abs(c_ad - 2.0) > tol) error stop 1',
                    '  call add_numbers_fwd_ad(a, 0.0, b, 1.0, c_ad)',
                    '  if (abs(c_ad - 1.0) > tol) error stop 1',
                    '  exp_c = 2.0 * a + b + 3.0',
                    '  if (abs(c - exp_c) > tol) error stop 1',
                    'end program run_add_numbers',
                ])
            )
            exe = self._build(tmp, driver, src, ad_path)
            subprocess.run([str(exe)], check=True)


if __name__ == '__main__':
    unittest.main()
