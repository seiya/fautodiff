import os
import sys
import shutil
import subprocess
import tempfile
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

class TestFortranRuntime(unittest.TestCase):
    compiler = shutil.which('gfortran')

    def _build(self, tmp: Path, target: str) -> Path:
        """Compile runtime driver using the Makefile."""
        makefile = Path(__file__).resolve().parent / 'fortran_runtime' / 'Makefile'
        env = os.environ.copy()
        env['VPATH'] = str(tmp)
        target_out = f"{target}.out"
        subprocess.check_call(
            [
                'make',
                '-C', str(makefile.parent),
                '-f', str(makefile),
                f'OUTDIR={tmp}',
                target_out,
            ],
            env=env,
        )
        exe = tmp / target_out
        return exe

    def test_stack_push_pop(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'fortran_modules' / 'fautodiff_stack.f90'
        driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_fautodiff_stack.f90'
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, 'run_fautodiff_stack')
            try:
                run = subprocess.run([str(exe)], stdout=subprocess.PIPE, text=True, check=True)
            except subprocess.CalledProcessError as e:
                if e.stdout:
                    print(e.stdout)
                raise
            self.assertEqual(run.stdout.strip(), 'OK')


if __name__ == '__main__':
    unittest.main()
