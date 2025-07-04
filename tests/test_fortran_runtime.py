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
    def test_data_storage_push_pop(self):
        base = Path(__file__).resolve().parents[1]
        src = base / 'fortran_modules' / 'data_storage.f90'
        driver = Path(__file__).resolve().parent / 'fortran_runtime' / 'run_data_storage.f90'
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, 'run_data_storage')
            run = subprocess.run([str(exe)], stdout=subprocess.PIPE, text=True, check=True)
            self.assertEqual(run.stdout.strip(), 'OK')


if __name__ == '__main__':
    unittest.main()
