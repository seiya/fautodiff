import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))


class TestFortranRuntime(unittest.TestCase):
    compiler = shutil.which("gfortran")

    def _build(self, tmp: Path, target: str) -> Path:
        """Compile runtime driver using the Makefile."""
        makefile = Path(__file__).resolve().parent / "fortran_runtime" / "Makefile"
        env = os.environ.copy()
        env["VPATH"] = str(tmp)
        target_out = f"{target}.out"
        subprocess.check_call(
            [
                "make",
                "-C",
                str(makefile.parent),
                "-f",
                str(makefile),
                f"OUTDIR={tmp}",
                target_out,
            ],
            env=env,
        )
        exe = tmp / target_out
        return exe

    def test_stack_push_pop(self):
        base = Path(__file__).resolve().parents[1]
        src = base / "fortran_modules" / "fautodiff_stack.f90"
        driver = (
            Path(__file__).resolve().parent
            / "fortran_runtime"
            / "run_fautodiff_stack.f90"
        )
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, "run_fautodiff_stack")
            try:
                run = subprocess.run(
                    [str(exe)], stdout=subprocess.PIPE, text=True, check=True
                )
            except subprocess.CalledProcessError as e:
                if e.stdout:
                    print(e.stdout)
                raise
            self.assertEqual(run.stdout.strip(), "OK")

    def _run_mpi(self, exe: Path, arg: str | None = None):
        mpirun = shutil.which("mpirun") or "mpirun"
        cmd = [mpirun, "-np", "2"]
        try:
            # May be needed in some CI environments
            if os.geteuid() == 0:
                cmd.append("--allow-run-as-root")
        except AttributeError:
            # os.geteuid not available on some platforms (e.g., Windows)
            pass
        if arg is not None:
            cmd += [str(exe), arg]
        else:
            cmd += [str(exe)]
        subprocess.run(cmd, check=True)

    def test_mpi_example_runtime(self):
        """Build and run the MPI example runtime driver with 2 ranks."""
        if shutil.which("mpifort") is None or shutil.which("mpirun") is None:
            self.skipTest("MPI toolchain not available (mpifort/mpirun)")
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, "run_mpi_example")
            # Run two representative subtests
            self._run_mpi(exe, "sum_reduce")
            self._run_mpi(exe, "isend_irecv")

    def test_mpi_persistent_runtime(self):
        """Build and run the persistent MPI runtime driver with 2 ranks."""
        if shutil.which("mpifort") is None or shutil.which("mpirun") is None:
            self.skipTest("MPI toolchain not available (mpifort/mpirun)")
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, "run_mpi_persistent")
            self._run_mpi(exe)


if __name__ == "__main__":
    unittest.main()
