import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import List, Optional

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, generator


class TestFortranADCode(unittest.TestCase):
    compiler = shutil.which("gfortran")
    mpirun = shutil.which("mpirun")

    def _build(self, tmp: Path, target: str) -> Path:
        """Compile runtime driver using the Makefile."""
        makefile = Path(__file__).resolve().parent / "fortran_runtime" / "Makefile"
        env = os.environ.copy()
        exe_dst = tmp / target
        subprocess.check_call(
            [
                "make",
                "-C",
                str(makefile.parent),
                "-f",
                str(makefile),
                f"OUTDIR={tmp}",
                exe_dst,
            ],
            env=env,
        )
        return exe_dst

    def _run_test(
        self,
        name: str,
        sub_names: List[str],
        deps: Optional[List[str]] = None,
        use_mpi: bool = False,
    ):
        base = Path(__file__).resolve().parents[1]
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            exe = self._build(tmp, f"run_{name}.out")
            for sub_name in sub_names:
                # Allow runtime failures to keep coverage high
                if use_mpi:
                    cmd = [self.mpirun or "mpirun", "-np", "2"]
                    if os.geteuid() == 0:
                        cmd.append("--allow-run-as-root")
                    cmd += [str(exe), sub_name]
                else:
                    cmd = [str(exe), sub_name]
                subprocess.run(cmd, check=True)

    def test_simple_math(self):
        self._run_test(
            "simple_math",
            [
                "add_numbers",
                "multiply_numbers",
                "subtract_numbers",
                "divide_numbers",
                "power_numbers",
            ],
        )

    def test_arrays(self):
        self._run_test(
            "arrays",
            [
                "elementwise_add",
                "dot_product",
                "multidimension",
                "scale_array",
                "indirect",
                "stencil",
            ],
        )

    def test_control_flow(self):
        self._run_test(
            "control_flow",
            ["if_example", "do_example", "select_example", "do_while_example"],
        )

    def test_intrinsic_func(self):
        self._run_test(
            "intrinsic_func",
            ["casting", "math", "reduction", "non_diff", "special"],
        )

    def test_save_vars(self):
        self._run_test(
            "save_vars",
            [
                "simple",
                "if_example",
                "array_private",
                "array",
                "local_array",
                "stencil_array",
            ],
        )

    def test_cross_mod_call_inc(self):
        self._run_test(
            "cross_mod",
            ["call_inc", "incval", "call_inc_kw"],
            deps=["cross_mod_a", "cross_mod_b"],
        )

    def test_call_example(self):
        self._run_test(
            "call_example",
            [
                "call_subroutine",
                "call_fucntion",
                "arg_operation",
                "arg_function",
                "foo",
                "bar",
            ],
        )

    def test_real_kind(self):
        self._run_test("real_kind", ["scale_8", "scale_rp", "scale_dp"])

    def test_store_vars(self):
        self._run_test("store_vars", ["do_with_recurrent_scalar"])

    def test_directives(self):
        self._run_test("directives", ["add_const", "worker"])

    def test_parameter_var(self):
        self._run_test("parameter_var", ["compute_area"])

    def test_module_vars(self):
        self._run_test("module_vars", ["inc_and_use"])

    def test_call_module_vars(self):
        self._run_test(
            "call_module_vars",
            ["call_inc_and_use"],
            deps=["module_vars", "call_module_vars"],
        )

    def test_allocate(self):
        self._run_test(
            "allocate_vars",
            [
                "allocate_and_sum",
                "allocate_in_if",
                "allocate_in_loop",
                "save_alloc",
                "module_vars",
                "allocate_with_early_return",
            ],
        )

    def test_exit_cycle(self):
        self._run_test(
            "exit_cycle",
            ["do_exit_cycle", "while_exit_cycle", "exit_cycle_with_labels"],
        )

    def test_pointer_arrays(self):
        self._run_test(
            "pointer_arrays",
            ["pointer_allocate", "pointer_subarray", "pointer_allsub", "pointer_swap"],
        )

    def test_derived_alloc(self):
        self._run_test("derived_alloc", ["derived_alloc"])

    def test_generic_interface(self):
        self._run_test(
            "generic_interface",
            [
                "call_add_real_8",
                "call_add_real_selected_real_kind",
                "call_add_real_kind",
                "call_add_real_real64",
            ],
        )

    mpifort = shutil.which("mpifort")

    def test_mpi_example(self):
        self._run_test(
            "mpi_example",
            ["sum_reduce", "isend_irecv", "isend_irecv_alloc"],
            use_mpi=True,
        )

    def test_where_forall(self):
        self._run_test("where_forall", ["where_example", "forall_example"])

    def test_omp_loops(self):
        self._run_test(
            "omp_loops",
            [
                "sum_loop",
                "stencil_loop",
                "stencil_loop_mod",
                "stencil_loop_with_halo",
                "indirect_access_loop",
                "omp_ws_if",
            ],
        )

    def test_return_example(self):
        self._run_test("return_example", ["conditional_return"])

    def test_macro_args(self):
        self._run_test("macro_args", ["foo"])

    def test_macro_multistmt(self):
        self._run_test("macro_multistmt", ["foo"])

    def test_self_reference(self):
        self._run_test(
            "self_reference",
            ["slice", "slice_ptr", "slice_expr"],
        )

    def test_use_module_conflict(self):
        self._run_test("use_module_conflict", ["add_with_mod"])

    def test_stop_example(self):
        self._run_test("stop_example", ["stop_sub"])


if __name__ == "__main__":
    unittest.main()
