# AGENTS.md

This file provides guidelines for contributors about the repository and develop smoothly.

## 1. Repository Overview
- This is a generator of auto differentiation (AD) code from Fortran code.

## 2. Setup Steps
- `pip install fparser`.

## 3. Code Style and Development Policy
- Python code is recommended to follow [PEP 8](https://www.python.org/dev/peps/pep-0008/).
- The generated automatic differentiation code maintains the structure of the original source code as much as possible.
- The names of modules, functions, subroutines, variables, etc. in the generated code should be the original code names followed by “_ad” as much as possible.
- Fortran modules in this repository must use file names that match the module
  name (e.g. ``my_mod`` resides in ``my_mod.f90``).

## 4. Commit and PR Guidelines
- Aim for one topic per commit and write clear messages.
- When opening a PR, explain the changes and test steps.
- Run the tests before pushing.
  Use ``python tests/test_generator.py`` to run the unit tests.
- Messages such as commits and PRs should be in English.
- Branch names **must** be in English (ASCII only) to avoid issues with non-English characters.
- Do not disable or skip tests to work around failures. Ensure all tests run.

## 5. Tests and Examples
- Place the original Fortran code samples under “examples” and the test scripts under “tests.”
- The provided test script reads code from ``examples`` and checks the generated
  AD output against the expected Fortran code.
- Comparison tests between ``examples/*.f90`` files and their ``*_ad.f90``
  counterparts are generated automatically in ``tests/test_generator.py``.
  Do not write explicit tests for these pairs.
- When adding modules or subroutines under ``examples/``, also create a driver and
  test subroutine in ``tests/fortran_runtime`` to verify the generated AD code via
  finite differences. Update the ``Makefile`` and ``tests/test_fortran_adcode.py``
  to build and run the new checks.

## 6. Generator Usage
- By default ``fautodiff.generator`` prints the generated code to standard
  output. Provide an output path if you want the code written to a file.

## 7. AD Code Generation Logic
- The generator parses the original source using ``fparser`` and walks through
  modules and their contained routines.
- For every ``module`` ``foo`` in the input a new module named ``foo_ad`` is
  produced.  The new module preserves the ordering of routines and uses
  ``implicit none``.
- Each Fortran function or subroutine is converted to a ``subroutine`` with the
  original name followed by ``_ad``.
  - Arguments of the generated subroutine consist of the original arguments
      followed by their corresponding adjoint variables (``arg`` and ``arg_ad``).
      Forward-mode routines also return the original ``intent(out)`` variables
      together with their gradients (``result`` and ``result_ad``).  These
      routines execute the original computations internally so callers do not
      need to invoke the non-AD version. Integer arguments and results are
      treated as constants and do not have ``_ad`` variables.
- The body is processed in reverse order of assignment statements.  For each
  assignment ``lhs = expr`` the partial derivatives ``d<lhs>_d<var>`` are
  computed symbolically.  Gradients are accumulated using these partials to
  update ``<var>_ad`` variables.
- Temporary derivative variables are declared as the same type as ``<var>`` and named
  ``d<lhs>_d<var>``.  Gradient variables use the ``_ad`` suffix.
- The generator keeps the overall structure close to the original so the output
  is easy to compare with the source.

## 8. AD Code Style Guidelines
- Use two spaces for indentation in generated Fortran code.
- Keep routine, module and variable names identical to the original with ``_ad``
  appended.
- Gradient updates should accumulate contributions in the order they appear in
  the reversed execution of the routine.
- Do not introduce additional control flow or modify the numerical logic other
  than inserting the derivative computations.

## 9. Fortran Modules in `fortran_modules`
- Module names must start with the prefix ``fautodiff_``.
  Wrappers for external modules (e.g. MPI) are an exception: these use
  ``<module>_ad`` as the module name without the ``fautodiff_`` prefix.
- Each module should be placed in a file named after the module itself.
  For example, ``fautodiff_stack`` lives in ``fautodiff_stack.f90``.
- Public subroutines provided by these modules must also start with the module
  name followed by an underscore.  For example, ``stack`` becomes
  ``fautodiff_stack`` and its public routine ``push`` is renamed to
  ``fautodiff_stack_push``.
- For AD wrappers of external modules such as MPI, use ``<module>_ad.f90`` as the
  file name and name the module ``<module>_ad``.
- The files ``fautodiff_stack.f90`` and ``mpi.fadmod`` are generated. Update
  their respective generator scripts (``gen_fautodiff_stack.py`` and
  ``gen_mpi_fadmod.py``) and regenerate the sources instead of editing the
  generated files directly. In general, if a file has a corresponding
  ``gen_*.py`` script, modify the generator and re-run it rather than editing
  the generated output.

## 10. Runtime Verification in `tests/fortran_runtime`
The programs under `tests/fortran_runtime` check that the generated AD code
produces correct derivatives. Follow these steps when adding or updating the
checks.

### Forward code tests
1. Provide reasonable input values for the original routine.
2. Run the original routine with these inputs and record the outputs.
3. Perturb the inputs by a small amount `eps` and run the routine again. Use
   `1.0e-3` for single precision and `1.0e-6` for double precision.
4. Divide the difference between the results of steps 2 and 3 by `eps` to obtain
   a reference derivative.
5. Execute the forward AD version with the derivative of each input set to `1`
   and record the output derivatives.
6. Compare the derivatives from step 5 with the reference from step 4.

### Reverse code tests
1. Compute the inner product of the derivatives obtained in the forward test
   (the sum of squares of all derivative components).
2. Use these derivatives as inputs to the reverse AD code and record the output
   derivatives.
3. Take the inner product between the results of step 2 and the forward-test
   input derivatives (which are `1`, so this is the sum of the derivatives from
   step 2).
4. Verify that the value from step 1 matches the value from step 3.
