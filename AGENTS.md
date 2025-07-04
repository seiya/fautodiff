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

## 4. Commit and PR Guidelines
- Aim for one topic per commit and write clear messages.
- When opening a PR, explain the changes and test steps.
- Run the tests before pushing.
  Use ``python tests/test_generator.py`` to run the unit tests.
- Messages such as commits and PRs should be in English.
- Branch names **must** be in English (ASCII only) to avoid issues with non-English characters.

## 5. Tests and Examples
- Place the original Fortran code samples under “examples” and the test scripts under “tests.”
- The provided test script reads code from ``examples`` and checks the generated
  AD output against the expected Fortran code.
- When adding modules or subroutines under ``examples/``, also create a driver and
  test subroutine in ``tests/fortran_runtime`` to verify the generated AD code via
  finite differences. Update the ``Makefile`` and ``tests/test_forward_adcode.py``
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
    Result variables and ``intent(out)`` arguments are treated as gradients only
    (``result_ad``). Integer arguments and results are treated as constants and do
    not have ``_ad`` variables.
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
- Public subroutines provided by these modules must also start with the module
  name followed by an underscore.  For example, ``data_storage`` becomes
  ``fautodiff_data_storage`` and its public routine ``push`` is renamed to
  ``fautodiff_data_storage_push``.
