![fautodiff Logo](docs/logo.svg)

# fautodiff
Autodiff code generator for Fortran

## Highlights

* Works seamlessly with existing Fortran code
  * Performs source-to-source transformations
  * Minimizes the need to modify the original files
* Optimized for computational efficiency
  * Supports MPI, OpenMP, and SIMD parallelism
  * Trades extra arithmetic for reduced memory traffic

This source-to-source tool parses existing Fortran programs and emits new modules
with ``_ad`` suffixes, preserving the original structure while providing forward
and reverse mode derivatives. Documentation of the supported Fortran constructs
can be found in [docs/fortran_support.md](docs/fortran_support.md).
Basic C preprocessor macros are also recognized; see [docs/preprocessor.md](docs/preprocessor.md) for
details.

## Installation

The parser relies on the `fparser` package (version **0.2.0 or later**).

Install `fautodiff` and its dependencies with:

```bash
pip install .
```

## Generating AD code

For simple examples the ``fautodiff.generator`` module can create a forward or reverse mode automatic differentiation version of a Fortran source file.
The generator parses the input via :mod:`fautodiff.parser` (which internally relies on ``fparser2``) and retains the original structure with ``_ad`` appended to
routine names.
Integer variables are treated as constants, so no ``_ad`` variables are generated for them.
The generated module is named ``<module>_ad`` and provides both a forward (``<name>_fwd_ad``) and a reverse (``<name>_rev_ad``) version of each original routine.

After installation the ``fautodiff`` console script is available on your ``PATH``.
When working directly from a source checkout you can invoke the same entry point
with ``python -m fautodiff.cli`` or run ``./bin/fautodiff``.

Generate the AD code for an example file ``examples/simple_math.f90``:
```bash
# print to standard output
fautodiff examples/simple_math.f90

# or write to a file
fautodiff examples/simple_math.f90 -o examples/simple_math_ad.f90
```

Example of the original code in ``examples/simple_math.f90``:
```fortran
  work = a + b
  c = a + 1.0
  c = c + 2.0 + work
```

Generated forward mode AD code in ``examples/simple_math_ad.f90``:
```fortran
  work_ad = a_ad + b_ad ! work = a + b
  work = a + b
  c_ad = a_ad ! c = a + 1.0
  c = a + 1.0
  c_ad = c_ad + work_ad ! c = c + 2.0 + work
  c = c + 2.0 + work
```

Generated reverse mode AD code in ``examples/simple_math_ad.f90``:
```fortran
  work_ad = c_ad ! c = c + 2.0 + work
  a_ad = c_ad ! c = a + 1.0
  c_ad = 0.0 ! c = a + 1.0
  a_ad = work_ad + a_ad ! work = a + b
  b_ad = work_ad ! work = a + b
```

Additional examples are available under the directory ``examples``.

You can select the differentiation mode with ``--mode``.
Pass ``forward`` for forward mode only or ``reverse`` for reverse mode only.
The default ``both`` generates both sets of routines.

Generate forward mode only:
```bash
fautodiff --mode forward examples/simple_math.f90
```

Generate reverse mode only:
```bash
fautodiff --mode reverse examples/simple_math.f90
```

Suppress warnings about unsupported derivatives with ``--no-warn``:
```bash
fautodiff --no-warn examples/simple_math.f90
```

Disable processing of any ``!$FAD`` directives in the source:
```bash
fautodiff --disable-directives examples/directives.f90
```

Keep OpenMP scatter stores as-is instead of rewriting them to gathers with
``--disable-scatter-to-gather``:
```bash
fautodiff --disable-scatter-to-gather examples/omp_loops.f90
```

Generate a validation driver template alongside the AD output with
``--emit-validation-driver``. The flag accepts an optional file name; when
omitted, ``run_<module>_validation.f90`` is emitted next to the AD output. The
driver performs both the forward finite-difference vs. AD comparison and a
transpose consistency check, printing the individual norms in addition to the
difference. Array workspaces are declared with placeholder extent variables and
TODO comments so you can quickly fill in problem-specific sizes. Validation
drivers require both forward and reverse derivatives, so combine the flag with
``--mode both`` (the default):

```bash
# emit run_simple_math_validation.f90 next to the AD file
fautodiff examples/simple_math.f90 -o examples/simple_math_ad.f90 \
    --emit-validation-driver

# emit a custom driver name
fautodiff examples/simple_math.f90 -o examples/simple_math_ad.f90 \
    --emit-validation-driver simple_math_validate.f90
```

Each module's routine signatures are also written to a `<module>.fadmod` file when AD code is generated.
These JSON files can be loaded when differentiating another file that uses the module.
Add search directories with ``-I`` (repeat as needed), choose the output directory with ``-M DIR`` (defaults to the current directory), and disable writing with ``--no-fadmod``:
```bash
# write ``cross_mod_a.fadmod`` under ``examples``
fautodiff -M examples examples/cross_mod_a.f90

# load that file when differentiating another module
fautodiff -I examples examples/cross_mod_b.f90
```

The structure of these files is documented in [docs/fadmod.md](docs/fadmod.md).

Generate code programmatically from Python:
```python
from pathlib import Path
from fautodiff.generator import generate_ad

src_path = Path("examples/simple_math.f90")
ad_code = generate_ad(src_path.read_text(), str(src_path))
print(ad_code)
```

### Calling forward-mode routines

Forward-mode subroutines execute the original computations and return each ``intent(out)`` variable along with its derivative.
Arguments consist of the original inputs followed by their derivative counterparts:
```fortran
interface
  subroutine foo(x, y)
    real, intent(in) :: x
    real, intent(out) :: y
  end subroutine foo
end interface

real :: x, x_ad, y, y_ad
x = 2.0
x_ad = 1.0
call foo_fwd_ad(x, x_ad, y, y_ad) ! x: in, x_ad: in, y: out, y_ad: out
```

### Calling reverse-mode routines

Reverse-mode subroutines accept the original inputs and the derivatives of any
``intent(out)`` results. They return derivatives for ``intent(in)`` arguments and
update the derivatives of ``intent(inout)`` and ``intent(out)`` variables in
place:
```fortran
real :: x, x_ad, y_ad
x = 2.0
y_ad = 1.0
call foo_rev_ad(x, x_ad, y_ad) ! x: in, x_ad: out, y_ad: inout
```

## Directives

See [docs/directives.md](docs/directives.md) for a description of optional directives that can control how AD code is generated.
Details on how module variable assignments are handled in reverse mode are in the [Module variables in reverse mode](docs/fortran_support.md#module-variables-in-reverse-mode) section.

## Supported Fortran syntax and features

See [docs/fortran_support.md](docs/fortran_support.md) for a description of supported Fortran constructs.

## OpenMP constructs

OpenMP directives are preserved in the generated code.  Supported constructs
include `parallel`, `parallel do` (and related forms such as `do`,
`parallel do simd`, and `do simd`), `sections`, `parallel sections`, and
`single` blocks, as well as stand‑alone directives like `barrier`, `flush`,
`taskwait`, and `taskyield`.  Clauses that list variables—e.g. `private`,
`firstprivate`, or `reduction`—automatically receive the corresponding
derivative variables so that both the primal and `_ad` versions participate.
Loops with cross‑iteration dependencies are detected during reverse-mode
generation; in these cases the OpenMP directive is dropped and the loop runs
sequentially. When the dependency arises from scatter-style updates (e.g.
stencil kernels) the generator rewrites the reverse loop to gather
contributions instead. The OpenMP directive is preserved in those cases and a
diagnostic message notes that the loop was rewritten for thread-safety. Pass
``--disable-scatter-to-gather`` to skip this rewrite when you would rather keep
the original scatter stores.

## Runtime stack module

The helper module `fautodiff_stack` stores data that must persist between the forward and reverse sweeps.
It defines stack types for real, double precision, logical and integer values and pointers.
Four default stacks (`fautodiff_stack_r4`, `fautodiff_stack_r8`, `fautodiff_stack_l`, `fautodiff_stack_i`, and `fautodiff_stack_p) are provided for convenience.
Each stack allocates memory in pages and the size of a page can be changed by modifying the `page_size` component before the first push.

# Tests

Run the included tests with:

```bash
python tests/test_generator.py
python tests/test_fortran_runtime.py
python tests/test_fortran_adcode.py
```

## Code tree structure

Automatic differentiation output is built from a tree of nodes defined in ``fautodiff.code_tree``.
Each node represents a Fortran construct such as a ``Block``, ``Assignment`` or ``IfBlock`` and provides a ``render`` method that returns formatted source lines.
The generator populates this tree while walking the parsed input and then renders it to produce the final AD code.

Contributors adding new features should rely on these new node classes instead of creating raw strings so that the tree remains consistent.
