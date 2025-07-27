# fautodiff
Autodiff code generator for Fortran

This repository includes utilities for parsing Fortran source code using
[fparser2](https://fparser.readthedocs.io/en/latest/index.html) (specifically
the ``fparser.two`` API).
Documentation of the supported Fortran constructs can be found under
``doc/fortran_support.md``.

## Installation

The parser relies on the `fparser` package (version **0.2.0 or later**) which can be installed via pip:

```bash
pip install "fparser>=0.2.0"
```

## Parsing Fortran

The ``fautodiff.parser`` module exposes helper functions for creating an AST
from Fortran code and for finding subroutines. These helpers rely on
``fparser.two``'s `ParserFactory` under the hood. Example usage:

```python
from fautodiff import parser

# Parse a Fortran source file
modules = parser.parse_file("example.f90")

# List all routine names in the file
print(parser.find_subroutines(modules))

# Inspect the parsed modules
for mod in modules:
    print(mod.name, [r.name for r in mod.routines])
```

## Generating AD code

For simple examples the ``fautodiff.generator`` module can create a forward or
reverse mode automatic differentiation version of a Fortran source file. The generator
parses the input via :mod:`fautodiff.parser` (which internally relies on
``fparser2``) and retains the original structure with ``_ad`` appended to
routine names. Integer variables are treated as constants, so no ``_ad``
variables are generated for them. The generated module is named ``<module>_ad``
and provides both a forward (``<name>_fwd_ad``) and a reverse
(``<name>_rev_ad``) version of each original routine.

Generate the AD code for ``examples/simple_math.f90``:

```bash
# print to standard output
bin/fautodiff examples/simple_math.f90

# or write to a file
bin/fautodiff examples/simple_math.f90 -o examples/simple_math_ad.f90
```
Additional examples illustrating Fortran intrinsic routines are available in ``examples/intrinsic_func.f90``.

By default the generator prints warnings when it encounters code it cannot
differentiate.  Use ``--no-warn`` to silence these messages:

```bash
bin/fautodiff --no-warn examples/simple_math.f90
```

Each module's routine signatures are also written to a `<module>.fadmod` file
when AD code is generated.  These JSON files can be loaded when differentiating
another file that uses the module.  Add search directories with ``-I`` (repeat
as needed), choose the output directory with ``-M DIR`` (defaults to the current
directory), and disable writing with ``--no-fadmod``:

You can select the differentiation mode with ``--mode``.  Pass ``forward`` for
forward mode only or ``reverse`` for reverse mode only.  The default ``both``
generates both sets of routines.

```bash
# write ``cross_mod_a.fadmod`` under ``examples``
bin/fautodiff -M examples examples/cross_mod_a.f90

# load that file when differentiating another module
bin/fautodiff -I examples examples/cross_mod_b.f90
```

Generate code programmatically from Python:

```python
from fautodiff.generator import generate_ad

ad_code = generate_ad("examples/simple_math.f90")
print(ad_code)
```

Generate forward mode only:

```bash
bin/fautodiff --mode forward examples/simple_math.f90
```

Generate reverse mode only:

```bash
bin/fautodiff --mode reverse examples/simple_math.f90
```

### Calling forward-mode routines

Forward-mode subroutines execute the original computations and return each
``intent(out)`` variable along with its derivative. Arguments consist of the
original inputs followed by their derivative counterparts.

```fortran
real :: x, x_ad, y, y_ad
x = 2.0
x_ad = 1.0
call foo_fwd_ad(x, x_ad, y, y_ad)
```

## Runtime stack module

The helper module `fautodiff_stack` stores data that must persist between the
forward and reverse sweeps.  It defines stack types for real, double precision,
logical and integer values.  Four default stacks (`fautodiff_stack_r4`,
`fautodiff_stack_r8`, `fautodiff_stack_l` and `fautodiff_stack_i`) are provided
for convenience.  Each stack allocates memory in pages and the size of a page
can be changed by modifying the `page_size` component before the first push.


See ``doc/directives.md`` for a description of optional directives that can
control how AD code is generated. Details on how module variable assignments are
handled in reverse mode are in the
[Module variables in reverse mode](doc/fortran_support.md#module-variables-in-reverse-mode)
section.

Run the included tests with:

```bash
python tests/test_generator.py
python tests/test_fortran_runtime.py
python tests/test_fortran_adcode.py
```

## Code tree structure

Automatic differentiation output is built from a tree of nodes defined in
``fautodiff.code_tree``. Each node represents a Fortran construct such as a
``Block``, ``Assignment`` or ``IfBlock`` and provides a ``render`` method that
returns formatted source lines. The generator populates this tree while walking
the parsed input and then renders it to produce the final AD code.

Contributors adding new features should rely on these new node classes instead of
creating raw strings so that the tree remains consistent.
