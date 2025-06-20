# fautodiff
Autodiff code generator for Fortran

This repository includes utilities for parsing Fortran source code using
[fparser2](https://fparser.readthedocs.io/en/latest/index.html) (specifically
the ``fparser.two`` API).

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

For simple examples the ``fautodiff.generator`` module can create a reverse
mode automatic differentiation version of a Fortran source file. The generator
parses the input via :mod:`fautodiff.parser` (which internally relies on
``fparser2``) and retains the original structure with ``_ad`` appended to
routine names. Integer variables are treated as constants, so no ``_ad``
variables are generated for them.

Generate the AD code for ``examples/simple_math.f90``:

```bash
# print to standard output
python -m fautodiff.generator examples/simple_math.f90

# or write to a file
python -m fautodiff.generator examples/simple_math.f90 examples/simple_math_ad.f90
```
Additional examples illustrating Fortran intrinsic routines are available in ``examples/intrinsic_func.f90``.

By default the generator prints warnings when it encounters code it cannot
differentiate.  Use ``--no-warn`` to silence these messages:

```bash
python -m fautodiff.generator --no-warn examples/simple_math.f90
```

Run the included tests with:

```bash
python tests/test_generator.py
```

## Code tree structure

Automatic differentiation output is built from a tree of nodes defined in
``fautodiff.code_tree``. Each node represents a Fortran construct such as a
``Block``, ``Assignment`` or ``IfBlock`` and provides a ``render`` method that
returns formatted source lines. The generator populates this tree while walking
the parsed input and then renders it to produce the final AD code.

Contributors adding new features should rely on these new node classes instead of
creating raw strings so that the tree remains consistent.
