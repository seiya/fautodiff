# fautodiff
Autodiff code generator for Fortran

This repository includes utilities for parsing Fortran source code using
[fparser2](https://fparser.readthedocs.io/en/latest/index.html) (specifically
the ``fparser.two`` API).

## Installation

The parser relies on the `fparser` package which can be installed via pip:

```bash
pip install fparser
```

## Parsing Fortran

The ``fautodiff.parser`` module exposes helper functions for creating an AST
from Fortran code and for finding subroutines. These helpers rely on
``fparser.two``'s `ParserFactory` under the hood. Example usage:

```python
from fautodiff import parser

# Parse a Fortran source file
ast = parser.parse_file("example.f90")

# List all subroutine names in the file
print(parser.find_subroutines(ast))
```

## Generating AD code

For simple examples the ``fautodiff.generator`` module can create a reverse
mode automatic differentiation version of a Fortran source file. The generator
parses the input via :mod:`fautodiff.parser` (which internally relies on
``fparser2``) and retains the original structure with ``_ad`` appended to
routine names.

Generate the AD code for ``examples/simple_math.f90``:

```bash
# print to standard output
python -m fautodiff.generator examples/simple_math.f90

# or write to a file
python -m fautodiff.generator examples/simple_math.f90 examples/simple_math_ad.f90
```

Run the included tests with:

```bash
python tests/test_generator.py
```
