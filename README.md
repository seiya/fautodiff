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
