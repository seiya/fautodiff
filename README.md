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

Generate forward mode only:

```bash
bin/fautodiff --mode forward examples/simple_math.f90
```

Generate reverse mode only:

```bash
bin/fautodiff --mode reverse examples/simple_math.f90
```

## Optimization Directives

Comment directives may be placed immediately before a subroutine or function
definition to influence how the AD code is generated. They are optional
comments processed by ``fautodiff`` and do not change the original Fortran
semantics.

Use ``CONSTANT_ARGS`` to mark arguments that should be treated as constants
during differentiation. Specify the argument names after a colon:

```fortran
!$FAD CONSTANT_ARGS: arg1, arg2
subroutine foo(arg1, arg2, arg3)
  ! ...
end subroutine foo
```

Arguments listed in ``CONSTANT_ARGS`` do not receive corresponding ``_ad``
variables in the generated routine. See ``examples/directives.f90``
for a full example that also demonstrates skipping a routine with ``SKIP``.

Use ``SKIP`` to indicate that a routine should be parsed but skipped when
generating AD code:

```fortran
!$FAD SKIP
subroutine skip_me(x)
  ! ...
end subroutine skip_me
```

Use ``DIFF_MODULE_VARS`` to differentiate selected module variables which are
normally treated as constants. Place the directive before ``contains``:

```fortran
module test
  real :: c
  !$FAD DIFF_MODULE_VARS: c
contains
  subroutine foo(x)
    real, intent(inout) :: x
    c = c + x
  end subroutine foo
end module test
```

Module variables or names brought in with ``use`` remain constants unless
``DIFF_MODULE_VARS`` (or other directives) requests their differentiation.  If
no corresponding ``_ad`` variable exists, any ``allocate`` or ``deallocate``
statements for such variables are omitted from the generated code.

Run the included tests with:

```bash
python tests/test_generator.py
python tests/test_fortran_runtime.py
```

## Code tree structure

Automatic differentiation output is built from a tree of nodes defined in
``fautodiff.code_tree``. Each node represents a Fortran construct such as a
``Block``, ``Assignment`` or ``IfBlock`` and provides a ``render`` method that
returns formatted source lines. The generator populates this tree while walking
the parsed input and then renders it to produce the final AD code.

Contributors adding new features should rely on these new node classes instead of
creating raw strings so that the tree remains consistent.
