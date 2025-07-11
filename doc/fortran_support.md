# Supported Fortran Syntax and Features

This document summarizes the Fortran constructs handled by the AD code generator.

## Modules and Routines
- Each module in the input becomes a `<module>_ad` module.
- Every subroutine or function produces `<name>_fwd_ad` and `<name>_rev_ad` versions.
- Integer variables are treated as constants and do not receive `_ad` counterparts.

## Control Flow Constructs
- `if`/`else if`/`else` blocks
- `select case`
- `do` and `do while` loops

## Array Operations
- Array assignments and loops over arrays are supported.

## Intrinsic Functions
- Standard numeric intrinsics can be differentiated.
- Unsupported intrinsics trigger warnings; use `--no-warn` to suppress them.

## Keyword Arguments
- Calls with keyword arguments are processed correctly.

## Dynamic Allocation
- `allocate` and `deallocate` statements generate corresponding operations for AD variables.
- Module variables imported with `use` remain constants unless `DIFF_MODULE_VARS` is specified.
- If no `_ad` variable exists for such module variables, any `allocate` or `deallocate` is omitted.

## Parameter and Module Variables
- Parameter constants remain untouched.
- Module variables can be differentiated with the `DIFF_MODULE_VARS` directive.

## Module variables in reverse mode

When a routine assigns to a module variable the reverse-mode version saves the
old value in a temporary before the assignment.  Derivative updates for later
statements are computed and then the original value is restored so that the
derivative of the assignment itself can be propagated correctly.  The temporary
is named `a_save_<n>_ad` where `<n>` is a counter.

Example from `examples/module_vars_ad.f90`:

```fortran
  real :: a_save_19_ad

  y = (c + x) * a
  a_save_19_ad = a
  a = a + x

  a_ad = y_ad * y + a_ad ! y = y * a
  y_ad = y_ad * a ! y = y * a
  a = a_save_19_ad
```

The old value is restored before accumulating derivatives for `a = a + x` and
earlier statements.

The generator internally builds a tree of nodes (`Block`, `Assignment`, `IfBlock`, and so on) defined in `fautodiff.code_tree` and renders it back to Fortran after inserting derivative updates.
