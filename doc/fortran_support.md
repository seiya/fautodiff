# Supported Fortran Syntax and Features

This document summarizes the Fortran constructs handled by the AD code generator.

## Modules and Routines
- Each module in the input becomes a `<module>_ad` module.
- Every subroutine or function produces `<name>_fwd_ad` and `<name>_rev_ad` versions.
- Forward-mode routines also return the original `intent(out)` variables together with their gradients. These routines execute the original computations internally so callers do not need to invoke the non-AD version.
- Integer variables are treated as constants and do not receive `_ad` counterparts.

## Control Flow Constructs
- `if`/`else if`/`else` blocks
- `select case`
- `do` and `do while` loops
- `exit` and `cycle` statements inside `do` loops

## Array Operations
- Array assignments and loops over arrays are supported.

## Intrinsic Functions
- Standard numeric intrinsics can be differentiated.
- Unsupported intrinsics trigger warnings; use `--no-warn` to suppress them.

## Keyword Arguments
- Calls with keyword arguments are processed correctly.

## Dynamic Allocation
- `allocate` and `deallocate` statements generate corresponding operations for AD variables.
- Module variables imported with `use` are differentiated by default. Use the `CONSTANT_VARS` directive to mark them as constants.
- If no `_ad` variable exists for such module variables, any `allocate` or `deallocate` is omitted.

## Parameter and Module Variables
- Parameter constants remain untouched.
- Module variables and variables imported with `use` are differentiated by default. Use `CONSTANT_VARS` to treat them as constants.

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

## Cross-subroutine module variables

When a routine both reads and writes a module variable, its reverse-mode version
needs the value from before the call.  The generator emits a wrapper
`<name>_fwd_rev_ad` that pushes such variables to the `fautodiff_data_storage`
stack before calling the original routine.  The corresponding `<name>_rev_ad`
subroutine pops the values at entry so that derivative computations use the
correct state.  Calls to this routine from other AD code automatically invoke
the wrapper in the forward sweep.
