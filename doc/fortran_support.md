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

The generator internally builds a tree of nodes (`Block`, `Assignment`, `IfBlock`, and so on) defined in `fautodiff.code_tree` and renders it back to Fortran after inserting derivative updates.
