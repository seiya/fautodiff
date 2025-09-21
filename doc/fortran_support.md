# Supported Fortran syntax and features

This document summarizes the Fortran constructs handled by the AD code generator.

## Modules and routines
- Each module in the input becomes a `<module>_ad` module.
- By default, every subroutine or function produces `<name>_fwd_ad` and `<name>_rev_ad` versions. The command line `--mode` flag can restrict generation to only forward or reverse routines.
- Forward-mode routines also return the original `intent(out)` variables together with their gradients. These routines execute the original computations internally so callers do not need to invoke the non-AD version.
- Integer variables are treated as constants and do not receive `_ad` counterparts.

## Control flow constructs
- `if`/`else if`/`else` blocks
- `select case`
- `do` and `do while` loops
- `exit` and `cycle` statements inside `do` loops
- `block` constructs providing local scoping

## Array operations
- Array assignments and loops over arrays are supported.

## Intrinsic functions
- Standard numeric intrinsics can be differentiated.

## Keyword arguments
- Calls with keyword arguments are processed correctly.

## Dynamic allocation
- `allocate` and `deallocate` statements generate corresponding operations for AD variables.

## Pointer constructs
- Pointer variables can be declared with the `pointer` attribute.
- Basic assignments to pointer targets are differentiated just like assignments to normal variables.
- `allocate` and `deallocate` on pointer variables create and destroy the associated `_ad` pointers. Module-level pointers use an `associated` check before allocation.

## OpenMP constructs
- OpenMP directives are preserved for `parallel`, `parallel do`, `parallel do simd`, `do`,
  `do simd`, `sections`, `parallel sections`, and `single`.  Standalone directives
  `barrier`, `flush`, `taskwait`, and `taskyield` are also supported.
- Clauses that list variables (such as `private`, `firstprivate`, `lastprivate`,
  `reduction`, `copyin`, or `copyprivate`) automatically include the
  corresponding derivative variables with the `_ad` suffix.
- In reverse mode, loops that introduce cross‑iteration dependencies are detected and
  their OpenMP directives are removed so that the loop executes sequentially. When the
  dependency stems from scatter-style assignments (such as stencil updates), the
  generator rewrites the loop to use gather updates so that the OpenMP directive can be
  preserved. A warning is emitted when this rewrite is applied.

## Parameter and module variables
- Parameter constants remain untouched.
- Module variables and variables imported with `use` are differentiated by default. Use `CONSTANT_VARS` to treat them as constants.


# Variables in reverse mode

When a routine assigns to a variable the reverse-mode version saves the old value in a temporary before the assignment.
Derivative updates for later statements are computed and then the original value is restored so that the derivative of the assignment itself can be propagated correctly.
The temporary is named `a_save_<n>_ad` where `<n>` is a counter.

Example from `examples/save_vars_ad.f90`:

```fortran
  work = x + 1.0
  work_save_12_ad = work
  work = work**2
  work_save_14_ad = work
  work = x**2

  work_ad = z_ad * x ! z = work * x + z
  x_ad = z_ad * work ! z = work * x + z
  work = work_save_14_ad
  x_ad = work_ad * 2.0 * x + x_ad ! work = x**2
  work_ad = z_ad * x ! z = work * x + z
  x_ad = z_ad * work + x_ad ! z = work * x + z
  work = work_save_12_ad
  work_ad = work_ad * 2.0 * work ! work = work**2
  work_ad = z_ad + work_ad ! z = work + y
  y_ad = z_ad ! z = work + y
  z_ad = 0.0 ! z = work + y
  x_ad = work_ad + x_ad ! work = x + 1.0
```

## Wrapper Routine in forward step in reverse-mode AD

Wrapper routines whose names end with `_fwd_rev_ad` are used whenever the forward sweep of reverse-mode AD requires extra processing in addition to
calling the primal routine.
They may push or restore module variables or register MPI requests for later completion.
The corresponding `<name>_rev_ad` routines run during the reverse sweep to complete these actions and accumulate gradients.

### Cross-subroutine module variables

When a routine both reads and writes a module variable, its reverse-mode version needs the value from before the call.
The generator emits a wrapper `<name>_fwd_rev_ad` that pushes such variables to the `fautodiff_stack` stack before calling the original routine.
The corresponding `<name>_rev_ad` subroutine pops the values at entry so that derivative computations use the correct state.
Calls to this routine from other AD code automatically invoke the wrapper in the forward sweep.

Example from `examples/call_module_vars_ad.f90`:

```fortran
  call inc_and_use_fwd_rev_ad() ! save the module variable 'a' in the 'module_vars' module to the stack by calling fautodiff_stack_r4%push(a)
    :
  call inc_and_use_rev_ad(...) ! restore the variable from the stack by calling fautodiff_stack_r4%pop(a)
```

### MPI communication with requests

The helper module `mpi_ad` provides wrappers for several MPI operations.
Asyncronic operations requires some preparations in forward step in reverse-mode AD.

For the persistent communication, use `mpi_send_init_fwd_rev_ad` and its receive counterpart `mpi_recv_init_fwd_rev_ad` to create mpi requests.
The `_fwd_rev_ad` routines register each request so that the reverse sweep can update gradients.
The corresponding `mpi_send_init_rev_ad` and `mpi_recv_init_rev_ad` routines free these registrations during the reverse sweep.
Persistent requests are started by calling `mpi_wait_rev_ad` (or `mpi_waitall_rev_ad`) which internally invokes `MPI_Start` on the registered handles.
The `mpi_start_rev_ad` wrappers then wait for completion and accumulate received adjoint data (or reset send buffers).

Nonblocking routines such as `MPI_Isend` and `MPI_Irecv` use the same infrastructure.
Their wrappers `mpi_isend_fwd_rev_ad` and `mpi_irecv_fwd_rev_ad` register each request so that `mpi_wait_ad` can issue the corresponding `MPI_Irecv` or `MPI_Isend`.
During the reverse sweep the `*_rev_ad` variants wait on these operations, update gradients, and clean up the stored records.