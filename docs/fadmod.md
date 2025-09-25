# FADMOD schema

This document describes the JSON schema used by `fautodiff` to record
routine signatures and module variables.  Files follow the
`<module>.fadmod` naming convention and are written next to generated AD
sources.

## Top-level structure

A ``.fadmod`` file is a JSON object with these fields:

- ``version`` – integer schema version.
- ``routines`` – object mapping routine names to routine entries.
- ``variables`` – object mapping variable names to variable entries.
- ``generics`` – optional object mapping generic procedure names to lists
  of specific routine names.

## Routine entries

Each entry under ``routines`` is a JSON object describing one Fortran
routine.  All array values keep the same order as the corresponding
arguments in the original source.  The following keys may appear:

- ``args`` – array of argument names.
- ``intents`` – array of intents for each argument (``"in"``,
  ``"out"`` or ``"inout"``) or ``null`` when unknown.
- ``dims`` – array describing the shape of each argument; elements are
  either ``null`` for scalars or an array of dimension expressions.
- ``type`` – array of type names.
- ``kind`` – array of kind values (integers) or ``null`` when not
  specified.
- ``module`` – name of the module that defines the routine.
- ``skip`` – ``true`` when the routine should not be differentiated.
- ``name_fwd_ad`` / ``args_fwd_ad`` /
  ``intents_fwd_ad`` – forward‑mode routine name, argument list and
  intents.
- ``name_rev_ad`` / ``args_rev_ad`` /
  ``intents_rev_ad`` – reverse‑mode routine name, argument list and
  intents.
- ``name_fwd_rev_ad`` / ``args_fwd_rev_ad`` /
  ``intents_fwd_rev_ad`` – combined forward/reverse routine information
  when present.

All ``*_ad`` fields use strings or arrays of strings, with intent arrays
matching the length and order of the argument arrays.

## Variable entries

Each entry under ``variables`` describes a module variable and may
contain:

- ``typename`` – base type of the variable.
- ``kind`` – optional kind value (integer).
- ``dims`` – optional array of dimension expressions.
- ``constant`` – ``true`` if the variable has the ``constant`` attribute.
- ``parameter`` – ``true`` when declared as a ``parameter``.
- ``allocatable`` – ``true`` if the variable is ``allocatable``.
- ``pointer`` – ``true`` if the variable is a ``pointer``.
- ``init_val`` – optional initial value.
- ``access`` – optional access specifier such as ``"public"`` or
  ``"private"``.

Absent boolean fields default to ``false``.  ``init_val`` and ``dims``
mirror the syntax used in the original Fortran source.
