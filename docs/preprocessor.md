# Preprocessor support

The generator understands a limited subset of C preprocessor directives embedded in Fortran sources.

## Supported range

- Object-like `#define` macros.
- Simple function-like macros without token pasting or variadic arguments.
- Conditionals using `#if defined(NAME)`, `#if !defined(NAME)`, `#ifdef`, `#ifndef`, `#else`, and `#endif`.
- Macros declared inside modules are tracked so later references expand within the module.

## Unsupported features

- `#include` directives.
- `#elif` branches and general `#if` expressions beyond `defined(...)`.
- Macros that use token pasting (`##`), stringification (`#`), variadic arguments, or line continuation characters (`\`).
- Complex expression evaluation inside `#if` directives or macro bodies.

## Known limitations

- Expansion is purely textual and does not perform full C preprocessing.
- Only macros defined in the current file are considered; macros from included files or build system definitions are ignored.
- Directives that cannot be interpreted are preserved verbatim in the generated output.

