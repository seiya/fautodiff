"""Mapping of Fortran intrinsic functions to derivative expressions."""

# Map intrinsic function name (lowercase) to a format string for its derivative.
# The placeholder ``{arg}`` is replaced with the argument expression.

INTRINSIC_DERIVATIVES = {
    'abs': 'sign(1.0, {arg})',
    'sqrt': '0.5 / sqrt({arg})',
    'exp': 'exp({arg})',
    'log': '1.0 / {arg}',
    'log10': '1.0 / ({arg} * log(10.0))',
    'sin': 'cos({arg})',
    'cos': '-sin({arg})',
    'tan': '1.0 / cos({arg})**2',
    'asin': '1.0 / sqrt(1.0 - ({arg})**2)',
    'acos': '-1.0 / sqrt(1.0 - ({arg})**2)',
    'atan': '1.0 / (1.0 + ({arg})**2)',
    'sinh': 'cosh({arg})',
    'cosh': 'sinh({arg})',
    'tanh': '1.0 / cosh({arg})**2',
    'asinh': '1.0 / sqrt(({arg})**2 + 1.0)',
    'acosh': '1.0 / sqrt({arg} - 1.0) / sqrt({arg} + 1.0)',
    'atanh': '1.0 / (1.0 - ({arg})**2)',
}
