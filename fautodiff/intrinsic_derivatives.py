"""Mapping of Fortran intrinsic functions to derivative expressions.

Each entry maps the lowercase intrinsic name to either a single format string
for unary functions or a tuple of strings for multi argument functions.  The
placeholders ``{arg}``, ``{arg1}``, ``{arg2}``, ... are replaced with the
corresponding argument expressions when forming the derivative.
"""

INTRINSIC_DERIVATIVES = {
    'abs': 'sign(1.0, {arg})',
    'sqrt': '0.5 / sqrt({arg})',
    'exp': 'exp({arg})',
    'log': '1.0 / {arg}',
    'log10': '1.0 / (({arg}) * log(10.0))',
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
    'erf': '2.0 / sqrt(acos(-1.0)) * exp(-({arg})**2)',
    'erfc': '-2.0 / sqrt(acos(-1.0)) * exp(-({arg})**2)',
    # Two argument intrinsics map to a tuple of partial derivative expressions
    # (d/darg1, d/darg2)
    'mod': ('1.0', '-real(int({arg1} / {arg2}), kind({arg1}))'),
    'int': '0.0',
    'nint': '0.0',
    'real': '1.0',
    'dble': '1.0',
    'min': ('merge(1.0, 0.0, {arg1} <= {arg2})',
            'merge(0.0, 1.0, {arg1} <= {arg2})'),
    'max': ('merge(1.0, 0.0, {arg1} >= {arg2})',
            'merge(0.0, 1.0, {arg1} >= {arg2})'),
    'sign': ('sign(1.0, {arg1}) * sign(1.0, {arg2})', '0.0'),
    'atan2': ('{arg2} / ({arg1}**2 + {arg2}**2)',
              '-{arg1} / ({arg1}**2 + {arg2}**2)'),
}
