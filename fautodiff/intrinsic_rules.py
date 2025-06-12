"""Rules for handling Fortran intrinsic functions during AD generation.

``DERIVATIVE_TEMPLATES`` contains mappings from intrinsic names to derivative
expressions.  Each entry maps the lowercase intrinsic name to either a single
format string for unary functions or a tuple of strings for multi argument
functions.  The placeholders ``{arg}``, ``{arg1}``, ``{arg2}``, ... are replaced
with the corresponding argument expressions when forming the derivative.
Intrinsic functions returning integer values are considered non-differentiable.

``SPECIAL_HANDLERS`` provides callbacks for intrinsics that require custom
gradient propagation logic (e.g. ``transpose`` and ``cshift``).
"""

DERIVATIVE_TEMPLATES = {
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

# Intrinsic functions that are constant or otherwise not differentiable.
# Derivatives of these functions are treated as zero and no warnings are
# produced when they are encountered during AD code generation.  Special
# handlers for ``transpose`` and ``cshift`` are defined below.
NONDIFF_INTRINSICS = {
    'len',
    'len_trim',
    'adjustl',
    'index',
    'lbound',
    'ubound',
    'size',
    'epsilon',
    'huge',
    'tiny',
    'ichar',
    'achar',
    'int',
    'nint',
}


def _handle_transpose(lhs, items, grad_var, defined, decl_names, decl_set):
    """Propagate gradient through ``transpose``."""
    arg = items[0].tofortran()
    lhs_grad = grad_var.get(lhs, f"{lhs}_ad")
    block = []
    if arg in defined:
        block.append(f"{arg}_ad = transpose({lhs_grad}) + {arg}_ad\n")
    else:
        block.append(f"{arg}_ad = transpose({lhs_grad})\n")
        defined.add(arg)
    return block, {lhs, arg}


def _handle_cshift(lhs, items, grad_var, defined, decl_names, decl_set):
    """Propagate gradient through ``cshift``."""
    arr = items[0].tofortran()
    shift = items[1].tofortran()
    dim = items[2].tofortran() if len(items) > 2 else None
    lhs_grad = grad_var.get(lhs, f"{lhs}_ad")
    update = f"cshift({lhs_grad}, -{shift}" + (f", {dim})" if dim else ")")
    block = []
    if arr == lhs:
        new_grad = f"{lhs}_ad_"
        block.append(f"{new_grad} = {update}\n")
        grad_var[lhs] = new_grad
        if new_grad not in decl_set:
            decl_names.append(new_grad)
            decl_set.add(new_grad)
    else:
        if arr in defined:
            block.append(f"{arr}_ad = {update} + {arr}_ad\n")
        else:
            block.append(f"{arr}_ad = {update}\n")
            defined.add(arr)
    return block, {lhs, arr}


SPECIAL_HANDLERS = {
    'transpose': _handle_transpose,
    'cshift': _handle_cshift,
}

