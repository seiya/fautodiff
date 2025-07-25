# Directives

Optional directives can be added in Fortran comments to adjust how the
automatic differentiation code is generated. Each directive starts with
`!$FAD` and may appear immediately before a subroutine, function or
module definition.

## CONSTANT_VARS

```
!$FAD CONSTANT_VARS: var1, var2
```

Variables listed after the colon are treated as constants during
 differentiation. This directive can be applied to routine arguments,
local variables or module level variables. A constant variable does not
receive a corresponding `_ad` variable and no derivatives are computed
for it.

## SKIP

```
!$FAD SKIP
```

Use `SKIP` before a routine to parse it but omit the generation of AD
code.

