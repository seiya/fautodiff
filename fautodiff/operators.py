"""Utilities for assembling computing operations."""

from __future__ import annotations

import copy
import re
from dataclasses import dataclass, field
from fractions import Fraction
from typing import Any, ClassVar, Iterable, Iterator, List, Optional, Tuple, Union

from .var_dict import VarDict

_NAME_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")  # pattern for valid variable names

# Suffix used for adjoint variables.  This is set by ``generator`` when a
# different suffix is desired but defaults to ``_ad``.
AD_SUFFIX = "_ad"


@dataclass
class Kind:
    """Representation of a Fortran kind."""

    var: Operator
    val: int = field(default=None)
    use_kind_keyword: bool = True

    def __post_init__(self):
        """Validate the kind."""
        if not isinstance(self.var, Operator):
            raise ValueError(f"var must be an Operator: {type(self.var)}")
        if self.val is not None and not isinstance(self.val, int):
            raise ValueError(f"val must be an int or None: {type(self.val)}")
        if self.val is None:
            if isinstance(self.var, OpInt):
                self.val = self.var.val
            elif not isinstance(self.var, OpVar):
                raise ValueError("val must be provided if var is not OpInt or OpVar")

    def copy(self) -> "Kind":
        """Create a copy of this kind."""
        return Kind(var=self.var, val=self.val, use_kind_keyword=self.use_kind_keyword)

    def __str__(self) -> str:
        """Return a string representation of the kind."""
        if isinstance(self.var, OpInt):
            return str(self.var.val)
        return str(self.var)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Kind):
            return self.var == other.var
        if isinstance(other, str):
            return str(self.var) == other
        if isinstance(other, int):
            return isinstance(self.var, OpInt) and self.var.val == other
        return NotImplemented


@dataclass
class VarType:
    """Representation of a Fortran variable type."""

    typename: str
    kind: Optional[Kind] = None
    char_len: Optional[str] = None

    def __post_init__(self):
        """Validate the variable type."""
        if not isinstance(self.typename, str):
            raise ValueError(f"typename must be a string: {type(self.typename)}")
        if self.kind is not None and not isinstance(self.kind, Kind):
            raise ValueError(f"kind must be an Kind or None: {type(self.kind)}")
        if self.char_len is not None and not isinstance(self.char_len, str):
            raise ValueError(
                f"char_len must be a string or None: {type(self.char_len)}"
            )

    def copy(self) -> "VarType":
        return VarType(
            typename=self.typename,
            kind=self.kind.copy() if self.kind else None,
            char_len=self.char_len,
        )

    def __str__(self) -> str:
        parts = []
        if self.kind is not None:
            if isinstance(self.kind.var, OpInt):
                parts.append(str(self.kind.var))
            elif isinstance(self.kind.var, OpVar) and not self.kind.use_kind_keyword:
                parts.append(str(self.kind.var))
            else:
                parts.append(f"kind={self.kind.var}")
        if self.char_len is not None:
            parts.append(f"len={self.char_len}")
        if parts:
            return f"{self.typename}(" + ",".join(parts) + ")"
        return self.typename

    def is_real_type(self) -> bool:
        typename = self.typename.lower()
        return typename.startswith("real") or typename.startswith("double")

    def is_complex_type(self) -> bool:
        return self.typename.lower().startswith("complex")

    def is_integer_type(self) -> bool:
        return self.typename.lower().startswith("integer")

    def _binary_result(self, other: "VarType") -> "VarType":
        if other is None:
            return self.copy()
        if self.kind is not None and other.kind is not None:
            kind = self.kind if self.kind.val > other.kind.val else other.kind
        else:
            kind = self.kind or other.kind
        if self.is_complex_type() and other.is_complex_type():
            return VarType("complex", kind=kind)
        if self.is_complex_type():
            return VarType("complex", kind=self.kind)
        if other.is_complex_type():
            return VarType("complex", kind=other.kind)
        if self.is_real_type() and other.is_real_type():
            return VarType("real", kind=kind)
        if self.is_real_type():
            return VarType("real", kind=self.kind)
        if other.is_real_type():
            return VarType("real", kind=other.kind)
        if self.is_integer_type() and other.is_integer_type():
            return VarType("integer", kind=kind)
        raise NotImplementedError(
            f"Cannot determine result type for {self} and {other}"
        )

    def __add__(self, other: "VarType") -> "VarType":
        return self._binary_result(other)

    __sub__ = __add__
    __mul__ = __add__
    __truediv__ = __add__


@dataclass
class AryIndex:
    """Class to represent index of fortran array"""

    dims: List[Optional["Operator"]] = field(default_factory=list)

    def __post_init__(self):
        """Validate and normalise the dimension list."""
        if not (isinstance(self.dims, list) or isinstance(self.dims, tuple)):
            raise ValueError(f"dims must be either list or tuple: {type(self.dims)}")
        if isinstance(self.dims, tuple):
            self.dims = list(self.dims)
        for i, dim in enumerate(self.dims):
            # Convert integers to ``OpInt`` and ensure allowed types.
            if isinstance(dim, int):
                self.dims[i] = OpInt(dim)
                continue
            if dim is None:
                continue
            if not isinstance(dim, Operator):
                raise ValueError(
                    f"dim must be either None, int, or Operator] {type(dim)}"
                )

    def list(self) -> List[str]:
        """Return list of dimension strings, using ':' for undefined."""
        return (
            [(":" if dim is None else str(dim)) for dim in self.dims]
            if self.dims is not None
            else []
        )

    def copy(self) -> AryIndex:
        """Create a shallow copy of this index."""
        return AryIndex(list(self.dims) if self.dims is not None else None)

    def deep_clone(self) -> "AryIndex":
        """Return a deep copy where nested operators are cloned."""
        dims: List[Optional[Operator]] | None = None
        if self.dims is not None:
            dims = []
            for dim in self.dims:
                if isinstance(dim, Operator):
                    dims.append(dim.deep_clone())
                else:
                    dims.append(dim)
        return AryIndex(dims)

    def __len__(self) -> int:
        """Return the number of dimensions stored."""
        return len(self.dims) if self.dims is not None else 0

    def __getitem__(
        self, index: Union[int, slice]
    ) -> Optional[Union["Operator", List[Optional["Operator"]]]]:
        """Provide list-style indexing access to dimensions."""
        return self.dims[index] if self.dims is not None else None

    def __setitem__(self, index: int, var: Optional[Operator]) -> None:
        """Set a dimension value, expanding the list as necessary."""
        if self.dims is None:
            self.dims = []
        if index >= len(self.dims):
            self.dims.extend([None] * (index - len(self.dims) + 1))  # type: ignore
        self.dims[index] = var

    def __iter__(self) -> Iterator[Optional["Operator"]]:
        """Iterate through each dimension entry."""
        return iter(self.dims) if self.dims is not None else iter([])

    def __str__(self) -> str:
        """Human-readable string form of the index."""
        return ",".join(self.list())

    def __eq__(self, other) -> bool:
        """Equality with support for wildcards and ranges."""
        if other is not None and not isinstance(other, AryIndex):
            return NotImplemented
        if other is None or other.dims is None:
            return not self.is_partial_access()
        if self.dims is None:
            return not other.is_partial_access()
        if len(self.dims) != len(other.dims):
            print([self])
            print([other])
            raise ValueError(f"Different number of dimensions: {self} {other}")
        for i, dim1 in enumerate(self.dims):
            dim2 = other.dims[i]
            if dim1 == dim2:
                continue
            if (dim1 is None or isinstance(dim1, OpRange)) and (
                dim2 is None or isinstance(dim2, OpRange)
            ):
                if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                    return False
                continue
            return False
        return True

    @classmethod
    def dim_is_entire(cls, dim) -> bool:
        """Return ``True`` if ``dim`` spans the whole dimension."""
        if dim is None:
            return True
        if isinstance(dim, OpRange):
            if dim[0] is None and dim[1] is None:
                return True
        return False

    @classmethod
    def get_diff_dim(cls, index1: AryIndex | None, index2: AryIndex | None) -> int:
        """Identify the first dimension where two indices differ."""

        if index1 is None and index2 is None:
            return -1
        if (index1 is not None and index2 is not None) and len(index1) != len(index2):
            raise ValueError("Different number of dimensions")
        if index1 is None:
            index1 = [None] * len(index2)

        # ``diff_dim`` stores the first dimension where a difference is
        # encountered.  If we find differences in more than one dimension the
        # function returns ``-1`` to signal that the indices are too dissimilar
        # to be merged.
        diff_dim = -1
        for i, dim1 in enumerate(index1):
            dim2 = index2[i] if index2 is not None else None
            if dim1 == dim2:
                continue
            if AryIndex.dim_is_entire(dim1) and AryIndex.dim_is_entire(dim2):
                continue
            if diff_dim >= 0:
                return -1
            diff_dim = i

        return diff_dim

    @staticmethod
    def _get_int(op) -> Optional[int]:
        """Return integer value of ``op`` if it represents a literal."""
        if isinstance(op, OpInt):
            return op.val
        if isinstance(op, OpNeg) and isinstance(op.args[0], OpInt):
            return -op.args[0].val
        return None

    @staticmethod
    def check_overlap(
        index1: "AryIndex", index2: "AryIndex", context: Optional[VarDict] = None
    ) -> bool:
        """Return true if ``index1`` overlaps ``index2``."""
        if len(index1.dims) != len(index2.dims):
            raise ValueError(f"Different number of dimensions: {index1} {index2}")
        if index1 == index2:
            return True
        for i, dim1 in enumerate(index1):
            dim2 = index2.dims[i]
            if context is not None:
                for var, range in context.items():
                    if dim1 is not None:
                        dim1 = dim1.replace_with(var, range)
                    if dim2 is not None:
                        dim2 = dim2.replace_with(var, range)
            if dim1 == dim2:
                continue
            if AryIndex.dim_is_entire(dim1):
                continue
            if dim2 is None:
                continue
            if isinstance(dim1, OpRange):
                if dim2 not in dim1:
                    return False
            if isinstance(dim2, OpRange):
                if dim1 not in dim2:
                    return False
            if not (isinstance(dim1, OpRange) or isinstance(dim2, OpRange)):
                diff = AryIndex._get_int(dim1 - dim2)
                if diff is not None and diff != 0:
                    return False
        return True

    def check_outside(self, other: "AryIndex") -> bool:
        """Return true if other is partially outside of this index."""
        if len(self.dims) != len(other.dims):
            raise ValueError(f"Different number of dimensions: {self} {other}")
        if self == other:
            return False
        for i, dim1 in enumerate(self):
            dim2 = other.dims[i]
            if isinstance(dim1, OpRange) and dim1[0] == dim1[1]:
                dim1 = dim1[0]
            if isinstance(dim2, OpRange) and dim2[0] == dim2[1]:
                dim1 = dim2[0]
            if dim1 == dim2:
                continue
            if AryIndex.dim_is_entire(dim1):
                continue
            if dim2 is None:
                continue
            if not isinstance(dim1, OpRange):
                if isinstance(dim2, OpRange) or AryIndex._get_int(dim1 - dim2) != 0:
                    return True
                continue

            i0_op, i1_op, _ = dim1.ascending()

            if not isinstance(dim2, OpRange):
                if dim1.strict_in(dim2):
                    continue
                if i0_op == dim2 or i1_op == dim2:
                    continue
                diff = AryIndex._get_int(i0_op - dim2)
                if diff is not None and diff > 0:
                    return True
                diff = AryIndex._get_int(dim2 - i1_op)
                if diff is not None and diff > 0:
                    return True
                continue

            j0_op, j1_op, _ = dim2.ascending()

            if i0_op == j0_op and i1_op == j1_op:
                continue
            d0 = AryIndex._get_int(j0_op - i0_op) if i0_op is not None else 999
            d1 = AryIndex._get_int(i1_op - j1_op) if i1_op is not None else 999
            if (d0 is not None and d0 < 0) or (d1 is not None and d1 < 0):
                return True
            continue
        return False

    @staticmethod
    def _check_cover(index1: "AryIndex", index2: "AryIndex") -> bool:
        """Return true if ``index1`` fully covers ``index2``."""
        if len(index1.dims) != len(index2.dims):
            raise ValueError(f"Different number of dimensions: {index1} {index2}")
        if index1 == index2:
            return True
        for i, dim1 in enumerate(index1):
            dim2 = index2.dims[i]
            if dim1 == dim2:
                continue
            if AryIndex.dim_is_entire(dim1):
                continue
            if dim2 is None:
                return False
            if not isinstance(dim1, OpRange):
                return False

            if not isinstance(dim2, OpRange):
                if dim1.strict_in(dim2):
                    continue
                return False

            i0_op, i1_op, stride1 = dim1.ascending()
            if stride1 is not None:
                stride1_val = AryIndex._get_int(stride1)
                if stride1_val is None or abs(stride1_val) != 1:
                    return False

            j0_op, j1_op, stride2 = dim2
            if stride2 is not None:
                stride2_val = AryIndex._get_int(stride2)
                if stride2_val is None or abs(stride2_val) != 1:
                    return False

            if i0_op == j0_op and i1_op == j1_op:
                continue
            if (j0_op is None and i0_op is not None) or (
                j1_op is None and i1_op is not None
            ):
                return False
            d0 = AryIndex._get_int(j0_op - i0_op) if i0_op is not None else 999
            d1 = AryIndex._get_int(i1_op - j1_op) if i1_op is not None else 999
            if (d0 is not None and d0 >= 0) and (d1 is not None and d1 >= 0):
                continue
            else:
                return False
        return True

    def __le__(self, other) -> bool:
        """Return True if ``self`` is covered by ``other``."""
        if other is not None and not isinstance(other, AryIndex):
            raise NotImplementedError
        if other is None:
            return True
        return AryIndex._check_cover(other, self)

    def __ge__(self, other) -> bool:
        """Return True if ``self`` covers ``other``."""
        if other is not None and not isinstance(other, AryIndex):
            raise NotImplementedError
        if other is None:
            return self.dims is None or all(
                [dim is None or isinstance(dim, OpRange) for dim in self.dims]
            )
        return AryIndex._check_cover(self, other)

    def ascending(self) -> "AryIndex":
        new_aryindex: List[Operator | None] = []
        replaced = False
        for dim in self.dims:
            if dim is None:
                new_aryindex.append(None)
            elif isinstance(dim, OpRange):
                dim_new = dim.ascending()
                new_aryindex.append(dim_new)
                if dim_new is not dim:
                    replaced = True
            else:
                new_aryindex.append(dim)
        if replaced:
            return AryIndex(new_aryindex)
        else:
            return self

    def replace_with(self, src: Operator, dest: Operator) -> AryIndex:
        updated = False
        new_dims: List[Operator] = []
        for dim in self.dims:
            new_dim = dim.replace_with(src, dest) if dim is not None else None
            if new_dim is not dim:
                updated = True
            new_dims.append(new_dim)
        if updated:
            return AryIndex(new_dims)
        else:
            return self

    def collect_vars(self) -> List[OpVar]:
        """Collect all variables used within the index expressions."""
        if self.dims is None:
            return []
        vars = []
        for dim in self.dims:
            if dim is None:
                continue
            for var in dim.collect_vars():
                if var not in vars:
                    vars.append(var)
        return vars

    def is_partial_access(self) -> bool:
        """Check if the index specifies a partial array access."""
        if self.dims is None:
            return False
        for dim in self.dims:
            if dim is None:
                continue
            if isinstance(dim, OpRange):
                if dim[0] is not None or dim[1] is not None:
                    return True
                continue
            return True
        return False

    def is_depended_on(self, var: OpVar) -> bool:
        """Return True if ``var`` appears in any dimension expression."""
        if self.dims is None:
            return False
        for dim in self.dims:
            if dim is None:
                continue
            if var in dim.collect_vars():
                return True
        return False


@dataclass
class Operator:
    """Abstract fortran oprations."""

    args: Optional[List[Optional[Operator]]] = None
    var_type: VarType = field(default=None, repr=False)
    macro_name: Optional[str] = None
    PRIORITY: ClassVar[int] = -999

    @property
    def kind(self):
        return self.var_type.kind

    @kind.setter
    def kind(self, value):
        self.var_type.kind = value

    def __post_init__(self):
        """Ensure the argument list is stored as a Python list."""
        if self.args is not None and not isinstance(self.args, list):
            raise ValueError(f"args must be a list: {type(self.args)}")
        if self.var_type is None and self.args:
            for arg in self.args:
                if isinstance(arg, OpRange):
                    raise ValueError("arg mut not be OpRange")
                if isinstance(arg, Operator):
                    if arg.var_type is None:
                        raise ValueError(f"var_type must not be None: {type(arg)}")
                    self.var_type = arg.var_type.copy()
                    break
        if self.var_type is None:
            raise ValueError("var_type must not be None")
        return None

    def _paren(self, arg: Operator, eq: bool = False) -> str:
        """Add parentheses around ``arg`` if precedence requires it."""
        if self.PRIORITY < arg.PRIORITY or (eq and self.PRIORITY == arg.PRIORITY):
            return f"({arg})"
        else:
            return f"{arg}"

    def deep_clone(self) -> "Operator":
        """Create a deep copy of the operator tree."""
        clone = copy.copy(self)
        if self.args is not None:
            clone.args = [
                arg.deep_clone() if isinstance(arg, Operator) else arg
                for arg in self.args
            ]
        return clone

    def simplify(self) -> "Operator":
        """Return a simplified operator by recursively simplifying arguments
        and invoking the class' ``eval`` to fold constants and normalize.

        This performs a light-weight pass and relies on each operator's
        algebra implemented in ``__add__``, ``__sub__``, etc., and ``eval``.
        """
        # Leaf or no-arg operators are already simplest
        if self.args is None or len(self.args) == 0:
            return self

        # Simplify children first
        new_args = []
        changed = False
        for a in self.args:
            if isinstance(a, Operator):
                sa = a.simplify()
                new_args.append(sa)
                if sa is not a:
                    changed = True
            else:
                new_args.append(a)

        # Try to re-evaluate with simplified args to trigger folding
        try:
            return type(self).eval(new_args)
        except Exception:
            return self.copy_with_args(new_args) if changed else self

    @classmethod
    def eval(cls, args: List[Optional[Operator]]) -> "Operator":
        return cls(args)

    def get_int(self) -> int | None:
        """Return integer value of ``op`` if it represents a literal."""
        if isinstance(self, OpInt):
            return self.val
        if isinstance(self, OpNeg) and isinstance(self.args[0], OpInt):
            return - self.args[0].val
        return None

    def copy_with_args(self, args: List[Optional[Operator]]) -> "Operator":
        for i, arg in enumerate(args):
            if isinstance(arg, OpRange):
                arg_new = list(args)
                dims = []
                for j in range(3):
                    if arg[j] is None:
                        dims.append(None)
                    else:
                        arg_new[i] = arg.args[j]
                        dims.append(type(self).eval(arg_new))
                return OpRange(dims)

        clone = copy.copy(self)
        clone.args = args
        return clone

    def collect_vars(
        self,
        without_index: bool = False,
        without_refvar: bool = False,
        without_checkfunc: bool = False,
    ) -> List[OpVar]:
        """Gather variables referenced by this operator and its children."""
        vars: List[OpVar] = []
        if self.args is None:
            return vars
        for arg in self.args:
            for var in arg.collect_vars(
                without_index, without_refvar, without_checkfunc
            ):
                if var not in vars:
                    vars.append(var)
        return vars

    def is_array(self) -> bool:
        """Return true if this is an array"""
        raise NotImplementedError(f"is_array in {type(self)}")

    def find_userfunc(self) -> List[OpFuncUser]:
        """Return a list of user-defined functions referenced."""
        funcs: List[OpFuncUser] = []
        if self.args is None:
            return funcs
        for arg in self.args:
            if arg is not None:
                funcs.extend(arg.find_userfunc())
        return funcs

    def replace_with(self, src: Operator, dest: Operator) -> Operator:
        """Replace occurrences of ``src`` with ``dest`` in the tree."""
        if self == src:
            return dest
        if self.args is None:
            return self
        args_new: List[Any] = []
        flag = False  # True if replaced
        for arg in self.args:
            if arg == src:
                args_new.append(dest)
                flag = True
            elif isinstance(arg, Operator):
                arg_new = arg.replace_with(src, dest)
                args_new.append(arg_new)
                if arg_new is not arg:
                    flag = True
            else:
                args_new.append(arg)
        if not flag:
            return self
        # Build a new operator from updated args (preserving OpRange lifting)
        new_op = self.copy_with_args(args_new)
        # And simplify the result once to fold constants and normalize
        if isinstance(new_op, Operator):
            return new_op.simplify()
        return new_op

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        """Return the derivative operation with respetive to ```var```."""
        raise NotImplementedError(f"derivative in {type(self)}")

    def __neg__(self) -> "Operator":
        """Return the negated operator, simplifying when possible."""
        if isinstance(self, OpNeg):
            return self.args[0]
        if isinstance(self, OpInt) and self.val == 0:
            return self
        return OpNeg(args=[self])

    def __add__(self, other) -> "Operator":
        """Addition with various algebraic simplifications."""
        if isinstance(other, int):
            return self + OpInt(other)
        if isinstance(other, Operator):
            if self.macro_name or other.macro_name:
                return OpAdd(args=[self, other])
            if isinstance(self, OpInt) and self.val == 0:
                return other
            if isinstance(other, OpInt) and other.val == 0:
                return self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                return OpInt(self.val + other.val, target=self.target, kind=self.kind)
            if (
                isinstance(self, OpReal)
                and isinstance(other, OpReal)
                and self.kind == other.kind
                and self.val == f"{float(self.val)}"
                and other.val == f"{float(other.val)}"
            ):
                return OpReal(
                    val=f"{float(self.val) + float(other.val)}", kind=self.kind
                )
            if (
                isinstance(self, OpReal)
                and isinstance(other, OpInt)
                and self.val == f"{float(self.val)}"
            ):
                return OpReal(val=f"{float(self.val) + other.val}", kind=self.kind)
            if (
                isinstance(other, OpReal)
                and isinstance(self, OpInt)
                and other.val == f"{float(other.val)}"
            ):
                return OpReal(val=f"{float(other.val) + self.val}", kind=other.kind)
            if (
                isinstance(self, OpAdd)
                and isinstance(self.args[0], OpNum)
                and isinstance(other, OpNum)
            ):
                return self.args[1] + (self.args[0] + other)
            if (
                isinstance(self, OpAdd)
                and isinstance(self.args[1], OpNum)
                and isinstance(other, OpNum)
            ):
                return self.args[0] + (self.args[1] + other)
            if (
                isinstance(other, OpAdd)
                and isinstance(other.args[0], OpNum)
                and isinstance(self, OpNum)
            ):
                return other.args[1] + (other.args[0] + self)
            if (
                isinstance(other, OpAdd)
                and isinstance(other.args[1], OpNum)
                and isinstance(self, OpNum)
            ):
                return other.args[0] + (other.args[1] + self)
            if isinstance(other, OpAdd):
                return self + other.args[0] + other.args[1]
            if (
                isinstance(self, OpSub)
                and isinstance(self.args[0], OpNum)
                and isinstance(other, OpNum)
            ):
                return -self.args[1] + (self.args[0] + other)
            if (
                isinstance(self, OpSub)
                and isinstance(self.args[1], OpNum)
                and isinstance(other, OpNum)
            ):
                return self.args[0] + (other - self.args[1])
            if (
                isinstance(other, OpSub)
                and isinstance(other.args[0], OpNum)
                and isinstance(self, OpNum)
            ):
                return -other.args[1] + (other.args[0] + self)
            if (
                isinstance(other, OpSub)
                and isinstance(other.args[1], OpNum)
                and isinstance(self, OpNum)
            ):
                return other.args[0] + (self - other.args[1])
            if isinstance(self, OpSub) and self.args[1] == other:
                return self.args[0]
            if isinstance(other, OpSub) and other.args[1] == self:
                return other.args[0]
            if isinstance(other, OpSub):
                return self + other.args[0] - other.args[1]
            if isinstance(other, OpNeg):
                return self - other.args[0]
            return OpAdd(args=[self, other])
        return NotImplemented

    def __sub__(self, other) -> "Operator":
        """Subtraction with algebraic simplifications."""
        if isinstance(other, int):
            return self - OpInt(other)
        if isinstance(other, Operator):
            if self.macro_name or other.macro_name:
                return OpSub(args=[self, other])
            if self == other:
                return OpInt(0)
            if isinstance(self, OpInt) and self.val == 0:
                return -other
            if isinstance(other, OpInt) and other.val == 0:
                return self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                val = self.val - other.val
                if val < 0:
                    return -OpInt(-val, target=self.target, kind=self.kind)
                else:
                    return OpInt(val, target=self.target, kind=self.kind)
            if (
                isinstance(self, OpReal)
                and isinstance(other, OpReal)
                and self.kind == other.kind
                and self.val == f"{float(self.val)}"
                and other.val == f"{float(other.val)}"
            ):
                return OpReal(
                    val=f"{float(self.val) - float(other.val)}", kind=self.kind
                )
            if (
                isinstance(self, OpReal)
                and isinstance(other, OpInt)
                and self.val == f"{float(self.val)}"
            ):
                return OpReal(val=f"{float(self.val) - other.val}", kind=self.kind)
            if (
                isinstance(other, OpReal)
                and isinstance(self, OpInt)
                and other.val == f"{float(other.val)}"
            ):
                return OpReal(val=f"{float(other.val) - self.val}", kind=other.kind)
            if (
                isinstance(self, OpAdd)
                and isinstance(self.args[0], OpNum)
                and isinstance(other, OpNum)
            ):
                return self.args[1] + (self.args[0] - other)
            if (
                isinstance(self, OpAdd)
                and isinstance(self.args[1], OpNum)
                and isinstance(other, OpNum)
            ):
                return self.args[0] + (self.args[1] - other)
            if (
                isinstance(other, OpAdd)
                and isinstance(other.args[0], OpNum)
                and isinstance(self, OpNum)
            ):
                return -other.args[1] + (self - other.args[0])
            if (
                isinstance(other, OpAdd)
                and isinstance(other.args[1], OpNum)
                and isinstance(self, OpNum)
            ):
                return -other.args[0] + (self - other.args[1])
            if isinstance(self, OpAdd) and self.args[0] == other:
                return self.args[1]
            if isinstance(self, OpAdd) and self.args[1] == other:
                return self.args[0]
            if isinstance(other, OpAdd) and other.args[0] == self:
                return -other.args[1]
            if isinstance(other, OpAdd) and other.args[1] == self:
                return other.args[0]
            if isinstance(other, OpAdd):
                return self - other.args[0] - other.args[1]
            if (
                isinstance(self, OpSub)
                and isinstance(self.args[0], OpNum)
                and isinstance(other, OpNum)
            ):
                return -self.args[1] + (self.args[0] - other)
            if (
                isinstance(self, OpSub)
                and isinstance(self.args[1], OpNum)
                and isinstance(other, OpNum)
            ):
                return self.args[0] - (self.args[1] + other)
            if (
                isinstance(other, OpSub)
                and isinstance(other.args[0], OpNum)
                and isinstance(self, OpNum)
            ):
                return other.args[1] + (self - other.args[0])
            if (
                isinstance(other, OpSub)
                and isinstance(other.args[1], OpNum)
                and isinstance(self, OpNum)
            ):
                return -other.args[0] + (self + other.args[1])
            if isinstance(self, OpSub) and self.args[0] == other:
                return -self.args[1]
            if isinstance(other, OpSub) and other.args[0] == self:
                return other.args[1]
            if isinstance(other, OpSub):
                return self - other.args[0] + other.args[1]
            if isinstance(other, OpNeg):
                return self + other.args[0]
            return OpSub(args=[self, other])
        return NotImplemented

    def __mul__(self, other) -> "Operator":
        """Multiplication with constant folding and simplifications."""
        if isinstance(other, int):
            return self * OpInt(other)
        if isinstance(other, Operator):
            if self.macro_name or other.macro_name:
                return OpMul(args=[self, other])
            if isinstance(self, OpInt) and self.val == 1:
                return other
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(self, OpInt) and self.val == 0:
                return OpInt(0, target=self.target, kind=other.var_type.kind)
            if isinstance(other, OpInt) and other.val == 0:
                return OpInt(0, target=other.target, kind=self.var_type.kind)
            if isinstance(self, OpInt) and self.val == -1:
                return -other
            if isinstance(other, OpInt) and other.val == -1:
                return -self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                return OpInt(
                    self.val * other.val, target=self.target, kind=self.var_type.kind
                )
            if (
                isinstance(self, OpReal)
                and isinstance(other, OpReal)
                and self.var_type == other.var_type
                and self.val == str(float(self.val))
                and other.val == str(float(other.val))
            ):
                return OpReal(
                    val=str(float(self.val) * float(other.val)), var_type=self.var_type
                )
            if (
                isinstance(self, OpReal)
                and isinstance(other, OpInt)
                and self.val == str(float(self.val))
            ):
                if float(self.val) == float(int(self.val)):
                    return OpInt(
                        int(self.val) * other.val,
                        target=other.target,
                        var_type=other.var_type,
                    )
                else:
                    return OpReal(
                        val=str(float(self.val) * other.val), var_type=self.var_type
                    )
            if (
                isinstance(other, OpReal)
                and isinstance(self, OpInt)
                and other.val == str(float(other.val))
            ):
                if float(other.val) == float(int(other.val)):
                    return OpInt(
                        int(other.val) * self.val,
                        target=self.target,
                        var_type=self.var_type,
                    )
                else:
                    return OpReal(
                        val=str(float(other.val) * self.val), var_type=other.var_type
                    )
            if isinstance(self, OpNeg):
                return -(self.args[0] * other)
            if isinstance(other, OpNeg):
                return -(self * other.args[0])
            if (
                isinstance(other, OpMul)
                and isinstance(self, OpNum)
                and isinstance(other.args[0], OpNum)
            ):
                return (self * other.args[0]) * other.args[1]
            if (
                isinstance(other, OpMul)
                and isinstance(self, OpNum)
                and isinstance(other.args[1], OpNum)
            ):
                return (self * other.args[1]) * other.args[0]
            if (
                isinstance(other, OpDiv)
                and isinstance(other.args[0], OpInt)
                and other.args[0].val == 1
            ):
                return self / other.args[1]
            if isinstance(other, OpPow) and isinstance(other.args[1], OpNeg):
                return self / other.args[0] ** (other.args[1].args[0])
            if isinstance(self, OpPow) and self.args[0] == other:
                expo = self.args[1]
                if isinstance(expo, OpVar) and expo.is_real_type:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (self.args[0]) ** (self.args[1] + one)
            if isinstance(other, OpPow) and other.args[0] == self:
                expo = other.args[1]
                if isinstance(expo, OpVar) and expo.is_real_type:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (other.args[0]) ** (one + other.args[1])
            return OpMul(args=[self, other])
        return NotImplemented

    def __truediv__(self, other) -> "Operator":
        """Division with simplifications and fraction handling."""
        if isinstance(other, int):
            return self / OpInt(other)
        if isinstance(other, Operator):
            if self.macro_name or other.macro_name:
                return OpDiv(args=[self, other])
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(self, OpInt) and self.val == 0:
                return self
            if isinstance(self, OpNeg):
                return -(self.args[0] / other)
            if isinstance(other, OpNeg):
                return -(self / other.args[0])
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                kind = self.kind or other.kind
                target = self.target or other.target
                val = Fraction(self.val, other.val)
                nume = OpInt(val.numerator, kind=kind, target=target)
                deno = OpInt(val.denominator, kind=kind, target=target)
                return OpDiv(args=[nume, deno])
            if isinstance(self, OpDiv) and isinstance(other, OpDiv):
                return (self.args[0] * other.args[1]) / (self.args[1] * other.args[0])
            if isinstance(self, OpDiv):
                return self.args[0] / (self.args[1] * other)
            if isinstance(other, OpDiv):
                return (self * other.args[1]) / other.args[0]
            if isinstance(other, OpPow) and isinstance(other.args[1], OpNeg):
                return self * other.args[0] ** (other.args[1].args[0])
            if isinstance(self, OpPow) and self.args[0] == other:
                expo = self.args[1]
                if isinstance(expo, OpVar) and expo.is_real_type:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (self.args[0]) ** (self.args[1] - one)
            if isinstance(other, OpPow) and other.args[0] == self:
                expo = other.args[1]
                if isinstance(expo, OpVar) and expo.is_real_type:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (other.args[0]) ** (one - other.args[1])
            return OpDiv(args=[self, other])
        return NotImplemented

    def __pow__(self, other) -> "Operator":
        if isinstance(other, int):
            return OpPow(args=[self, OpInt(other)])
        if isinstance(other, Operator):
            if self.macro_name or other.macro_name:
                return OpPow(args=[self, other])
            if isinstance(other, OpInt) and other.val == 0:
                return OpInt(1, target=other.target, kind=other.kind)
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(other, OpNeg) and isinstance(other.args[0], OpInt):
                return (
                    OpInt(1, target=other.args[0].target, kind=other.args[0].kind)
                    / self ** other.args[0]
                )
            if (
                isinstance(self, OpInt)
                and isinstance(other, OpInt)
                and isinstance(other.val, int)
            ):
                return OpInt(
                    self.val ** (other.val), target=self.target, kind=self.kind
                )
            if (
                isinstance(self, OpReal)
                and isinstance(other, OpInt)
                and self.val == f"{float(self.val)}"
            ):
                return OpReal(f"{float(self.val)**(other.val)}", kind=self.kind)
            return OpPow(args=[self, other])
        return NotImplemented

    def __lt__(self, other) -> OpLogic:
        if isinstance(other, int):
            return self < OpInt(other)
        return OpLogic("<", args=[self, other])

    def __le__(self, other) -> OpLogic:
        if isinstance(other, int):
            return self <= OpInt(other)
        return OpLogic("<=", args=[self, other])

    def __gt__(self, other) -> OpLogic:
        if isinstance(other, int):
            return self > OpInt(other)
        return OpLogic(">", args=[self, other])

    def __ge__(self, other) -> OpLogic:
        if isinstance(other, int):
            return self >= OpInt(other)
        return OpLogic(">=", args=[self, other])

    def __and__(self, other) -> OpLogic:
        return OpLogic(".and.", args=[self, other])

    def __or__(self, other) -> OpLogic:
        return OpLogic(".or.", args=[self, other])


@dataclass
class OpLeaf(Operator):
    PRIORITY: ClassVar[int] = 0

    def collect_vars(
        self,
        without_index: bool = False,
        without_refvar: bool = False,
        without_checkfunc: bool = False,
    ) -> List[OpVar]:
        return []

    def is_array(self) -> bool:
        return False

    def find_userfunc(self) -> List[OpFuncUser]:
        return []


@dataclass
class OpNum(OpLeaf):
    def collect_vars(
        self,
        without_index: bool = False,
        without_refvar: bool = False,
        without_checkfunc: bool = False,
    ) -> List[OpVar]:
        return []

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        if target is not None and getattr(target, "is_complex_type", False):
            zero = OpReal("0.0", kind=target.kind)
            return OpComplex(zero, zero, kind=target.kind)
        return OpInt(0, target=target)


@dataclass
class OpInt(OpNum):
    val: int = field(default=-999)
    target: Optional[OpVar] = None
    typename: ClassVar[Optional[str]] = "integer"
    dims: ClassVar[Optional[Tuple[str]]] = None

    def __init__(
        self, val: int, kind: Optional[Kind] = None, target: Optional[OpVar] = None
    ):
        super().__init__(
            args=[],
            var_type=VarType("integer", kind=kind),
        )
        if val < 0:
            raise ValueError(f"val must be >= 0: {val}")
        self.val = val
        self.target = target

    def __new__(
        cls, val: int, kind: Optional[Kind] = None, target: Optional[OpVar] = None
    ):
        if val < 0:
            instance = super().__new__(cls)
            instance.__init__(-val, kind=kind, target=target)
            return -instance
        else:
            return super().__new__(cls)

    def __reduce__(self):
        return (
            self.__class__,
            (
                self.val,
                self.var_type.kind,
                self.target,
            ),
        )

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        if self.target is not None:
            kind = self.target.var_type.kind
            if isinstance(kind, Kind):
                kind_val = kind.var
            else:
                kind_val = kind
            if self.target.is_real_type or self.target.is_complex_type:
                if isinstance(kind_val, OpInt) and kind_val.val == 8:
                    return f"{self.val}.0d0"
                if (
                    isinstance(kind_val, OpInt) and kind_val.val == 4
                ) or kind_val is None:
                    return f"{self.val}.0"
                return f"{self.val}.0_{kind_val}"
            if self.target.var_type.is_integer_type():
                if kind_val is not None and (
                    not isinstance(kind_val, OpInt) or kind_val.val != 4
                ):
                    return f"{self.val}_{kind_val}"
                return str(self.val)
        kind = self.var_type.kind
        if isinstance(kind, Kind):
            kind_val = kind.var
        else:
            kind_val = kind
        if kind_val is not None and kind_val != "4":
            return f"{self.val}_{kind_val}"
        return str(self.val)


@dataclass
class OpReal(OpNum):
    val: str = field(default="-999.0e99")
    expo: int = field(default=0)

    def __init__(self, val, kind: Optional[Kind] = None, expo: int = 0):
        super().__init__(
            args=[],
            var_type=VarType("real", kind=kind),
        )
        self.val = val
        self.expo = expo

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        kind = self.var_type.kind
        if kind is not None:
            if isinstance(kind.var, OpInt) and kind.var.val == 8:
                return f"{self.val}d{self.expo}"
            if isinstance(kind.var, OpInt) and kind.var.val == 4:
                return f"{self.val}e{self.expo}"
            if self.expo != 0:
                return f"{self.val}e{self.expo}_{kind.var}"
            else:
                return f"{self.val}_{kind.var}"
        return str(self.val)


@dataclass
class OpAry(OpLeaf):
    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        return f"[{', '.join(self.args)}]"


@dataclass
class OpChar(OpLeaf):
    name: str = field(default="")

    def __init__(self, name: str):
        super().__init__(args=[], var_type=VarType("character"))
        self.name = name

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        return self.name


@dataclass
class OpTrue(OpLeaf):
    def __init__(self):
        super().__init__(args=[], var_type=VarType("logical"))

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        return ".true."


@dataclass
class OpFalse(OpLeaf):
    def __init__(self):
        super().__init__(args=[], var_type=VarType("logical"))

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        return ".false."


@dataclass
class OpComplex(OpNum):
    real: Operator = field(default=None)
    imag: Operator = field(default=None)

    def __init__(self, real: Operator, imag: Operator, kind: Optional[Kind] = None):
        super().__init__(args=[real, imag], var_type=VarType("complex", kind=kind))
        self.real = real
        self.imag = imag
        if kind is not None:
            self.kind = kind

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        return f"({self.real}, {self.imag})"


@dataclass
class OpVar(OpLeaf):
    name: str = field(default="")
    index: Optional[AryIndex] = None
    dims: Optional[Tuple[Optional[Operator]]] = field(repr=False, default=None)
    dims_raw: Optional[Tuple[str]] = field(repr=False, default=None)
    intent: Optional[str] = field(default=None, repr=False)
    ad_target: Optional[bool] = field(default=None, repr=False)
    is_constant: Optional[bool] = field(default=None, repr=False)
    reference: Optional["OpVar"] = field(repr=False, default=None)
    allocatable: Optional[bool] = field(default=None, repr=False)
    pointer: Optional[bool] = field(default=None, repr=False)
    optional: Optional[bool] = field(default=None, repr=False)
    target: Optional[bool] = field(default=None, repr=False)
    save: Optional[bool] = field(default=None, repr=False)
    value: Optional[bool] = field(default=None, repr=False)
    volatile: Optional[bool] = field(default=None, repr=False)
    asynchronous: Optional[bool] = field(default=None, repr=False)
    declared_in: Optional[str] = field(default=None, repr=False)
    ref_var: Optional["OpVar"] = field(default=None)
    reduced_dims: Optional[List[int]] = field(init=False, repr=False, default=None)

    def __init__(
        self,
        name: str,
        index: Optional[AryIndex] = None,
        var_type: Optional[VarType] = None,
        dims: Optional[Tuple[Optional[Operator]]] = None,
        dims_raw: Optional[Tuple[str]] = None,
        reference: Optional[OpVar] = None,
        intent: Optional[str] = None,
        ad_target: Optional[bool] = None,
        is_constant: Optional[bool] = None,
        allocatable: Optional[bool] = None,
        pointer: Optional[bool] = None,
        optional: Optional[bool] = None,
        target: Optional[bool] = None,
        save: Optional[bool] = None,
        value: Optional[bool] = None,
        volatile: Optional[bool] = None,
        asynchronous: Optional[bool] = None,
        declared_in: Optional[str] = None,
        ref_var: Optional[OpVar] = None,
    ):
        if var_type is None:
            var_type = VarType("unknown")
        super().__init__(args=[], var_type=var_type)
        if not isinstance(name, str):
            raise ValueError(f"name must be str: {type(name)}")
        if not _NAME_RE.fullmatch(name):
            raise ValueError(f"invalid Fortran variable name: {name}")
        self.name = name
        if index is not None and not isinstance(index, AryIndex):
            index = AryIndex(index)
        self.index = index
        if self.var_type is not None:
            self.kind = self.var_type.kind
        if isinstance(dims, tuple):
            for dim in dims:
                if not (dim is None or isinstance(dim, Operator)):
                    raise ValueError(f"dims must be None or tuple of Operator: {type(dim)}")
        elif dims is not None:
            raise ValueError(f"dims must be None or tuple of Operator: {type(dims)}")
        self.dims = dims
        self.dims_raw = dims_raw
        self.reference = reference
        self.intent = intent
        self.ad_target = ad_target
        self.is_constant = is_constant
        self.allocatable = allocatable
        self.pointer = pointer
        self.optional = optional
        self.target = target
        self.save = save
        self.value = value
        self.volatile = volatile
        self.asynchronous = asynchronous
        self.declared_in = declared_in
        self.ref_var = ref_var
        if self.ad_target is None and self.var_type is not None:
            typename = self.var_type.typename.lower()
            if typename.startswith(("type", "class")):
                self.ad_type = self.var_type.is_real_type()
            else:
                is_deriv_type = (
                    typename.startswith("real")
                    or typename.startswith("double")
                    or typename.startswith("complex")
                )
                self.ad_target = is_deriv_type and not self.is_constant
        elif self.ad_target is None:
            self.ad_target = False

    @property
    def kind_val(self) -> Optional[str]:
        return (
            self.var_type.kind.val
            if self.var_type and self.var_type.kind is not None
            else None
        )

    def name_ext(self) -> str:
        name = self.name
        if self.ref_var is not None:
            name = f"{self.ref_var.name_ext()}%{name}"
        return name

    def is_same_var(self, other: OpVar) -> bool:
        if self.name != other.name:
            return False
        if self.ref_var is not None:
            if other.ref_var is None:
                return False
            return self.ref_var.is_same_var(other.ref_var)
        return True

    def get_dims(self) -> Optional[List[Optional[Operator]]]:
        if self.index is None and self.dims is None:
            return None
        if self.dims is None:
            ndims = len(self.index)
            if self.reduced_dims:
                ndims -= len(self.reduced_dims)
            return [None] * ndims
        if self.reduced_dims is None:
            return list(self.dims)
        return [dim for i, dim in enumerate(self.dims) if i not in self.reduced_dims]

    def ndims_ext(self) -> List[int]:
        ndims: List[int] = []
        if self.ref_var is not None:
            ndims.extend(self.ref_var.ndims_ext())
        if self.dims is not None or self.index is not None:
            ld = len(self.dims) if self.dims is not None else None
            li = len(self.index) if self.index is not None else None
            if ld is not None and li is not None:
                if ld != li:
                    raise RuntimeError(
                        f"dims and index is inconsistent: {self.name} {self.dims} {self.index}"
                    )
            lr = len(self.reduced_dims) if self.reduced_dims is not None else 0
            if ld is not None:
                ndims.append(ld - lr)
            else:
                ndims.append(li - lr)
        else:
            ndims.append(0)
        return ndims

    def get_index(self, ignore_dims: bool = False) -> Optional[AryIndex]:
        index = self.index
        if ignore_dims:
            if index is None:
                return None
        else:
            if index is None and self.dims is None:
                return None
            if index is None:
                index = AryIndex([None] * len(self.dims))
        if self.reduced_dims is None:
            return index
        return AryIndex(
            [idx for i, idx in enumerate(index) if i not in self.reduced_dims]
        )

    def concat_index(self, ignore_dims: bool = False) -> Optional[AryIndex]:
        index: List[Optional[Operator]] = []
        if self.ref_var is not None:
            index_ref = self.ref_var.concat_index()
            if index_ref is not None:
                index.extend(index_ref)
        index_self = self.get_index(ignore_dims)
        if index_self:
            for idx in index_self:
                index.append(idx)
        if index:
            return AryIndex(index)
        return None

    def concat_dims(self) -> Optional[List[Optional[Operator]]]:
        """Return concatenated dims along the ref_var chain.

        - For derived-type components (a%b%c), this concatenates the dims of
          the reference variables followed by the dims of the leaf var.
        - Mirrors concat_index semantics but for declaration dims (sizes), not
          runtime slice indices.
        """
        dims: List[Optional[Operator]] = []
        if self.ref_var is not None:
            ref_dims = self.ref_var.concat_dims()
            if ref_dims:
                dims.extend(ref_dims)
        self_dims = self.get_dims()
        if self_dims:
            dims.extend(self_dims)
        return dims if dims else None

    @property
    def is_real_type(self) -> bool:
        if self.var_type is None:
            return False
        typename = self.var_type.typename.lower()
        return typename.startswith("real") or typename.startswith("double")

    @property
    def is_complex_type(self) -> bool:
        if self.var_type is None:
            return False
        return self.var_type.typename.lower().startswith("complex")

    @property
    def is_integer_type(self) -> bool:
        if self.var_type is None:
            return False
        return self.var_type.typename.lower().startswith("integer")

    def is_array(self) -> bool:
        if self.dims is None and self.index is None:
            return False
        if self.index is None or any(
            idx is None or isinstance(idx, OpRange) or idx.is_array()
            for idx in self.index.dims
        ):
            return True
        return False

    def change_index(self, index: Optional[AryIndex]) -> OpVar:
        if (index is None and self.index is None) or (
            isinstance(index, AryIndex)
            and isinstance(self.index, AryIndex)
            and index == self.index
        ):
            return self
        return OpVar(
            name=self.name,
            index=index,
            var_type=self.var_type.copy() if self.var_type else None,
            dims=self.dims,
            dims_raw=self.dims_raw,
            reference=self.reference,
            intent=self.intent,
            ad_target=self.ad_target,
            is_constant=self.is_constant,
            allocatable=self.allocatable,
            pointer=self.pointer,
            optional=self.optional,
            target=self.target,
            save=self.save,
            value=self.value,
            volatile=self.volatile,
            asynchronous=self.asynchronous,
            declared_in=self.declared_in,
            ref_var=self.ref_var,
        )

    def replace_with(self, src: Operator, dest: Operator) -> "OpVar":
        if self == src:
            return dest
        index = self.index.replace_with(src, dest) if self.index is not None else None
        if index is not self.index:
            obj = self.deep_clone().change_index(index)
        else:
            obj = self
        if self.ref_var:
            ref_var = self.ref_var.replace_with(src, dest)
            if ref_var is not self.ref_var:
                if obj is not self:
                    obj = self.deep_clone()
                obj.ref_var = ref_var
        return obj

    def add_suffix(self, suffix: Optional[str] = None) -> "OpVar":
        if suffix is None:
            return self
        index = self.index
        name = f"{self.name}{suffix}"
        if self.ref_var is None:
            if index is not None:
                index = AryIndex(list(index.dims) if index.dims is not None else None)
            ref_var = None
        else:
            ref_var = self.ref_var.add_suffix(suffix)
        return OpVar(
            name,
            index=index,
            var_type=self.var_type.copy() if self.var_type else None,
            dims=self.dims,
            dims_raw=self.dims_raw,
            reference=self.reference,
            intent=self.intent,
            ad_target=self.ad_target,
            is_constant=self.is_constant,
            allocatable=self.allocatable,
            pointer=self.pointer,
            optional=self.optional,
            target=self.target,
            save=self.save,
            value=self.value,
            volatile=self.volatile,
            asynchronous=self.asynchronous,
            declared_in=self.declared_in,
            ref_var=ref_var,
        )

    def is_module_var(self, mod_var_names: List[str], check_ad: bool = False) -> bool:
        """Return ``True`` if this variable refers to a (possibly AD) module variable.

        The check walks to the root variable (ignoring components) and verifies
        that it was not declared within the current routine.  When ``check_ad``
        is ``True`` variables with the global ``AD_SUFFIX`` appended are also
        considered module variables."""

        root = self
        while root.ref_var is not None:
            root = root.ref_var
        name = root.name
        if check_ad and name.endswith(AD_SUFFIX):
            name = name[: -len(AD_SUFFIX)]
        return root.declared_in != "routine" and name in mod_var_names

    def remove_suffix(self, suffix: Optional[str] = None) -> "OpVar":
        if suffix is None:
            return self
        index = self.index
        name = self.name.removesuffix(suffix)
        if self.ref_var is None:
            if index is not None:
                index = AryIndex(list(index.dims) if index.dims is not None else None)
            ref_var = None
        else:
            ref_var = self.ref_var.remove_suffix(suffix)
        return OpVar(
            name,
            index=index,
            var_type=self.var_type.copy() if self.var_type else None,
            dims=self.dims,
            dims_raw=self.dims_raw,
            reference=self.reference,
            intent=self.intent,
            ad_target=self.ad_target,
            is_constant=self.is_constant,
            allocatable=self.allocatable,
            pointer=self.pointer,
            optional=self.optional,
            target=self.target,
            save=self.save,
            value=self.value,
            volatile=self.volatile,
            asynchronous=self.asynchronous,
            declared_in=self.declared_in,
            ref_var=ref_var,
        )

    def deep_clone(self) -> "OpVar":
        clone = copy.copy(self)
        if self.index is not None:
            clone.index = self.index.deep_clone()
        if self.dims is not None:
            clone.dims = tuple(
                d.deep_clone() if d is not None else None for d in self.dims
            )
        if self.dims_raw is not None:
            clone.dims_raw = tuple(self.dims_raw)
        return clone

    def collect_vars(
        self,
        without_index: bool = False,
        without_refvar: bool = False,
        without_checkfunc: bool = False,
    ) -> List["OpVar"]:
        vars = [self]
        if (not without_index) and self.index is not None:
            for i, idx in enumerate(self.index):
                if idx is not None and (
                    not self.reduced_dims or i not in self.reduced_dims
                ):
                    for var in idx.collect_vars():
                        if not var in vars:
                            vars.append(var)
        if not without_refvar and self.ref_var is not None:
            vars.extend(self.ref_var.collect_vars(without_index))
        return vars

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        if var == self:
            return OpInt(1, target=target)
        return OpInt(0, target=target)

    def is_partial_access(self) -> bool:
        if self.index is None:
            return False
        return self.get_index().is_partial_access()

    def index_list(self, without_refvar: bool = False) -> List[str]:
        if without_refvar:
            index = self.get_index(ignore_dims=True)
        else:
            index = self.concat_index(ignore_dims=True)
        if index is None:
            return []
        return index.list()

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        if self.ref_var is not None:
            ref_var = f"{self.ref_var}%"
        else:
            ref_var = ""
        index_str = ",".join(self.index_list(without_refvar=True))
        if not index_str:
            return f"{ref_var}{self.name}"
        else:
            return f"{ref_var}{self.name}({index_str})"

    def __eq__(self, other) -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        if self.name == other.name:
            if self.index == other.index:
                if self.ref_var == other.ref_var:
                    return True
        return False


@dataclass
class OpUnary(Operator):
    OP: ClassVar[str] = "undef"

    def __post_init__(self):
        super().__post_init__()
        if self.args is None or len(self.args) != 1:
            raise ValueError("length of args must 1")

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        a0 = self._paren(self.args[0])
        return f"{self.OP} {a0}"

    def is_array(self) -> bool:
        return self.args[0].is_array()


@dataclass
class OpNeg(OpUnary):
    OP: ClassVar[str] = "-"
    PRIORITY: ClassVar[int] = 4

    @classmethod
    def eval(cls, args: List[Operator]) -> Operator:
        return -args[0]

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        return -self.args[0].derivative(var, target, info, warnings)


@dataclass
class OpNot(OpUnary):
    OP: ClassVar[str] = ".not."
    PRIORITY: ClassVar[int] = 7


@dataclass
class OpBinary(Operator):
    OP: ClassVar[str] = "undef"

    def __post_init__(self):
        super().__post_init__()
        if self.args is None or len(self.args) != 2:
            raise ValueError("length of args must 2")

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        a0 = self._paren(self.args[0])
        eq = (
            isinstance(self.args[1], OpNeg)
            or (isinstance(self, OpDiv) and isinstance(self.args[1], (OpMul, OpDiv)))
            or (isinstance(self, OpSub) and isinstance(self.args[1], (OpAdd, OpSub)))
        )
        a1 = self._paren(self.args[1], eq=eq)
        return f"{a0} {self.OP} {a1}"

    def is_array(self) -> bool:
        return self.args[0].is_array() or self.args[1].is_array()


@dataclass
class OpAdd(OpBinary):
    OP: ClassVar[str] = "+"
    PRIORITY: ClassVar[int] = 5

    @classmethod
    def eval(cls, args: List[Operator]) -> Operator:
        return args[0] + args[1]

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        return self.args[0].derivative(var, target, info, warnings) + self.args[
            1
        ].derivative(var, target, info, warnings)


@dataclass
class OpSub(OpBinary):
    OP: ClassVar[str] = "-"
    PRIORITY: ClassVar[int] = 5

    @classmethod
    def eval(self, args: List[Operator]) -> Operator:
        return args[0] - args[1]

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        return self.args[0].derivative(var, target, info, warnings) - self.args[
            1
        ].derivative(var, target, info, warnings)


@dataclass
class OpMul(OpBinary):
    OP: ClassVar[str] = "*"
    PRIORITY: ClassVar[int] = 4

    @classmethod
    def eval(cls, args: List[Operator]) -> Operator:
        return args[0] * args[1]

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        arg0 = self.args[0]
        arg1 = self.args[1]
        arg0_dev = arg0.derivative(var, target, info, warnings)
        arg1_dev = arg1.derivative(var, target, info, warnings)
        return arg0_dev * arg1 + arg0 * arg1_dev


@dataclass
class OpDiv(OpBinary):
    OP: ClassVar[str] = "/"
    PRIORITY: ClassVar[int] = 4

    @classmethod
    def eval(cls, args: List[Operator]) -> Operator:
        return args[0] / args[1]

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        arg0 = self.args[0]
        arg1 = self.args[1]
        arg0_dev = arg0.derivative(var, target, info, warnings)
        if isinstance(arg1, OpNum):
            return arg0_dev / arg1
        arg1_dev = arg1.derivative(var, target, info, warnings)
        if isinstance(arg1_dev, OpInt) and arg1_dev.val == 0:
            return arg0_dev / arg1
        return (arg0_dev * arg1 - arg0 * arg1_dev) / arg1**2


@dataclass
class OpPow(OpBinary):
    OP: ClassVar[str] = "**"
    PRIORITY: ClassVar[int] = 2

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        # remove target and kind
        if isinstance(self.args[1], OpInt):
            a1 = OpInt(val=self.args[1].val)
        if isinstance(self.args[1], OpNeg) and isinstance(self.args[1].args[0], OpInt):
            a1 = OpInt(val=self.args[1].args[0].val)
        a0 = self._paren(self.args[0])
        a1 = self._paren(self.args[1])
        if a1 == "1":
            return a0
        else:
            return f"{a0}**{a1}"

    @classmethod
    def eval(cls, args: List[Operator]) -> Operator:
        return args[0] ** args[1]

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        base = self.args[0]
        expo = self.args[1]
        dbase = base.derivative(var, target, info, warnings)
        dexpo = expo.derivative(var, target, info, warnings)
        if isinstance(expo, OpVar) and expo.is_real_type:
            one = OpReal("1.0", kind=expo.kind)
        else:
            one = OpInt(1)
        if isinstance(expo, OpInt) and expo.target is None:
            expo2 = OpInt(expo.val, target=target)
        else:
            expo2 = expo
        return (
            expo2 * base ** (expo - one) * dbase
            + base**expo * OpFunc("log", args=[base]) * dexpo
        )


@dataclass
class OpLogic(OpBinary):
    """Logical operations (.and., .or., .eq., .gt., .ge., .lt., and .le.)."""

    op: str = field(default="")
    PRIORITY: ClassVar[int] = 6

    def __init__(self, op: str, args: List[Operator]):
        super().__init__(args=args, var_type=VarType("logical"))
        if not op:
            raise ValueError("op should not be empty")
        self.op = op

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        a0 = self._paren(self.args[0])
        eq = isinstance(self.args[1], OpNeg)
        a1 = self._paren(self.args[1], eq=eq)
        return f"{a0} {self.op} {a1}"

    def __and__(self, other):
        return OpLogic(".and.", args=[self, other])

    def __or__(self, other):
        return OpLogic(".or.", args=[self, other])


INTRINSIC_FUNCTIONS = {
    "abs": VarType("real"),
    "sqrt": VarType("real"),
    "exp": VarType("real"),
    "log": VarType("real"),
    "log10": VarType("real"),
    "sin": VarType("real"),
    "cos": VarType("real"),
    "tan": VarType("real"),
    "asin": VarType("real"),
    "acos": VarType("real"),
    "atan": VarType("real"),
    "sinh": VarType("real"),
    "cosh": VarType("real"),
    "tanh": VarType("real"),
    "asinh": VarType("real"),
    "acosh": VarType("real"),
    "atanh": VarType("real"),
    "erf": VarType("real"),
    "erfc": VarType("real"),
    "real": VarType("real"),
    "dble": VarType("real"),
    "aimag": VarType("real"),
    "conjg": VarType("complex"),
    "cmplx": VarType("complex"),
    "atan2": VarType("real"),
    "transpose": VarType("unknown"),
    "cshift": VarType("unknown"),
    "dot_product": VarType("real"),
    "matmul": VarType("unknown"),
}

ARG_TYPE_INTRINSICS = {
    "mod",
    "min",
    "max",
    "sign",
    "sum",
    "product",
    "minval",
    "maxval",
}

NONDIFF_INTRINSICS = {
    "len": VarType("integer"),
    "len_trim": VarType("integer"),
    "adjustl": VarType("character"),
    "index": VarType("integer"),
    "lbound": VarType("integer"),
    "ubound": VarType("integer"),
    "size": VarType("integer"),
    "epsilon": VarType("real"),
    "huge": VarType("real"),
    "tiny": VarType("real"),
    "ichar": VarType("integer"),
    "achar": VarType("character"),
    "int": VarType("integer"),
    "nint": VarType("integer"),
    "allocated": VarType("logical"),
    "all": VarType("logical"),
    "any": VarType("logical"),
    "command_argument_count": VarType("integer"),
    "count": VarType("integer"),
    "maxloc": VarType("integer"),
    "minloc": VarType("integer"),
}


@dataclass
class OpFunc(Operator):
    name: str = field(default="")
    PRIORITY: ClassVar[int] = 1

    def __init__(
        self, name: str, args: List[Operator], var_type: Optional[VarType] = None
    ):
        super().__init__(args=args, var_type=var_type)
        if not name:
            raise ValueError("name should not be empty")
        if self.args is None:
            self.args = []
        self.name = name

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        args = []
        for arg in self.args:
            args.append(f"{arg}")
        args = ", ".join(args)
        return f"{self.name}({args})"

    def is_array(self) -> bool:
        return any([arg.is_array() for arg in self.args])

    def collect_vars(
        self,
        without_index: bool = False,
        without_refvar: bool = False,
        without_checkfunc: bool = False,
    ) -> List[OpVar]:
        vars = []
        if without_checkfunc and self.name in ("allocated", "associated"):
            return vars
        if self.args is None:
            return vars
        for arg in self.args:
            for var in arg.collect_vars(
                without_index, without_refvar, without_checkfunc
            ):
                if var not in vars:
                    vars.append(var)
        return vars

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        if self.name in NONDIFF_INTRINSICS:
            return OpInt(0, target=target)

        kind = target.kind if target is not None else None

        def _pi(target):
            return OpFunc(
                "sqrt", args=[OpFunc("acos", args=[-OpReal(1.0, kind=target.kind)])]
            )

        if len(self.args) == 0:
            raise ValueError(f"Function ({self.name}) is not supported")

        # One argument intrinsics map
        arg0 = self.args[0]
        dvar0 = arg0.derivative(var, target, info, warnings)
        zero = OpReal("0.0", kind=kind)
        one = OpReal("1.0", kind=kind)

        if self.name == "abs":
            return dvar0 * OpFunc("sign", args=[OpInt(1, target=var), arg0])
        if self.name == "sqrt":
            return dvar0 / (OpInt(2, target=target) * OpFunc("sqrt", args=[arg0]))
        if self.name == "exp":
            return dvar0 * OpFunc("exp", args=[arg0])
        if self.name == "log":
            return dvar0 / arg0
        if self.name == "log10":
            return dvar0 / (arg0 * OpFunc("log", args=[OpReal("10.0", kind=kind)]))
        if self.name == "sin":
            return dvar0 * OpFunc("cos", args=[arg0])
        if self.name == "cos":
            return -dvar0 * OpFunc("sin", args=[arg0])
        if self.name == "tan":
            return dvar0 / OpFunc("cos", args=[arg0]) ** 2
        if self.name == "asin":
            return dvar0 / OpFunc("sqrt", args=[OpReal(1.0, kind=kind) - arg0**2])
        if self.name == "acos":
            return -dvar0 / OpFunc("sqrt", args=[OpReal(1.0, kind=kind) - arg0**2])
        if self.name == "atan":
            return dvar0 / (OpReal(1.0, kind=kind) + arg0**2)
        if self.name == "sinh":
            return dvar0 * OpFunc("cosh", args=[arg0])
        if self.name == "cosh":
            return dvar0 * OpFunc("sinh", args=[arg0])
        if self.name == "tanh":
            return dvar0 / OpFunc("cosh", args=[arg0]) ** 2
        if self.name == "asinh":
            return dvar0 / OpFunc("sqrt", args=[arg0**2 + OpReal(1.0, kind=kind)])
        if self.name == "acosh":
            return dvar0 / (
                OpFunc("sqrt", args=[arg0 - one]) * OpFunc("sqrt", args=[arg0 + one])
            )
        if self.name == "atanh":
            return dvar0 / (OpReal(1.0, kind=kind) - arg0**2)
        if self.name == "erf":
            return (
                dvar0
                * OpInt(2, target=target)
                / _pi(target)
                * OpFunc("exp", args=[-(arg0**2)])
            )
        if self.name == "erfc":
            return (
                -dvar0
                * OpInt(2, target=target)
                / _pi(target)
                * OpFunc("exp", args=[-(arg0**2)])
            )
        if self.name == "real":
            return dvar0
        if self.name == "dble":
            return dvar0
        if self.name == "aimag":
            return OpFunc("aimag", args=[dvar0])
        if self.name == "conjg":
            return OpFunc("conjg", args=[dvar0])
        if self.name == "cmplx":
            if len(self.args) == 1:
                dvar1 = OpReal("0.0", kind=kind)
            else:
                dvar1 = self.args[1].derivative(var, target, info, warnings)
            return OpFunc("cmplx", args=[dvar0, dvar1])
        if self.name == "sum":
            return dvar0
        if self.name == "minval":
            return dvar0 * OpFunc(
                "merge", args=[one, zero, OpLogic("==", args=[arg0, self])]
            )
        if self.name == "maxval":
            return dvar0 * OpFunc(
                "merge", args=[one, zero, OpLogic("==", args=[arg0, self])]
            )

        if len(self.args) < 2:
            raise ValueError(f"Function ({self.name}) is not supported")

        # Two argument intrinsics map
        arg1 = self.args[1]
        dvar1 = arg1.derivative(var, target, info, warnings)

        if self.name == "mod":
            return dvar0 - dvar1 * OpFunc(
                "real",
                args=[OpFunc("int", args=[arg0 / arg1]), OpFunc("kind", args=[arg0])],
            )
        if self.name == "min":
            cond = arg0 >= arg1
            return dvar0 * OpFunc("merge", args=[one, zero, cond]) + dvar1 * OpFunc(
                "merge", args=[zero, one, cond]
            )
        if self.name == "max":
            cond = arg0 <= arg1
            return dvar0 * OpFunc("merge", args=[one, zero, cond]) + dvar1 * OpFunc(
                "merge", args=[zero, one, cond]
            )
        if self.name == "sign":
            return (
                dvar0
                * OpFunc("sign", args=[one, arg0])
                * OpFunc("sign", args=[one, arg1])
            )
        if self.name == "atan2":
            return (dvar0 * arg1 - dvar1 * arg0) / (arg0**2 + arg1**2)
        if self.name == "dot_product":
            return arg1 * dvar0 + arg0 * dvar1
        if self.name == "matmul":
            term0 = OpFunc("matmul", args=[dvar0, arg1])
            term1 = OpFunc("matmul", args=[arg0, dvar1])
            if isinstance(dvar0, OpInt) and dvar0.val == 0:
                term0 = OpInt(0, target=target)
            if isinstance(dvar1, OpInt) and dvar1.val == 0:
                term1 = OpInt(0, target=target)
            return term0 + term1

        raise ValueError(f"Function ({self.name}) is not supported")

    def special_handler(self, dsc, args, suffix, reverse: bool):
        if self.name == "transpose":
            """Propagate gradient through ``transpose``."""
            if reverse:
                src = dsc
            else:
                src = args[0]
            return OpFunc(name="transpose", args=[src.add_suffix(suffix)])
        if self.name == "cshift":
            """Propagate gradient through ``cshift``."""
            if reverse:
                src = dsc
            else:
                src = args[0]
            shift = -args[1]
            dim = args[2]
            return OpFunc(name="cshift", args=[src.add_suffix(suffix), shift, dim])
        return None


@dataclass
class OpFuncUser(Operator):
    """A user-defined function in the form of `name(args)`, where `args` is a list of Operators."""

    name: str = field(default="")
    intents: Optional[List[str]] = field(default=None)
    PRIORITY: ClassVar[int] = 1

    def __init__(
        self,
        name: str,
        args: List[Operator],
        intents: Optional[List[str]] = None,
        var_type: Optional[VarType] = None,
    ):
        super().__init__(args=args, var_type=var_type)
        if not name:
            raise ValueError("name should not be empty")
        self.name = name
        self.intents = intents
        if self.args is None:
            self.args = []
        else:
            for i, arg in enumerate(self.args):
                if not isinstance(arg, Operator):
                    raise TypeError(f"args[{i}] must be an Operator: {type(arg)}")

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        args = []
        for arg in self.args:
            args.append(f"{arg}")
        args = ", ".join(args)
        return f"{self.name}({args})"

    def find_userfunc(self) -> List[OpFuncUser]:
        funcs = []
        for arg in self.args:
            funcs.extend(arg.find_userfunc())
        funcs.append(self)
        return funcs

    def derivative(
        self,
        var: OpVar,
        target: Optional[OpVar] = None,
        info: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> Operator:
        raise NotImplementedError


@dataclass
class OpRange(Operator):
    def __post_init__(self):
        if self.args is None:
            self.args = []
        if len(self.args) > 3:
            raise ValueError(f"Length of args must be at most 3: {self.args}")
        for i, val in enumerate(self.args):
            if isinstance(val, int):
                self.args[i] = OpInt(val)
                continue
            if isinstance(val, OpRange):
                raise ValueError("Dimension of OpRange must not be OpRange")
            if not (isinstance(val, Operator) or val is None):
                raise ValueError(f"Dimension must be Operator: {val} {type(val)}")

    def replace_with(self, src: Operator, dest: Operator) -> "OpRange":
        dims = []
        updated = False
        if isinstance(dest, OpRange):
            range_found = False
            for dim in self.args:
                if dim is not None and src in dim.collect_vars():
                    range_found = True
                    break
                dims.append(dim)
            if range_found:
                i0, i1, i2 = self
                j0, j1, j2 = dest
                if i2 is not None and isinstance(i2, OpNeg):
                    if j2 is not None and not isinstance(j2, OpNeg):
                        j0, j1 = j1, j0
                else:
                    if j2 is not None and isinstance(j2, OpNeg):
                        j0, j1 = j1, j0
                i0 = i0.replace_with(src, j0)
                i1 = i1.replace_with(src, j1)
                return OpRange([i0, i1, i2])
        else:
            for dim in self.args:
                dims.append(dim.replace_with(src, dest))
                if dims[-1] is not dim:
                    updated = True
        if updated:
            return OpRange(dims)
        else:
            return self

    def collect_vars(
        self,
        without_index: bool = False,
        without_refvar: bool = False,
        without_checkfunc: bool = False,
    ) -> List[OpVar]:
        """Collect variables from the range operator."""
        vars: List[OpVar] = []
        if not without_index:
            for arg in self.args:
                if arg is not None:
                    for v in arg.collect_vars():
                        if not v in vars:
                            vars.append(v)
        return vars

    def reverse(self) -> "OpRange":
        if len(self.args) == 0:
            args = []
        if len(self.args) == 1:
            args = [None, self.args[0], -1]
        if len(self.args) == 2:
            args = [self.args[1], self.args[0], -1]
        if len(self.args) == 3:
            if self.args[2] == -OpInt(1):
                args = [self.args[1], self.args[0]]
            elif self.args[2] is None:
                args = [self.args[1], self.args[0], -OpInt(1)]
            else:
                args = [self.args[1], self.args[0], -self.args[2]]
        return OpRange(args)

    def ascending(self) -> "OpRange":
        if len(self.args) < 3:
            return self
        start, end, inc = self.args
        if inc is None:
            return self
        if isinstance(inc, OpInt):
            return self
        if isinstance(inc, OpNeg) and isinstance(inc.args[0], OpInt):
            return self.reverse()
        diff = (
            AryIndex._get_int(end - start)
            if end is not None and start is not None
            else None
        )
        if diff is not None:
            if diff < 0:
                return self.reverse()
            else:
                return self
        # cannot judge, so gessing
        if isinstance(inc, OpNeg):
            return self.reverse()
        return self

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        if len(self.args) == 0:
            args = [None, None]
        if len(self.args) == 1:
            args = [self.args[0], None]
        if len(self.args) == 2:
            args = self.args
        if len(self.args) == 3:
            if self.args[2] is None:
                args = self.args[0:2]
            else:
                args = self.args
        return ":".join(["" if arg is None else str(arg) for arg in args])

    def __repr__(self) -> str:
        return self.__str__()

    def __getitem__(self, index: int) -> Operator:
        if index >= 3:
            raise IndexError("index out of range")
        if len(self.args) <= index:
            return None
        return self.args[index]

    def __eq__(self, other) -> bool:
        if not isinstance(other, Operator):
            return NotImplemented
        return str(self) == str(other)

    def __contains__(self, other) -> bool:
        if other is None:
            return True
        i0 = self[0]
        i1 = self[1]
        if isinstance(self[2], OpNeg):
            i0, i1 = i1, i0
        if isinstance(other, OpRange):
            j0 = other[0]
            j1 = other[1]
            if isinstance(other[2], OpNeg):
                j0, j1 = j1, j0
            if i0 is not None and j1 is not None:
                d = AryIndex._get_int(i0 - j1)
                if d is not None and d > 0:
                    return False
            if i1 is not None and j0 is not None:
                d = AryIndex._get_int(j0 - i1)
                if d is not None and d > 0:
                    return False
            return True
        if i0 is not None:
            v = i0 - other
            if isinstance(v, OpInt) and v.val > 0:
                return False
        if i1 is not None:
            v = other - i1
            if isinstance(v, OpInt) and v.val > 0:
                return False
        return True

    def strict_in(self, other) -> bool:
        if other is None:
            return False
        if isinstance(other, OpRange):
            raise NotImplementedError
        i0, i1, i2 = self
        if i0 == other:
            return True
        if i1 == other and (i2 is None or AryIndex._get_int(i2) == 1):
            return True
        i0, i1, _ = self.ascending()
        if i1 is not None:
            v = i1 - other
            if not (isinstance(v, OpInt) and v.val >= 0):
                return False
        if i0 is not None:
            v = other - i0
            if not (isinstance(v, OpInt) and v.val >= 0):
                return False
        return True


@dataclass
class OpType(Operator):
    name: str = field(default="")
    PRIORITY: ClassVar[int] = 1

    def __init__(self, name: str):
        self.name = name
        self.args = None

    def __str__(self) -> str:
        if self.macro_name:
            return self.macro_name
        return self.name
