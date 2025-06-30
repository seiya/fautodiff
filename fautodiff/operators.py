"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
from fractions import Fraction
import re
import copy


@dataclass
class AryIndex:
    """Class to represent index of fortran array"""

    dims: List["Operator"] = field(default_factory=list)

    def __post_init__(self):
        if not (isinstance(self.dims, list) or isinstance(self.dims, tuple)):
            raise ValueError(f"dims must be either list or tuple: {type(self.dims)}")
        if isinstance(self.dims, tuple):
            self.dims = list(self.dims)
        for i, dim in enumerate(self.dims):
            if isinstance(dim, int):
                self.dims[i] = OpInt(dim)
                continue
            if dim is None:
                self.dims[i] = None
                continue
            if not isinstance(dim, Operator):
                raise ValueError("dim must be either None, int, or Operator")

    def list(self) -> List[str]:
        return [(":" if dim is None else str(dim)) for dim in self.dims]

    def copy(self) -> AryIndex:
        return AryIndex(list(self.dims))

    def __len__(self) -> int:
        return len(self.dims)

    def __getitem__(self, index: int) -> "Operator":
        return self.dims[index]

    def __setitem__(self, index: int, var: Operator) -> None:
        self.dims[index] = var

    def __iter__(self) -> iter:
        return iter(self.dims)

    def __str__(self) -> str:
        return",".join(self.list())

    def __eq__(self, other) -> bool:
        if other is not None and not isinstance(other, AryIndex):
            return NotImplemented
        if other is None:
            return not self.is_partial_access()
        if len(self.dims) != len(other.dims):
            raise ValueError("Different number of dimensions")
        for i, dim1 in enumerate(self.dims):
            dim2 = other.dims[i]
            if dim1 == dim2:
                continue
            if (dim1 is None or isinstance(dim1, OpRange)) and (dim2 is None or isinstance(dim2, OpRange)):
                if isinstance(dim1, OpRange) and isinstance(dim2, OpRange):
                    return False
                continue
            return False
        return True

    @classmethod
    def dim_is_entire(cls, dim) -> bool:
        if dim is None:
            return True
        if isinstance(dim, OpRange):
            if dim[0] is None and dim[1] is None:
                return True
        return False

    @classmethod
    def get_diff_dim(cls, index1, index2) -> int:
        if len(index1.dims) != len(index2.dims):
            raise ValueError("Different number of dimensions")

        def _check_found():
            if diff_dim > 0:
                return -1 # not allow difference in multiple dimensions

        diff_dim = -1 # dimension at which difference was found
        for i, dim1 in enumerate(index1):
            dim2 = index2[i]
            if dim1 == dim2:
                continue
            if AryIndex.dim_is_entire(dim1) and AryIndex.dim_is_entire(dim2):
                continue
            _check_found()
            diff_dim = i

        return diff_dim

    @staticmethod
    def _check_cover(index1, index2) -> bool:
        """Return true if index1 >= index2"""
        if len(index1.dims) != len(index2.dims):
            raise ValueError("Different number of dimensions")
        if index1 == index2:
            return True
        for i, dim1 in enumerate(index1):
            dim2 = index2.dims[i]
            if dim1 == dim2:
                continue
            if AryIndex.dim_is_entire(dim1):
                continue
            if isinstance(dim1, OpRange) and (isinstance(dim1[0], OpVar) or isinstance(dim1[1], OpVar)):
                # This is not sure but assumue that dim1 covers entire region.
                continue
            if dim2 is None:
                return False
            if isinstance(dim1, OpVar) and isinstance(dim2, OpVar):
                # This is not sure but different variables can be identical.
                continue
            if not isinstance(dim1, OpRange):
                return False
            if not (isinstance(dim2, OpInt) or isinstance(dim2, OpRange)):
                continue
            i0 = dim1[0]
            i1 = dim1[1]
            if i0 is None and i1 is None:
                continue
            if not ((i0 is None or isinstance(i0, OpInt)) and (i1 is None or isinstance(i1, OpInt))):
                continue
            if not (dim1[2] is None or (isinstance(dim1[2], OpInt) and abs(dim1[2].val)==1)):
                continue
            if dim1[2] is not None and dim1[2].val < 0:
                i0, i1 = i1, i0
            if isinstance(dim2, OpInt):
                j = dim2.val
                if i0 is None and i1.val >= j:
                    continue
                if i1 is None and i0.val <= j:
                    continue
                i0 = i0.val
                i1 = i1.val
                if j < i0 or j > i1:
                    return False
                continue # i0 <= j and j <= i1
            if isinstance(dim2, OpRange):
                j0 = dim2[0]
                j1 = dim2[1]
                if j0 is None and j1 is None:
                    return False
                if not ((j0 is None or isinstance(j0, OpInt)) and (j1 is None or isinstance(j1, OpInt))):
                    return False
                if not (dim2[2] is None or (isinstance(dim2[2], OpInt) and abs(dim2[2].val)==1)):
                    return False
                if dim2[2] is not None and dim2[2].val < 0:
                    j0, j1 = j1, j0
                if (j0 is None and i0 is not None) or (isinstance(i0, OpInt) and j0.val < i0.val):
                    return False
                if (j1 is None and i1 is not None) or (isinstance(i1, OpInt) and i1.val < j1.val):
                    return False
                continue
            return False
        return True

    def __le__(self, other) -> bool:
        if other is not None and not isinstance(other, AryIndex):
            raise NotImplemented
        if other is None:
            return True
        return AryIndex._check_cover(other, self)

    def __ge__(self, other) -> bool:
        if other is not None and not isinstance(other, AryIndex):
            raise NotImplemented
        if other is None:
            return all([dim is None or isinstance(dim, OpRange) for dim in self.dims])
        return AryIndex._check_cover(self, other)

    def collect_vars(self) -> List[OpVar]:
        if self.dims is None:
            return []
        vars = []
        for dim in self.dims:
            if dim is None:
                continue
            for var in dim.collect_vars():
                if not var in vars:
                    vars.append(var)
        return vars

    def is_partial_access(self) -> bool:
        return any([(dim is not None and not isinstance(dim, OpRange)) for dim in self.dims])

    def is_depended_on(self, var:OpVar) -> bool:
        for dim in self.dims:
            if dim is None:
                continue
            if var in dim.collect_vars():
                return True
        return False


@dataclass
class Operator:
    """Abstract fortran oprations."""

    args: Optional[List[Operator]] = None
    PRIORITY: ClassVar[int] = -999

    def __post_init__(self):
        return None

    def _paren(self, arg: Operator, eq: bool = False) -> str:
        if self.PRIORITY < arg.PRIORITY or (eq and self.PRIORITY == arg.PRIORITY):
            return f"({arg})"
        else:
            return f"{arg}"

    def deep_clone(self) -> "Node":
        clone = copy.deepcopy(self)
        return clone

    def collect_vars(self, without_index: bool = False) -> List[OpVar]:
        vars = []
        for arg in self.args:
            for var in arg.collect_vars(without_index):
                if var not in vars:
                    vars.append(var)
        return vars

    def find_userfunc(self) -> List[OpFuncUser]:
        funcs = []
        for arg in self.args:
            funcs.extend(arg.find_userfunc())
        return funcs

    def replace_with(self, src: Operator, dest: Operator) -> Operator:
        if self is src:
            return dest
        args_new = []
        for arg in self.args:
            if arg is src:
                args_new.append(dest)
            elif isinstance(arg, Operator):
                args_new.append(arg.replace_with(src, dest))
            else:
                args_new.append(arg)
        self.args = args_new
        return self

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        """Return the derivative operation with respetive to ```var```."""
        raise NotImplementedError(f"derivative in {type(self)}")

    def __neg__(self):
        if isinstance(self, OpNeg):
            return self.args[0]
        if isinstance(self, OpInt) and self.val == 0:
            return self
        return OpNeg(args=[self])

    def __add__(self, other):
        if isinstance(other, int):
            return self + OpInt(other)
        if isinstance(other, Operator):
            if isinstance(self, OpInt) and self.val == 0:
                return other
            if isinstance(other, OpInt) and other.val == 0:
                return self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                return OpInt(self.val + other.val, target=self.target, kind=self.kind)
            if isinstance(self, OpReal) and isinstance(other, OpReal) and self.kind==other.kind and self.val==f"{float(self.val)}" and other.val==f"{float(other.val)}":
                return OpReal(val=f"{float(self.val) + float(other.val)}", kind=self.kind)
            if isinstance(self, OpReal) and isinstance(other, OpInt) and self.val==f"{float(self.val)}":
                return OpReal(val=f"{float(self.val) + other.val}", kind=self.kind)
            if isinstance(other, OpReal) and isinstance(self, OpInt) and other.val==f"{float(other.val)}":
                return OpReal(val=f"{float(other.val) + self.val}", kind=other.kind)
            if isinstance(self, OpAdd) and isinstance(self.args[0], OpNum) and isinstance(other, OpNum):
                return self.args[1] + (self.args[0] + other)
            if isinstance(self, OpAdd) and isinstance(self.args[1], OpNum) and isinstance(other, OpNum):
                return self.args[0] + (self.args[1] + other)
            if isinstance(other, OpAdd) and isinstance(other.args[0], OpNum) and isinstance(self, OpNum):
                return other.args[1] + (other.args[0] + self)
            if isinstance(other, OpAdd) and isinstance(other.args[1], OpNum) and isinstance(self, OpNum):
                return other.args[0] + (other.args[1] + self)
            if isinstance(self, OpSub) and isinstance(self.args[0], OpNum) and isinstance(other, OpNum):
                return - self.args[1] + (self.args[0] + other)
            if isinstance(self, OpSub) and isinstance(self.args[1], OpNum) and isinstance(other, OpNum):
                return self.args[0] + (other - self.args[1])
            if isinstance(other, OpSub) and isinstance(other.args[0], OpNum) and isinstance(self, OpNum):
                return - other.args[1] + (other.args[0] + self)
            if isinstance(other, OpSub) and isinstance(other.args[1], OpNum) and isinstance(self, OpNum):
                return other.args[0] + (self - other.args[1])
            if isinstance(other, OpNeg):
                return self - other.args[0]
            return OpAdd(args=[self, other])
        return NotImplemented

    def __sub__(self, other):
        if isinstance(other, int):
            return self - OpInt(other)
        if isinstance(other, Operator):
            if isinstance(self, OpInt) and self.val == 0:
                return - other
            if isinstance(other, OpInt) and other.val == 0:
                return self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                val = self.val - other.val
                if val < 0:
                    return - OpInt(-val, target=self.target, kind=self.kind)
                else:
                    return OpInt(val, target=self.target, kind=self.kind)
            if isinstance(self, OpReal) and isinstance(other, OpReal) and self.kind==other.kind and self.val==f"{float(self.val)}" and other.val==f"{float(other.val)}":
                return OpReal(val=f"{float(self.val) - float(other.val)}", kind=self.kind)
            if isinstance(self, OpReal) and isinstance(other, OpInt) and self.val==f"{float(self.val)}":
                return OpReal(val=f"{float(self.val) - other.val}", kind=self.kind)
            if isinstance(other, OpReal) and isinstance(self, OpInt) and other.val==f"{float(other.val)}":
                return OpReal(val=f"{float(other.val) - self.val}", kind=other.kind)
            if isinstance(self, OpAdd) and isinstance(self.args[0], OpNum) and isinstance(other, OpNum):
                return self.args[1] + (self.args[0] - other)
            if isinstance(self, OpAdd) and isinstance(self.args[1], OpNum) and isinstance(other, OpNum):
                return self.args[0] + (self.args[1] - other)
            if isinstance(other, OpAdd) and isinstance(other.args[0], OpNum) and isinstance(self, OpNum):
                return - other.args[1] + (self - other.args[0])
            if isinstance(other, OpAdd) and isinstance(other.args[1], OpNum) and isinstance(self, OpNum):
                return - other.args[0] + (self - other.args[1])
            if isinstance(self, OpSub) and isinstance(self.args[0], OpNum) and isinstance(other, OpNum):
                return - self.args[1] + (self.args[0] - other)
            if isinstance(self, OpSub) and isinstance(self.args[1], OpNum) and isinstance(other, OpNum):
                return self.args[0] - (self.args[1] + other)
            if isinstance(other, OpSub) and isinstance(other.args[0], OpNum) and isinstance(self, OpNum):
                return other.args[1] + (self - other.args[0])
            if isinstance(other, OpSub) and isinstance(other.args[1], OpNum) and isinstance(self, OpNum):
                return - other.args[0] + (self + other.args[1])
            if isinstance(other, OpNeg):
                return self + other.args[0]
            return OpSub(args=[self, other])
        return NotImplemented

    def __mul__(self, other):
        if isinstance(other, int):
            return other * OpInt(other)
        if isinstance(other, Operator):
            if isinstance(self, OpInt) and self.val == 1:
                return other
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(self, OpInt) and self.val == 0:
                return OpInt(0, target=self.target, kind=self.kind)
            if isinstance(other, OpInt) and other.val == 0:
                return OpInt(0, target=other.target, kind=other.kind)
            if isinstance(self, OpInt) and self.val == -1:
                return - other
            if isinstance(other, OpInt) and other.val == -1:
                return - self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                return OpInt(self.val * other.val, target=self.target, kind=self.kind)
            if isinstance(self, OpReal) and isinstance(other, OpReal) and self.kind==other.kind and self.val==str(float(self.val)) and other.val==str(float(other.val)):
                return OpReal(val=str(float(self.val) * float(other.val)), kind=self.kind)
            if isinstance(self, OpReal) and isinstance(other, OpInt) and self.val==str(float(self.val)):
                if float(self.val) == float(int(self.val)):
                    return OpInt(int(self.val) * other.val, target=other.target, kind=other.kind)
                else:
                    return OpReal(val=str(float(self.val) * other.val), kind=self.kind)
            if isinstance(other, OpReal) and isinstance(self, OpInt) and other.val==str(float(other.val)):
                if float(other.val) == float(int(other.val)):
                    return OpInt(int(other.val) * self.val, target=self.target, kind=self.kind)
                else:
                    return OpReal(val=str(float(other.val) * self.val), kind=other.kind)
            if isinstance(self, OpNeg):
                return - (self.args[0] * other)
            if isinstance(other, OpNeg):
                return - (self * other.args[0])
            if isinstance(other, OpMul) and isinstance(self, OpNum) and isinstance(other.args[0], OpNum):
                return (self * other.args[0]) * other.args[1]
            if isinstance(other, OpMul) and isinstance(self, OpNum) and isinstance(other.args[1], OpNum):
                return (self * other.args[1]) * other.args[0]
            if isinstance(other, OpDiv) and isinstance(other.args[0], OpInt) and other.args[0].val == 1:
                return self / other.args[1]
            if isinstance(other, OpPow) and isinstance(other.args[1], OpNeg):
                return self / other.args[0]**(other.args[1].args[0])
            if isinstance(self, OpPow) and self.args[0] == other:
                expo = self.args[1]
                if isinstance(expo, OpVar) and expo.is_real:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (self.args[0])**(self.args[1] + one)
            if isinstance(other, OpPow) and other.args[0] == self:
                expo = other.args[1]
                if isinstance(expo, OpVar) and expo.is_real:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (other.args[0])**(one + other.args[1])
            return OpMul(args=[self, other])
        return NotImplemented

    def __truediv__(self, other):
        if isinstance(other, int):
            return other / OpInt(other)
        if isinstance(other, Operator):
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(self, OpInt) and self.val == 0:
                return self
            if isinstance(self, OpNeg):
                return - (self.args[0] / other)
            if isinstance(other, OpNeg):
                return - (self / other.args[0])
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
                return self * other.args[0]**(other.args[1].args[0])
            if isinstance(self, OpPow) and self.args[0] == other:
                expo = self.args[1]
                if isinstance(expo, OpVar) and expo.is_real:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (self.args[0])**(self.args[1] - one)
            if isinstance(other, OpPow) and other.args[0] == self:
                expo = other.args[1]
                if isinstance(expo, OpVar) and expo.is_real:
                    one = OpReal("1.0", kind=expo.kind)
                else:
                    one = OpInt(1)
                return (other.args[0])**(one - other.args[1])
            return OpDiv(args=[self, other])
        return NotImplemented

    def __pow__(self, other):
        if isinstance(other, int):
            return OpPow(args=[self, OpInt(other)])
        if isinstance(other, Operator):
            if isinstance(other, OpInt) and other.val == 0:
                return OpInt(1, target=other.target, kind=other.kind)
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(other, OpNeg) and isinstance(other.args[0], OpInt):
                return OpInt(1, target=other.args[0].target, kind=other.args[0].kind) / self**other.args[0]
            if isinstance(self, OpInt) and isinstance(other, OpInt) and isinstance(other.val, int):
                return OpInt(self.val**(other.val), target=self.target, kind=self.kind)
            if isinstance(self, OpReal) and isinstance(other, OpInt) and self.val == f"{float(self.val)}":
                return OpReal(f"{float(self.val)**(other.val)}", kind=self.kind)
            return OpPow(args=[self, other])
        return NotImplemented

    def __lt__(self, other):
        if isinstance(other, int):
            return self < OpInt(other)
        return OpLog("<", args=[self, other])

    def __le__(self, other):
        if isinstance(other, int):
            return self <= OpInt(other)
        return OpLog("<=", args=[self, other])

    def __gt__(self, other):
        if isinstance(other, int):
            return self > OpInt(other)
        return OpLog(">", args=[self, other])

    def __ge__(self, other):
        if isinstance(other, int):
            return self >= OpInt(other)
        return OpLog(">=", args=[self, other])

@dataclass
class OpLeaf(Operator):

    kind: Optional[str] = None
    PRIORITY: ClassVar[int] = 0

    def find_userfunc(self) -> List[OpFuncUser]:
        return []

    def replace_with(self, src: Operator, dest: Operator) -> Operator:
        return self

@dataclass
class OpNum(OpLeaf):

    def collect_vars(self, without_index: bool = False) -> List[OpVar]:
        return []

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        return OpInt(0, target=target)

@dataclass
class OpInt(OpNum):

    val: int = field(default=-999)
    target: Optional[OpVar] = None

    def __init__(self, val: int, kind: str = None, target: OpVar = None):
        super().__init__(args=[], kind=kind)
        if val < 0:
            raise ValueError(f"val must be >= 0: {val}")
        self.val = val
        self.target = target

    def __new__(cls, val: int, kind: str = None, target: OpVar = None):
        if val < 0:
            instance = super().__new__(cls)
            instance.__init__(- val, kind, target)
            return -instance
        else:
            return super().__new__(cls)

    def __reduce__(self):
        return(self.__class__, (self.val, self.kind, self.target,))

    def __str__(self) -> str:
        kind = None
        if self.target is not None:
            kind = self.target.kind
            if self.target.is_real:
                if self.kind is not None:
                    kind = self.kind
                if kind == "8":
                    return f"{self.val}.0d0"
                if kind is not None and kind != "4":
                    return f"{self.val}.0_{kind}"
                return f"{self.val}.0"
        if self.kind is not None:
            kind = self.kind
        if kind is not None and kind != "4":
            return f"{self.val}_{kind}"
        return str(self.val)

@dataclass
class OpChr(OpLeaf):

    name: str = field(default="")

    def collect_vars(self, without_index: bool = False) -> List[OpVar]:
        return []

@dataclass
class OpReal(OpNum):

    val: str = field(default="-999.0e99")

    def __init__(self, val, kind=None):
        super().__init__(args=[], kind=kind)
        self.val = val

    def __str__(self) -> str:
        if self.kind is not None:
            if self.kind == "8":
                return f"{self.val}d0"
            if self.kind != "4":
                return f"{self.val}_{self.kind}"
        return str(self.val)

@dataclass
class OpVar(OpLeaf):

    name: str = field(default="")
    index: Optional[AryIndex] = None
    is_real: Optional[bool] = None

    def __init__(self, name: str, index: Optional[AryIndex] = None, is_real: bool = None, kind: str = None):
        super().__init__(args=[])
        if not isinstance(name, str):
            raise ValueError(f"name must be str: {type(name)}")
        self.name = name
        if index is not None and not isinstance(index, AryIndex):
            index = AryIndex(index)
        self.index = index
        self.is_real = is_real
        self.kind = kind

    def change_index(self, index) -> OpVar:
        if index == self.index:
            return self
        return OpVar(name=self.name, index=index, is_real=self.is_real, kind=self.kind)

    def add_suffix(self, suffix: str = None) -> str:
        if suffix is None:
            return self
        name = f"{self.name}{suffix}"
        index = self.index
        if index is not None:
            index = AryIndex(list(index.dims))
        return OpVar(name, index=index, is_real=self.is_real, kind=self.kind)

    def collect_vars(self, without_index: bool = False) -> List[OpVar]:
        vars = [self]
        if (not without_index) and self.index is not None:
            for v in self.index.collect_vars():
                if not v in vars:
                    vars.append(v)
        return vars

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        if var == self:
            return OpInt(1, target=target)
        return OpInt(0, target=target)

    def is_partial_access(self) -> bool:
        if self.index is None:
            return False
        return self.index.is_partial_access()

    def index_list(self) -> List[str]:
        if self.index is None:
            return []
        return self.index.list()

    def index_str(self) -> str:
        return ",".join(self.index_list())

    def __str__(self) -> str:
        if self.index is None or len(self.index) == 0:
            return self.name
        else:
            return f"{self.name}({self.index_str()})"

    def __eq__(self, other) -> bool:
        if not isinstance(other, type(self)):
            return NotImplemented
        if self.name == other.name:
            if self.index == other.index:
                return True
        return False

@dataclass
class OpUnary(Operator):

    OP: ClassVar[str] = "undef"

    def __post_init__(self):
        super().__post_init__()
        if len(self.args) != 1:
            raise ValueError("length of args must 1")

    def __str__(self) -> str:
        a0 = self._paren(self.args[0])
        return f"{self.OP} {a0}"

@dataclass
class OpNeg(OpUnary):

    OP: ClassVar[str] = "-"
    PRIORITY: ClassVar[int] = 4

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        return - self.args[0].derivative(var, target, info, warnings)

@dataclass
class OpBinary(Operator):

    OP: ClassVar[str] = "undef"

    def __post_init__(self):
        super().__post_init__()
        if len(self.args) != 2:
            raise ValueError("length of args must 2")

    def __str__(self) -> str:
        a0 = self._paren(self.args[0])
        eq = isinstance(self.args[1], OpNeg) or (isinstance(self, OpDiv) and isinstance(self.args[1], OpMul))
        a1 = self._paren(self.args[1], eq=eq)
        return f"{a0} {self.OP} {a1}"

@dataclass
class OpAdd(OpBinary):

    OP: ClassVar[str] = "+"
    PRIORITY: ClassVar[int] = 5

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        return self.args[0].derivative(var, target, info, warnings) + self.args[1].derivative(var, target, info, warnings)

@dataclass
class OpSub(OpBinary):

    OP: ClassVar[str] = "-"
    PRIORITY: ClassVar[int] = 5

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        return self.args[0].derivative(var, target, info, warnings) - self.args[1].derivative(var, target, info, warnings)

@dataclass
class OpMul(OpBinary):

    OP: ClassVar[str] = "*"
    PRIORITY: ClassVar[int] = 4

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        arg0 = self.args[0]
        arg1 = self.args[1]
        arg0_dev = arg0.derivative(var, target, info, warnings)
        arg1_dev = arg1.derivative(var, target, info, warnings)
        return arg0_dev * arg1 + arg0 * arg1_dev

@dataclass
class OpDiv(OpBinary):

    OP: ClassVar[str] = "/"
    PRIORITY: ClassVar[int] = 4

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        arg0 = self.args[0]
        arg1 = self.args[1]
        arg0_dev = arg0.derivative(var, target, info, warnings)
        if isinstance(arg1, OpNum):
            return arg0_dev / arg1
        arg1_dev = arg1.derivative(var, target, info, warnings)
        if isinstance(arg1_dev, OpInt) and arg1_dev.val == 0:
            return arg0_dev / arg1
        return  (arg0_dev * arg1 - arg0 * arg1_dev) / arg1**2

@dataclass
class OpPow(OpBinary):

    OP: ClassVar[str] = "**"
    PRIORITY: ClassVar[int] = 2

    def __str__(self) -> str:
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

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
        base = self.args[0]
        expo = self.args[1]
        dbase = base.derivative(var, target, info, warnings)
        dexpo = expo.derivative(var, target, info, warnings)
        if isinstance(expo, OpVar) and expo.is_real:
            one = OpReal("1.0", kind=expo.kind)
        else:
            one = OpInt(1)
        if isinstance(expo, OpInt) and expo.target is None:
            expo2 = OpInt(expo.val, target=target)
        else:
            expo2 = expo
        return expo2 * base**(expo - one) * dbase + base**expo * OpFunc("log", args=[base]) * dexpo

@dataclass
class OpLog(OpBinary):

    op: str = field(default="")
    PRIORITY: ClassVar[int] = 6

    def __init__(self, op: str, args:List[Operator]):
        super().__init__(args=args)
        if not op:
            raise ValueError("op should not be empty")
        self.op = op

    def __str__(self) -> str:
        a0 = self._paren(self.args[0])
        eq = isinstance(self.args[1], OpNeg)
        a1 = self._paren(self.args[1], eq=eq)
        return f"{a0} {self.op} {a1}"

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

@dataclass
class OpFunc(Operator):

    name: str = field(default="")
    PRIORITY: ClassVar[int] = 1

    def __init__(self, name: str, args:List[Operator]):
        super().__init__(args=args)
        if not name:
            raise ValueError("name should not be empty")
        self.name = name

    def __str__(self) -> str:
        args = []
        for arg in self.args:
            args.append(f"{arg}")
        args = ", ".join(args)
        return f"{self.name}({args})"

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:

        if self.name in NONDIFF_INTRINSICS:
            return OpInt(0, target=target)

        def _pi(target):
            return OpFunc("sqrt", args=[OpFunc("acos", args=[-OpReal(1.0, kind=target.kind)])])

        # One argument intrinsics map
        arg0 = self.args[0]
        dvar0 = arg0.derivative(var, target, info, warnings)

        if self.name == "abs":
            return dvar0 * OpFunc("sign", args=[OpInt(1, target=var), arg0])
        if self.name == "sqrt":
            return dvar0 / (OpInt(2, target=target) * OpFunc("sqrt", args=[arg0]) )
        if self.name == "exp":
            return dvar0 * OpFunc("exp", args=[arg0])
        if self.name == "log":
            return dvar0 / arg0
        if self.name == "log10":
            return dvar0 / ( arg0 * OpFunc("log", args=[OpReal("10.0", kind=target.kind)]) )
        if self.name == "sin":
            return dvar0 * OpFunc("cos", args=[arg0])
        if self.name == "cos":
            return - dvar0 * OpFunc("sin", args=[arg0])
        if self.name == "tan":
            return dvar0 / OpFunc("cos", args=[arg0])**2
        if self.name == "asin":
            return dvar0 / OpFunc("sqrt", args=[OpReal(1.0, kind=target.kind) - arg0**2])
        if self.name == "acos":
            return - dvar0 / OpFunc("sqrt", args=[OpReal(1.0, kind=target.kind) - arg0**2])
        if self.name == "atan":
            return dvar0 / (OpReal(1.0, kind=target.kind) + arg0**2)
        if self.name == "sinh":
            return dvar0 * OpFunc("cosh", args=[arg0])
        if self.name == "cosh":
            return dvar0 * OpFunc("sinh", args=[arg0])
        if self.name == "tanh":
            return dvar0 / OpFunc("cosh", args=[arg0])**2
        if self.name == "asinh":
            return dvar0 / OpFunc("sqrt", args=[arg0**2 + OpReal(1.0, kind=target.kind)])
        if self.name == "acosh":
            one = OpReal(1.0, kind=target.kind)
            return dvar0 / (OpFunc("sqrt", args=[arg0 - one]) * OpFunc("sqrt", args=[arg0 + one]))
        if self.name == "atanh":
            return dvar0 / (OpReal(1.0, kind=target.kind) - arg0**2)
        if self.name == "erf":
            return dvar0 * OpInt(2, target=target) / _pi(target) * OpFunc("exp", args=[-arg0**2])
        if self.name == "erfc":
            return - dvar0 * OpInt(2, target=target) / _pi(target) * OpFunc("exp", args=[-arg0**2])
        if self.name == "real":
            return dvar0
        if self.name == "dble":
            return dvar0

        if len(self.args) < 2:
            raise ValueError(f"Function ({self.name}) is not supported")

        # Two argument intrinsics map
        arg1 = self.args[1]
        dvar1 = arg1.derivative(var, target, info, warnings)

        if self.name == "mod":
            return dvar0 - dvar1 * OpFunc("real", args=[OpFunc("int", args=[arg0 / arg1]), OpFunc("kind", args=[arg0])])
        if self.name == "min":
            one = OpReal("1.0", kind=target.kind)
            zero = OpReal("0.0", kind=target.kind)
            cond = arg0 >= arg1
            return dvar0 * OpFunc("merge", args=[one, zero, cond]) + dvar1 * OpFunc("merge", args=[zero, one, cond])
        if self.name == "max":
            one = OpReal("1.0", kind=target.kind)
            zero = OpReal("0.0", kind=target.kind)
            cond = arg0 <= arg1
            return dvar0 * OpFunc("merge", args=[one, zero, cond]) + dvar1 * OpFunc("merge", args=[zero, one, cond])
        if self.name == "sign":
            one = OpReal("1.0", kind=target.kind)
            return dvar0 * OpFunc("sign", args=[one, arg0]) * OpFunc("sign", args=[one, arg1])
        if self.name == "atan2":
            return (dvar0 * arg1 - dvar1 * arg0) / (arg0**2 + arg1**2)

        raise ValueError(f"Function ({self.name}) is not supported")

    def special_handler(self, dsc, args):
        if self.name == "transpose":
            """Propagate gradient through ``transpose``."""
            return OpFunc(name="transpose", args=[dsc])
        if self.name == "cshift":
            """Propagate gradient through ``cshift``."""
            shift = - args[1]
            dim = args[2]
            return OpFunc(name="cshift", args=[dsc, shift, dim])
        return None

@dataclass
class OpFuncUser(Operator):

    name: str = field(default="")
    intents: List[name] = field(default=None)
    PRIORITY: ClassVar[int] = 1

    def __init__(self, name: str, args:List[Operator]):
        super().__init__(args=args)
        if not name:
            raise ValueError("name should not be empty")
        self.name = name

    def __str__(self) -> str:
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

    def derivative(self, var: OpVar, target: OpVar = None, info: dict = None, warnings: List[str] = None) -> Operator:
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

    def collect_vars(self, without_index: bool = False) -> List[OpLeaf]:
        vars = []
        if not without_index:
            for arg in self.args:
                if arg is not None:
                    for v in arg.collect_vars():
                        if not v in vars:
                            vars.append(v)
        return vars

    def __str__(self) -> str:
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

    def __getitem__(self, index: int) -> Operator:
        if len(self.args) <= index:
          return None
        return self.args[index]

    def __eq__(self, other) -> bool:
        if not isinstance(other, Operator):
            return NotImplemented
        return str(self) == str(other)
