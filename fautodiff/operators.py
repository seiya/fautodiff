"""Utilities for assembling computing operations."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
import re
import copy


@dataclass
class Operation:
    """Abstract fortran oprations."""

    args: Optional[List[Operation]]
    priority: int = field(init=False, default=999)

    def _paren(self, arg: Operation) -> str:
        if self.priority < arg.priority:
            return f"({arg})"
        else:
            return f"{arg}"

    def collect_vars(self) -> List[OpLeaf]:
        vars = []
        for arg in self.args:
            vars.extend(arg.collect_vars())
        return vars

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        """Return the derivative operation with respetive to ```var```."""
        raise NotImplementedError(f"derivative in {type(self)}")

    def __neg__(self):
        if isinstance(self, OpInt):
            return OpInt(-self.val)
        return OpNeg(args=[self])

    def __add__(self, other):
        if isinstance(other, Operation):
            if isinstance(self, OpInt) and self.val == 0:
                return other
            if isinstance(other, OpInt) and other.val == 0:
                return self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                return OpInt(self.val + other.val)
            return OpAdd(args=[self, other])
        if isinstance(other, int):
            return self + OpInt(other)
        if isinstance(other, float):
            return self + OpReal(other)
        return NotImplemented

    def __sub__(self, other):
        if isinstance(other, Operation):
            if isinstance(self, OpInt) and self.val == 0:
                return - other
            if isinstance(other, OpInt) and other.val == 0:
                return self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                return OpInt(self.val - other.val)
            return OpSub(args=[self, other])
        if isinstance(other, int):
            return self - OpInt(other)
        if isinstance(other, float):
            return self - OpReal(other)
        return NotImplemented

    def __mul__(self, other):
        if isinstance(other, Operation):
            if isinstance(self, OpInt) and self.val == 1:
                return other
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(self, OpInt) and self.val == 0:
                return OpInt(0)
            if isinstance(other, OpInt) and other.val == 0:
                return OpInt(0)
            if isinstance(self, OpInt) and self.val == -1:
                return - other
            if isinstance(other, OpInt) and other.val == -1:
                return - self
            if isinstance(self, OpInt) and isinstance(other, OpInt):
                return OpInt(self.val * other.val)
            return OpMul(args=[self, other])
        if isinstance(other, int):
            return self * OpInt(other)
        if isinstance(other, float):
            return self * OpReal(other)
        return NotImplemented

    def __truediv__(self, other):
        if isinstance(other, Operation):
            if isinstance(other, OpInt) and other.val == 1:
                return self
            if isinstance(self, OpInt) and self.val == 0:
                return self
            return OpDiv(args=[self, other])
        if isinstance(other, int):
            return self / OpInt(other)
        if isinstance(other, float):
            return self / OpReal(other)
        return NotImplemented

    def __pow__(self, other):
        if isinstance(other, Operation):
            if isinstance(other, OpInt) and other.val == 1:
                return self
            return OpPow(args=[self, other])
        if isinstance(other, int):
            return self**OpInt(other)
        if isinstance(other, float):
            return self**OpReal(other)
        return NotImplemented


@dataclass
class OpLeaf(Operation):

    priority: int = field(default=0)

@dataclass
class OpNum(OpLeaf):

    kind: Optional[str] = None

    def collect_vars(self) -> List[OpLeaf]:
        return []

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        return OpInt(val=0)

    def __str__(self) -> str:
        if self.kind is None:
            return f"{self.val}"
        else:
            return f"{self.val}#{self.kind}"

@dataclass
class OpInt(OpNum):

    val: int = field(default=-999)

    def __init__(self, val, kind=None):
        super().__init__(args=[])
        self.val = val

@dataclass
class OpReal(OpNum):

    val: float = field(default=-999.0)

    def __init__(self, val, kind=None):
        super().__init__(args=[])
        self.val = val

@dataclass
class OpVar(OpLeaf):

    name: str = field(default="")
    index: List[str] = None

    def __init__(self, name, index=None):
        super().__init__(args=[])
        self.name = name
        self.index = index

    def add_suffix(self, suffix: str = None) -> str:
        if suffix is None:
            return copy.deepcopy(self)
        name = f"{self.name}{suffix}"
        index = copy.deepcopy(self.index)
        return OpVar(name, index=index)

    def serialized_name(self) -> str:
        if self.index is None or len(self.index)==0:
            return self.name
        suffix = "_".join(self.index)
        return f"{self.name}_{suffix}"

    def collect_vars(self) -> List[OpLeaf]:
        return [self]

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        if var.name == self.name and var.index == self.index:
            return OpInt(1)
        return OpInt(0)

    def __str__(self) -> str:
        if self.index is None or len(self.index)==0:
            return self.name
        else:
            index = ','.join(self.index)
            return f"{self.name}({index})"

@dataclass
class OpUnary(Operation):

    op: str = field(init=False)

    def __post_init(self):
        if len(self.args) != 1:
            raise ValueError("length of args must 1")

    def __str__(self) -> str:
        a0 = self._paren(self.args[0])
        return f"{self.op} {a0}"

@dataclass
class OpNeg(OpUnary):

    op: str = field(init=False, default="-")
    priority: int = field(default=3)

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        return - self.args[0].derivative(var, info, warnings)

@dataclass
class OpBinary(Operation):

    op: str = field(init=False)

    def __post_init(self):
        if len(self.args) != 2:
            raise ValueError("length of args must 2")

    def __str__(self) -> str:
        a0 = self._paren(self.args[0])
        a1 = self._paren(self.args[1])
        return f"{a0} {self.op} {a1}"

@dataclass
class OpAdd(OpBinary):

    op: str = field(init=False, default="+")
    priority: int = field(default=5)

    def derivative(self, var: OpLearf, info: dict = None, warnings: List[str] = None) -> Operation:
        return self.args[0].derivative(var, info, warnings) + self.args[1].derivative(var, info, warnings)

@dataclass
class OpSub(OpBinary):

    op: str = field(init=False, default="-")
    priority: int = field(default=5)

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        return self.args[0].derivative(var, info, warnings) - self.args[1].derivative(var, info, warnings)

@dataclass
class OpMul(OpBinary):

    op: str = field(init=False, default="*")
    priority: int = field(default=4)

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        arg0 = self.args[0]
        arg1 = self.args[1]
        arg0_dev = arg0.derivative(var, info, warnings)
        arg1_dev = arg1.derivative(var, info, warnings)
        return arg0_dev * arg1 + arg0 * arg1_dev

@dataclass
class OpDiv(OpBinary):

    op: str = field(init=False, default="/")
    priority: int = field(default=4)

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        arg0 = self.args[0]
        arg1 = self.args[1]
        arg0_dev = arg0.derivative(var, info, warnings)
        arg1_dev = arg1.derivative(var, info, warnings)
        return  (arg0_dev * arg1 - arg0 * arg1_dev) / arg1**2

@dataclass
class OpPow(OpBinary):

    op: str = field(init=False, default="**")
    priority: int = field(default=2)

    def __str__(self) -> str:
        a0 = self._paren(self.args[0])
        a1 = self._paren(self.args[1])
        if a1 == "1":
            return a0
        else:
            return f"{a0}{self.op}{a1}"

    def derivative(self, var: OpLeaf, info: dict = None, warnings: List[str] = None) -> Operation:
        base = self.args[0]
        exponent = self.args[1]
        return exponent * base**(exponent - 1)

@dataclass
class OpFunc(Operation):

    name: str = field(default="")
    priority: int = field(default=1)

    def __init__(self, name: str, args:List[Operation]):
        super().__init__(args=args)
        if not name:
            raise ValueError("name should not be empty")
        self.name = name

    def __str__(self) -> str:
        args = []
        for arg in self.args:
            args.append(f"{arg}")
        args = ", ".join(args)
        return f"{name}({args})"
