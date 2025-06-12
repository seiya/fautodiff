"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Tuple, Optional


class Node:
    """Abstract syntax tree node for Fortran code fragments."""

    def render(self, indent: int = 0) -> List[str]:
        """Return the formatted Fortran code lines for this node."""
        raise NotImplementedError


@dataclass
class Block(Node):
    """A container for a sequence of nodes."""

    children: List[Node] = field(default_factory=list)

    def render(self, indent: int = 0) -> List[str]:
        lines: List[str] = []
        for child in self.children:
            lines.extend(child.render(indent))
        return lines


@dataclass
class Assignment(Node):
    """An assignment statement ``lhs = rhs``."""

    lhs: str
    rhs: str

    def render(self, indent: int = 0) -> List[str]:
        space = " " * indent
        return [f"{space}{self.lhs} = {self.rhs}\n"]


@dataclass
class Declaration(Node):
    """A declaration statement."""

    text: str

    def render(self, indent: int = 0) -> List[str]:
        space = " " * indent
        return [f"{space}{self.text}\n"]


@dataclass
class IfBlock(Node):
    """An ``if``/``else`` block."""

    condition: str
    body: Block
    else_body: Optional[Block] = None

    def render(self, indent: int = 0) -> List[str]:
        space = " " * indent
        lines = [f"{space}if ({self.condition}) then\n"]
        lines.extend(self.body.render(indent + 2))
        if self.else_body is not None:
            lines.append(f"{space}else\n")
            lines.extend(self.else_body.render(indent + 2))
        lines.append(f"{space}end if\n")
        return lines


@dataclass
class SelectBlock(Node):
    """A ``select case`` construct."""

    expr: str
    cases: List[Tuple[str, Block]] = field(default_factory=list)
    default: Optional[Block] = None

    def render(self, indent: int = 0) -> List[str]:
        space = " " * indent
        lines = [f"{space}select case ({self.expr})\n"]
        for cond, block in self.cases:
            lines.append(f"{space}case ({cond})\n")
            lines.extend(block.render(indent + 2))
        if self.default is not None:
            lines.append(f"{space}case default\n")
            lines.extend(self.default.render(indent + 2))
        lines.append(f"{space}end select\n")
        return lines


@dataclass
class DoLoop(Node):
    """A ``do`` loop."""

    header: str
    body: Block

    def render(self, indent: int = 0) -> List[str]:
        space = " " * indent
        lines = [f"{space}do {self.header}\n"]
        lines.extend(self.body.render(indent + 2))
        lines.append(f"{space}end do\n")
        return lines


class Return(Node):
    """A ``return`` statement."""

    def render(self, indent: int = 0) -> List[str]:
        space = " " * indent
        return [f"{space}return\n"]


