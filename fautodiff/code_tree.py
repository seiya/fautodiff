"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional


class Node:
    """Abstract syntax tree node for Fortran code fragments."""

    def render(self, indent: int = 0) -> List[str]:
        """Return the formatted Fortran code lines for this node."""
        raise NotImplementedError


@dataclass
class Block(Node):
    """A container for a sequence of nodes."""

    children: List[Node] = field(default_factory=list)

    def append(self, node: Node) -> None:
        """Append ``node`` to this block."""
        self.children.append(node)

    def extend(self, nodes: Iterable[Node]) -> None:
        """Extend this block with ``nodes``."""
        self.children.extend(nodes)

    def insert(self, index: int, node: Node) -> None:
        """Insert ``node`` at ``index``."""
        self.children.insert(index, node)

    def __iter__(self):
        return iter(self.children)

    def __len__(self) -> int:
        return len(self.children)

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
    """An ``if`` block with optional ``else if`` branches and ``else``."""

    condition: str
    body: Block
    elif_blocks: List[Tuple[str, Block]] = field(default_factory=list)
    else_body: Optional[Block] = None
    indent: str = ""

    def render(self, indent: int = 0) -> List[str]:
        space = self.indent
        lines = [f"{space}IF ({self.condition}) THEN\n"]
        lines.extend(self.body.render(0))
        for cond, blk in self.elif_blocks:
            lines.append(f"{space}ELSE IF ({cond}) THEN\n")
            lines.extend(blk.render(0))
        if self.else_body is not None:
            lines.append(f"{space}ELSE\n")
            lines.extend(self.else_body.render(0))
        lines.append(f"{space}END IF\n")
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


def render_program(node: Node, indent: int = 0) -> str:
    """Return formatted Fortran code for the entire ``node`` tree."""

    lines = node.render(indent)
    return "".join(lines)


