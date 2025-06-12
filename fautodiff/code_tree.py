"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator
import re
import copy


_NAME_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")


def _append_unique(items: List[str], name: str) -> None:
    if name not in items:
        items.append(name)


def _extract_names(text: str) -> List[str]:
    """Return a list of variable-like names from ``text`` preserving order."""
    names = []
    for tok in _NAME_RE.findall(text or ""):
        _append_unique(names, tok)
    return names


@dataclass
class Node:
    """Abstract syntax tree node for Fortran code fragments."""

    id: int = field(init=False, repr=False)

    _id_counter = 0

    def __post_init__(self):
        self.id = Node._id_counter
        Node._id_counter += 1

    # ------------------------------------------------------------------
    # basic node API
    # ------------------------------------------------------------------

    def render(self, indent: int = 0) -> List[str]:
        """Return the formatted Fortran code lines for this node."""
        raise NotImplementedError

    def is_effectively_empty(self) -> bool:
        """Return ``True`` if removing this node does not change execution."""
        return False

    def has_assignment_to(self, var: str) -> bool:
        """Return ``True`` if ``var`` is assigned within this node."""
        return False

    # ------------------------------------------------------------------
    # node tree helpers
    # ------------------------------------------------------------------

    def iter_children(self) -> Iterator["Node"]:
        """Yield child nodes."""
        return iter(())

    # ------------------------------------------------------------------
    # new features
    # ------------------------------------------------------------------

    def deep_clone(self) -> "Node":
        """Return a deep clone of this node tree with new ids."""
        clone = copy.deepcopy(self)
        Node._assign_new_ids(clone)
        return clone

    @classmethod
    def _assign_new_ids(cls, node: "Node") -> None:
        node.id = cls._id_counter
        cls._id_counter += 1
        for child in node.iter_children():
            cls._assign_new_ids(child)

    def get_id(self) -> int:
        """Return this node's unique id."""
        return self.id

    def find_by_id(self, node_id: int) -> Optional["Node"]:
        """Return the node with ``node_id`` from this subtree or ``None``."""
        if self.id == node_id:
            return self
        for child in self.iter_children():
            found = child.find_by_id(node_id)
            if found is not None:
                return found
        return None

    def remove_by_id(self, node_id: int) -> bool:
        """Remove the node with ``node_id`` from this subtree.

        Returns ``True`` if a node was removed.
        """
        for child in list(self.iter_children()):
            if child.id == node_id:
                self._remove_child(child)
                return True
            if child.remove_by_id(node_id):
                return True
        return False

    def _remove_child(self, child: "Node") -> None:
        """Remove ``child`` from this node. Override in subclasses."""
        # Only ``Block`` implements actual removal.
        pass

    # ------------------------------------------------------------------
    # variable analysis helpers
    # ------------------------------------------------------------------

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        """Return variables assigned within this node and children."""
        return list(names or [])

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        """Return variables referenced before assignment within this node."""
        return list(names or [])


@dataclass
class Block(Node):
    """A container for a sequence of nodes."""

    children: List[Node] = field(default_factory=list)

    def iter_children(self) -> Iterator[Node]:
        return iter(self.children)

    def _remove_child(self, child: Node) -> None:
        self.children.remove(child)

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

    def is_effectively_empty(self) -> bool:
        return all(child.is_effectively_empty() for child in self.children)

    def has_assignment_to(self, var: str) -> bool:
        return any(child.has_assignment_to(var) for child in self.children)

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for child in self.children:
            res = child.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        assigned: List[str] = []
        for child in self.children:
            req = child.required_vars([])
            for n in req:
                if n not in assigned and n not in res:
                    res.append(n)
            assigned = child.assigned_vars(assigned)
        return res


@dataclass
class Assignment(Node):
    """An assignment statement ``lhs = rhs``."""

    lhs: str
    rhs: str

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        return [f"{space}{self.lhs} = {self.rhs}\n"]

    def has_assignment_to(self, var: str) -> bool:
        lhs_name = self.lhs.split("(")[0].strip().lower()
        return lhs_name == var.lower()

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        lhs_name = self.lhs.split("(")[0].strip()
        _append_unique(res, lhs_name)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for n in _extract_names(self.rhs):
            if n != self.lhs.split("(")[0].strip():
                _append_unique(res, n)
        return res


@dataclass
class Subroutine(Node):
    """A ``subroutine`` with declaration and execution blocks."""

    name: str
    args: str = ""
    decls: Block = field(default_factory=Block)
    body: Block = field(default_factory=Block)

    def iter_children(self) -> Iterator[Node]:
        return iter([self.decls, self.body])

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        args = f"({self.args})" if self.args else "()"
        lines = [f"{space}subroutine {self.name}{args}\n"]
        lines.extend(self.decls.render(indent + 1))
        lines.append("\n")
        lines.extend(self.body.render(indent + 1))
        lines.append(f"{space}end subroutine {self.name}\n")
        return lines

    def is_effectively_empty(self) -> bool:
        return self.decls.is_effectively_empty() and self.body.is_effectively_empty()

    def has_assignment_to(self, var: str) -> bool:
        return self.decls.has_assignment_to(var) or self.body.has_assignment_to(var)

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = self.decls.assigned_vars(names)
        return self.body.assigned_vars(res)

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        res = self.decls.required_vars(res)
        assigned = self.decls.assigned_vars([])
        for child in self.body.children:
            req = child.required_vars([])
            for n in req:
                if n not in assigned and n not in res:
                    res.append(n)
            assigned = child.assigned_vars(assigned)
        return res

    def defined_var_names(self) -> List[str]:
        names: List[str] = []
        for node in self.decls.children:
            if isinstance(node, Declaration):
                names.append(node.name)
        return names


@dataclass
class Function(Node):
    """A ``function`` with declaration and execution blocks."""

    name: str
    args: str = ""
    decls: Block = field(default_factory=Block)
    body: Block = field(default_factory=Block)

    def iter_children(self) -> Iterator[Node]:
        return iter([self.decls, self.body])

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        args = f"({self.args})" if self.args else "()"
        lines = [f"{space}function {self.name}{args}\n"]
        lines.extend(self.decls.render(indent + 1))
        lines.append("\n")
        lines.extend(self.body.render(indent + 1))
        lines.append(f"{space}end function {self.name}\n")
        return lines

    def is_effectively_empty(self) -> bool:
        return self.decls.is_effectively_empty() and self.body.is_effectively_empty()

    def has_assignment_to(self, var: str) -> bool:
        return self.decls.has_assignment_to(var) or self.body.has_assignment_to(var)

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = self.decls.assigned_vars(names)
        return self.body.assigned_vars(res)

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        res = self.decls.required_vars(res)
        assigned = self.decls.assigned_vars([])
        for child in self.body.children:
            req = child.required_vars([])
            for n in req:
                if n not in assigned and n not in res:
                    res.append(n)
            assigned = child.assigned_vars(assigned)
        return res

    def defined_var_names(self) -> List[str]:
        names: List[str] = []
        for node in self.decls.children:
            if isinstance(node, Declaration):
                names.append(node.name)
        return names


@dataclass
class EmptyLine(Node):
    """Represents a blank line."""

    def render(self, indent: int = 0) -> List[str]:
        return ["\n"]

    def is_effectively_empty(self) -> bool:
        return True


@dataclass
class Declaration(Node):
    """A variable declaration."""

    typename: str
    name: str
    intent: Optional[str] = None
    shape: Optional[str] = None

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        line = f"{space}{self.typename}"
        if self.intent is not None:
            pad = "  " if self.intent == "in" else " "
            line += f", intent({self.intent})" + pad + f":: {self.name}"
        else:
            line += f" :: {self.name}"
        if self.shape:
            line += self.shape
        line += "\n"
        return [line]

    def has_assignment_to(self, var: str) -> bool:
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        if self.intent in ("in", "inout"):
            _append_unique(res, self.name)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        return list(names or [])


@dataclass
class IfBlock(Node):
    """An ``if`` block with optional ``else if`` branches and ``else``."""

    condition: str
    body: Block
    elif_blocks: List[Tuple[str, Block]] = field(default_factory=list)
    else_body: Optional[Block] = None

    def iter_children(self) -> Iterator[Node]:
        yield self.body
        for _, blk in self.elif_blocks:
            yield blk
        if self.else_body is not None:
            yield self.else_body

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}IF ({self.condition}) THEN\n"]
        lines.extend(self.body.render(indent+1))
        for cond, blk in self.elif_blocks:
            lines.append(f"{space}ELSE IF ({cond}) THEN\n")
            lines.extend(blk.render(indent+1))
        if self.else_body is not None:
            lines.append(f"{space}ELSE\n")
            lines.extend(self.else_body.render(indent+1))
        lines.append(f"{space}END IF\n")
        return lines

    def is_effectively_empty(self) -> bool:
        if not self.body.is_effectively_empty():
            return False
        for _, blk in self.elif_blocks:
            if not blk.is_effectively_empty():
                return False
        if self.else_body is not None and not self.else_body.is_effectively_empty():
            return False
        return True

    def has_assignment_to(self, var: str) -> bool:
        if self.body.has_assignment_to(var):
            return True
        for _, blk in self.elif_blocks:
            if blk.has_assignment_to(var):
                return True
        if self.else_body is not None and self.else_body.has_assignment_to(var):
            return True
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        res = self.body.assigned_vars(res)
        for _, blk in self.elif_blocks:
            res = blk.assigned_vars(res)
        if self.else_body is not None:
            res = self.else_body.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for n in _extract_names(self.condition):
            _append_unique(res, n)
        branches = [self.body] + [blk for _, blk in self.elif_blocks]
        if self.else_body is not None:
            branches.append(self.else_body)
        for blk in branches:
            req = blk.required_vars([])
            for n in req:
                _append_unique(res, n)
        return res


@dataclass
class SelectBlock(Node):
    """A ``select case`` construct."""

    expr: str
    cases: List[Tuple[str, Block]] = field(default_factory=list)
    default: Optional[Block] = None

    def iter_children(self) -> Iterator[Node]:
        for _, blk in self.cases:
            yield blk
        if self.default is not None:
            yield self.default

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}SELECT CASE ({self.expr})\n"]
        for cond, block in self.cases:
            lines.append(f"{space}CASE ({cond})\n")
            lines.extend(block.render(indent+1))
        if self.default is not None:
            lines.append(f"{space}CASE DEFAULT\n")
            lines.extend(self.default.render(indent+1))
        lines.append(f"{space}END SELECT\n")
        return lines

    def is_effectively_empty(self) -> bool:
        for _, blk in self.cases:
            if not blk.is_effectively_empty():
                return False
        if self.default is not None and not self.default.is_effectively_empty():
            return False
        return True

    def has_assignment_to(self, var: str) -> bool:
        for _, blk in self.cases:
            if blk.has_assignment_to(var):
                return True
        if self.default is not None and self.default.has_assignment_to(var):
            return True
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for _, blk in self.cases:
            res = blk.assigned_vars(res)
        if self.default is not None:
            res = self.default.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for n in _extract_names(self.expr):
            _append_unique(res, n)
        blocks = [blk for _, blk in self.cases]
        if self.default is not None:
            blocks.append(self.default)
        for blk in blocks:
            req = blk.required_vars([])
            for n in req:
                _append_unique(res, n)
        return res


@dataclass
class DoLoop(Node):
    """A ``do`` loop."""

    header: str
    body: Block

    def iter_children(self) -> Iterator[Node]:
        yield self.body

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}{self.header}\n"]
        lines.extend(self.body.render(indent+1))
        lines.append(f"{space}END DO\n")
        return lines

    def is_effectively_empty(self) -> bool:
        return self.body.is_effectively_empty()

    def has_assignment_to(self, var: str) -> bool:
        return self.body.has_assignment_to(var)

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        tokens = [t for t in _extract_names(self.header) if t.lower() != "do"]
        if tokens:
            _append_unique(res, tokens[0])
        res = self.body.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        tokens = [t for t in _extract_names(self.header) if t.lower() != "do"]
        if tokens:
            for t in tokens[1:]:
                _append_unique(res, t)
        else:
            for t in _extract_names(self.header):
                _append_unique(res, t)
        req = self.body.required_vars([])
        for n in req:
            _append_unique(res, n)
        return res


@dataclass
class Return(Node):
    """A ``return`` statement."""

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        return [f"{space}return\n"]

    def has_assignment_to(self, var: str) -> bool:
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        return list(names or [])

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        return list(names or [])


def render_program(node: Node, indent: int = 0) -> str:
    """Return formatted Fortran code for the entire ``node`` tree."""

    lines = node.render(indent)
    return "".join(lines)


