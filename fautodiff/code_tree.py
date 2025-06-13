"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
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
        """Return variables needed before executing this node.

        ``names`` is the list of variables that must be defined *after* this
        node has executed.  The default implementation simply returns ``names``
        unchanged.  Subclasses override this to remove variables that are
        assigned and to add any variables referenced by this node.
        """
        return list(names or [])

    # ------------------------------------------------------------------
    # optimization helpers
    # ------------------------------------------------------------------

    def prune_for(self, targets: Iterable[str]) -> "Node":
        """Return a copy of this node with only code needed for ``targets``."""
        return self.deep_clone()

    # ------------------------------------------------------------------
    # initialization optimization helper
    # ------------------------------------------------------------------

    def remove_initial_self_add(self, var: str) -> int:
        """Remove self-add from the first assignment to ``var`` if safe.

        Returns ``1`` for pattern1, ``2`` for pattern2 and ``3`` for pattern3
        as defined in the documentation. ``3`` indicates that a self-add was
        removed.
        """
        return 1


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
        needed = list(names or [])
        for child in reversed(self.children):
            needed = child.required_vars(needed)
        return needed

    def prune_for(self, targets: Iterable[str]) -> "Block":
        needed = list(targets)
        new_children: List[Node] = []
        for child in reversed(self.children):
            assigned = child.assigned_vars([])
            if any(v in needed for v in assigned):
                pruned = child.prune_for(needed)
                new_children.insert(0, pruned)
                needed = pruned.required_vars(needed)
        return Block(new_children)

    def remove_initial_self_add(self, var: str) -> int:
        for child in self.children:
            if child.has_assignment_to(var):
                return child.remove_initial_self_add(var)
        return 1

    def remove_redundant_inits(self, init_block: "Block", keep: Iterable[str]) -> None:
        """Remove redundant gradient initializations using node helpers."""
        for idx, node in enumerate(list(init_block.children)):
            base = node.lhs.split("(")[0].strip()
            if base in keep:
                continue
            self.remove_by_id(node.get_id())
            res = self.remove_initial_self_add(base)
            if res == 1:
                self.insert(idx, node)


@dataclass
class DeclBlock(Block):
    """Block containing declarations."""


@dataclass
class InitBlock(Block):
    """Block containing gradient initializations."""


@dataclass
class AdBlock(Block):
    """Block containing AD computation statements."""


@dataclass
class Assignment(Node):
    """An assignment statement ``lhs = rhs``."""

    lhs: str
    rhs: str
    accumulate: bool = False
    rhs_names: List[str] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        self.rhs_names = _extract_names(self.rhs)
        if self.accumulate and self._detect_self_add():
            raise ValueError("rhs must not reference lhs when accumulate=True")

    def _detect_self_add(self) -> bool:
        lhs_base = self.lhs.split("(")[0].strip().lower()
        parts = [p.strip().lower() for p in self.rhs.split("+")]
        if len(parts) != 2:
            return False
        left_base = parts[0].split("(")[0].strip()
        right_base = parts[1].split("(")[0].strip()
        return lhs_base in (left_base, right_base)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        rhs = self.rhs
        if self.accumulate:
            rhs = f"{rhs} + {self.lhs}"
        return [f"{space}{self.lhs} = {rhs}\n"]

    def has_assignment_to(self, var: str) -> bool:
        lhs_name = self.lhs.split("(")[0].strip().lower()
        return lhs_name == var.lower()

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        lhs_name = self.lhs.split("(")[0].strip()
        _append_unique(res, lhs_name)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        lhs_base = self.lhs.split("(")[0].strip()
        needed = [n for n in (names or []) if n != lhs_base]
        ignore_self = self.accumulate or self._detect_self_add()
        for n in self.rhs_names:
            if ignore_self and n == lhs_base:
                continue
            if n not in needed:
                needed.append(n)
        return needed

    def remove_initial_self_add(self, var: str) -> int:
        lhs_base = self.lhs.split("(")[0].strip()
        if lhs_base != var:
            return 1
        if not self.accumulate:
            return 2
        self.accumulate = False
        return 3


@dataclass
class Routine(Node):
    """Common functionality for ``subroutine`` and ``function`` blocks."""

    name: str
    args: str = ""
    decls: Block = field(default_factory=Block)
    body: Block = field(default_factory=Block)
    kind: ClassVar[str] = "subroutine"

    def iter_children(self) -> Iterator[Node]:
        return iter([self.decls, self.body])

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        args = f"({self.args})" if self.args else "()"
        lines = [f"{space}{self.kind} {self.name}{args}\n"]
        lines.extend(self.decls.render(indent + 1))
        lines.append("\n")
        lines.extend(self.body.render(indent + 1))
        lines.append(f"{space}end {self.kind} {self.name}\n")
        return lines

    def is_effectively_empty(self) -> bool:
        return self.decls.is_effectively_empty() and self.body.is_effectively_empty()

    def has_assignment_to(self, var: str) -> bool:
        return self.decls.has_assignment_to(var) or self.body.has_assignment_to(var)

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = self.decls.assigned_vars(names)
        return self.body.assigned_vars(res)

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        needed = self.body.required_vars(needed)
        needed = self.decls.required_vars(needed)
        return needed

    def prune_for(self, targets: Iterable[str]) -> "Routine":
        return type(self)(
            self.name,
            self.args,
            self.decls.prune_for(targets),
            self.body.prune_for(targets),
        )

    def defined_var_names(self) -> List[str]:
        names: List[str] = []
        for node in self.decls.children:
            if isinstance(node, Declaration):
                names.append(node.name)
        return names

    def remove_initial_self_add(self, var: str) -> int:
        return self.body.remove_initial_self_add(var)


@dataclass
class Subroutine(Routine):
    """A ``subroutine`` with declaration and execution blocks."""

    kind: ClassVar[str] = "subroutine"


@dataclass
class Function(Routine):
    """A ``function`` with declaration and execution blocks."""

    kind: ClassVar[str] = "function"




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
        if self.intent in ("in", "inout"):
            return [n for n in (names or []) if n != self.name]
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
        needed = list(names or [])
        branches = [self.body] + [blk for _, blk in self.elif_blocks]
        if self.else_body is not None:
            branches.append(self.else_body)

        if branches:
            common_assigned = set(branches[0].assigned_vars([]))
            for blk in branches[1:]:
                common_assigned &= set(blk.assigned_vars([]))
            needed = [n for n in needed if n not in common_assigned]

        res: List[str] = []
        for n in _extract_names(self.condition):
            _append_unique(res, n)
        for blk in branches:
            req = blk.required_vars(needed)
            for n in req:
                _append_unique(res, n)
        return res

    def prune_for(self, targets: Iterable[str]) -> "Node":
        new_body = self.body.prune_for(targets)
        new_elifs = [(c, blk.prune_for(targets)) for c, blk in self.elif_blocks]
        new_else = self.else_body.prune_for(targets) if self.else_body is not None else None
        if (
            new_body.is_effectively_empty()
            and all(b.is_effectively_empty() for _, b in new_elifs)
            and (new_else is None or new_else.is_effectively_empty())
        ):
            return Block([])
        return IfBlock(self.condition, new_body, new_elifs, new_else)

    def remove_initial_self_add(self, var: str) -> int:
        branches = [self.body] + [blk for _, blk in self.elif_blocks]
        if self.else_body is not None:
            branches.append(self.else_body)
        results = [b.remove_initial_self_add(var) for b in branches]
        if all(r == 3 for r in results):
            return 3
        if any(r != 1 for r in results):
            return 2
        return 1


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
        needed = list(names or [])
        blocks = [blk for _, blk in self.cases]
        if self.default is not None:
            blocks.append(self.default)

        if blocks:
            common_assigned = set(blocks[0].assigned_vars([]))
            for blk in blocks[1:]:
                common_assigned &= set(blk.assigned_vars([]))
            needed = [n for n in needed if n not in common_assigned]

        res: List[str] = []
        for n in _extract_names(self.expr):
            _append_unique(res, n)
        for blk in blocks:
            req = blk.required_vars(needed)
            for n in req:
                _append_unique(res, n)
        return res

    def prune_for(self, targets: Iterable[str]) -> "Node":
        new_cases = [(c, blk.prune_for(targets)) for c, blk in self.cases]
        new_default = self.default.prune_for(targets) if self.default is not None else None
        if all(blk.is_effectively_empty() for _, blk in new_cases) and (
            new_default is None or new_default.is_effectively_empty()
        ):
            return Block([])
        return SelectBlock(self.expr, new_cases, new_default)

    def remove_initial_self_add(self, var: str) -> int:
        blocks = [blk for _, blk in self.cases]
        if self.default is not None:
            blocks.append(self.default)
        if not blocks:
            return 1
        results = [b.remove_initial_self_add(var) for b in blocks]
        if all(r == 3 for r in results):
            return 3
        if any(r != 1 for r in results):
            return 2
        return 1


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
        needed = self.body.required_vars(list(names or []))
        tokens = [t for t in _extract_names(self.header) if t.lower() != "do"]
        for t in (tokens[1:] if tokens else _extract_names(self.header)):
            if t not in needed:
                needed.append(t)
        return needed

    def prune_for(self, targets: Iterable[str]) -> "Node":
        new_body = self.body.prune_for(targets)
        if new_body.is_effectively_empty():
            return Block([])
        return DoLoop(self.header, new_body)

    def remove_initial_self_add(self, var: str) -> int:
        if self.body.has_assignment_to(var):
            return 2
        return 1


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


