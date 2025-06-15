"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
import re
import copy

from .operators import (
    OpVar,
)

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


def variable_from_expr(expr: str, typename: str = "") -> "Variable":
    """Create :class:`Variable` from ``expr`` splitting any indexing part."""
    base = expr.split("(", 1)[0].strip()
    dim = ""
    idx = expr.find("(")
    if idx != -1:
        dim = expr[idx:].strip()
    return Variable(base, typename, None, dim or None)


@dataclass
class Variable:
    """Representation of a Fortran variable."""

    name: str
    typename: str
    kind: Optional[str] = None
    dims: Optional[str] = None
    intent: Optional[str] = None

    def __post_init__(self) -> None:
        if self.dims is not None and self.dims == "":
            raise ValueError("dimension must not be empty")
        if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", self.name):
            raise ValueError(f"invalid Fortran variable name: {self.name}")

    def is_array(self) -> bool:
        """Return ``True`` if this variable represents an array."""
        return self.dims is not None

    def to_decl(self) -> Declaration:
        """Return declaration node corresponding to self."""
        return Declaration(self.name, self.typename, self.kind, self.dims, self.intent)

    def __str__(self) -> str:
        return self.name + (self.dims or "")


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

    def convert_assignments(self, func, reverse= False) -> Node:
        """New with converted assignment nodes."""
        return None

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

    def check_initial(self, var: str) -> int:
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

    def convert_assignments(self, func, reverse=False) -> Node:
        children = []
        iterator = self.children
        if reverse:
            iterator = reversed(iterator)
        for node in iterator:
            res = node.convert_assignments(func, reverse)
            if res is not None:
                children.append(res)
        return Block(children)

    def find_by_name(self, name: str) -> None:
        for child in self.iter_children():
            if child.name == name:
                return child
        return None

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

    def check_initial(self, var: str) -> int:
        for child in self.children:
            if child.has_assignment_to(var):
                return child.check_initial(var)
        return 1

    def remove_redundant_inits(self, init_block: "Block", keep: Iterable[str]) -> None:
        """Remove redundant gradient initializations using node helpers."""
        for idx, node in enumerate(list(init_block.children)):
            base = node.lhs.name
            if base in keep:
                continue
            self.remove_by_id(node.get_id())
            res = self.check_initial(base)
            if res == 1:
                self.insert(idx, node)


@dataclass
class Statement(Node):
    """Representation of a Fortran statement."""
    body: str

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}{self.body}"]
        return lines


@dataclass
class Module(Node):
    """Representation of a Fortran module."""
    name: str
    body: Block = field(default_factory=Block)
    routines: Block = field(default_factory=Block)

    def __post_init__(self):
        super().__post_init__()
        self.body.append(Statement("implicit none"))

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}module {self.name}\n"]
        lines.extend(self.body.render(indent+1))
        lines.append("\n")
        lines.append(f"{space}contains\n")
        lines.append("\n")
        for routine in self.routines:
            lines.extend(routine.render(indent+1))
            lines.append("\n")
        lines.append(f"{space}end module {self.name}\n")
        return lines


@dataclass
class Routine(Node):
    """Common functionality for ``subroutine`` and ``function`` blocks."""

    name: str
    args: List[str]
    result: Optional[str] = None
    decls: Block = field(default_factory=Block)
    content: Block = field(default_factory=Block)
    ad_init: Optional[Block] = None
    ad_content: Optional[Block] = None
    kind: ClassVar[str] = "subroutine"

    def _all_blocks():
        blocks = [self.decls, self.content]
        if self.ad_init is not None:
            blocks.append(self.ad_init)
        if self.ad_content is not None:
            blocks.append(self.ad_content)

    def iter_children(self) -> Iterator[Node]:
        return iter(_all_blocks())

    def convert_assignments(self: T, func, reverse=False) -> T:
        name = self.name.copy
        args = self.args.copy
        result = self.result.copy
        decls = self.decls.deep_copy
        content = self.content.convert_assignments(func, reverse)
        ad_init = self.ad_init.convert_assignments(func, reverse)
        ad_content = self.ad_content.convert_assignments(func, reverse)
        return type(self)(name, args, result, decls, content, ad_init, ad_content)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        args = ", ".join(self.args)
        lines = [f"{space}{self.kind} {self.name}({args})\n"]
        lines.extend(self.decls.render(indent+1))
        lines.append("\n")
        if not self.content.is_effectively_empty():
            lines.extend(self.content.render(indent+1))
            lines.append("\n")
        if self.ad_init is not None and not self.ad_init.is_effectively_empty():
            lines.extend(self.ad_init.render(indent+1))
            lines.append("\n")
        if self.ad_content is not None and not self.ad_content.is_effectively_empty():
            lines.extend(self.ad_content.render(indent+1))
            lines.append("\n")
        lines.append(f"{space}  return\n")
        lines.append(f"{space}end {self.kind} {self.name}\n")
        return lines

    def get_var(self, name: str) -> Variable:
        decl = self.decls.find_by_name(name)
        if decl is None:
            raise ValueError(f"variable ({name}) is not found in declarations")
        intent = decl.intent
        if self.result == name:
            intent = "out"
        return Variable(name, decl.typename, decl.kind, decl.dims, intent)

    def arg_vars(self) -> List[Variable]:
        vars = []
        for arg in self.args:
            vars.append(self.get_var(arg))
        if self.result is not None:
            vars.append(self.get_var(self.result))
        return vars

    def is_effectively_empty(self) -> bool:
        return all(block.is_effectively_empty() for block in self._all_blocks())

    def has_assignment_to(self, var: str) -> bool:
        return any(block.has_assignment_to(var) for block in self._all_blocks())

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = name
        for block in self._all_blocks():
            res = block.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = name
        for block in self._all_blocks():
            res = block.required_vars(res)
        return res

    def prune_for(self, targets: Iterable[str]) -> "Routine":
        return type(self)(
            self.name,
            self.args,
            self.decls.prune_for(targets),
            self.content.prune_for(targets),
            self.ad_init or self.ad_init.prune_for(targets),
            self.ad_content or self.ad_content.prune_for(targets),
        )

    def defined_var_names(self) -> List[str]:
        names: List[str] = []
        for node in self.decls.children:
            if isinstance(node, Declaration):
                names.append(node.name)
        return names

    def expand_decls(self, decls: Block) -> "Routine":
        self.decls.expand(decls)
        return self

    def check_initial(self, var: str) -> int:
        return self.body.check_initial(var)


@dataclass
class Subroutine(Routine):
    """A ``subroutine`` with declaration and execution blocks."""

    kind: ClassVar[str] = "subroutine"


@dataclass
class Function(Routine):
    """A ``function`` with declaration and execution blocks."""

    result: str
    kind: ClassVar[str] = "function"


@dataclass
class Assignment(Node):
    """An assignment statement ``lhs = rhs``."""

    lhs: OpVar
    rhs: Operation
    accumulate: bool = False
    info: Optional[dict] = None
    rhs_names: List[str] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.lhs, OpVar):
            raise ValueError(f"lhs must be OpVar: {type(self.lhs)}")
        self.rhs_names = self.rhs.collect_vars

    def _detect_self_add(self) -> bool:
        lhs_base = self.lhs.name.lower()
        parts = [p.strip().lower() for p in self.rhs.split("+")]
        if len(parts) != 2:
            return False
        left_base = parts[0].split("(")[0].strip()
        right_base = parts[1].split("(")[0].strip()
        return lhs_base in (left_base, right_base)

    def convert_assignments(self, func, reverse=False) -> List[Assignment]:
        return func(self.lhs, self.rhs, self.info)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        rhs = self.rhs
        if self.accumulate:
            rhs = f"{rhs} + {self.lhs}"
        return [f"{space}{self.lhs} = {rhs}\n"]

    def has_assignment_to(self, var: str) -> bool:
        lhs_name = self.lhs.name.lower()
        return lhs_name == var.lower()

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        lhs_name = self.lhs.name
        _append_unique(res, lhs_name)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        lhs_base = self.lhs.name
        needed = [n for n in (names or []) if n != lhs_base]
        ignore_self = self.accumulate or self._detect_self_add()
        for n in self.rhs_names:
            if ignore_self and n == lhs_base:
                continue
            if n not in needed:
                needed.append(n)
        return needed

    def check_initial(self, var: str) -> int:
        lhs_base = self.lhs.name
        if lhs_base != var:
            return 1
        if not self.accumulate:
            return 2
        self.accumulate = False
        return 3


@dataclass
class Declaration(Node):
    """A variable declaration."""

    name: str
    typename: str
    kind: Optional[str] = None
    dims: Optional[str] = None
    intent: Optional[str] = None

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        line = f"{space}{self.typename}"
        if self.kind is not None:
            line += f"({self.kind})"
        if self.intent is not None:
            pad = "  " if self.intent == "in" else " "
            line += f", intent({self.intent})" + pad + f":: {self.name}"
        else:
            line += f" :: {self.name}"
        if self.dims:
            line += self.dims
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

    def _all_blocks() -> List[Block]:
        blocks = [self.body]
        for _, block in self.elif_blocks:
            blocks.append(block)
        if self.else_body is not None:
            blocks.append(self.else_body)

    def iter_children(self) -> Iterator[Node]:
        iter(_all_blocks)

    def convert_assignments(self, func, reverse=False) -> Node:
        condition = self.condition.copy
        body = self.body.convert_assignments(func, reverse)
        elif_blocks = Block([])
        for cond, block in self.elif_blocks:
            res = block.convert_assignments(func, reverse)
            if res is None:
                res = Block([])
            elif_blocks.append((cond, res))
        else_block = None
        if self.else_block is not None:
            else_block = self.else_block.convert_assignments(func, reverse)
        return IfBlock(condition, body, elif_blocks, else_block)

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
        for block in _all_blocks():
            if not bolock.is_effectively_empty():
                return False
        return True

    def has_assignment_to(self, var: str) -> bool:
        for block in _all_blocks():
            if block.has_assignment_to(var):
                return True
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for block in _all_blocks():
            res = block.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        reqs = []
        for branch in _all_blocks():
            for var in branch.required_vars(needed):
                _append_unique(reqs, var)
        for var in _extract_names(self.condition):
            _append_unique(reqs, var)
        return reqs

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

    def check_initial(self, var: str) -> int:
        branches = [self.body] + [blk for _, blk in self.elif_blocks]
        if self.else_body is not None:
            branches.append(self.else_body)
        results = [b.check_initial(var) for b in branches]
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

    def convert_assignments(self, func, reverse=False) -> Node:
        expr = self.expr.copy
        cases = []
        for cond, block in self.cases:
            cases.append((cond, block.convert_assignments(func, reverse)))
        default = None
        if self.default is not None:
            default = default.convert_assignments(func, reverse)
        return SelectBlock(expr, cases, default)

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

    def check_initial(self, var: str) -> int:
        blocks = [blk for _, blk in self.cases]
        if self.default is not None:
            blocks.append(self.default)
        if not blocks:
            return 1
        results = [b.check_initial(var) for b in blocks]
        if all(r == 3 for r in results):
            return 3
        if any(r != 1 for r in results):
            return 2
        return 1


@dataclass
class DoLoop(Node):
    """A ``do`` loop."""

    body: Block
    index: str
    start: str
    end: str
    step: Optional[str] = None

    def iter_children(self) -> Iterator[Node]:
        yield self.body

    def convert_assignments(self, func, reverse=False) -> Node:
        body = self.body.convert_assignments(func, reverse)
        index = self.index.copy
        start = self.end.copy
        end = self.start.copy
        step = None
        if self.step is not None:
            step = f"-({step.copy})"
        return DoLoop(body, index, start, end, step)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        header = f"{space}do {self.index} = {self.start}, {self.end}"
        if self.step is not None:
            header = f"{header}, #{self.step}"
        lines = [ f"{header}\n"]
        lines.extend(self.body.render(indent+1))
        lines.append(f"{space}end do\n")
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

    def check_initial(self, var: str) -> int:
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
