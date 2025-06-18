"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
import re
import copy

from .operators import (
    Operator,
    OpVar,
    OpInt,
)

_NAME_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")


def _append_unique(items: List[str], name: str) -> None:
    if name not in items:
        items.append(name)


@dataclass
class Variable:
    """Representation of a Fortran variable."""

    name: str
    typename: str
    kind: Optional[str] = None
    dims: Optional[Tuple[str]] = None
    intent: Optional[str] = None
    ad_target: Optional[bool] = None

    def __post_init__(self) -> None:
        if not re.fullmatch(r"[A-Za-z][A-Za-z0-9_]*", self.name):
            raise ValueError(f"invalid Fortran variable name: {self.name}")
        if self.dims is not None and not isinstance(self.dims, tuple):
            raise ValueError(f"dims must be tuple of str: {type(self.dims)}")
        if self.dims is not None and len(self.dims) == 0:
            raise ValueError("dimension must not be empty")
        if self.ad_target is None:
            typename = self.typename.lower()
            self.ad_target = typename.startswith("real") or typename.startswith("double")

    def is_array(self) -> bool:
        """Return ``True`` if this variable represents an array."""
        return self.dims is not None

    def to_decl(self) -> Declaration:
        """Return declaration node corresponding to self."""
        return Declaration(self.name, self.typename, self.kind, self.dims, self.intent)

    def __str__(self) -> str:
        if self.dims is None:
            return self.name
        else:
            dims = ",".join(self.dims)
            return f"{self.name}({dims})"


@dataclass
class Node:
    """Abstract syntax tree node for Fortran code fragments."""

    id: int = field(init=False, repr=False)
    do_index_list: List[str] = field(init=False, default=None)

    _id_counter = 0

    def __post_init__(self):
        self.id = Node._id_counter
        Node._id_counter += 1

    def build_do_index_list(self, list: List[str]) -> None:
        self.do_index_list = list
        for child in self.iter_children():
            child.build_do_index_list(list)

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

    def convert_assignments(self, func, reverse= False) -> List["Node"]:
        """New with converted assignment nodes."""
        return []

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

    def collect_vars(self) -> List[str]:
        """Return variables used in this node."""
        return list([])

    # ------------------------------------------------------------------
    # optimization helpers
    # ------------------------------------------------------------------

    def prune_for(self, targets: Iterable[str]) -> "Node":
        """Return a copy of this node with only code needed for ``targets``."""
        return self.deep_clone()

    def check_array_index(var: str, index: str) -> bool:
        """Check all the reference to array ``var`` are with the same index ``index``"""
        return True

    def check_initial(self, var: str) -> int:
        """Remove self-add from the first assignment to ``var`` if safe.

        Returns ``0`` for not-found, ``1`` or ``2`` for assignment w/o self-reference and ``-1`` for assignment w/ self-reference.
        ``2`` indicates that a self-reference was removed.
        """
        return 0


@dataclass
class Block(Node):
    """A container for a sequence of nodes."""

    children: List[Node] = field(default_factory=list)

    def iter_children(self) -> Iterator[Node]:
        return iter(self.children)

    def convert_assignments(self, func, reverse=False) -> List[Node]:
        children = []
        iterator = self.children
        if reverse:
            iterator = reversed(iterator)
        for node in iterator:
            for res in node.convert_assignments(func, reverse):
                if res is not None and not res.is_effectively_empty():
                    children.append(res)
        return [Block(children)]

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

    def collect_vars(self) -> List[str]:
        vars = []
        for child in self.children:
            for var in child.collect_vars():
                _append_unique(vars, var)
        return vars

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

    def check_array_index(self, var: str, index: str) -> bool:
        for child in self.iter_children():
            if not child.check_array_index(var, index):
                return False
        return True

    def check_initial(self, var: str) -> int:
        for child in self.children:
            if child.has_assignment_to(var):
                ret = child.check_initial(var)
                if ret != 0:
                    return ret
        return 0


@dataclass
class Statement(Node):
    """Representation of a Fortran statement."""
    body: str

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}{self.body}\n"]
        return lines


@dataclass
class Module(Node):
    """Representation of a Fortran module."""
    name: str
    body: Block = field(default_factory=Block)
    routines: Block = field(default_factory=Block)

    def iter_children(self):
        yield self.body
        for routine in self.routines:
            yield routine

    def __post_init__(self):
        super().__post_init__()

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

    def _all_blocks(self):
        blocks = [self.decls, self.content]
        if self.ad_init is not None:
            blocks.append(self.ad_init)
        if self.ad_content is not None:
            blocks.append(self.ad_content)
        return blocks

    def iter_children(self) -> Iterator[Node]:
        return iter(self._all_blocks())

    def convert_assignments(self: T, func, reverse=False) -> List[T]:
        name = self.name
        args = self.args.copy
        result = self.result
        decls = self.decls.deep_copy
        content = self.content.convert_assignments(func, reverse)[0]
        ad_init = self.ad_init.convert_assignments(func, reverse)[0]
        ad_content = self.ad_content.convert_assignments(func, reverse)[0]
        return [type(self)(name, args, result, decls, content, ad_init, ad_content)]

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

    def get_nondiff_vars(self) -> List[str]:
        vars = []
        for decl in self.decls:
            if decl.typename == "integer" or decl.typename == "character":
                vars.append(decl.name)
        return vars

    def arg_vars(self) -> List[Variable]:
        vars = []
        for arg in self.args:
            vars.append(self.get_var(arg))
        if self.result is not None:
            vars.append(self.get_var(self.result))
        return vars

    def is_effectively_empty(self) -> bool:
        return all(block.is_effectively_empty() for block in self.iter_children())

    def has_assignment_to(self, var: str) -> bool:
        return any(block.has_assignment_to(var) for block in self.iter_children())

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = names
        for block in self.iter_children():
            res = block.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = names
        for block in reversed(self._all_blocks()):
            res = block.required_vars(res)
        return res

    def collect_vars(self) -> List[str]:
        vars = []
        for block in self.iter_children():
            for var in block.collect_vars():
                _append_unique(vars, var)
        return vars

    def defined_var_names(self) -> List[str]:
        names: List[str] = []
        for node in self.decls.children:
            if isinstance(node, Declaration):
                names.append(node.name)
        return names

    def expand_decls(self, decls: Block) -> "Routine":
        self.decls.expand(decls)
        return self

    def prune_for(self, targets: Iterable[str]) -> "Routine":
        return type(self)(
            self.name,
            self.args,
            self.decls.prune_for(targets),
            self.body.prune_for(targets),
        )

    def check_initial(self, var: str) -> int:
        for block in [self.content, self.ad_init, self.ad_content]:
            ret = block.check_initial(var)
            if ret != 0:
                return ret
        return 0


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
    rhs: Operator
    accumulate: bool = False
    info: Optional[dict] = None
    rhs_names: List[str] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.lhs, OpVar):
            raise ValueError(f"lhs must be OpVar: {type(self.lhs)}")
        if not isinstance(self.rhs, Operator):
            raise ValueError(f"rhs must be Operator: {type(self.rhs)}")
        self.rhs_names = []
        for var in self.rhs.collect_vars():
            self.rhs_names.append(var.name)

    def convert_assignments(self, func, reverse=False) -> List[Assignment]:
        return func(self.lhs, self.rhs, self.info)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        rhs = self.rhs
        if self.accumulate:
            rhs = rhs + self.lhs
        if self.lhs == rhs:
            return []
        return [f"{space}{self.lhs} = {rhs}\n"]

    def has_assignment_to(self, var: str) -> bool:
        lhs_name = self.lhs.name.lower()
        return lhs_name == var.lower()

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        _append_unique(res, self.lhs.name)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        lhs_base = self.lhs.name
        needed = [n for n in (names or []) if n != lhs_base]
        for var in self.lhs.collect_vars():
            if var.name != lhs_base:
                _append_unique(needed, var.name)
        for var in self.rhs_names:
            _append_unique(needed, var)
        if self.accumulate:
            _append_unique(needed, lhs_base)
        return needed

    def collect_vars(self) -> List[str]:
        vars = []
        for var in self.lhs.collect_vars():
            _append_unique(vars, var.name)
        for var in self.rhs_names:
            _append_unique(vars, var)
        return vars

    def check_array_index(self, var: str, index: str) -> bool:
        for v in self.lhs.collect_vars():
            if v.name == var and v.index_str() != index:
                return False
        for v in self.rhs.collect_vars():
            if v.name == var and v.index_str() != index:
                return False
        return True

    def check_initial(self, var: str) -> int:
        if self.lhs.name != var:
            return 0
        if var in self.rhs_names:
            return -1
        if not self.accumulate:
            return 1
        self.accumulate = False
        return 2


@dataclass
class Declaration(Node):
    """A variable declaration."""

    name: str
    typename: str
    kind: Optional[str] = None
    dims: Optional[Tuple[str]] = None
    intent: Optional[str] = None

    def __post_init__(self):
        super().__post_init__()
        if self.dims is not None and not isinstance(self.dims, tuple):
            raise ValueError(f"dims must be tuple of str: {type(dims)}")

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
        if self.dims is not None:
            dims = ",".join(self.dims)
            line += f"({dims})"
        line += "\n"
        return [line]

    def is_real(self) -> bool:
        typename = self.typename.lower()
        return typename.startswith("real") or typename.startswith("double")

    def has_assignment_to(self, var: str) -> bool:
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        if self.intent in ("in", "inout"):
            _append_unique(res, self.name)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        if self.intent in ("in", "inout"):
            if self.name in needed:
                needed.remove(self.name)
        return needed

@dataclass
class IfBlock(Node):
    """An ``if`` block with optional ``else if`` branches and ``else``."""

    condition: Operator
    body: Block
    elif_bodies: List[Tuple[Operator, Block]] = field(default_factory=list)
    else_body: Optional[Block] = None

    def iter_children(self) -> Iterator[Node]:
        yield self.body
        for _, block in self.elif_bodies:
            yield block
        if self.else_body is not None:
            yield self.else_body

    def convert_assignments(self, func, reverse=False) -> [IfBlock]:
        condition = self.condition
        body = self.body.convert_assignments(func, reverse)[0]
        elif_bodies = Block([])
        for cond, block in self.elif_bodies:
            res = block.convert_assignments(func, reverse)[0]
            if res is None:
                res = Block([])
            elif_bodies.append((cond, res))
        else_body = None
        if self.else_body is not None:
            else_body = self.else_body.convert_assignments(func, reverse)[0]
        return [IfBlock(condition, body, elif_bodies, else_body)]

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}if ({self.condition}) then\n"]
        lines.extend(self.body.render(indent+1))
        for cond, blk in self.elif_bodies:
            lines.append(f"{space}else if ({cond}) then\n")
            lines.extend(blk.render(indent+1))
        if self.else_body is not None:
            lines.append(f"{space}else\n")
            lines.extend(self.else_body.render(indent+1))
        lines.append(f"{space}end if\n")
        return lines

    def is_effectively_empty(self) -> bool:
        for block in self.iter_children():
            if not block.is_effectively_empty():
                return False
        return True

    def has_assignment_to(self, var: str) -> bool:
        for block in self.iter_children():
            if block.has_assignment_to(var):
                return True
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for block in self.iter_children():
            res = block.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        reqs = []
        for branch in self.iter_children():
            for var in branch.required_vars(needed):
                _append_unique(reqs, var)
        for var in self.condition.collect_vars():
            _append_unique(reqs, var.name)
        return reqs

    def collect_vars(self) -> List[str]:
        vars = []
        for branch in self.iter_children():
            for var in branch.collect_vars():
                _append_unique(vars, var)
        for var in self.condition.collect_vars():
            _append_unique(vars, var.name)
        return vars

    def prune_for(self, targets: Iterable[str]) -> Node:
        new_body = self.body.prune_for(targets)
        new_elifs = [(c, blk.prune_for(targets)) for c, blk in self.elif_bodies]
        new_else = self.else_body.prune_for(targets) if self.else_body is not None else None
        if (
            new_body.is_effectively_empty()
            and all(b.is_effectively_empty() for _, b in new_elifs)
            and (new_else is None or new_else.is_effectively_empty())
        ):
            return Block([])
        return IfBlock(self.condition, new_body, new_elifs, new_else)

    def check_array_index(self, var: str, index: str) -> bool:
        for v in self.condition.collect_vars():
            if v.name == str and v.index_str() != index:
                return False
        if not self.body.check_array_index(var, index):
            return False
        for cond, block in self.elif_bodies:
            for v in cond.collect_vars():
                if v.name == str and v.index_str() != index:
                    return False
            if not block.check_array_index(var, index):
                return False
        if self.else_body is not None:
            if not self.else_body.check_array_index(var, index):
                return False
        return True

    def check_initial(self, var: str) -> int:
        results = [b.check_initial(var) for b in self.iter_children()]
        if all(r > 0 for r in results):
            if any(r == 2 for r in results):
                return 2
            else:
                return 1
        if any(r < 0 for r in results):
            return -1
        return 0


@dataclass
class SelectBlock(Node):
    """A ``select case`` construct."""

    expr: Operator
    cases: List[Tuple[Operator, Block]] = field(default_factory=list)
    default: Optional[Block] = None

    def iter_children(self) -> Iterator[Node]:
        for _, blk in self.cases:
            yield blk
        if self.default is not None:
            yield self.default

    def convert_assignments(self, func, reverse=False) -> [SelectBlock]:
        expr = self.expr
        cases = []
        for cond, block in self.cases:
            cases.append((cond, block.convert_assignments(func, reverse)[0]))
        default = None
        if self.default is not None:
            default = self.default.convert_assignments(func, reverse)[0]
        return [SelectBlock(expr, cases, default)]

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}select case ({self.expr})\n"]
        for cond, block in self.cases:
            conds = ', '.join([str(co) for co in cond])
            lines.append(f"{space}case ({conds})\n")
            lines.extend(block.render(indent+1))
        if self.default is not None:
            lines.append(f"{space}case default\n")
            lines.extend(self.default.render(indent+1))
        lines.append(f"{space}end select\n")
        return lines

    def is_effectively_empty(self) -> bool:
        for block in self.iter_children():
            if not block.is_effectively_empty():
                return False
        return True

    def has_assignment_to(self, var: str) -> bool:
        for block in self.iter_children():
            if block.has_assignment_to(var):
                return True
        return False

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        for block in self.iter_children():
            res = block.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        common_assigned = set()
        for block in self.iter_children():
            common_assigned &= set(block.assigned_vars())
        needed = [n for n in needed if n not in common_assigned]

        res = [var.name for var in self.expr.collect_vars()]
        for block in self.iter_children():
            req = block.required_vars(needed)
            for n in req:
                _append_unique(res, n)
        return res

    def collect_vars(self) -> List[str]:
        vars = [var.name for var in self.expr.collect_vars()]
        for block in self.iter_children():
            for var in block.collect_vars():
                _append_unique(vars, var)
        return vars

    def prune_for(self, targets: Iterable[str]) -> Node:
        new_cases = [(c, blk.prune_for(targets)) for c, blk in self.cases]
        new_default = self.default.prune_for(targets)
        if all(b.is_effectively_empty() for _, b in new_cases) and new_default.is_effectively_empty():
            return Block([])
        if new_default.is_effectively_empty():
            new_default = None
        return SelectBlock(self.expr, new_cases, new_default)

    def check_array_index(self, var: str, index: str) -> bool:
        for cond, block in self.cases:
            for v in cond.collect_vars():
                if cond.name == str and cond.index_str() != index:
                    return False
            if not block.check_array_index(var, index):
                return False
        if self.default is not None:
            if not self.default.check_array_index(var, index):
                return False
        return True
        
    def check_initial(self, var: str) -> int:
        results = [b.check_initial(var) for b in self.iter_children()]
        if all(r > 0 for r in results):
            if any(r == 2 for r in results):
                return 2
            else:
                return 1
        if any(r < 0 for r in results):
            return -1
        return 0


@dataclass
class DoAbst(Node):

    body: Block

    def iter_children(self) -> Iterator[Node]:
        yield self.body

    def is_effectively_empty(self) -> bool:
        return self.body.is_effectively_empty()

@dataclass
class DoLoop(DoAbst):
    """A ``do`` loop."""

    index: OpVar
    start: Operaion
    end: Operaion
    step: Optional[Operator] = None

    def build_do_index_list(self, list: List[str]):
        self.do_index_list = [self.index.name]
        self.do_index_list.extend(list)
        self.body.build_do_index_list(self.do_index_list)

    def convert_assignments(self, func, reverse=False) -> [DoLoop]:
        body = self.body.convert_assignments(func, reverse)[0]
        required_vars = body.required_vars()
        new_body = self.body.prune_for(required_vars)
        if not new_body.is_effectively_empty():
            for node in body.children:
                new_body.append(node)
            body = new_body

        index = self.index
        if reverse:
            start = self.end
            end = self.start
            if self.step is not None:
                step = - self.step
            else:
                step = - OpInt(1)
        else:
            start = self.start
            end = self.end
            step = self.step
        return [DoLoop(body, index, start, end, step)]

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        header = f"{space}do {self.index} = {self.start}, {self.end}"
        if self.step is not None:
            header = f"{header}, {self.step}"
        lines = [f"{header}\n"]
        lines.extend(self.body.render(indent+1))
        lines.append(f"{space}end do\n")
        return lines

    def has_assignment_to(self, var: str) -> bool:
        return self.body.has_assignment_to(var) or self.index.name==var

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        if not self.index.name in res:
            res.append(self.index.name)
        res = self.body.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = self.body.required_vars(list(names or []))
        if self.index.name in needed:
            needed.remove(self.index.name)
        for op in [self.start, self.end, self.step]:
            if op is not None:
                for var in op.collect_vars():
                    _append_unique(needed, var.name)
        return needed

    def collect_vars(self) -> List[str]:
        vars = self.body.collect_vars()
        _append_unique(vars, self.index.name)
        for op in [self.start, self.end, self.step]:
            if op is not None:
                for var in op.collect_vars():
                    _append_unique(vars, var.name)
        return vars

    def prune_for(self, targets: Iterable[str]) -> Node:
        new_body = self.body.prune_for(targets)
        if new_body.is_effectively_empty():
            return Block([])
        return DoLoop(new_body, self.index, self.start, self.end, self.step)

    def check_array_index(self, var: str, index: str) -> bool:
        if self.do_index_list is None:
            raise RuntimeError("Execute build_do_index_list first.")
        for op in [self.start, self.end, self.step]:
            if op is not None:
                for v in op.collect_vars():
                    if v.name == var and v.index_str() != index:
                        return False
        index = ",".join(self.do_index_list)
        return self.body.check_array_index(str, index)

    def check_initial(self, var: str) -> int:
        if self.do_index_list is None:
            raise RuntimeError("Execute build_do_index_list first.")
        if self.body.has_assignment_to(var):
            index = ",".join(self.do_index_list)
            if self.body.check_array_index(var, index):
                return self.body.check_initial(var)
            return -1
        return 0

@dataclass
class DoWhile(DoAbst):
    """A ``do while`` loop."""

    cond: OpVar

    def convert_assignments(self, func, reverse=False) -> [DoWhile]:
        body = self.body.convert_assignments(func, reverse)[0]
        return [DoWhile(body, self.cond)]

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}do while ({self.cond})\n"]
        lines.extend(self.body.render(indent+1))
        lines.append(f"{space}end do\n")
        return lines

    def has_assignment_to(self, var: str) -> bool:
        return self.body.has_assignment_to(var)

    def assigned_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = list(names or [])
        res = self.body.assigned_vars(res)
        return res

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = self.body.required_vars(list(names or []))
        for var in self.cond.collect_vars():
            _append_unique(needed, var.name)
        return needed

    def collect_vars(self) -> List[str]:
        vars = self.body.collect_vars()
        for var in self.cond.collect_vars():
            _append_unique(vars, var.name)
        return vars

    def prune_for(self, targets: Iterable[str]) -> Node:
        new_body = self.body.prune_for(targets)
        if new_body.is_effectively_empty():
            return Block([])
        return DoWhild(new_body, self.cond)

    def check_array_index(self, var: str, index: str) -> bool:
        for v in self.cond.collect_vars():
            if v.name == var and v.index_str() != index:
                return False
        return self.body.check_array_index(str, index)

    def check_initial(self, var: str) -> int:
        if self.body.has_assignment_to(var):
            return -1
        return 0

def render_program(node: Node, indent: int = 0) -> str:
    """Return formatted Fortran code for the entire ``node`` tree."""

    lines = node.render(indent)
    return "".join(lines)
