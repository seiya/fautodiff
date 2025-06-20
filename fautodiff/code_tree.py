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

def _extend_unique(items: List[str], names: List[str]) -> None:
    for name in names:
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
        if not _NAME_RE.fullmatch(self.name):
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

    __id: int = field(init=False, repr=False)
    __parent: "Node" = field(init=False, repr=False, default=None)
    do_index_list: List[str] = field(init=False, repr=False, default=None)

    _id_counter = 0

    def __post_init__(self):
        self.__id = Node._id_counter
        Node._id_counter += 1

    # ------------------------------------------------------------------
    # basic node API
    # ------------------------------------------------------------------

    def render(self, indent: int = 0) -> List[str]:
        """Return the formatted Fortran code lines for this node."""
        raise NotImplementedError

    def is_effectively_empty(self) -> bool:
        """Return ``True`` if removing this node does not change execution."""
        return all(child.is_effectively_empty() for child in self.iter_children())

    def has_assignment_to(self, var: str) -> bool:
        """Return ``True`` if ``var`` is assigned within this node."""
        if any(var == v.name for v in self.iter_ref_vars()):
            return True
        if any(var == v.name for v in self.iter_assign_vars()):
            return True
        return any(child.has_assignment_to(var) for child in self.iter_children())

    # ------------------------------------------------------------------
    # node tree helpers
    # ------------------------------------------------------------------

    def iter_children(self) -> Iterator["Node"]:
        """Yield child nodes."""
        return iter(())

    def iter_ref_vars(self) -> Iterator[OpVar]:
        """Yield variables which is referenced in this node w/o children nodes."""
        return iter(())

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        """Yield variables which is assigned in this node w/o children nodes."""
        return iter(())

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse: bool = False) -> List["Node"]:
        """New with converted assignment nodes."""
        return []

    def _save_vars(self, var: OpVar, saved_vars: List[SaveAssignment]) -> SaveAssignment:
        id = self.get_id()
        save = SaveAssignment(var, id)
        self.__parent.insert_before(id, save)
        saved_vars.append(save)
        return save.to_load()

    def deep_clone(self) -> "Node":
        """Return a deep clone of this node tree with new ids."""
        clone = copy.deepcopy(self)
        Node._assign_new_ids(clone)
        return clone

    @classmethod
    def reset(cls) -> None:
        cls._id_counter = 0

    @classmethod
    def _assign_new_ids(cls, node: "Node") -> None:
        node.set_id(cls._id_counter)
        cls._id_counter += 1
        for child in node.iter_children():
            cls._assign_new_ids(child)

    def get_id(self) -> int:
        """Return this node's unique id."""
        return self.__id

    def set_id(self, id: int) -> int:
        """Set this node's unique id."""
        self.__id = id
        return self.__id

    def set_parent(self, node: "Node") -> "Node":
        self.__parent = node

    def find_by_id(self, node_id: int) -> Optional["Node"]:
        """Return the node with ``node_id`` from this subtree or ``None``."""
        if self.__id == node_id:
            return self
        for child in self.iter_children():
            found = child.find_by_id(node_id)
            if found is not None:
                return found
        return None

    def insert_before(self, id: int, node: "Node"):
        """Insert node to the before of node with id"""
        raise NotImplementedError

    def remove_by_id(self, node_id: int) -> bool:
        """Remove the node with ``node_id`` from this subtree.

        Returns ``True`` if a node was removed.
        """
        for child in self.iter_children():
            if child.get_id() == node_id:
                self.remove_child(child)
                return True
            if child.remove_by_id(node_id):
                return True
        return False

    def remove_child(self, child: "Node") -> None:
        """Remove ``child`` from this node. Override in subclasses."""
        # Only ``Block`` implements actual removal.
        pass

    # ------------------------------------------------------------------
    # variable analysis helpers
    # ------------------------------------------------------------------

    def assigned_vars(self, names: Optional[List[str]] = None, without_savevar: bool = False) -> List[str]:
        """Return variables assigned within this node and children."""
        res = list(names or [])
        for var in self.iter_assign_vars(without_savevar=without_savevar):
            _append_unique(res, var.name)
        for child in self.iter_children():
            res = child.assigned_vars(res, without_savevar=without_savevar)
        return res

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
        vars = []
        for child in self.iter_children():
            _extend_unique(vars, child.collect_vars())
        for var in self.iter_ref_vars():
            _append_unique(vars, var.name)
        for var in self.iter_assign_vars():
            _append_unique(vars, var.name)
        return vars

    def build_do_index_list(self, list: List[str]) -> None:
        """Build index list of do loop."""
        self.do_index_list = list
        for child in self.iter_children():
            child.build_do_index_list(list)

    # ------------------------------------------------------------------
    # optimization helpers
    # ------------------------------------------------------------------

    def prune_for(self, targets: Iterable[str]) -> "Node":
        """Return a copy of this node with only code needed for ``targets``."""
        return self.deep_clone()

    def check_array_index(self, var: str, index: str) -> bool:
        """Check all the reference to array ``var`` are with the same index ``index``"""
        for v in self.iter_ref_vars():
            if v.name == var and (v.index is not None and v.index_str() != index):
                return False
        for v in self.iter_assign_vars():
            if v.name == var and (v.index is not None and v.index_str() != index):
                return False
        for child in self.iter_children():
            if not child.check_array_index(var, index):
                return False
        return True

    def check_initial(self, var: str) -> int:
        """Remove self-add from the first assignment to ``var`` if safe.

        Returns ``0`` for not-found, ``1`` or ``2`` for assignment w/o self-reference and ``-1`` for assignment w/ self-reference.
        ``2`` indicates that a self-reference was removed.
        """
        for child in self.iter_children():
            ret = child.check_initial(var)
            if ret != 0:
                return ret
        return 0


@dataclass
class Block(Node):
    """A container for a sequence of nodes."""

    __children: List[Node] = field(default_factory=list)

    def iter_children(self) -> Iterator[Node]:
        return iter(self.__children)

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse: bool = False) -> List[Node]:
        children = []
        iterator = self.__children
        if reverse:
            iterator = reversed(iterator)
        for node in iterator:
            nodes = node.convert_assignments(saved_vars, func, reverse)
            for res in nodes:
                if res is not None and not res.is_effectively_empty():
                    children.append(res)
        return [Block(children)]

    def find_by_name(self, name: str) -> None:
        for child in self.iter_children():
            if child.name == name:
                return child
        return None

    def remove_child(self, child: Node) -> None:
        self.__children.remove(child)

    def first(self) -> Node:
        """Return the first element."""
        if len(self.__children) > 0:
            return self.__children[0]
        return None

    def last(self) -> Node:
        """Return the last element."""
        if len(self.__children) > 0:
            return self.__children[-1]
        return None

    def append(self, node: Node) -> None:
        """Append ``node`` to this block."""
        node.set_parent(self)
        self.__children.append(node)

    def extend(self, nodes: Iterable[Node]) -> None:
        """Extend this block with ``nodes``."""
        self.__children.extend(nodes)

    def insert_before(self, id: int, node:Node) -> Node:
        for i, child in enumerate(self.__children):
            if child.get_id() == id:
                return self.__children.insert(i, node)
        raise ValueError("id is not found")

    def __iter__(self):
        return self.iter_children()

    def __len__(self) -> int:
        return len(self.__children)

    def render(self, indent: int = 0) -> List[str]:
        lines: List[str] = []
        for child in self.iter_children():
            lines.extend(child.render(indent))
        return lines

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        for child in reversed(self.__children):
            needed = child.required_vars(needed)
        return needed

    def prune_for(self, targets: Iterable[str]) -> "Block":
        needed = list(targets)
        new_children: List[Node] = []
        for child in reversed(self.__children):
            if isinstance(child, Declaration):
                if (child.intent is not None) or (child.name in needed):
                    new_children.insert(0, child)
                    continue
            assigned = child.assigned_vars([])
            if any(v in needed for v in assigned):
                pruned = child.prune_for(needed)
                new_children.insert(0, pruned)
                needed = pruned.required_vars(needed)
        last = None
        for child in iter(new_children):
            if isinstance(last, SaveAssignment) and isinstance(child, SaveAssignment) and last.var == child.var and last.load != child.load:
                new_children.remove(last)
                new_children.remove(child)
            last = child
        return Block(new_children)


@dataclass
class Statement(Node):
    """Representation of a Fortran statement."""
    body: str

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}{self.body}\n"]
        return lines

    def is_effectively_empty(self) -> bool:
        return False

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

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse: bool = False) -> List[Routine]:
        raise RuntimeError("convert_assignments for Routine is not allowed")

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

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        res = names
        for block in reversed(self._all_blocks()):
            res = block.required_vars(res)
        return res

    def expand_decls(self, decls: Block) -> "Routine":
        self.decls.expand(decls)
        return self

    def prune_for(self, targets: Iterable[str]) -> "Routine":
        needed = list(targets)
        ad_content = self.ad_content.prune_for(needed)
        needed = ad_content.required_vars(needed)
        ad_init = self.ad_init.prune_for(needed)
        needed = ad_init.required_vars(needed)
        content = self.content.prune_for(needed)
        all_vars = content.collect_vars()
        _extend_unique(all_vars, ad_init.collect_vars())
        _extend_unique(all_vars, ad_content.collect_vars())
        decls = self.decls.prune_for(all_vars)
        return type(self)(name = self.name,
                          args = self.args,
                          result = self.result,
                          decls = decls,
                          content = content,
                          ad_init = ad_init,
                          ad_content = ad_content)


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
    info: Optional[dict] = field(repr=False, default=None)
    ad_info: Optional[str] = field(repr=False, default=None)
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

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self.rhs.collect_vars():
            yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        yield self.lhs

    def is_effectively_empty(self) -> bool:
        return self.lhs == self.rhs

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse=False) -> List[Assignment]:
        if isinstance(self, SaveAssignment):
            raise RuntimeError("AD code is beeing converted")
        assigns = [self._save_vars(self.lhs, saved_vars)]
        assigns.extend(func(self.lhs, self.rhs, self.info))
        return assigns

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        rhs = self.rhs
        if self.accumulate:
            rhs = rhs + self.lhs
        if self.lhs == rhs:
            return []
        ad_comment = ""
        if self.ad_info is not None:
            ad_comment = f" ! {self.ad_info}"
        return [f"{space}{self.lhs} = {rhs}{ad_comment}\n"]

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        lhs_base = self.lhs.name
        needed = [n for n in (names or []) if n != lhs_base]
        for var in self.lhs.collect_vars():
            if var.name != lhs_base:
                _append_unique(needed, var.name)
        _extend_unique(needed, self.rhs_names)
        if self.accumulate:
            _append_unique(needed, lhs_base)
        return needed

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
class SaveAssignment(Node):
    """Store intermediate values into temporary variable."""

    var: OpVar
    id: int
    load: bool = False
    tmpvar: OpVar = field(repr=False, default=None)
    lhs: OpVar = field(init=False, repr=False, default=None)
    rhs: OpVar = field(init=False, repr=False, default=None)
    scalar: bool = field(init=False, default=None)

    def __post_init__(self):
        super().__post_init__()
        name = self.var.name
        if re.search(r"save_\d+_ad", name):
            raise RuntimeError(f"Variable has aleady saved: {name}")
        if self.tmpvar is None:
            self.tmpvar = OpVar(f"{name}_save_{self.id}_ad", index=self.var.index, is_real=self.var.is_real)
        if self.load:
            self.lhs = self.var
            self.rhs = self.tmpvar
        else:
            self.lhs = self.tmpvar
            self.rhs = self.var

    def __deepcopy__(self, memo):
        return self

    def iter_ref_vars(self) -> Iterator[OpVar]:
        yield self.rhs
            
    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        if without_savevar:
            return iter(())
        else:
            yield self.lhs

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        return [f"{space}{self.lhs} = {self.rhs}\n"]

    def is_effectively_empty(self) -> bool:
        return False

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = [n for n in (names or []) if n != self.lhs.name]
        _append_unique(needed, self.rhs.name)
        return needed

    def check_initial(self, var: str) -> int:
        if self.lhs.name != var:
            return 0
        raise ValueError("This must not appeared at first time.")

    def to_load(self) -> "SaveAssignment":
        return SaveAssignment(self.var, tmpvar=self.tmpvar, id=self.id, load=True)


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

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        if self.intent in ("in", "inout"):
            yield OpVar(name=self.name, is_real=self.is_real(), kind=self.kind)
        else:
            return iter(())

    def is_effectively_empty(self) -> bool:
        return False

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

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        if self.intent in ("in", "inout"):
            if self.name in needed:
                needed.remove(self.name)
        return needed


@dataclass
class BranchBlock(Node):
    """An abstract class for ``if`` and ``select case`` branch stracture."""

    cond_blocks: List[Tuple[Operator, Block]] = field(default_factory=list)

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.cond_blocks, list):
            raise ValueError(f"cond_blocks must be a list: {type(self.cond_blocks)}")
        for tup in self.cond_blocks:
            if not isinstance(tup, tuple):
                raise ValueError(f"item in cond_blocks must be a tuple: {type(tup)}")
            cond, block = tup
            if isinstance(self, IfBlock):
                if not (isinstance(cond, Operator) or cond is None):
                    raise ValueError(f"cond must be a Operator: {type(cond)}")
            if isinstance(self, SelectBlock):
                if not (isinstance(cond, tuple) or cond is None):
                    raise ValueError(f"conds must be a tuple: {type(cond)}")
                if cond is not None:
                    for con in cond:
                        if not isinstance(con, Operator):
                            raise ValueError(f"cond must be a Operator: {type(con)}")
            if not isinstance(block, Block):
                raise ValueError(f"cond must be a Block: {type(block)}")

    def iter_children(self) -> Iterator[Node]:
        for _, block in self.cond_blocks:
            yield block

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse: bool = False) -> List[Node]:
        cond_blocks = []
        for cond, block in self.cond_blocks:
            res = block.convert_assignments(saved_vars, func, reverse)[0]
            required_vars = res.required_vars()
            new_res = block.prune_for(required_vars)
            if not new_res.is_effectively_empty():
                for node in res.iter_children():
                    new_res.append(node)
                res = new_res
            cond_blocks.append((cond, res))
        if isinstance(self, IfBlock):
            block = IfBlock(cond_blocks)
        elif isinstance(self, SelectBlock):
            block = SelectBlock(cond_blocks, self.expr)
        else:
            raise RuntimeError(f"Invalid class type: {type(self)}")
        loads = []
        for var in block.assigned_vars():
            if not var.endswith("_ad"):
                var = OpVar(var)
                loads.append(self._save_vars(var, saved_vars))
        blocks = []
        for load in loads:
            blocks.append(load)
        blocks.append(block)
        for load in loads:
            blocks.append(load)
        return blocks

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = list(names or [])
        common_assigned = set()
        for block in self.iter_children():
            common_assigned &= set(block.assigned_vars())
        needed = [n for n in needed if n not in common_assigned]
        res = [var.name for var in self.iter_ref_vars()]
        for block in self.iter_children():
            req = block.required_vars(needed)
            _extend_unique(res, req)
        return res

    def prune_for(self, targets: Iterable[str]) -> Node:
        new_condblocks = []
        for cond, block in self.cond_blocks:
            new_block = block.prune_for(targets)
            if not new_block.is_effectively_empty():
                new_condblocks.append((cond, new_block))
        if len(new_condblocks) == 0:
            return Block([])
        if isinstance(self, IfBlock):
            return IfBlock(new_condblocks)
        elif isinstance(self, SelectBlock):
            return SelectBlock(new_condblocks, expr=self.expr)
        else:
            raise RuntimeError(f"Invalid class type: {type(self)}")

    def check_initial(self, var: str) -> int:
        results = [b.check_initial(var) for b in self.iter_children()]
        if all(r ==0 for r in results):
            return 0
        if any(r < 0 for r in results):
            return -1
        if any(r == 2 for r in results):
            return 2
        return 1


@dataclass
class IfBlock(BranchBlock):
    """An ``if`` block with optional ``else if`` branches and ``else``."""

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for cond, _ in self.cond_blocks:
            if cond is not None:
                for var in cond.collect_vars():
                    yield var

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        first = True
        for cond, block in self.cond_blocks:
            if first or not block.is_effectively_empty():
                if first:
                    lines = [f"{space}if ({cond}) then\n"]
                elif cond is None:
                    lines.append(f"{space}else\n")
                else:
                    lines.append(f"{space}else if ({cond}) then\n")
                lines.extend(block.render(indent+1))
            first = False
        lines.append(f"{space}end if\n")
        return lines


@dataclass
class SelectBlock(BranchBlock):
    """A ``select case`` construct."""

    expr: Operator = field(default=None)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self.expr.collect_vars():
            yield var
        for conds, _ in self.cond_blocks:
            if conds is not None:
                for cond in conds:
                    for var in cond.collect_vars():
                        yield var

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}select case ({self.expr})\n"]
        for cond, block in self.cond_blocks:
            if cond is not None:
                conds = ', '.join([str(co) for co in cond])
                lines.append(f"{space}case ({conds})\n")
            else:
                lines.append(f"{space}case default\n")
            lines.extend(block.render(indent+1))
        lines.append(f"{space}end select\n")
        return lines


@dataclass
class DoAbst(Node):

    body: Block

    def iter_children(self) -> Iterator[Node]:
        yield self.body

@dataclass
class DoLoop(DoAbst):
    """A ``do`` loop."""

    index: OpVar
    start: Operaion
    end: Operaion
    step: Optional[Operator] = None

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self.start.collect_vars():
            yield var
        for var in self.end.collect_vars():
            yield var
        if self.step is not None:
            for var in self.step.collect_vars():
                yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[str]:
        yield self.index

    def build_do_index_list(self, list: List[str]):
        self.do_index_list = [self.index.name]
        self.do_index_list.extend(list)
        self.body.build_do_index_list(self.do_index_list)

    def is_independent(self) -> bool:
        if self.do_index_list is None:
            raise RuntimeError("Execute build_do_index_list first.")

        index = ",".join(self.do_index_list)

        def _check(node: Node) -> bool:
            for var in node.iter_ref_vars():
                if var.index is not None and var.index_str() != index:
                    return False
            for var in node.iter_assign_vars():
                if var.index is not None and var.index_str() != index:
                    return False
            for child in node.iter_children():
                if isinstance(child, DoLoop):
                    if not child.is_independent():
                        return False
                else:
                    if not _check(child):
                        return False
            return True

        return _check(self.body)

    def scalarize(self) -> None:
        for child in self.body.iter_children():
            if isinstance(child, SaveAssignment):
                child.tmpvar.index = None
                child.scalar = True
            elif isinstance(child, DoLoop):
                child.scalarize()

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse=False) -> List[Node]:
        body = self.body.convert_assignments(saved_vars, func, reverse)[0]
        required_vars = body.required_vars()
        new_body = self.body.prune_for(required_vars)
        if not new_body.is_effectively_empty():
            for node in body.iter_children():
                last = new_body.last()
                if isinstance(last, SaveAssignment) and isinstance(node, SaveAssignment) and last.var == node.var:
                    new_body.remove_child(last)
                    if last.load != node.load:
                        continue
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
        block = DoLoop(body, index, start, end, step)
        block.build_do_index_list([])
        if block.is_independent():
            block.scalarize()
        blocks = []
        for var in set(block.required_vars()) & set(block.assigned_vars()):
            if var == self.index.name or var.endswith("_ad"):
                continue
            var = OpVar(var)
            blocks.append(self._save_vars(var, saved_vars))
        blocks.append(block)
        return blocks

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        header = f"{space}do {self.index} = {self.start}, {self.end}"
        if self.step is not None:
            header = f"{header}, {self.step}"
        lines = [f"{header}\n"]
        lines.extend(self.body.render(indent+1))
        lines.append(f"{space}end do\n")
        return lines

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = self.body.required_vars(list(names or []))
        if self.index.name in needed:
            needed.remove(self.index.name)
        for op in [self.start, self.end, self.step]:
            if op is not None:
                for var in op.collect_vars():
                    _append_unique(needed, var.name)
        return needed

    def prune_for(self, targets: Iterable[str]) -> Node:
        new_body = self.body.prune_for(targets)
        if new_body.is_effectively_empty():
            return Block([])
        return DoLoop(new_body, self.index, self.start, self.end, self.step)

    def check_array_index(self, var: str, index: str) -> bool:
        if self.do_index_list is None:
            raise RuntimeError("Execute build_do_index_list first.")
        for v in self.iter_ref_vars():
            if v.name == var and (v.index is not None and v.index_str() != index):
                return False
        index = ",".join(self.do_index_list)
        return self.body.check_array_index(var, index)

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

    def iter_children(self) -> Iterator[Node]:
        yield self.body

    def iter_ref_vars(self) -> Iterator[OpVar]:
        iter(self.cond.collect_vars())

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse=False) -> List[Node]:
        body = self.body.convert_assignments(saved_vars, func, reverse)[0]
        required_vars = body.required_vars()
        new_body = self.body.prune_for(required_vars)
        if not new_body.is_effectively_empty():
            for node in body.children:
                new_body.append(node)
            body = new_body
        block = DoWhile(body, self.cond)
        blocks = [block]
        for var in block.assigned_vars():
            var = OpVar(var)
            blocks.append(self._save_vars(var, saved_vars))
        return blocks

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}do while ({self.cond})\n"]
        lines.extend(self.body.render(indent+1))
        lines.append(f"{space}end do\n")
        return lines

    def required_vars(self, names: Optional[List[str]] = None) -> List[str]:
        needed = self.body.required_vars(list(names or []))
        for var in self.cond.collect_vars():
            _append_unique(needed, var.name)
        return needed

    def prune_for(self, targets: Iterable[str]) -> Node:
        new_body = self.body.prune_for(targets)
        if new_body.is_effectively_empty():
            return Block([])
        return DoWhild(new_body, self.cond)

    def check_initial(self, var: str) -> int:
        if self.body.has_assignment_to(var):
            return -1
        return 0

def render_program(node: Node, indent: int = 0) -> str:
    """Return formatted Fortran code for the entire ``node`` tree."""

    lines = node.render(indent)
    return "".join(lines)
