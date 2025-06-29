"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
import re
import copy

from .operators import (
    AryIndex,
    Operator,
    OpVar,
    OpInt,
    OpReal,
    OpRange,
)

from .var_dict import (
    Vardict
)

from .var_list import (
    VarList
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
        self.do_index_list = []

    # ------------------------------------------------------------------
    # basic node API
    # ------------------------------------------------------------------

    def render(self, indent: int = 0) -> List[str]:
        """Return the formatted Fortran code lines for this node."""
        raise NotImplementedError

    def is_effectively_empty(self) -> bool:
        """Return ``True`` if removing this node does not change execution."""
        return all(child.is_effectively_empty() for child in self.iter_children())

    def has_reference_to(self, var: str) -> bool:
        """Return ``True`` if ``var`` is refered within this node."""
        if any(var == v.name for v in self.iter_ref_vars()):
            return True
        return any(child.has_ref_to(var) for child in self.iter_children())

    def has_assignment_to(self, var: str) -> bool:
        """Return ``True`` if ``var`` is assigned within this node."""
        if any(var == v.name for v in self.iter_assign_vars()):
            return True
        return any(child.has_assignment_to(var) for child in self.iter_children())

    def has_access_to(self, var: str) -> bool:
        """Return ``True`` if ``var`` is accessed within this node."""
        if self.has_assignment_to(var):
            return True
        if self.has_reference_to(var):
            return True
        return False

    def has_partial_access_to(self, var: str) -> bool:
        """Return ``True`` if ``var`` is accessed with index within this node."""
        for v in self.iter_ref_vars():
            if v.name==var and v.index is not None and any(i is not None for i in v.index):
                return True
        for v in self.iter_assign_vars():
            if v.name==var and v.index is not None and any(i is not None for i in v.index):
                return True
        for child in self.iter_children():
            if child.has_partial_access_to(var):
                return True
        return False

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

    def assigned_vars(self, vars: Optional[VarList] = None, without_savevar: bool = False, check_init_advars: bool = False) -> VarList:
        """Return variables assigned within this node and children."""
        flag = False
        for child in self.iter_children():
            vars = child.assigned_vars(vars, without_savevar=without_savevar, check_init_advars=check_init_advars)
            flag = True
        if not flag:
            if vars is None:
                vars = VarList()
            else:
                vars = vars.copy()
        for var in self.iter_assign_vars(without_savevar=without_savevar):
            if check_init_advars and not var.name.endswith("_ad"):
                continue
            vars.push(var)
        return vars

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        """Return variables needed before executing this node.

        ``vars`` is the ```VarList`` which consists of variables that must be defined *after* this
        node has executed.  The default implementation simply returns ``vars``
        unchanged.  Subclasses override this to remove variables that are
        assigned and to add any variables referenced by this node.
        """
        if vars is None:
            vars = VarList()
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        """Return AD variables without reference after executing this node."""
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            for var in self.iter_ref_vars():
                if var.name.endswith("_ad"):
                    vars.remove(var)
        for var in self.iter_assign_vars():
            if var.name.endswith("_ad"):
                vars.push(var)
        for child in self.iter_children():
            vars = child.unrefered_advars(vars)
        return vars

    def collect_access_info(self, info: Optional[Vardict] = None) -> dict:
        """Return variable access information in this node.
        This ignores access to the save variables and self-reference associated with accumulate attribute in Assignment object.

        ``None``: the variable is referred before assignment
        ``True``: the variable has assigned
        ``False``: the variable has referred
        """
        if info is None:
            info = Vardict()
        else:
            info = info.copy()
        for var in self.iter_ref_vars():
            if not var in info:
                info[var] = None
            if info[var] is not None:
                info[var] = False
        for var in self.iter_assign_vars():
            if not (var in info and info[var] is None):
                info[var] = True
        for child in self.iter_children():
            info = child.collect_access_info(info)
        return info

    def collect_vars(self) -> List[OpVar]:
        """Return variables used in this node."""
        vars = []
        for child in self.iter_children():
            _extend_unique(vars, child.collect_vars())
        for var in self.iter_ref_vars():
            _append_unique(vars, var)
        for var in self.iter_assign_vars():
            _append_unique(vars, var)
        return vars

    def build_do_index_list(self, list: List[str]) -> None:
        """Build index list of do loop."""
        self.do_index_list = list
        for child in self.iter_children():
            child.build_do_index_list(list)

    # ------------------------------------------------------------------
    # optimization helpers
    # ------------------------------------------------------------------

    def prune_for(self, targets: VarList) -> "Node":
        """Return a copy of this node with only code needed for ``targets``."""
        return self.deep_clone()

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        """Remove self-add from the first assignment if safe.
        This method is for only AD variables.
        """
        return assigned_vars


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
        node.build_do_index_list(self.do_index_list)
        self.__children.append(node)

    def extend(self, nodes: Iterable[Node]) -> None:
        """Extend this block with ``nodes``."""
        for node in nodes:
            node.build_do_index_list(self.do_index_list)
        self.__children.extend(nodes)

    def insert_before(self, id: int, node:Node) -> Node:
        for i, child in enumerate(self.__children):
            if child.get_id() == id:
                node.build_do_index_list(self.do_index_list)
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

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        for child in reversed(self.__children):
            vars = child.required_vars(vars, no_accumulate, without_savevar)
        return vars

    def private_vars(self) -> List[OpVar]:
        vars_info = {}
        for child in self.iter_children():
            if isinstance(child, SaveAssignment): # ignore Save variables
                continue
            if isinstance(child, Assignment):
                for var in child.iter_ref_vars():
                    if not var in vars_info:
                        vars_info[var] = None # not private

    def prune_for(self, targets: VarList) -> "Block":
        new_children: List[Node] = []
        for child in reversed(self.__children):
            # Declaration
            target_names = targets.names()
            if isinstance(child, Declaration):
                if child.intent is not None or child.name in target_names:
                    new_children.insert(0, child)
                    continue
            # Other nodes
            for var in child.assigned_vars():
                if var in targets:
                    pruned = child.prune_for(targets)
                    new_children.insert(0, pruned)
                    targets = pruned.required_vars(targets)
                    break
        children = new_children
        if len(children) >= 2:
            new_children = []
            for child in children:
                if len(new_children) > 0 and isinstance(child, SaveAssignment):
                    last = new_children[-1]
                    if isinstance(last, SaveAssignment) and last.var == child.var and last.load != child.load:
                        new_children.remove(last)
                        continue
                new_children.append(child)
            children = new_children
        return Block(children)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        for child in self.iter_children():
            assigned_vars = child.check_initial(assigned_vars)
        return assigned_vars

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

    def is_declared(self, name: str) -> bool:
        return self.decls.find_by_name(name) is not None

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        for block in reversed(self._all_blocks()):
            vars = block.required_vars(vars, no_accumulate, without_savevar)
        return vars

    def expand_decls(self, decls: Block) -> "Routine":
        self.decls.expand(decls)
        return self

    def prune_for(self, targets: VarList) -> "Routine":
        ad_content = self.ad_content.prune_for(targets)
        targets = ad_content.required_vars(targets)
        ad_init = self.ad_init.prune_for(targets)
        targets = ad_init.required_vars(targets)
        content = self.content.prune_for(targets)
        all_vars = content.collect_vars()
        _extend_unique(all_vars, ad_init.collect_vars())
        _extend_unique(all_vars, ad_content.collect_vars())
        decls = self.decls.prune_for(VarList(all_vars))
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
    _rhs_vars: List[OpVar] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.lhs, OpVar):
            raise ValueError(f"lhs must be OpVar: {type(self.lhs)}")
        if not isinstance(self.rhs, Operator):
            raise ValueError(f"rhs must be Operator: {type(self.rhs)}")
        self._rhs_vars = []
        for var in self.rhs.collect_vars():
            self._rhs_vars.append(var)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self._rhs_vars:
            yield var
        if self.lhs.index is not None:
            for var in self.lhs.index.collect_vars():
                yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        yield self.lhs

    def is_effectively_empty(self) -> bool:
        return self.lhs == self.rhs

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse=False) -> List[Assignment]:
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

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        lhs = self.lhs
        if vars is None:
            vars = VarList()
        else:
            if not isinstance(vars, VarList):
                raise ValueError(f"Must be VarList: {type(vars)}")
            vars = vars.copy()
            vars.remove(lhs)
        for var in lhs.collect_vars(): # variables in indexes
            if var != lhs:
                vars.push(var)
        for var in self._rhs_vars:
            vars.push(var)
        if (not no_accumulate) and self.accumulate:
            vars.push(lhs)
        return vars

    def collect_access_info(self, info: Optional[Vardict] = None) -> dict:
        if info is None:
            info = Vardict()
        else:
            info = info.copy()
        for var in self.iter_ref_vars():
            found = False
            for v in info.keys():
                if v.name == var.name:
                    if v.index == var.index:
                        found = True
                        if info[v] is not None:
                            info[v] = False
                    elif var.index is None or var.index >= v.index:
                        found = True
                        if info[v] is not None:
                            info[v] = False
                    elif v.index is None or v.index >= var.index:
                        found = True
                        if info[v] is not None:
                            info[var] = False
            if not found:
                info[var] = None
        if not (self.lhs in info and info[self.lhs] is None):
            info[self.lhs] = True
        return info

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        if self.accumulate:
            if not self.lhs in assigned_vars:
                self.accumulate = False
        if self.lhs.name.endswith("_ad"):
            assigned_vars = assigned_vars.copy()
            assigned_vars.push(self.lhs)
        return assigned_vars


@dataclass
class ClearAssignment(Node):

    lhs: OpVar
    info: Optional[dict] = field(repr=False, default=None)
    ad_info: Optional[str] = field(repr=False, default=None)

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.lhs, OpVar):
            raise ValueError(f"lhs must be OpVar: {type(self.lhs)}")

    def iter_ref_vars(self) -> Iterator[OpVar]:
        if self.lhs.index is not None:
            for var in self.lhs.index.collect_vars():
                yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        yield self.lhs

    def is_effectively_empty(self) -> bool:
        return False

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse=False) -> List[Assignment]:
        raise RuntimeError("This is AD code")

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        ad_comment = ""
        if self.ad_info is not None:
            ad_comment = f" ! {self.ad_info}"
        zero = OpReal("0.0", kind=self.lhs.kind)
        return [f"{space}{self.lhs} = {zero}{ad_comment}\n"]

    def assigned_vars(self, vars: Optional[VarList] = None, without_savevar: bool = False, check_init_advars: bool = False) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            if check_init_advars:
                vars.remove(self.lhs)
        if not check_init_advars:
            vars.push(self.lhs)
        return vars

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        lhs = self.lhs
        if vars is None:
            vars = VarList()
        else:
            if not isinstance(vars, VarList):
                raise ValueError(f"Must be VarList: {type(vars)}")
            vars = vars.copy()
            vars.remove(lhs)
        for var in lhs.collect_vars(): # variables in indexes
            if var != lhs:
                vars.push(var)
        return vars

    def collect_access_info(self, info: Optional[Vardict] = None) -> dict:
        if info is None:
            info = Vardict()
        else:
            info = info.copy()
        if not (self.lhs in info and info[self.lhs] is None):
            info[self.lhs] = True
        return info

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        else:
            assigned_vars = assigned_vars.copy()
            assigned_vars.remove(self.lhs)
        return assigned_vars


@dataclass
class SaveAssignment(Node):
    """Store intermediate values into temporary variable."""

    var: OpVar
    id: int
    tmpvar: OpVar = field(repr=False, default=None)
    load: bool = False
    lhs: OpVar = field(init=False, repr=False, default=None)
    rhs: OpVar = field(init=False, repr=False, default=None)
    reduced_dims: List[int] = field(init=False, repr=False, default=None)

    def __post_init__(self):
        super().__post_init__()
        name = self.var.name
        if re.search(r"save_\d+_ad", name):
            raise RuntimeError(f"Variable has aleady saved: {name}")
        if self.tmpvar is None:
            self.var = self.var.deep_clone()
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
        if without_savevar and self.lhs == self.tmpvar:
            return iter(())
        else:
            yield self.lhs

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        return [f"{space}{self.lhs} = {self.rhs}\n"]

    def is_effectively_empty(self) -> bool:
        return False

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            vars.remove(self.lhs)
        rhs = self.rhs
        if (not without_savevar) or rhs != self.var: # rhs is not saved var
            vars.push(rhs)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            if self.rhs == self.var:
                if self.rhs.name.endswith("_ad"):
                    vars.remove(self.rhs)
        if self.lhs == self.var:
            if self.lhs.name.endswith("_ad"):
                vars.push(self.lhs)
        return vars

    def collect_access_info(self, info: Optional[Vardict] = None) -> dict:
        return info

    def to_load(self) -> "SaveAssignment":
        return SaveAssignment(self.var, id=self.id, tmpvar=self.tmpvar, load=True)

    def reduce_dim(self, dims: List[str]) -> None:
        if self.reduced_dims is None:
            self.reduced_dims = []
        if self.tmpvar.index is None:
            raise RuntimeError(f"No index {self.tmpvar.name}")
        index_new = []
        for i, idx in enumerate(self.tmpvar.index):
            if isinstance(idx, OpVar) and idx.name in dims:
                self.reduced_dims.append(i)
            else:
                index_new.append(idx)
        self.tmpvar.index = AryIndex(index_new)


@dataclass
class PushPop(Node):
    """Push or pop a variable to/from a stack."""

    var: OpVar
    load: bool = False

    def __post_init__(self) -> None:
        super().__post_init__()
        if not isinstance(self.var, OpVar):
            raise ValueError(f"var must be OpVar: {type(self.var)}")

    def iter_ref_vars(self) -> Iterator[OpVar]:
        if not self.load:
            yield self.var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        if self.load:
            yield self.var

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        op = "pop" if self.load else "push"
        return [f"{space}call {op}({self.var})\n"]

    def is_effectively_empty(self) -> bool:
        return False

    def to_load(self) -> "PushPop":
        return PushPop(self.var, load=not self.load)

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

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            return VarList()
        if self.intent in ("in", "inout"):
            vars = vars.copy()
            vars.remove(OpVar(self.name))
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        if vars is None:
            vars = VarList()
        if self.intent in ("in", "inout"):
            if self.name.endswith("_ad"):
                vars = vars.copy()
                vars.push(OpVar(self.name, is_real=self.is_real, kind=self.kind))
        return vars


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
                raise ValueError(f"block must be a Block: {type(block)}")

    def iter_children(self) -> Iterator[Node]:
        for _, block in self.cond_blocks:
            yield block

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse: bool = False) -> List[Node]:
        cond_blocks = []
        for cond, block in self.cond_blocks:
            res = block.convert_assignments(saved_vars, func, reverse)[0]
            new_res = block.deep_clone()
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
        blocks = []
        for var in block.assigned_vars():
            if not var.name.endswith("_ad"):
                load = self._save_vars(var, saved_vars)
                loads.append(load)
                blocks.insert(0, load)
        blocks.append(block)
        for load in loads:
            blocks.append(load)
        return blocks

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
        for var in self.iter_ref_vars():
            vars.push(var)
        vars_new = VarList()
        for block in self.iter_children():
            vars_new.merge(block.required_vars(vars, no_accumulate, without_savevar))
        return vars_new

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars_list = VarList()
        for block in self.iter_children():
            for v in block.unrefered_advars(vars):
                if v.name.endswith("_ad"):
                    vars_list.push(v)
        return vars_list

    def collect_access_info(self, info: Optional[Vardict] = None) -> dict:
        if info is None:
            info = Vardict()
        else:
            info = info.copy()
        for var in self.iter_ref_vars():
            if not var in info:
                info[var] = None
            if info[var] is not None:
                info[var] = False
        infos = []
        for child in self.iter_children():
            infos.append(child.collect_access_info(info))
        if len(infos) == 0:
            return info
        if len(infos) == 1:
            return infos[0]
        vars = []
        for info in infos:
            _extend_unique(vars, info.keys())
        info_new = Vardict()
        for var in vars:
            values = [info[var] for info in infos if var in info]
            if any(v is None for v in values):
                info_new[var] = None
            else:
                merged = values[0]
                for v in values[1:]:
                    merged = merged or v
                info_new[var] = merged
        info = info_new
        for var in self.iter_assign_vars():
            if not (var in info[var] and info[var] is None):
                info[var] = True
        return info

    def prune_for(self, targets: VarList) -> Node:
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

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        vars_list = VarList()
        for block in self.iter_children():
            vars_list.merge(block.check_initial(assigned_vars))
        return vars_list


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

    _body: Block

    def iter_children(self) -> Iterator[Node]:
        yield self._body

@dataclass
class DoLoop(DoAbst):
    """A ``do`` loop."""

    index: OpVar
    start: Operaion
    end: Operaion
    step: Optional[Operator] = None

    def __post_init__(self):
        super().__post_init__()
        self.build_do_index_list([])

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
        self._body.build_do_index_list(self.do_index_list)

    def _build_index_map(self) -> dict:
        # build index map: variable name -> position of the loop index in the array index
        index_map = {}
        for var in self.collect_vars():
            if var.index is not None:
                for i, idx in enumerate(var.index):
                    if isinstance(idx, OpVar) and idx == self.index:
                        index_map[var.name] = (i, len(var.index))
        return index_map

    def find_index(self, var: OpVar, name: str) -> Union[int, None]:
        if var.index is None:
            return None
        for i, idx in enumerate(var.index):
            if idx is not None:
                for v in idx.collect_vars():
                    if v.name == name:
                        return i
        return None

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse=False) -> List[Node]:
        body = self._body.convert_assignments(saved_vars, func, reverse)[0]
        new_body = self._body.deep_clone()
        if not new_body.is_effectively_empty():
            for node in body.iter_children():
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

        common_vars = block.required_vars() & block.assigned_vars()
        loads = []
        blocks = []
        for cvar in common_vars:
            if cvar == self.index or cvar.name.endswith("_ad"):
                continue
            load = self._save_vars(cvar, saved_vars)
            loads.append(load)
            blocks.insert(0, load)
        blocks.append(block)
        for load in loads:
            blocks.append(load)
        return blocks

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        header = f"{space}do {self.index} = {self.start}, {self.end}"
        if self.step is not None:
            header = f"{header}, {self.step}"
        lines = [f"{header}\n"]
        lines.extend(self._body.render(indent+1))
        lines.append(f"{space}end do\n")
        return lines

    def private_vars(self) -> List[OpVar]:
        """Return variables which have no effect to the outer region of this loop"""
        access_info = self.collect_access_info()
        vars = VarList([var for var, v in access_info.items() if v==False])
        vars.update_index_downward(self._build_index_map(), self.index)
        vars_list = []
        for var in vars:
            if (var.index is None or not var.index.is_depended_on(self.index)):
                _append_unique(vars_list, var)
        return vars_list

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            vars.update_index_downward(self._build_index_map(), self.index)
        vars = self._body.required_vars(vars, no_accumulate, without_savevar)

        # remove private variables
        #for var in self.private_vars():
        #    for v in list(vars):
        #        if var == v:
        #            vars.remove(v)
        #            continue
        #        if var.name == v.name:
        #            if var.index is None or v.index <= var.index:
        #                vars.remove(v)
        #                continue
        #            # tentative (integer index is assumed to cover all)
        #            check = True
        #            for i, dim in enumerate(var.index):
        #                if dim is None or isinstance(dim, OpInt):
        #                    continue
        #                check = False
        #                break
        #            if check:
        #                vars.remove(v)
        #                continue

        if self.index in vars:
            vars.remove(self.index)
        vars.update_index_upward(self._build_index_map(), range=OpRange([self.start,self.end,self.step]))
        for op in [self.start, self.end, self.step]:
            if op is not None:
                for var in op.collect_vars():
                    vars.push(var)
        return vars

    def assigned_vars(self, vars: Optional[VarList] = None, without_savevar: bool = False, check_init_advars: bool = False) -> VarList:
        vars = self._body.assigned_vars(vars, without_savevar=without_savevar, check_init_advars=check_init_advars)
        vars.update_index_upward(self._build_index_map(), range=OpRange([self.start,self.end,self.step]))
        if not check_init_advars:
            vars.push(self.index)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars = self._body.unrefered_advars(vars)
        vars.update_index_upward(self._build_index_map(), range=OpRange([self.start,self.end,self.step]))
        return vars

    def collect_access_info(self, info: Optional[vardict] = None) -> dict:
        raise NotImplementedError
        if info is None:
            info = Vardict()
        else:
            info = info.copy()
        for var in self.iter_ref_vars():
            if not var in info:
                info[var] = None
            if info[var] is not None:
                info[var] = False

        vars = info.keys()
        vars_child = self._update_index_downward(vars)
        info_child = Vardict()
        for i, var in enumerate(vars):
            info_child[vars_child[i]] = info[var]

        info_child = self._body.collect_access_info(info_child)
        if self.index in info_child:
            info_child.remove(self.index)

        vars_child = info_child.keys()
        vars = self._update_index_upward(vars_child)
        info = Vardict()
        for i, var in enumerate(vars_child):
            info[vars[i]] = info_child[var]
        return info

    def prune_for(self, targets: VarList) -> Node:
        targets = targets.copy()
        targets.update_index_downward(self._build_index_map(), self.index)
        new_body = self._body.prune_for(targets)

        #for var in new_body.required_vars(targets, no_accumulate=True, without_savevar=True):
        for var in new_body.required_vars(targets):
            if var.name == self.index.name:
                continue
            #if var.name.endswith("_ad"):
            #    continue
            # check if the variable has no reccurent in this loop
            if var.index is not None and set(self.do_index_list) <= set(var.index_list()):
                continue
            targets.push(var)
        new_body = self._body.prune_for(targets)

        def _reducedim_from_tmpvar(node: Node) -> None:
            for child in node.iter_children():
                if isinstance(child, SaveAssignment):
                    if child.tmpvar.index is None:
                        continue
                    name = child.tmpvar.name
                    if (not any(name == var.name for var in targets)) and child.tmpvar == child.lhs:
                        if set(self.do_index_list) <= set(child.tmpvar.index_list()):
                            child.reduce_dim(self.do_index_list)
                    continue
                if isinstance(child, DoLoop):
                    continue
                _reducedim_from_tmpvar(child)
        _reducedim_from_tmpvar(new_body)

        if new_body.is_effectively_empty():
            return Block([])
        return DoLoop(new_body, self.index, self.start, self.end, self.step)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        else:
            assigned_vars = assigned_vars.copy()
        assigned_vars.update_index_downward(self._build_index_map(), do_index_var=self.index)
        #assigned_vars.merge(self._body.assigned_vars(check_init_advars = True))
        for var in self._body.assigned_vars(check_init_advars = True):
            if not set(self.do_index_list) <= set(var.index_list()):
                assigned_vars.push(var)
        assigned_vars = self._body.check_initial(assigned_vars)
        assigned_vars.update_index_upward(self._build_index_map(), range=OpRange([self.start,self.end,self.step]))
        return assigned_vars

@dataclass
class DoWhile(DoAbst):
    """A ``do while`` loop."""

    cond: OpVar

    def __post_init__(self):
        self.do_index_list = ["__nerver_match__"]

    def iter_ref_vars(self) -> Iterator[OpVar]:
        iter(self.cond.collect_vars())

    def build_do_index_list(self, list: List[str]) -> None:
        for child in self.iter_children():
            child.build_do_index_list(list)

    def convert_assignments(self, saved_vars: List[SaveAssignment], func: Callable[[OpVar, Operator, dict], List[Assignment]], reverse=False) -> List[Node]:
        body = self._body.convert_assignments(saved_vars, func, reverse)[0]
        new_body = self._body.deep_clone()
        if not new_body.is_effectively_empty():
            for node in body.children:
                new_body.append(node)
            body = new_body
        block = DoWhile(body, self.cond)
        loads = []
        blocks = []
        for var in block.assigned_vars():
            load = self._save_vars(OpVar(var), saved_vars)
            loads.append(load)
            blocks.insert(0, load)
        blocks.append(block)
        for load in loads:
            blocks.append(load)
        return blocks

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}do while ({self.cond})\n"]
        lines.extend(self._body.render(indent+1))
        lines.append(f"{space}end do\n")
        return lines

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        vars = self._body.required_vars(vars, no_accumulate, without_savevar)
        vars = vars.copy()
        for var in self.cond.collect_vars():
            vars.push(var)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars = self._body.unrefered_advars(vars)
        return vars

    def prune_for(self, targets: VarList) -> Node:
        new_body = self._body.prune_for(targets)
        targets = targets.copy()
        targets.merge(new_body.required_vars(targets))
        new_body = self._body.prune_for(targets)
        if new_body.is_effectively_empty():
            return Block([])
        return DoWhile(new_body, self.cond)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        else:
            assigned_vars = assigned_vars.copy()
        assigned_vars.merge(self._body.assigned_vars(check_init_advars = True))
        assigned_vars = self._body.check_initial(assigned_vars)
        return assigned_vars

def render_program(node: Node, indent: int = 0) -> str:
    """Return formatted Fortran code for the entire ``node`` tree."""

    lines = node.render(indent)
    return "".join(lines)
