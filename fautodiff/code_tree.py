"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Optional, Iterator, ClassVar
import re
import copy

AD_SUFFIX = "_ad"
FWD_SUFFIX = "_fwd_ad"
REV_SUFFIX = "_rev_ad"

from .operators import (
    AryIndex,
    Operator,
    OpVar,
    OpLeaf,
    OpInt,
    OpReal,
    OpNeg,
    OpRange,
    OpFunc,
    OpFuncUser
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
class Node:
    """Abstract syntax tree node for Fortran code fragments."""

    __id: int = field(init=False, repr=False)
    parent: "Node" = field(init=False, repr=False, default=None)
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

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        """Return AD converted nodes."""
        return []

    def _save_vars(self, var: OpVar, saved_vars: List[SaveAssignment]) -> SaveAssignment:
        id = self.get_id()
        save = SaveAssignment(var, id)
        self.parent.insert_before(id, save)
        saved_vars.append(save.tmpvar)
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
        self.parent = node

    def get_routine(self) -> Optional["Routine"]:
        node: Optional[Node] = self
        while node is not None:
            if isinstance(node, Routine):
                return node
            node = node.parent
        return None

    def get_module(self) -> Optional["Module"]:
        node: Optional[Node] = self
        while node is not None:
            if isinstance(node, Module):
                return node
            node = node.parent
        return None

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

    def insert_begin(self, node: "Node"):
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
            if check_init_advars and not var.name.endswith(AD_SUFFIX):
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
                if var.name.endswith(AD_SUFFIX):
                    vars.remove(var)
        for var in self.iter_assign_vars():
            if var.name.endswith(AD_SUFFIX):
                vars.push(var)
        for child in self.iter_children():
            vars = child.unrefered_advars(vars)
        return vars

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

    def build_do_index_list(self, index_list: List[str]) -> None:
        """Build index list of ``do`` loops."""
        self.do_index_list = index_list
        for child in self.iter_children():
            child.build_do_index_list(index_list)

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

    # ------------------------------------------------------------------
    # other helpers
    # ------------------------------------------------------------------
    def _save_var_name(self, name: str, id: int, no_suffix: bool = False, pushpop: bool = False) -> str:
        if pushpop:
            ext = "_pushpop"
        else:
            ext = ""
        if no_suffix:
            return f"{name}_save_{id}{ext}"
        else:
            return f"{name}_save_{id}{ext}{AD_SUFFIX}"

    def _generate_ad_forward(
        self,
        lhs: OpVar,
        rhs: Operator,
        assigned_advars: VarList,
        saved_vars: List[SaveAssignment],
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        rhs = rhs.deep_clone()
        assigns: List[Node] = []
        for n, ufunc in enumerate(rhs.find_userfunc()):
            if routine_map is None:
                raise RuntimeError("routine_map is necessary for CallStatement")
            arg_info = routine_map[ufunc.name]
            name = self._save_var_name(f"{ufunc.name}{n}", self.get_id(), no_suffix=True)
            result = OpVar(name, typename=arg_info["type"][-1])
            saved_vars.append(
                OpVar(
                    f"{name}{AD_SUFFIX}",
                    typename=arg_info["type"][-1],
                    kind=arg_info["kind"][-1],
                    dims=arg_info["dims"][-1],
                )
            )
            intents = ufunc.intents
            if intents is None and ufunc.name in routine_map:
                intents = routine_map[ufunc.name]["intents"]
            callstmt = CallStatement(
                name=ufunc.name,
                args=ufunc.args,
                intents=intents,
                result=result,
                info=self.info,
            )
            assigns.extend(
                callstmt.generate_ad(
                    saved_vars, reverse=False, assigned_advars=assigned_advars, routine_map=routine_map, warnings=warnings
                )
            )
            rhs = rhs.replace_with(ufunc, result)

        if lhs.ad_target:
            grad_lhs = lhs.add_suffix(AD_SUFFIX)
            ad_info = self.info.get("code") if self.info is not None else None

            if isinstance(rhs, OpFunc):
                handler = rhs.special_handler(lhs, rhs.args, AD_SUFFIX, reverse=False)
                if handler is not None:
                    assigns.append(Assignment(grad_lhs, handler, ad_info=ad_info))
                    return assigns

            vars = rhs.collect_vars()
            expr = None
            for var in vars:
                if not var.ad_target:
                    continue
                v = var.add_suffix(AD_SUFFIX)
                if assigned_advars is not None and not v in assigned_advars:
                    continue
                dev = rhs.derivative(var, target=grad_lhs, info=self.info, warnings=warnings)
                term = v * dev
                if expr is None:
                    expr = term
                else:
                    expr = expr + term
            if expr is None:
                assigns.append(ClearAssignment(grad_lhs, ad_info=ad_info))
                assigned_advars.remove(grad_lhs)
            else:
                assigns.append(Assignment(grad_lhs, expr, ad_info=ad_info))
                assigned_advars.push(grad_lhs)
        return assigns

    def _generate_ad_reverse(
        self,
        lhs: OpVar,
        rhs: Operator,
        saved_vars: List[SaveAssignment],
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        rhs = rhs.deep_clone()
        extras: List[Node] = []
        for n, ufunc in enumerate(rhs.find_userfunc()):
            if routine_map is None:
                raise RuntimeError("routine_map is necessary for CallStatement")
            arg_info = routine_map[ufunc.name]
            name = self._save_var_name(f"{ufunc.name}{n}", self.get_id(), no_suffix=True)
            result = OpVar(name, typename=arg_info["type"][-1])
            saved_vars.append(
                OpVar(
                    f"{name}{AD_SUFFIX}",
                    typename=arg_info["type"][-1],
                    kind=arg_info["kind"][-1],
                    dims=arg_info["dims"][-1],
                )
            )
            callstmt = CallStatement(
                name=ufunc.name,
                args=ufunc.args,
                intents=ufunc.intents,
                result=result,
                info=self.info,
            )
            extras.extend(
                callstmt.generate_ad(
                    saved_vars, reverse=True, routine_map=routine_map, warnings=warnings
                )
            )
            rhs = rhs.replace_with(ufunc, result)

        assigns: List[Node] = []
        if lhs.ad_target:
            grad_lhs = lhs.add_suffix(AD_SUFFIX)
            ad_info = self.info.get("code") if self.info is not None else None

            if isinstance(rhs, OpFunc):
                handler = rhs.special_handler(lhs, rhs.args, AD_SUFFIX, reverse=True)
                if handler is not None:
                    v = rhs.args[0].add_suffix(AD_SUFFIX)
                    assigns.append(
                        Assignment(
                            v,
                            handler,
                            accumulate=(v != grad_lhs),
                            ad_info=ad_info,
                        )
                    )
                    assigns.extend(extras)
                    return assigns

            vars = rhs.collect_vars()
            if lhs in vars:
                # move lhs to the last of vars
                vars.remove(lhs)
                vars.append(lhs)
            for var in vars:
                if not var.ad_target:
                    continue
                dev = rhs.derivative(var, target=grad_lhs, info=self.info, warnings=warnings)
                v = var.add_suffix(AD_SUFFIX)
                res = grad_lhs * dev
                assigns.append(
                    Assignment(v, res, accumulate=(v != grad_lhs), ad_info=ad_info)
                )
            if lhs not in vars:
                assigns.append(ClearAssignment(grad_lhs, ad_info=ad_info))
        assigns.extend(extras)
        return assigns


@dataclass
class Block(Node):
    """A container for a sequence of nodes."""

    __children: List[Node] = field(default_factory=list)

    def iter_children(self) -> Iterator[Node]:
        return iter(self.__children)

    def __getitem__(self, index: int) -> Node:
        return self.__children[index]

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        children: List[Node] = []
        iterator = self.__children
        if reverse:
            iterator = reversed(iterator)
        for node in iterator:
            nodes = node.generate_ad(saved_vars, reverse, assigned_advars, routine_map, warnings)
            for res in nodes:
                if res is not None and not res.is_effectively_empty():
                    children.append(res)
        return [Block(children)]

    def find_by_name(self, name: str) -> Optional[Declaration]:
        for child in self.iter_children():
            if isinstance(child, Declaration) and child.name == name:
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

    def insert_begin(self, node:Node) -> Node:
        node.build_do_index_list(self.do_index_list)
        return self.__children.insert(0, node)

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
        if vars is None:
            vars = VarList()
        return vars

    def remove_push(self) -> "Block":
        children_new = []
        for child in self.iter_children():
            if not isinstance(child, PushPop):
                children_new.append(child)
        return Block(children_new)

    def prune_for(self, targets: VarList) -> "Block":
        new_children: List[Node] = []
        for child in reversed(self.__children):
            target_names = targets.names()
            # Declaration
            if isinstance(child, Declaration):
                if child.intent is not None or child.name in target_names:
                    new_children.insert(0, child)
                    continue
            # Allocate and Deallocate
            if isinstance(child, (Allocate, Deallocate)):
                pruned = child.prune_for(targets)
                if not pruned.is_effectively_empty():
                    new_children.insert(0, pruned)
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
                if len(new_children) > 0 and isinstance(child, SaveAssignment) and not child.pushpop:
                    last = new_children[-1]
                    if isinstance(last, SaveAssignment) and not last.pushpop and last.id == child.id and last.load != child.load:
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
class Use(Node):
    """Representation of a Fortran use statement."""
    name: str
    only: Optional[List[str]] = field(default=None)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        if self.only is not None:
            only = f", only: {', '.join(self.only)}"
        else:
            only = ""
        lines = [f"{space}use {self.name}{only}\n"]
        return lines

    def is_effectively_empty(self) -> bool:
        return False


@dataclass
class CallStatement(Node):
    """Representation of a ``call`` statement or function call."""

    name: str
    args: List[Operator] = field(default_factory=list)
    arg_keys: Optional[List[Optional[str]]] = None
    intents: Optional[List[str]] = None
    result: Optional[OpVar] = None
    info: Optional[dict] = field(repr=False, default=None)
    ad_info: Optional[str] = field(repr=False, default=None)

    def __post_init__(self) -> None:
        super().__post_init__()
        if not _NAME_RE.fullmatch(self.name):
            raise ValueError(f"invalid Fortran routine name: {self.name}")
        if not isinstance(self.args, list):
            raise ValueError(f"args must be a list: {type(self.args)}")
        for i, arg in enumerate(self.args):
            if isinstance(arg, int):
                self.args[i] = OpInt(arg)
            elif not isinstance(arg, Operator):
                raise ValueError(f"arg must be Operator: {type(arg)}")
        if self.arg_keys is None:
            self.arg_keys = [None] * len(self.args)
        if len(self.arg_keys) != len(self.args):
            raise ValueError("arg_keys length must match args length")
        if self.intents is not None:
            if not isinstance(self.intents, list):
                raise ValueError(f"intents must be a list: {type(self.intents)}")
            if len(self.intents) != len(self.args):
                raise ValueError("intents length must match args length")
            for intent in self.intents:
                if intent not in ("in", "out", "inout"):
                    raise ValueError(f"invalid intent: {intent}")

    def _iter_vars(self, intents: List[str], kinds: Tuple[str, ...]) -> Iterator[OpVar]:
        for arg, intent in zip(self.args, intents):
            if intent in kinds:
                for var in arg.collect_vars():
                    yield var

    def iter_ref_vars(self) -> Iterator[OpVar]:
        intents = self.intents or ["inout"] * len(self.args)
        yield from self._iter_vars(intents, ("in", "inout"))

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        intents = self.intents or ["inout"] * len(self.args)
        yield from self._iter_vars(intents, ("out", "inout"))
        if self.result is not None:
            yield self.result

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        arg_strs = []
        for key, arg in zip(self.arg_keys, self.args):
            if key is None:
                arg_strs.append(str(arg))
            else:
                arg_strs.append(f"{key}={arg}")
        args = ", ".join(arg_strs)
        ad_comment = ""
        if self.ad_info is not None:
            ad_comment = f" ! {self.ad_info}"
        if self.result is None:
            return [f"{space}call {self.name}({args}){ad_comment}\n"]
        else:
            return [f"{space}{self.result} = {self.name}({args}){ad_comment}\n"]

    def is_effectively_empty(self) -> bool:
        return False

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        intents = self.intents or ["in"] * len(self.args)
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            for arg, intent in zip(self.args, intents):
                if intent in ("out", "inout"):
                    for var in arg.collect_vars():
                        vars.remove(var)
            if self.result is not None:
                vars.remove(self.result)
        for arg, intent in zip(self.args, intents):
            if intent in ("in", "inout"):
                for var in arg.collect_vars():
                    vars.push(var)
        return vars

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        if routine_map is None:
            raise RuntimeError("routine_map is necessary for CallStatement")
        name = self.name
        if not name in routine_map:
            print(routine_map)
            raise RuntimeError(f"Not found in routime_map: {name}")
        arg_info = routine_map[name]

        name_key = "name_rev_ad" if reverse else "name_fwd_ad"
        if arg_info.get("skip") or arg_info.get(name_key) is None:
            return [self]

        def _push_arg(i, arg):
            if not isinstance(arg, OpLeaf) and arg_info["type"][i] == "real":
                name = self._save_var_name(f"{self.name}_arg{i}", self.get_id(), no_suffix=True)
                tmp = OpVar(name, typename="real")
                tmp_vars.append((tmp, arg))
                args_new.append(tmp)
                saved_vars.append(OpVar(f"{tmp.name}{AD_SUFFIX}", typename="real", kind=arg_info["kind"][i], dims=arg_info["dims"][i]))
            else:
                args_new.append(arg)
        tmp_vars = []
        call_args = list(self.args)
        arg_keys = list(self.arg_keys)
        param_names = list(arg_info["args"])
        if self.result is not None:
            param_names_no_res = param_names[:-1]
        else:
            param_names_no_res = param_names

        ordered = [None] * len(param_names_no_res)
        used = [False] * len(param_names_no_res)
        pos = 0
        for arg, key in zip(call_args, arg_keys):
            if key is None:
                while pos < len(param_names_no_res) and used[pos]:
                    pos += 1
                if pos < len(param_names_no_res):
                    ordered[pos] = arg
                    used[pos] = True
                    pos += 1
            else:
                if key in param_names_no_res:
                    idx = param_names_no_res.index(key)
                    ordered[idx] = arg
                    used[idx] = True
        args = []
        args_new = []
        for i, arg in enumerate(ordered):
            _push_arg(i, arg)
            args.append(arg)
        if self.result is not None:
            _push_arg(len(param_names_no_res), self.result)
            args.append(self.result)
        ad_args = []
        if reverse:
            name_key = "name_rev_ad"
            args_key = "args_rev_ad"
            intents_key = "intents_rev_ad"
        else:
            name_key = "name_fwd_ad"
            args_key = "args_fwd_ad"
            intents_key = "intents_fwd_ad"
        for ad_arg in arg_info[args_key]:
            if ad_arg.endswith(AD_SUFFIX):
                arg = ad_arg.removesuffix(AD_SUFFIX)
            else:
                arg = ad_arg
            i = arg_info["args"].index(arg)
            var = args[i]
            if ad_arg.endswith(AD_SUFFIX):
                if not isinstance(var, OpLeaf):
                    var = args_new[i]
                var = OpVar(f"{var.name}{AD_SUFFIX}", index=var.index, kind=var.kind, typename=var.typename, ad_target=var.ad_target, is_constant=var.is_constant)
            ad_args.append(var)
        ad_call = CallStatement(name=arg_info[name_key], args=ad_args, intents=arg_info[intents_key], ad_info=self.info["code"])
        if not reverse:
            for i, arg in enumerate(ad_args):
                if arg_info["intents_fwd_ad"][i] in ("out", "inout"):
                    assigned_advars.push(arg)
        ad_nodes = []
        if tmp_vars:
            if reverse:
                for lhs, rhs in tmp_vars:
                    ad_nodes.extend(
                        self._generate_ad_reverse(
                            lhs,
                            rhs,
                            saved_vars,
                            routine_map=routine_map,
                            warnings=warnings,
                        )
                    )
            else:
                for lhs, rhs in tmp_vars:
                    ad_nodes.extend(
                        self._generate_ad_forward(
                            lhs,
                            rhs,
                            assigned_advars,
                            saved_vars,
                            routine_map=routine_map,
                            warnings=warnings,
                        )
                    )

        if reverse:
            ad_nodes.insert(0, ad_call)
            loads = []
            blocks = []
            for var in ad_call.assigned_vars():
                if not var.name.endswith(AD_SUFFIX):
                    load = self._save_vars(var, saved_vars)
                    loads.append(load)
                    blocks.insert(0, load)
            blocks.extend(ad_nodes)
            blocks.extend(loads)
        else:
            ad_nodes.append(ad_call)
            blocks = ad_nodes
            blocks.append(self)

        return blocks

@dataclass
class Module(Node):
    """Representation of a Fortran module."""
    name: str
    body: Block = field(default_factory=Block)
    decls: Optional[Block] = None
    routines: Block = field(default_factory=Block)
    directives: dict = field(default_factory=dict)

    def iter_children(self):
        yield self.body
        if self.decls is not None:
            yield self.decls
        for routine in self.routines:
            yield routine

    def __post_init__(self):
        super().__post_init__()

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}module {self.name}\n"]
        lines.extend(self.body.render(indent+1))
        if self.decls is not None:
            lines.extend(self.decls.render(indent+1))
        lines.append("\n")
        lines.append(f"{space}contains\n")
        lines.append("\n")
        for routine in self.routines:
            lines.extend(routine.render(indent+1))
            lines.append("\n")
        lines.append(f"{space}end module {self.name}\n")
        return lines


    def find_use_modules(self) -> List[str]:
        mods = []
        for child in self.body.iter_children():
            if isinstance(child, Use):
                mods.append(child.name)
        return mods

@dataclass
class Routine(Node):
    """Common functionality for ``subroutine`` and ``function`` blocks."""

    name: str
    args: List[str]
    result: Optional[str] = None
    decls: Block = field(default_factory=Block)
    content: Block = field(default_factory=Block)
    directives: dict = field(default_factory=dict)
    mod_decls: Optional[Block] = None
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

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Routine]:
        raise RuntimeError("generate_ad for Routine is not allowed")

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

    def get_var(self, name: str) -> Optional[OpVar]:
        decl = self.decls.find_by_name(name)
        if decl is None and self.mod_decls is not None:
            decl = self.mod_decls.find_by_name(name)
        if decl is None:
            return None
        intent = decl.intent
        if self.result == name:
            intent = "out"
        return OpVar(
            name,
            kind=decl.kind,
            dims=decl.dims,
            typename=decl.typename,
            intent=intent,
            ad_target=None,
            is_constant=decl.parameter or getattr(decl, "constant", False),
            declared_in=decl.declared_in,
        )

    def arg_vars(self) -> List[OpVar]:
        vars = []
        for arg in self.args:
            vars.append(self.get_var(arg))
        if self.result is not None:
            vars.append(self.get_var(self.result))
        return vars

    def is_declared(self, name: str) -> bool:
        if self.decls.find_by_name(name) is not None:
            return True
        if self.mod_decls is not None and self.mod_decls.find_by_name(name) is not None:
            return True
        return False

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        for block in reversed(self._all_blocks()):
            vars = block.required_vars(vars, no_accumulate, without_savevar)
        if vars is None:
            vars = VarList()
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
    _ufuncs: List[OpFuncUser] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.lhs, OpVar):
            raise ValueError(f"lhs must be OpVar: {type(self.lhs)}")
        if not isinstance(self.rhs, Operator):
            raise ValueError(f"rhs must be Operator: {type(self.rhs)}")
        self._rhs_vars = []
        for var in self.rhs.collect_vars():
            self._rhs_vars.append(var)
        self._ufuncs = self.rhs.find_userfunc()

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

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Assignment]:
        if reverse:
            assigns = [self._save_vars(self.lhs, saved_vars)]
            assigns.extend(
                self._generate_ad_reverse(
                    self.lhs,
                    self.rhs,
                    saved_vars,
                    routine_map=routine_map,
                    warnings=warnings,
                )
            )
        else:
            assigns = self._generate_ad_forward(
                self.lhs,
                self.rhs,
                assigned_advars,
                saved_vars,
                routine_map=routine_map,
                warnings=warnings,
            )
            assigns.append(self)
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

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        if self.accumulate:
            if not self.lhs in assigned_vars:
                self.accumulate = False
        if self.lhs.name.endswith(AD_SUFFIX):
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

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Assignment]:
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
    pushpop: ClassVar[bool] = False

    def __post_init__(self):
        super().__post_init__()
        name = self.var.name
        if re.search(rf"save_\d+{AD_SUFFIX}", name):
            raise RuntimeError(f"Variable has aleady saved: {name}")
        if self.tmpvar is None:
            self.var = self.var.deep_clone()
            self.tmpvar = OpVar(
                self._save_var_name(name, self.id, pushpop=self.pushpop),
                index=self.var.index,
                kind=self.var.kind,
                typename=self.var.typename,
                ad_target=self.var.ad_target,
                is_constant=self.var.is_constant,
                reference=self.var,
            )
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
        #if without_savevar and self.lhs == self.tmpvar:
        if without_savevar:
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
        if (not without_savevar) or rhs == self.var: # if rhs is not saved var
            vars.push(rhs)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            if self.rhs == self.var:
                if self.rhs.name.endswith(AD_SUFFIX):
                    vars.remove(self.rhs)
        if self.lhs == self.var:
            if self.lhs.name.endswith(AD_SUFFIX):
                vars.push(self.lhs)
        return vars

    def prune_for(self, targets: VarList) -> "Node":
        if self.load:
            name = self.var.name
            index = self.var.index
            if not self.var.name in targets.names():
                raise RuntimeError
            index_target = None
            flag = True
            for idx in targets[name]:
                if idx is None or idx >= index:
                    flag = False
                    break
                if index_target is None or idx >= index_target:
                    index_target = idx
            if flag:
                self.var.index = index_target
                self.tmpvar.index = index_target

        if self.tmpvar.index is not None and self.tmpvar.reduced_dims is None:
            if not isinstance(self.tmpvar.index, AryIndex):
                print(type(self.tmpvar.index))
                raise RuntimeError
            index_new = []
            for i, idx in enumerate(self.tmpvar.index):
                if isinstance(idx, OpInt) or (isinstance(idx, OpRange) and isinstance(idx[0], OpInt) and idx[0]==idx[1]):
                    if self.tmpvar.reduced_dims is None:
                        self.tmpvar.reduced_dims = []
                    self.tmpvar.reduced_dims.append(i)
                else:
                    index_new.append(idx)
            if index_new:
                self.tmpvar.index = AryIndex(index_new)
            else:
                self.tmpvar.index = None
            
        return self

    def to_load(self) -> "SaveAssignment":
        return SaveAssignment(self.var, id=self.id, tmpvar=self.tmpvar, load=True)


@dataclass
class PushPop(SaveAssignment):
    """Push or pop a variable to/from a stack."""

    pushpop: ClassVar[bool] = True

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        op = "pop" if self.load else "push"
        op_name = f"fautodiff_data_storage_{op}"
        return [f"{space}call {op_name}({self.var})\n"]

    def to_load(self) -> "PushPop":
        return PushPop(self.var, id=self.id, tmpvar=self.tmpvar, load=True)


@dataclass
class Allocate(Node):
    """An ``allocate`` statement."""

    vars: List[OpVar] = field(default_factory=list)

    def __post_init__(self):
        super().__post_init__()
        for v in self.vars:
            if not isinstance(v, OpVar):
                raise ValueError(f"vars must be OpVar: {type(v)}")

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self.vars:
            if var.index is not None:
                for v in var.index.collect_vars():
                    yield v

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        return iter(())

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        names = ", ".join(str(v) for v in self.vars)
        return [f"{space}allocate({names})\n"]

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Optional[Node]]:
        ad_vars: List[OpVar] = []
        for var in self.vars:
            ad_vars.append(var.add_suffix(AD_SUFFIX))
        nodes: List[Node] = []
        if reverse:
            if ad_vars:
                nodes.append(Deallocate(ad_vars))
            nodes.append(Deallocate(self.vars))
        else:
            nodes.append(self)
            if ad_vars:
                nodes.append(Allocate(ad_vars))
        return nodes

    def is_effectively_empty(self) -> bool:
        return not self.vars

    def prune_for(self, targets: VarList) -> Allocate:
        return self

@dataclass
class Deallocate(Node):
    """A ``deallocate`` statement."""

    vars: List[OpVar] = field(default_factory=list)

    def __post_init__(self):
        super().__post_init__()
        for v in self.vars:
            if not isinstance(v, OpVar):
                raise ValueError(f"vars must be OpVar: {type(v)}")

    def iter_ref_vars(self) -> Iterator[OpVar]:
        return iter(())

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        return iter(())

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        names = ", ".join(v.name for v in self.vars)
        return [f"{space}deallocate({names})\n"]

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        ad_vars: List[OpVar] = []
        for var in self.vars:
            ad_vars.append(var.add_suffix(AD_SUFFIX))
        nodes: List[Node] = []
        if reverse:
            if ad_vars:
                nodes.append(Allocate(ad_vars))
        else:
            if ad_vars:
                nodes.append(Deallocate(ad_vars))
        nodes.append(self)
        return nodes

    def is_effectively_empty(self) -> bool:
        return not self.vars

    def prune_for(self, targets: VarList) -> Deallocate:
        vars = []
        for var in self.vars:
            if var.name not in targets.names():
                vars.append(var)
        if vars:
            return Deallocate(vars)
        return Deallocate([])

@dataclass
class Declaration(Node):
    """A variable declaration."""

    name: str
    typename: str
    kind: Optional[str] = None
    dims: Optional[Tuple[str]] = None
    intent: Optional[str] = None
    parameter: bool = False
    constant: bool = False
    init: Optional[str] = None
    access: Optional[str] = None
    allocatable: bool = False
    declared_in: Optional[str] = None

    def __post_init__(self):
        super().__post_init__()
        if self.dims is not None and not isinstance(self.dims, tuple):
            raise ValueError(f"dims must be tuple of str: {type(dims)}")

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        if self.intent in ("in", "inout"):
            yield OpVar(
                name=self.name,
                typename=self.typename,
                kind=self.kind,
                is_constant=self.parameter or self.constant,
                allocatable=self.allocatable,
                declared_in=self.declared_in,
            )
        else:
            return iter(())

    def is_effectively_empty(self) -> bool:
        return False
    
    def collect_vars(self):
        return[OpVar(
            name=self.name,
            typename=self.typename,
            kind=self.kind,
            is_constant=self.parameter or self.constant,
            allocatable=self.allocatable,
            dims=self.dims,
            intent=self.intent,
            declared_in=self.declared_in,
        )]

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        line = f"{space}{self.typename}"
        if self.kind is not None:
            line += f"({self.kind})"
        if self.parameter:
            line += ", parameter"
        if self.access is not None:
            line += f", {self.access}"
        if self.allocatable:
            line += ", allocatable"
        if self.intent is not None:
            pad = "  " if self.intent == "in" else " "
            line += f", intent({self.intent})" + pad + f":: {self.name}"
        else:
            line += f" :: {self.name}"
        if self.dims is not None:
            dims = ",".join(self.dims)
            line += f"({dims})"
        if self.parameter and self.init is not None:
            line += f" = {self.init}"
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
            if self.name.endswith(AD_SUFFIX):
                vars = vars.copy()
                vars.push(
                    OpVar(
                        self.name,
                        typename=self.typename,
                        kind=self.kind,
                        is_constant=self.parameter or self.constant,
                        allocatable=self.allocatable,
                        declared_in=self.declared_in,
                    )
                )
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

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        cond_blocks = []
        for cond, block in self.cond_blocks:
            res = block.generate_ad(saved_vars, reverse, assigned_advars, routine_map, warnings)[0]
            if reverse:
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
        if reverse:
            loads = []
            blocks = []
            for var in block.assigned_vars():
                if not var.name.endswith(AD_SUFFIX):
                    load = self._save_vars(var, saved_vars)
                    loads.append(load)
                    blocks.insert(0, load)
            blocks.append(block)
            blocks.extend(loads)
        else:
            blocks = [block]
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
                if v.name.endswith(AD_SUFFIX):
                    vars_list.push(v)
        return vars_list

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
    range: OpRange

    def __post_init__(self):
        super().__post_init__()
        self.build_do_index_list([])
        if not isinstance(self.range, OpRange):
            raise ValueError(f"range must be OpRange: f{type(self.range)}")

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self.range.collect_vars():
            yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[str]:
        yield self.index

    def build_do_index_list(self, index_list: List[str]):
        self.do_index_list = [self.index.name]
        self.do_index_list.extend(index_list)
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

    def recurrent_vars(self) -> List[str]:
        required_vars = self._body.required_vars()
        assigned_vars = self._body.assigned_vars()
        common_var_names = sorted(set(required_vars.names()) & set(assigned_vars.names()))
        do_index_list = set(self.do_index_list)
        var_names = []
        for name in common_var_names:
            flag = False
            for index in required_vars[name]:
                if not(index is not None and do_index_list <= set(index.list())):
                    flag = True
                    break
            if not flag:
                for index in assigned_vars[name]:
                    if not(index is not None and do_index_list <= set(index.list())):
                        flag = True
                        break
            if flag:
                var_names.append(name)
        return var_names

    def private_vars(self) -> List[str]:
        required_vars = self._body.required_vars()
        assigned_vars = self._body.assigned_vars()
        common_var_names = sorted(set(required_vars.names()) & set(assigned_vars.names()))
        do_index_list = set(self.do_index_list)
        var_names = []
        for name in common_var_names:
            flag = True
            for index in required_vars[name]:
                if index is not None and do_index_list <= set(index.list()):
                    flag = False
                    break
            if flag:
                for index in assigned_vars[name]:
                    if index is not None and do_index_list <= set(index.list()):
                        flag = False
                        break
            if flag:
                var_names.append(name)
        return var_names

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:

        if not reverse:
            for vname in self.recurrent_vars():
                assigned_advars.push(OpVar(name=f"{vname}{AD_SUFFIX}"))

        body = self._body.generate_ad(saved_vars, reverse, assigned_advars, routine_map, warnings)[0]
        if reverse:
            body_org = self._body.deep_clone()
            new_body = []
            recurrent_vars = self.recurrent_vars()
            private_vars = self.private_vars()
            pushed = []
            for node in self._body.iter_children():
                for var in node.assigned_vars():
                    if var.name in recurrent_vars:
                        if not var in pushed:
                            if var.name in private_vars:
                                save = PushPop(var, node.get_id())
                            else:
                                save = SaveAssignment(var, node.get_id())
                            self._body.insert_begin(save)
                            new_body.append(save.to_load())
                            pushed.append(var)
            for node in body_org.iter_children():
                new_body.append(node)
            for node in body.iter_children():
                new_body.append(node)
            body = Block(new_body)
        index = self.index
        range = self.range
        if reverse:
            range = range.reverse()
        block = DoLoop(body, index, range)
        #block = block.prune_for(VarList(block.collect_vars()))

        if reverse:
            #common_vars = block.required_vars() & block.assigned_vars(without_savevar=True)
            common_vars = self._body.required_vars() & self._body.assigned_vars()
            common_vars.update_index_upward(self._build_index_map(), range=self.range)
            blocks = [block]
            for cvar in common_vars:
                if cvar == self.index or cvar.name.endswith(AD_SUFFIX):
                    continue
                load = self._save_vars(cvar, saved_vars)
                blocks.insert(0, load)
                blocks.append(load)
        else:
            blocks = [block]
        return blocks

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        header = f"{space}do {self.index} = {self.range[0]}, {self.range[1]}"
        if self.range[2] is not None:
            header = f"{header}, {self.range[2]}"
        lines = [f"{header}\n"]
        lines.extend(self._body.render(indent+1))
        lines.append(f"{space}end do\n")
        return lines

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            vars.update_index_downward(self._build_index_map(), self.index)
        vars = self._body.required_vars(vars, no_accumulate, without_savevar)

        if self.index in vars:
            vars.remove(self.index)
        index_map = self._build_index_map()
        if self.range[2] is None or (isinstance(self.range[2], OpInt) and self.range[2].val==1) or (isinstance(self.range[2], OpNeg) and isinstance(self.range[2].args[0], OpInt) and self.range[2].args[0].val==1):
            step = 1 if self.range[2] is None else self.range[2]
            plusOne = self.index + step
            minusOne = self.index - step
            for name in vars.names():
                if name in index_map:
                    do_index, _ = index_map[name]
                else:
                    continue
                index_new = []
                for index in vars.vars[name]:
                    if index is not None and index[do_index] is not None:
                        if index[do_index] == plusOne:
                            index = index.copy()
                            index[do_index] = self.range[1] + step
                        elif index[do_index] == minusOne:
                            index = index.copy()
                            index[do_index] = self.range[0] - step
                    index_new.append(index)
                vars.vars[name] = index_new
        vars.update_index_upward(index_map, range=self.range)
        for var in self.range.collect_vars():
            vars.push(var)
        return vars

    def assigned_vars(self, vars: Optional[VarList] = None, without_savevar: bool = False, check_init_advars: bool = False) -> VarList:
        vars = self._body.assigned_vars(vars, without_savevar=without_savevar, check_init_advars=check_init_advars)
        vars.update_index_upward(self._build_index_map(), range=self.range)
        if not check_init_advars:
            vars.push(self.index)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars = self._body.unrefered_advars(vars)
        vars.update_index_upward(self._build_index_map(), range=self.range)
        return vars

    def prune_for(self, targets: VarList) -> Node:
        targets = targets.copy()
        #targets.update_index_downward(self._build_index_map(), self.index)
        new_body = self._body.prune_for(targets)

        for var in new_body.required_vars(targets):
            if var.name == self.index.name:
                continue
            #if var.name.endswith(AD_SUFFIX):
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
                            index_new = []
                            child.tmpvar.reduced_dims = []
                            for i, idx in enumerate(child.tmpvar.index):
                                if isinstance(idx, OpVar) and idx.name in self.do_index_list:
                                    child.tmpvar.reduced_dims.append(i)
                                else:
                                    index_new.append(idx)
                            child.tmpvar.index = AryIndex(index_new)
                    continue
                if isinstance(child, DoLoop):
                    continue
                _reducedim_from_tmpvar(child)
        _reducedim_from_tmpvar(new_body)

        if new_body.is_effectively_empty():
            return Block([])
        return DoLoop(new_body, self.index, self.range)

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
        assigned_vars.update_index_upward(self._build_index_map(), range=self.range)
        return assigned_vars

@dataclass
class DoWhile(DoAbst):
    """A ``do while`` loop."""

    cond: Operator

    def __post_init__(self):
        self.do_index_list = ["__never_match__"]

    def iter_ref_vars(self) -> Iterator[OpVar]:
        yield from self.cond.collect_vars()

    def build_do_index_list(self, index_list: List[str]) -> None:
        for child in self.iter_children():
            child.build_do_index_list(index_list)

    def generate_ad(
        self,
        saved_vars: List[SaveAssignment],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        body = self._body.generate_ad(saved_vars, reverse, assigned_advars, routine_map, warnings)[0]
        if reverse:
            new_body = self._body.deep_clone().remove_push()
            if not new_body.is_effectively_empty():
                for node in body.children:
                    new_body.append(node)
                body = new_body
        block = DoWhile(body, self.cond)
        if reverse:
            loads = []
            blocks = []
            for var in block.assigned_vars():
                load = self._save_vars(OpVar(var), saved_vars)
                loads.append(load)
                blocks.insert(0, load)
            blocks.append(block)
            for load in loads:
                blocks.append(load)
        else:
            blocks = [block]
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
