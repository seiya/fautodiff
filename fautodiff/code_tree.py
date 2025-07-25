"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Tuple, Dict, Optional, Iterator, Pattern, ClassVar, Union
import re
from functools import reduce

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
    OpTrue,
    OpFalse,
    OpNeg,
    OpRange,
    OpFunc,
    OpFuncUser,
    OpNot,
    OpLogic,
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

    def copy(self) -> "Node":
        """Return a shallow copy of this node."""
        raise NotImplementedError(f"class: {type(self)}")

    def render(self, indent: int = 0) -> List[str]:
        """Return the formatted Fortran code lines for this node."""
        raise NotImplementedError(f"class: {type(self)}")

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
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        """Return AD converted nodes."""
        return []

    def set_for_exitcycle(self,
                          exitcycle_flags: Optional[List[OpVar]] = None,
                          set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
                          label: Optional[str] = None,
                          label_map: Optional[List[Tuple[str,str]]] = None,
                          set_cond: bool = False,
                          keep: bool = False,
                          ) -> List["Node"]:
        return [self]

    def _save_vars(self, var: OpVar, saved_vars: List[OpVar]) -> SaveAssignment:
        id = self.get_id()
        save = SaveAssignment(var, id)
        self.parent.insert_before(id, save)
        load = save.to_load()
        saved_vars.append(save.tmpvar)
        return load

    def deep_clone(self) -> "Node":
        """Return a deep clone of this node tree with new ids."""
        raise NotImplementedError(f"class: {type(self)}")

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
        raise NotImplementedError(f"class: {type(self)}")

    def insert_begin(self, node: "Node"):
        """Insert node to the before of node with id"""
        raise NotImplementedError(f"class: {type(self)}")

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

    def collect_exitcycle(self) -> List["Node"]:
        """Return Exit and Cycle nodes in this node."""
        nodes = []
        for child in self.iter_children():
            if isinstance(child, (ExitStmt, CycleStmt)):
                nodes.append(child)
            else:
                nodes.extend(child.collect_exitcycle())
        return nodes

    def build_do_index_list(self, index_list: List[str]) -> None:
        """Build index list of ``do`` loops."""
        self.do_index_list = index_list
        for child in self.iter_children():
            child.build_do_index_list(index_list)

    def build_parent(self) -> None:
        for child in self.iter_children():
            child.set_parent(self)
            child.build_parent()

    # ------------------------------------------------------------------
    # optimization helpers
    # ------------------------------------------------------------------

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Optional["Node"]:
        """Return a copy of this node with only code needed for ``targets``."""
        for var in self.assigned_vars():
            if var in targets:
                return self.deep_clone()
        return None

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

    _pattern: ClassVar[Pattern[str]] = re.compile(r"([a-zA-Z][a-zA-Z0-9_]*)_save_([0-9]+)")
    @classmethod
    def is_savevar(cls, name: str) -> bool:
        return bool(cls._pattern.match(name))

    @classmethod
    def get_arg_info(cls, routine: Union[OpFuncUser, CallStatement], routine_map: Optional[dict] = None, generic_map: Optional[dict] = None) -> Optional[dict]:
        """Return argument information for the given name."""
        if routine_map is None:
            return None
        name = routine.name
        arg_info = routine_map.get(name)
        if arg_info is not None:
            nargs = len(routine.args)
            if isinstance(routine, OpFuncUser) or routine.result is not None:
                nargs += 1
            if not arg_info.get("skip") and (arg_info["args"] is None or len(arg_info["args"]) != nargs):
                raise RuntimeError(
                    f"Argument length mismatch for {name}: "
                    f"{len(arg_info['args'])} != {len(routine.args)} "
                    f"({arg_info['args']}) ({[v.name for v in routine.args]})"
                )
            return arg_info
        argtypes = [arg.typename for arg in routine.args]
        argkinds = [arg.kind for arg in routine.args]
        argdims = [(arg.dims and len(arg.dims)) for arg in routine.args]
        if arg_info is None and generic_map and name in generic_map:
            for cand in generic_map[name]:
                if not cand in routine_map:
                    raise RuntimeError(f"Not found in routine_map: {cand}")
                arg_info = routine_map[cand]
                if "type" in arg_info and arg_info["type"] == argtypes:
                    if "kind" in arg_info and arg_info["kind"] == argkinds:
                        if "dims" in arg_info and [(arg and len(arg)) for arg in arg_info["dims"]] == argdims:
                            return arg_info
        return None

    def _generate_ad_forward(
        self,
        lhs: OpVar,
        rhs: Operator,
        assigned_advars: VarList,
        saved_vars: List[OpVar],
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        rhs = rhs.deep_clone()
        assigns: List[Node] = []
        funcs = rhs.find_userfunc()
        for n, ufunc in enumerate(funcs):
            if routine_map is None:
                raise RuntimeError("routine_map is necessary for CallStatement")
            arg_info = Node.get_arg_info(ufunc, routine_map, generic_map)
            if arg_info is None:
                raise RuntimeError(f"Not found in routime_map: {ufunc.name}")
            if rhs == ufunc and len(funcs) == 1:
                result = lhs
            else:
                name = self._save_var_name(f"{ufunc.name}{n}", self.get_id(), no_suffix=True)
                result = OpVar(name, typename=arg_info["type"][-1])
                saved_vars.append(result)
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
            call_nodes = callstmt.generate_ad(
                saved_vars,
                reverse=False,
                assigned_advars=assigned_advars,
                routine_map=routine_map,
                generic_map=generic_map,
                mod_vars=mod_vars,
                warnings=warnings,
            )
            assigns.extend(call_nodes)
            if rhs == ufunc and len(funcs) == 1:
                return assigns
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
                if not grad_lhs.is_array() and expr.is_array():
                    expr = OpFunc("sum", args=[expr])
                assigns.append(Assignment(grad_lhs, expr, ad_info=ad_info))
                assigned_advars.push(grad_lhs)
        return assigns

    def _generate_ad_reverse(
        self,
        lhs: OpVar,
        rhs: Operator,
        saved_vars: List[OpVar],
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        rhs = rhs.deep_clone()
        extras: List[Node] = []
        funcs = rhs.find_userfunc()
        for n, ufunc in enumerate(funcs):
            if routine_map is None:
                raise RuntimeError("routine_map is necessary for CallStatement")
            arg_info = Node.get_arg_info(ufunc, routine_map, generic_map)
            if arg_info is None:
                raise RuntimeError(f"Not found in routime_map: {ufunc.name}")
            if rhs == ufunc and len(funcs) == 1:
                result = lhs
            else:
                name = self._save_var_name(f"{ufunc.name}{n}", self.get_id(), no_suffix=True)
                result = OpVar(name, typename=arg_info["type"][-1])
                saved_vars.append(result)
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
            call_nodes = callstmt.generate_ad(
                saved_vars,
                reverse=True,
                routine_map=routine_map,
                generic_map=generic_map,
                mod_vars=mod_vars,
                warnings=warnings,
            )
            extras.extend(call_nodes)
            if rhs == ufunc and len(funcs) == 1:
                return call_nodes
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
                if not v.is_array() and res.is_array():
                    res = OpFunc("sum", args=[res])
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

    _children: List[Node] = field(default_factory=list)

    def _set_parent(self, child: Node) -> None:
        if child.parent is not None:
            child = child.copy()
        child.set_parent(self)

    def __post_init__(self):
        super().__post_init__()
        for child in self._children:
            if isinstance(child, Block):
                raise ValueError("Block in Block is not allowed")
            self._set_parent(child)

    def copy(self) -> "Block":
        return Block(self._children)

    def deep_clone(self) -> "Block":
        clone = Block()
        for node in self._children:
            clone._children.append(node.deep_clone())
        return clone

    def iter_children(self) -> Iterator[Node]:
        return iter(self._children)

    def __getitem__(self, index: int) -> Node:
        return self._children[index]

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[List[str]] = None,
    ) -> List[Node]:
        ad_code: List[Node] = []
        iterator = self._children
        if reverse:
            iterator = reversed(iterator)
        for node in iterator:
            if reverse and exitcycle_flags:
                ec_flags = [ec.flag() for ec in node.collect_exitcycle()]
            else:
                ec_flags = None
            nodes = node.generate_ad(saved_vars, reverse, assigned_advars, routine_map, generic_map, mod_vars, ec_flags, warnings)
            nodes = [node for node in nodes if node and not node.is_effectively_empty()]
            if reverse and nodes and exitcycle_flags and not isinstance(node, (ExitStmt, CycleStmt)):
                if ec_flags:
                    flags = [flag for flag in exitcycle_flags if not flag in ec_flags]
                else:
                    flags = exitcycle_flags
                if flags:
                    cond = reduce(lambda x, y: x & y, flags)
                    ad_code.append(IfBlock([(cond, Block(nodes))]))
                else:
                    ad_code.extend(nodes)
            else:
                ad_code.extend(nodes)
        return ad_code

    def set_for_exitcycle(self,
                          exitcycle_flags: Optional[List[OpVar]] = None,
                          set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
                          label: Optional[str] = None,
                          label_map: Optional[List[Tuple[str,str]]] = None,
                          set_cond: bool = False,
                          keep: bool = False,
                          ) -> List[Node]:
        nodes: List[Node] = []
        if set_cond and exitcycle_flags:
            cond = reduce(lambda x, y: x & y, exitcycle_flags)
            block = []
            for node in self.iter_children():
                ce_flags = node.collect_exitcycle()
                if ce_flags:
                    node_new = node.set_for_exitcycle(exitcycle_flags, set_do_index, label, label_map, set_cond=False, keep=keep)
                    block.extend(node_new)
                    if block:
                        nodes.append(IfBlock([(cond, Block(block))]))
                        block = []
                    continue
                block.append(node)
            if block:
                nodes.append(IfBlock([(cond, Block(block))]))
        else:
            for node in self.iter_children():
                nodes.extend(node.set_for_exitcycle(exitcycle_flags, set_do_index, label, label_map, set_cond=False, keep=keep))
        return nodes

    def find_by_name(self, name: str) -> Optional[Declaration]:
        for child in self.iter_children():
            if isinstance(child, Declaration) and child.name == name:
                return child
        return None

    def remove_child(self, child: Node) -> None:
        self._children.remove(child)

    def first(self) -> Node:
        """Return the first element."""
        if len(self._children) > 0:
            return self._children[0]
        return None

    def last(self) -> Node:
        """Return the last element."""
        if len(self._children) > 0:
            return self._children[-1]
        return None

    def append(self, node: Node) -> None:
        """Append ``node`` to this block."""
        self._set_parent(node)
        node.build_do_index_list(self.do_index_list)
        # for declaration block
        if isinstance(node, Declaration):
            routine = self.get_routine()
            if routine is not None:
                if routine.decl_map is None:
                    routine.decl_map = {}
                routine.decl_map[node.name] = node
        self._children.append(node)

    def extend(self, nodes: Iterable[Node]) -> None:
        """Extend this block with ``nodes``."""
        for node in nodes:
            self.append(node)

    def insert_before(self, id: int, node:Node) -> Node:
        for i, child in enumerate(self._children):
            if child.get_id() == id:
                node.build_do_index_list(self.do_index_list)
                self._set_parent(node)
                return self._children.insert(i, node)
        raise ValueError("id is not found")

    def insert_begin(self, node:Node) -> Node:
        node.build_do_index_list(self.do_index_list)
        self._set_parent(node)
        return self._children.insert(0, node)

    def __iter__(self):
        return self.iter_children()

    def __len__(self) -> int:
        return len(self._children)

    def render(self, indent: int = 0) -> List[str]:
        lines: List[str] = []
        for child in self.iter_children():
            lines.extend(child.render(indent))
        return lines

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        for child in reversed(self._children):
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

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> "Block":
        new_children: List[Node] = []
        for child in reversed(self._children):
            pruned = child.prune_for(targets, mod_vars, decl_map)
            if pruned is not None and not pruned.is_effectively_empty():
                new_children.insert(0, pruned)
                targets = pruned.required_vars(targets)
        children = new_children
        if len(children) >= 2:
            i = 0
            while i < len(children)-1:
                child1 = children[i]
                if not isinstance(child1, SaveAssignment) or child1.load or child1.pushpop:
                    i += 1
                    continue
                var = child1.var
                for child2 in children[i+1:]:
                    if isinstance(child2, SaveAssignment) and child2.var == var and child2.id == child1.id and child2.load:
                        children.remove(child2)
                        children.remove(child1)
                        break
                    if var in child2.assigned_vars():
                        i += 1
                        break
                i += 1

            new_children = []
            for child in children:
                if len(new_children) > 0:
                    last = new_children[-1]
                    # if (isinstance(child, SaveAssignment) and not child.pushpop and
                    #     isinstance(last,  SaveAssignment) and not last.pushpop and
                    #     last.var.name == child.var.name and last.id == child.id and last.load != child.load):
                    #     new_children.remove(last)
                    #     continue
                    if isinstance(child, DoLoop) and isinstance(last, DoLoop):
                        item1 = child._body[0]
                        item2 = last._body[-1]
                        while (isinstance(item1, SaveAssignment) and not item1.pushpop and
                               isinstance(item2, SaveAssignment) and not item2.pushpop and
                               item1.var.name == item2.var.name and item1.id == item2.id and item1.load != item2.load):
                                child._body.remove_child(item1)
                                last._body.remove_child(item2)
                                if child._body.is_effectively_empty() or last._body.is_effectively_empty():
                                    break
                                item1 = child._body[0]
                                item2 = last._body[-1]
                        if last.is_effectively_empty():
                            new_children.remove(last)
                        if child.is_effectively_empty():
                            continue
                    if isinstance(child, IfBlock) and isinstance(last, IfBlock):
                        cb1 = child.cond_blocks[0]
                        cb2 = last.cond_blocks[0]
                        if (cb1[0] == cb2[0] and
                            cb1[1][0]  and isinstance(cb1[1][0],  SaveAssignment) and not cb1[1][0].pushpop and
                            cb2[1][-1] and isinstance(cb2[1][-1], SaveAssignment) and not cb2[1][-1].pushpop and
                            cb1[1][0].var.name == cb2[1][-1].var.name and cb1[1][0].id == cb2[1][-1].id and cb1[1][0].load != cb2[1][-1].load):
                            cb1[1].remove_child(cb1[1][0])
                            cb2[1].remove_child(cb2[1][-1])
                            flag = True
                            if last.is_effectively_empty():
                                new_children.remove(last)
                            if child.is_effectively_empty():
                                continue
                    if (isinstance(last, Assignment) and isinstance(last.rhs, OpTrue) and
                        isinstance(child, IfBlock) and len(child.cond_blocks) == 1 and child.cond_blocks[0][0] == last.lhs):
                        if not last.ad_info:
                            new_children.remove(last)
                        block = child.cond_blocks[0][1]
                        block.remove_child(block[0])
                        if not block.is_effectively_empty():
                            for node in block.iter_children():
                                new_children.append(node)
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

    def copy(self) -> "Statement":
        return Statement(self.body)

    def deep_clone(self) -> "Statement":
        return self.copy()

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}{self.body}\n"]
        return lines

    def is_effectively_empty(self) -> bool:
        return False


@dataclass
class ExitCycle(Node):
    """Abstract class for ExtiStmt and CycleStmt"""

    label: Optional[str] = field(default=None)

    def copy(self) -> "ExitCycle":
        return self.__class__(self.label)

    def deep_clone(self) -> "ExitCycle":
        return self.copy()

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        label = f" {self.label}" if self.label else ""
        return [f"{space}{self.name}{label}\n"]

    def iter_ref_vars(self) -> Iterator[OpVar]:
        return iter(())

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        return iter(())

    def collect_exitcycle(self) -> List[Node]:
        return [self]

    def is_effectively_empty(self) -> bool:
        return False

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> "ExitCycle":
        return self

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        if reverse:
            label = f" {self.label}" if self.label else ""
            return [Assignment(self.flag(), OpTrue(), ad_info=f"{self.name}{label}")]
        return [self]

    def set_for_exitcycle(self,
                          exitcycle_flags: Optional[List[OpVar]] = None,
                          set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
                          label: Optional[str] = None,
                          label_map: Optional[List[Tuple[str,str]]] = None,
                          set_cond: bool = False,
                          keep: bool = False,
                          ) -> List[Node]:
        nodes: List[Node] = []
        if set_do_index and isinstance(self, ExitStmt):
            nodes.append(Assignment(set_do_index[0], set_do_index[1]))
        if exitcycle_flags:
            nodes.append(Assignment(self.flag(), OpFalse()))
        if exitcycle_flags is None or keep:
            if label and label_map:
                label = next((label_new for label_org, label_new in label_map if label_org == self.label), label_map[0][1])
                nodes.append(self.__class__(label))
            else:
                nodes.append(self)
        return nodes

    def flag(self) -> OpVar:
        return OpVar(f"{self.name}_flag_{self.get_id()}_ad", typename="logical")


@dataclass
class ExitStmt(ExitCycle):
    """Representation of an ``exit`` statement."""

    name: ClassVar[str] = "exit"


@dataclass
class CycleStmt(ExitCycle):
    """Representation of a ``cycle`` statement."""

    name: ClassVar[str] = "cycle"


@dataclass
class Use(Node):
    """Representation of a Fortran use statement."""
    name: str
    only: Optional[List[str]] = field(default=None)

    def copy(self) -> "Use":
        return Use(self.name, self.only)

    def deep_clone(self) -> "Use":
        only = list(self.only) if self.only else None
        return Use(self.name, only)

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
    associated_vars: Optional[List[OpVar]] = field(default=None)
    donot_prune: bool = False

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

    def copy(self) -> "CallStatement":
        return CallStatement(self.name, self.args, self.arg_keys, self.intents, self.result, self.info, self.ad_info)

    def deep_clone(self) -> "CallStatement":
        args = [arg.deep_clone() for arg in self.args]
        arg_keys = list(self.arg_keys) if self.arg_keys else None
        intents = list(self.intents) if self.intents else None
        result = self.result.deep_clone() if self.result else None
        return CallStatement(name=self.name,
                             args=args,
                             arg_keys=arg_keys,
                             intents=intents,
                             result=result,
                             info=self.info,
                             ad_info=self.ad_info,
                             associated_vars=self.associated_vars,
                             donot_prune=self.donot_prune)

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
        intents = self.intents or ["inout"] * len(self.args)
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
                    if not var.is_constant:
                        vars.push(var)
                if self.associated_vars is not None:
                    for var in self.associated_vars:
                        vars.push(var)
        return vars

    @classmethod
    def rename_args(cls,
                    vars_org: List[Operator],
                    args_org: List[str],
                    args_target: List[str],
                    vars_tmp: Optional[List[OpVar]] = None,
                    reverse: bool = False,
                    ) -> List[OpVar]:
        if len(vars_org) != len(args_org):
            raise ValueError(f"length of vars_org and args_org must be the same")
        vars_target = []
        for arg in args_target:
            if arg.endswith(AD_SUFFIX):
                arg_org = arg.removesuffix(AD_SUFFIX)
                add_ad = True
            else:
                arg_org = arg
                add_ad = False
            idx = args_org.index(arg_org)
            var = vars_org[idx]
            if vars_tmp is not None:
                if not reverse and isinstance(var, OpFuncUser) or add_ad:
                    var = vars_tmp[idx]
            if add_ad:
                if isinstance(var, OpVar) and not var.is_constant:
                    var = var.add_suffix(AD_SUFFIX)
            vars_target.append(var)
        return vars_target

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        if routine_map is None:
            raise RuntimeError("routine_map is necessary for CallStatement")
        name = self.name
        arg_info = Node.get_arg_info(self, routine_map, generic_map)
        if arg_info is None:
            #print(routine_map)
            raise RuntimeError(f"Not found in routime_map: {name}")

        name_key = "name_rev_ad" if reverse else "name_fwd_ad"
        if arg_info.get("skip") or arg_info.get(name_key) is None:
            if reverse:
                return [Statement(f"! {name} is skiped")]
            else:
                return[self]

        tmp_vars = []
        call_args = list(self.args)
        arg_keys = list(self.arg_keys)
        param_names = list(arg_info["args"])
        if self.result is not None:
            param_names_no_res = param_names[:-1]
        else:
            param_names_no_res = param_names

        # for keyword arguments, we need to order them
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
        if self.result is not None:
            ordered.append(self.result)

        # if arguments are operators or functions, we need to save them
        args_new = []
        for i, arg in enumerate(ordered):
            if not isinstance(arg, OpLeaf) and arg_info["type"][i] == "real":
                name = self._save_var_name(f"{self.name}_arg{i}", self.get_id(), no_suffix=True)
                tmp = OpVar(name, typename="real")
                tmp_vars.append((tmp, arg))
                args_new.append(tmp)
                saved_vars.append(tmp)
                saved_vars.append(
                    OpVar(
                        f"{tmp.name}{AD_SUFFIX}",
                        typename="real",
                        kind=arg_info["kind"][i],
                        dims=arg_info["dims"][i],
                    )
                )
            else:
                args_new.append(arg)

        # get arguments for ad call based on fadmod information
        ad_args = []
        if reverse:
            name_key = "name_rev_ad"
            args_key = "args_rev_ad"
            intents_key = "intents_rev_ad"
        else:
            name_key = "name_fwd_ad"
            args_key = "args_fwd_ad"
            intents_key = "intents_fwd_ad"
        ad_args = CallStatement.rename_args(ordered, arg_info["args"], arg_info[args_key], args_new, reverse)
        # for ad_arg in arg_info[args_key]:
        #     if ad_arg.endswith(AD_SUFFIX):
        #         arg = ad_arg.removesuffix(AD_SUFFIX)
        #     else:
        #         arg = ad_arg
        #     i = arg_info["args"].index(arg)
        #     var = args[i]
        #     if not reverse and isinstance(var, OpFuncUser):
        #         var = args_new[i]
        #     if ad_arg.endswith(AD_SUFFIX):
        #         var = args_new[i]
        #         if var.is_constant:
        #             ad_name = var.name
        #         else:
        #             ad_name = f"{var.name}{AD_SUFFIX}"
        #         var = OpVar(
        #             name=ad_name,
        #             index=var.index,
        #             kind=var.kind,
        #             typename=var.typename,
        #             ad_target=var.ad_target,
        #             is_constant=var.is_constant,
        #         )
        #     ad_args.append(var)
        if self.associated_vars is None:
            associated_vars = None
        else:
            associated_vars = []
            for var in self.associated_vars:
                if not reverse:
                    associated_vars.append(var)
                associated_vars.append(var.add_suffix(AD_SUFFIX))

        ad_call = CallStatement(name=arg_info[name_key], args=ad_args, intents=arg_info[intents_key], ad_info=self.info["code"], associated_vars=associated_vars)
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
                            generic_map=generic_map,
                            mod_vars=mod_vars,
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
                            generic_map=generic_map,
                            mod_vars=mod_vars,
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
            need_orig = False
            try:
                ad_names = arg_info["args_fwd_ad"]
                ad_intents = arg_info["intents_fwd_ad"]
                for name, intent in zip(arg_info["args"], arg_info["intents"]):
                    if intent in ("out", "inout"):
                        if name in ad_names:
                            idx = ad_names.index(name)
                            if ad_intents[idx] not in ("out", "inout"):
                                need_orig = True
                                break
                        else:
                            need_orig = True
                            break
                if self.result is not None:
                    res_name = arg_info["args"][-1]
                    if res_name in ad_names:
                        idx = ad_names.index(res_name)
                        if ad_intents[idx] not in ("out", "inout"):
                            need_orig = True
                    else:
                        need_orig = True
            except Exception:
                need_orig = True

            blocks = ad_nodes
            if need_orig:
                blocks.append(self)

        return blocks

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Optional["CallStatement"]:
        if self.donot_prune:
            return self.deep_clone()
        for var in self.assigned_vars():
            if var in targets:
                return self.deep_clone()
        if self.associated_vars is not None:
            for var in self.associated_vars:
                if var in targets:
                    return self.deep_clone()
        return None

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        intents = self.intents or ["inout"] * len(self.args)
        for arg, intent in zip(self.args, intents):
            if intent in ("out", "inout"):
                for var in arg.collect_vars():
                    if var.name.endswith(AD_SUFFIX):
                        assigned_vars = assigned_vars.copy()
                        assigned_vars.push(var)
        if self.result is not None and self.result.name.endswith(AD_SUFFIX):
            assigned_vars = assigned_vars.copy()
            assigned_vars.push(self.result)
        return assigned_vars

@dataclass
class Module(Node):
    """Representation of a Fortran module."""
    name: str
    uses: Block = field(default_factory=Block)
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
        lines.extend(self.uses.render(indent+1))
        lines.extend(self.body.render(indent+1))
        if self.decls is not None:
            lines.append("\n")
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
    decl_map: Optional[dict] = None
    ad_init: Optional[Block] = None
    ad_content: Optional[Block] = None
    kind: ClassVar[str] = "subroutine"

    def __post_init__(self):
        super().__post_init__()
        self.decls.set_parent(self)
        self.content.set_parent(self)

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
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
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
        decl = None
        if self.decl_map is not None:
            decl = self.decl_map.get(name)
        if decl is None:
            decl = self.decls.find_by_name(name)
        if decl is None:
            return None
        intent = decl.intent
        if self.result == name:
            intent = "out"
        return OpVar(
            name,
            kind=decl.kind,
            char_len=decl.char_len,
            dims=decl.dims,
            typename=decl.typename,
            intent=intent,
            ad_target=None,
            is_constant=decl.parameter or getattr(decl, "constant", False),
            optional=decl.optional,
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
        if self.decl_map is not None and self.decl_map.get(name) is not None:
            return True
        if self.decls.find_by_name(name) is not None:
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

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> "Routine":
        ad_content = self.ad_content.prune_for(targets, mod_vars, decl_map)
        targets = ad_content.required_vars(targets)
        ad_init = self.ad_init.prune_for(targets, mod_vars, decl_map)
        targets = ad_init.required_vars(targets)
        content = self.content.prune_for(targets, mod_vars, decl_map)
        all_vars = content.collect_vars()
        _extend_unique(all_vars, ad_init.collect_vars())
        _extend_unique(all_vars, ad_content.collect_vars())
        decls = self.decls.prune_for(VarList(all_vars), mod_vars, decl_map)
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

    def copy(self) -> "Assignment":
        return Assignment(self.lhs, self.rhs, self.accumulate, self.info, self.ad_info)

    def deep_clone(self):
        lhs = self.lhs.deep_clone()
        rhs = self.rhs.deep_clone()
        return Assignment(lhs, rhs, self.accumulate, self.info, self.ad_info)

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
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Assignment]:
        if reverse:
            assigns: List[Node] = [self._save_vars(self.lhs, saved_vars)]
            assigns.extend(
                self._generate_ad_reverse(
                    self.lhs,
                    self.rhs,
                    saved_vars,
                    routine_map=routine_map,
                    generic_map=generic_map,
                    mod_vars=mod_vars,
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
                generic_map=generic_map,
                mod_vars=mod_vars,
                warnings=warnings,
            )
            if not (
                len(assigns) == 1
                and isinstance(assigns[0], CallStatement)
                and isinstance(self.rhs, OpFuncUser)
                and len(self._ufuncs) == 1
            ):
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
class PointerAssignment(Node):
    """A pointer assignment statement ``lhs => rhs``."""

    lhs: OpVar
    rhs: Operator
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
        self._rhs_vars = list(self.rhs.collect_vars())
        self._ufuncs = self.rhs.find_userfunc()

    def copy(self) -> "PointerAssignment":
        return PointerAssignment(self.lhs, self.rhs, self.info, self.ad_info)

    def deep_clone(self):
        lhs = self.lhs.deep_clone()
        rhs = self.rhs.deep_clone()
        return PointerAssignment(lhs, rhs, self.info, self.ad_info)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self._rhs_vars:
            yield var
        if self.lhs.index is not None:
            for var in self.lhs.index.collect_vars():
                yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        yield self.lhs

    def is_effectively_empty(self) -> bool:
        return False

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        return [self]

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        ad_comment = ""
        if self.ad_info is not None:
            ad_comment = f" ! {self.ad_info}"
        return [f"{space}{self.lhs} => {self.rhs}{ad_comment}\n"]

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        lhs = self.lhs
        if vars is None:
            vars = VarList()
        else:
            if not isinstance(vars, VarList):
                raise ValueError(f"Must be VarList: {type(vars)}")
            vars = vars.copy()
            vars.remove(lhs)
        for var in lhs.collect_vars():
            if var != lhs:
                vars.push(var)
        for var in self._rhs_vars:
            vars.push(var)
        return vars

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
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

    def copy(self) -> "ClearAssignment":
        return ClearAssignment(self.lhs, self.info, self.ad_info)

    def deep_clone(self):
        lhs = self.lhs.deep_clone()
        return ClearAssignment(lhs, self.info, self.ad_info)

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
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
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

    def copy(self) -> "SaveAssignment":
        return SaveAssignment(self.var, self.id, self.tmpvar, self.load)

    def deep_clone(self) -> "SaveAssignment":
        return self.copy()

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

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Optional[SaveAssignment]:
        found = False
        for var in self.iter_assign_vars():
            if var in targets:
                found = True
                break
        if not found:
            return None
        if self.load:
            name = self.var.name
            index = self.var.index
            if not self.var.name in targets.names():
                raise RuntimeError
            index_target = None
            flag = True
            for idx in targets[name]:
                if idx is None or not idx <= index:
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

    # def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
    #     if vars is None:
    #         vars = VarList()
    #     # else:
    #     #     vars_new = VarList()
    #     #     for name in vars.names():
    #     #         if not name.startswith(f"{self.var.name}_save_"):
    #     #             vars_new.vars[name] = vars.vars[name]
    #     #     vars = vars_new
    #     return super().required_vars(vars, no_accumulate, without_savevar)

    def to_load(self) -> "PushPop":
        return PushPop(self.var, id=self.id, tmpvar=self.tmpvar, load=True)


@dataclass
class PushPopL(PushPop):

    def __init__(self, flag: str):
        Node.__init__(self)
        self.flag = flag

    def __post_init__(self):
        Node.__post_init__(self)

    def copy(self) -> "PushPopL":
        return PushPopL(self.flag)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        if self.load:
            raise RuntimeError
        else:
            return [f"{space}call fautodiff_data_storage_push({self.flag})\n"]

    def iter_assign_vars(self, without_savevar=False):
        return iter(())

    def iter_ref_vars(self):
        return iter(())

    def iter_children(self):
        return iter(())

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            return VarList([])
        return vars

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> "PushPopL":
        return self

    def to_load(self):
        raise NotImplementedError

@dataclass
class Allocate(Node):
    """An ``allocate`` statement."""

    vars: List[OpVar] = field(default_factory=list)

    def __post_init__(self):
        super().__post_init__()
        for v in self.vars:
            if not isinstance(v, OpVar):
                raise ValueError(f"vars must be OpVar: {type(v)}")

    def copy(self) -> "Allocate":
        return Allocate(self.vars)

    def deep_clone(self) -> "Allocate":
        return Allocate(self.vars.deep_clone())

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
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        nodes: List[Node] = []
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        for var in self.vars:
            is_mod_var = var.name in mod_var_names
            ad_var = var.add_suffix(AD_SUFFIX)
            if reverse:
                if var.ad_target:
                    nodes.append(Allocate._add_if(Deallocate([ad_var], ad_code=True), ad_var, is_mod_var))
                if not is_mod_var:
                    nodes.append(Deallocate([var], ad_code=True))
            else:
                if not is_mod_var:
                    nodes.append(Allocate([var]))
                if var.ad_target:
                    nodes.append(Allocate._add_if(Allocate([ad_var]), ad_var, is_mod_var))
        return nodes

    def is_effectively_empty(self) -> bool:
        return not self.vars

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Optional[Allocate]:
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        vars = []
        for var in self.vars:
            if not var.name in mod_var_names and (any(var.name == f"{name}{AD_SUFFIX}" for name in mod_var_names) or (decl_map is None or var.name in decl_map)):
                vars.append(var)
        if vars:
            return Allocate(vars)
        return None

    @classmethod
    def _add_if(cls, node: Node, var: OpVar, is_mod_var: bool) -> Node:
        """Wrap ``node`` in a conditional block when needed."""

        # Pointer arguments and module variables might already be
        # allocated/associated outside of the current routine.  Guard the
        # (de)allocation so we do not operate on them twice.  Local pointer
        # variables have a well defined state so do not require this check.
        check = is_mod_var or (var.intent in ("in", "inout") and (var.allocatable or var.pointer))
        if check:
            func = "associated" if var.pointer else "allocated"
            cond = OpFunc(func, args=[var.change_index(None)])
            body = Block([node])
            if isinstance(node, Allocate):
                cond = OpNot([cond])
                if var.name.endswith(AD_SUFFIX):
                    body.append(ClearAssignment(var.change_index(None)))
            return IfBlock([(cond, body)])
        return node

@dataclass
class Deallocate(Node):
    """A ``deallocate`` statement."""

    vars: List[OpVar] = field(default_factory=list)
    ad_code: bool = field(default=False)

    def __post_init__(self):
        super().__post_init__()
        for v in self.vars:
            if not isinstance(v, OpVar):
                raise ValueError(f"vars must be OpVar: {type(v)}")

    def copy(self) -> "Deallocate":
        return Deallocate(self.vars, self.ad_code)

    def deep_clone(self):
        vars = [var.deep_clone() for var in self.vars]
        return Deallocate(vars, self.ad_code)

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
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        nodes: List[Node] = []
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        for var in self.vars:
            is_mod_var = var.name in mod_var_names
            ad_var = var.add_suffix(AD_SUFFIX)
            if reverse:
                if var.ad_target:
                    nodes.append(Allocate._add_if(Allocate([ad_var]), ad_var, is_mod_var))
            else:
                if var.ad_target:
                    nodes.append(Deallocate([ad_var], ad_code=True))
                nodes.append(Deallocate([var], ad_code=True))
        return nodes

    def is_effectively_empty(self) -> bool:
        return not self.vars

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Optional["Deallocate"]:
        if not self.ad_code:
            return None
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        vars = []
        for var in self.vars:
            if not var.name in mod_var_names and (any(var.name == f"{name}{AD_SUFFIX}" for name in mod_var_names) or (decl_map is None or var.name in decl_map)):
                vars.append(var)
        if vars:
            return Deallocate(vars, ad_code=True)
        return None


@dataclass
class Declaration(Node):
    """A variable declaration."""

    name: str
    typename: str
    kind: Optional[str] = None
    char_len: Optional[str] = None
    dims: Optional[Union[Tuple[str],str]] = None
    intent: Optional[str] = None
    parameter: bool = False
    constant: bool = False
    init_val: Optional[str] = None
    access: Optional[str] = None
    allocatable: bool = False
    pointer: bool = False
    optional: bool = False
    target: bool = False
    type_def: Optional[TypeDef] = None
    declared_in: Optional[str] = None

    def __post_init__(self):
        super().__post_init__()
        if self.kind is not None and not isinstance(self.kind, str):
            raise ValueError(f"kind must be str: {type(self.kind)}")
        if self.char_len is not None and not isinstance(self.char_len, str):
            raise ValueError(f"char_len must be str: {type(self.char_len)}")
        if self.dims is not None and not (isinstance(self.dims, tuple) or self.dims == "*"):
            raise ValueError(f"dims must be tuple of str or '*': {type(self.dims)}")
        if self.intent is not None and not isinstance(self.intent, str):
            raise ValueError(f"intent must be str: {type(self.intent)}")

    def copy(self) -> "Declaration":
        return Declaration(
            name=self.name,
            typename=self.typename,
            kind=self.kind,
            char_len=self.char_len,
            dims=self.dims,
            intent=self.intent,
            parameter=self.parameter,
            constant=self.constant,
            init_val=self.init_val,
            access=self.access,
            allocatable=self.allocatable,
            pointer=self.pointer,
            optional=self.optional,
            target=self.target,
            type_def=self.type_def,
            declared_in=self.declared_in,
        )

    def deep_clone(self) -> "Declaration":
        dims = tuple(self.dims) if self.dims else None
        return Declaration(
            name=self.name,
            typename=self.typename,
            kind=self.kind,
            char_len=self.char_len,
            dims=dims,
            intent=self.intent,
            parameter=self.parameter,
            constant=self.constant,
            init_val=self.init_val,
            access=self.access,
            allocatable=self.allocatable,
            pointer=self.pointer,
            optional=self.optional,
            target=self.target,
            type_def=self.type_def,
            declared_in=self.declared_in,
        )

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        if self.intent in ("in", "inout"):
            yield OpVar(
                name=self.name,
                typename=self.typename,
                kind=self.kind,
                is_constant=self.parameter or self.constant,
                allocatable=self.allocatable,
                pointer=self.pointer,
                optional=self.optional,
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
            pointer=self.pointer,
            optional=self.optional,
            dims=self.dims,
            intent=self.intent,
            declared_in=self.declared_in,
        )]

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        line = f"{space}{self.typename}"
        if self.kind is not None:
            line += f"({self.kind})"
        if self.char_len is not None:
            line += f"(len={self.char_len})"
        if self.parameter:
            line += ", parameter"
        if self.access is not None:
            line += f", {self.access}"
        if self.allocatable:
            line += ", allocatable"
        if self.pointer:
            line += ", pointer"
        if self.optional:
            line += ", optional"
        if self.target:
            line += ", target"
        if self.intent is not None:
            pad = "  " if self.intent == "in" else " "
            line += f", intent({self.intent})" + pad + f":: {self.name}"
        else:
            line += f" :: {self.name}"
        if self.dims is not None:
            dims = ",".join(self.dims)
            line += f"({dims})"
        if self.init_val is not None:
            line += f" = {self.init_val}"
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
                        pointer=self.pointer,
                        optional=self.optional,
                        declared_in=self.declared_in,
                    )
                )
        return vars

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Optional["Declaration"]:
        target_names = targets.names()
        if self.intent is not None or self.name in target_names:
            return self.deep_clone()
        return None


@dataclass
class Interface(Node):
    """Class for interface"""

    name: str
    module_procs: Optional[List[str]] = field(default=None) # module procedures

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.name, str):
            raise ValueError(f"name must be str: {type(self.name)}")
        if self.module_procs is not None and not isinstance(self.module_procs, list):
            raise ValueError(f"module_procs must be list: {type(self.module_procs)}")


@dataclass
class TypeDef(Node):
    """Class for type declaration"""

    name: str
    components: List[Declaration]
    procs: List[list]
    access: Optional[str] = None
    map: Dict[str, Declaration] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        self.map = {}
        for decl in self.components:
            if not isinstance(decl, Declaration):
                raise ValueError(f"components must be a list of Declaration: {self.components}")
            self.map[decl.name] = decl

    def __getitem__(self, name: str) -> Optional[Declaration]:
        return self.map.get(name)

    def iter_children(self) -> Iterator[Node]:
        return iter(self.components)

    def copy(self) -> "TypeDef":
        return TypeDef(self.name, self.components, self.procs, self.access)

@dataclass
class BranchBlock(Node):
    """An abstract class for ``if`` and ``select case`` branch stracture."""

    cond_blocks: List[Tuple[Union[Operator,Tuple[Operator]], Block]] = field(default_factory=list)

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
            block.set_parent(self)

    def iter_children(self) -> Iterator[Node]:
        for _, block in self.cond_blocks:
            yield block

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        cond_blocks = []
        for cond, block in self.cond_blocks:
            nodes = block.generate_ad(saved_vars, reverse, assigned_advars, routine_map, generic_map, mod_vars, exitcycle_flags, warnings)
            if reverse:
                nodes_new = block.set_for_exitcycle(exitcycle_flags)
                if nodes_new:
                    if exitcycle_flags:
                        for flag in exitcycle_flags:
                            nodes_new.insert(0, Assignment(flag, OpTrue()))
                    for node in nodes:
                        if not node.is_effectively_empty():
                            nodes_new.append(node)
                    nodes = nodes_new
            cond_blocks.append((cond, Block(nodes)))
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

    def set_for_exitcycle(self,
                          exitcycle_flags: Optional[List[OpVar]] = None,
                          set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
                          label: Optional[str] = None,
                          label_map: Optional[List[Tuple[str,str]]] = None,
                          set_cond: bool = False,
                          keep: bool = False,
                          ) -> List[Node]:
        cond_blocks: List[tuple] = []
        for cond, block in self.cond_blocks:
            nodes = block.set_for_exitcycle(exitcycle_flags, set_do_index, label, label_map, set_cond, keep)
            body = Block(nodes)
            cond_blocks.append((cond, body))
        if isinstance(self, IfBlock):
            return [IfBlock(cond_blocks)]
        else: # SelectBlock
            return [SelectBlock(cond_blocks, expr=self.expr)]

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
        vars_new = VarList()
        cover_all = False # if else or case default exists
        to_remove = set()
        origin_savevars = {name for name in vars.names() if Node.is_savevar(name)}
        for cond, block in self.cond_blocks:
            vs = block.required_vars(vars, no_accumulate, without_savevar)
            vars_new.merge(vs)
            advars = {name for name in vs.names() if Node.is_savevar(name)}
            to_remove = to_remove | (origin_savevars - advars)
            if cond is None:
                cover_all = True
        if not cover_all:
            vars_new.merge(vars)
        for name in to_remove:
            if vars_new.vars and name in vars_new.vars:
                del vars_new.vars[name]
        for var in self.iter_ref_vars():
            vars_new.push(var)
        return vars_new

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars_list = VarList()
        for block in self.iter_children():
            for v in block.unrefered_advars(vars):
                if v.name.endswith(AD_SUFFIX):
                    vars_list.push(v)
        return vars_list

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Node:
        new_condblocks = []
        for cond, block in self.cond_blocks:
            new_block = block.prune_for(targets, mod_vars, decl_map)
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

    def copy(self) -> "IfBlock":
        return IfBlock(self.cond_blocks)

    def deep_clone(self) -> "IfBlock":
        cond_blocks = []
        for cond, block in self.cond_blocks:
            cond = cond.deep_clone() if cond else None
            block = block.deep_clone()
            cond_blocks.append((cond, block))
        return IfBlock(cond_blocks)

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
    select_type: bool = False

    def copy(self) -> "SelectBlock":
        return SelectBlock(self.cond_blocks, self.expr, self.select_type)

    def deep_clone(self) -> "SelectBlock":
        cond_blocks = []
        for conds, block in self.cond_blocks:
            conds = tuple(cond.deep_clone() for cond in conds) if conds else None
            block = block.deep_clone()
            cond_blocks.append((conds, block))
        expr = self.expr.deep_clone()
        return SelectBlock(cond_blocks, expr, self.select_type)

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
        case = "type" if self.select_type else "case"
        lines = [f"{space}select {case} ({self.expr})\n"]
        case = "type is" if self.select_type else "case"
        for cond, block in self.cond_blocks:
            if cond is not None:
                conds = ', '.join([str(co) for co in cond])
                lines.append(f"{space}{case} ({conds})\n")
            else:
                lines.append(f"{space}{case} default\n")
            lines.extend(block.render(indent+1))
        lines.append(f"{space}end select\n")
        return lines


@dataclass
class DoAbst(Node):

    _body: Block
    label_id: int = field(init=False, default=0)


    def __post_init__(self):
        super().__post_init__()
        self._body.set_parent(self)

    def iter_children(self) -> Iterator[Node]:
        yield self._body

    def collect_exitcycle(self) -> List[Node]:
        vars = []
        for var in self._body.iter_children():
            if isinstance(var, (ExitStmt, CycleStmt)):
                if var.label and self.label != var.label:
                    vars.append(var)
            else:
                ec = var.collect_exitcycle()
                if ec:
                    for v in ec:
                        if v.label and self.label != v.label:
                            vars.append(v)
        return vars

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:

        exitcycle_flags = None
        if reverse:
            exitcycles = self._body.collect_exitcycle()
            exitcycle_flags = [node.flag() for node in exitcycles]
        else:
            for vname in self.recurrent_vars():
                assigned_advars.push(OpVar(name=f"{vname}{AD_SUFFIX}"))

        nodes = self._body.generate_ad(saved_vars, reverse, assigned_advars, routine_map, generic_map, mod_vars, exitcycle_flags, warnings)
        if self.do:
            range = self.range
        else:
            cond = self.cond
        label = self.label

        blocks: List[Node] = []
        if reverse:
            if self.do:
                range = range.reverse()
            new_body: List[Node] = []
            fwd_body = self._body.set_for_exitcycle(exitcycle_flags, label=label, set_cond=True)

            if exitcycle_flags:
                exit_flag = False
                for flag in exitcycle_flags:
                    assign = Assignment(flag, OpTrue())
                    if flag.name.startswith("cycle"):
                        new_body.append(assign)
                    else:
                        blocks.append(assign)
                        exit_flag = True
                if exit_flag and self.do:
                    range = OpRange([self.exit_do_start(), range[1], range[2]])
                label = self.label_new()

            required_vars = self._body.required_vars()
            assigned_vars = self._body.assigned_vars()
            common_vars = required_vars & assigned_vars
            loads = []
            if self.do:
                conflict_vars = self.conflict_vars()
                for cvar in common_vars:
                    if cvar.name.endswith(AD_SUFFIX) or cvar == self.index or cvar.name in conflict_vars:
                        continue
                    save = SaveAssignment(cvar, self._body.get_id())
                    self._body.insert_begin(save)
                    load = save.to_load()
                    saved_vars.append(save.tmpvar)
                    new_body.append(load)
                    loads.insert(0, load)

            pushed = []
            recurrent_vars = self.recurrent_vars()
            for node in self._body.iter_children():
                for var in node.assigned_vars():
                    if var.name in recurrent_vars and (not self.do or var.name in conflict_vars):
                        if not var in pushed:
                            save = PushPop(var, node.get_id())
                            self._body.insert_begin(save)
                            new_body.append(save.to_load())
                            pushed.append(var)

            if (isinstance(fwd_body[-1], IfBlock) and isinstance(nodes[0], IfBlock) and
                len(fwd_body[-1].cond_blocks) == 1 and len(nodes[0].cond_blocks) == 1 and
                fwd_body[-1].cond_blocks[0][0] == nodes[0].cond_blocks[0][0] and
                not fwd_body[-1].collect_exitcycle() and not nodes[0].collect_exitcycle()):
                last = fwd_body.pop()
                rev = []
                for node in last.cond_blocks[0][1].iter_children():
                    rev.insert(0, node)
                for node in rev:
                    nodes[0].cond_blocks[0][1].insert_begin(node)

            new_body.extend(fwd_body)
            for node in nodes:
                new_body.append(node)
            new_body.extend(loads)
            nodes = new_body
            if self.do:
                assigned_var_names = assigned_vars.names()
                for var in self.range.collect_vars():
                    if var.name in assigned_var_names:
                        load = self._save_vars(var, saved_vars)
                        blocks.append(load)
            else:
                save_false = PushPopL(".false.")
                self.parent.insert_before(self.get_id(), save_false)
                save_true = PushPopL(".true.")
                self._body.insert_begin(save_true)
                cond = OpFuncUser("fautodiff_data_storage_get", [])

        body = Block(nodes)
        if self.do:
            loop = DoLoop(body, self.index, range, label)
        else:
            loop = DoWhile(body, cond, label)
        blocks.append(loop)

        if reverse:
            for cvar in common_vars:
                if cvar.name.endswith(AD_SUFFIX):
                    continue
                if self.do and (cvar == self.index or not cvar.name in conflict_vars):
                    continue
                load = self._save_vars(cvar, saved_vars)
                blocks.append(load)

        return blocks

    def label_new(self, increment: bool = False) -> str:
        if increment:
            self.label_id += 1
        return f"label_{self.get_id()}_{self.label_id}{AD_SUFFIX}"

@dataclass
class DoLoop(DoAbst):
    """A ``do`` loop."""

    index: OpVar
    range: OpRange
    label: Optional[str] = field(default=None)
    do: ClassVar[bool] = True

    def __post_init__(self):
        super().__post_init__()
        self.build_do_index_list([])
        if not isinstance(self.range, OpRange):
            raise ValueError(f"range must be OpRange: f{type(self.range)}")

    def copy(self) -> "DoLoop":
        return DoLoop(self._body, self.index, self.range, self.label)

    def deep_clone(self) -> "DoLoop":
        return DoLoop(self._body.deep_clone(), self.index.deep_clone(), self.range.deep_clone(), self.label)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self.range.collect_vars():
            yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
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

    def find_index(self, var: OpVar, name: str) -> Optional[int]:
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

    def conflict_vars(self) -> List[str]:
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

    def set_for_exitcycle(self,
                          exitcycle_flags: Optional[List[OpVar]] = None,
                          set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
                          label: Optional[str] = None,
                          label_map: Optional[List[Tuple[str,str]]] = None,
                          set_cond: bool = False,
                          keep: bool = False,
                          ) -> List[Node]:
        exitcycles = self._body.collect_exitcycle()
        if exitcycles and any(isinstance(ec, ExitStmt) for ec in exitcycles):
            nodes: List[Node] = []
            exit_do_start = self.exit_do_start()
            if set_do_index is None:
                nodes.append(Assignment(exit_do_start, self.range[1]))
                set_do_index = (exit_do_start, self.index)
            if label: # this means that this node is in ad_code
                label = self.label_new(increment=True)
            else:
                label = self.label
            if label:
                label_pair = (label, self.label_new())
                if label_map:
                    label_map.append(label_pair)
                else:
                    label_map = [label_pair]
            else:
                label_map = None
            body = Block(self._body.set_for_exitcycle(exitcycle_flags, set_do_index, self.label, label_map, set_cond, keep=True))
            nodes.append(DoLoop(body, self.index, self.range, label))
            return nodes
        return [self]

    def exit_do_start(self) -> OpVar:
        return OpVar(f"exit_do_start_{self.get_id()}_ad", typename="integer")

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        label = f"{self.label}: " if self.label else ""
        header = f"{space}{label}do {self.index} = {self.range[0]}, {self.range[1]}"
        if self.range[2] is not None:
            header = f"{header}, {self.range[2]}"
        lines = [f"{header}\n"]
        lines.extend(self._body.render(indent+1))
        label = f" {self.label}" if self.label else ""
        lines.append(f"{space}end do{label}\n")
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

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Optional[Node]:
        targets = targets.copy()
        #targets.update_index_downward(self._build_index_map(), self.index)
        new_body = self._body.prune_for(targets, mod_vars, decl_map)

        for var in new_body.required_vars(targets):
            if var.name == self.index.name:
                continue
            #if var.name.endswith(AD_SUFFIX):
            #    continue
            # check if the variable has no reccurent in this loop
            if var.index is not None and set(self.do_index_list) <= set(var.index_list()):
                continue
            targets.push(var)
        new_body = self._body.prune_for(targets, mod_vars, decl_map)

        if new_body.is_effectively_empty():
            return None
        return DoLoop(new_body, self.index, self.range, self.label)

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
    label: Optional[str] = field(default=None)
    do: ClassVar[bool] = False

    def __post_init__(self):
        super().__post_init__()
        self.do_index_list = ["__never_match__"]

    def copy(self) -> "DoWhile":
        return DoWhile(self._body, self.cond, self.label)

    def deep_clone(self):
        return DoWhile(self._body.deep_clone(), self.cond.deep_clone(), self.label)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        yield from self.cond.collect_vars()

    def build_do_index_list(self, index_list: List[str]) -> None:
        self._body.build_do_index_list(index_list)

    def recurrent_vars(self) -> List[str]:
        required_vars = self._body.required_vars()
        assigned_vars = self._body.assigned_vars()
        return sorted(set(required_vars.names()) & set(assigned_vars.names()))

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        label = f"{self.label}: " if self.label else ""
        lines = [f"{space}{label}do while ({self.cond})\n"]
        lines.extend(self._body.render(indent+1))
        label = f" {self.label}" if self.label else ""
        lines.append(f"{space}end do{label}\n")
        return lines

    def required_vars(self, vars: Optional[VarList] = None, no_accumulate: bool = False, without_savevar: bool = False) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
        for var in self.cond.collect_vars():
            vars.push(var)
        vars = self._body.required_vars(vars, no_accumulate, without_savevar)
        for var in self.cond.collect_vars():
            vars.push(var)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars = self._body.unrefered_advars(vars)
        return vars

    def prune_for(self, targets: VarList, mod_vars: Optional[List[OpVar]] = None, decl_map: Optional[Dict[str, Declaration]] = None) -> Node:
        new_body = self._body.prune_for(targets, mod_vars, decl_map)
        targets = targets.copy()
        targets.merge(new_body.required_vars(targets))
        for var in self.cond.collect_vars():
            targets.push(var)
        new_body = self._body.prune_for(targets, mod_vars, decl_map)
        if new_body.is_effectively_empty():
            return Block([])
        return DoWhile(new_body, self.cond, self.label)

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
