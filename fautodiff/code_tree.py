"""Utilities for assembling Fortran code as a tree of nodes."""

from __future__ import annotations

import copy
import re
from dataclasses import dataclass, field
from functools import reduce
from typing import (
    Any,
    ClassVar,
    Dict,
    Iterable,
    Iterator,
    List,
    Optional,
    Pattern,
    Set,
    Tuple,
    Union,
)

AD_SUFFIX = "_ad"
FWD_SUFFIX = "_fwd_ad"
REV_SUFFIX = "_rev_ad"

from .operators import (
    AryIndex,
    Kind,
    OpChar,
    Operator,
    OpFalse,
    OpFunc,
    OpFuncUser,
    OpInt,
    OpLeaf,
    OpLogic,
    OpNeg,
    OpNot,
    OpRange,
    OpReal,
    OpTrue,
    OpVar,
    VarType,
)
from .var_list import VarList

_NAME_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")


def _append_unique(items: List[str], name: str) -> None:
    """Append ``name`` to ``items`` if it is not already present."""
    if name not in items:
        items.append(name)


def _extend_unique(items: List[str], names: List[str]) -> None:
    """Extend ``items`` with ``names`` while avoiding duplicates."""
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

    def __post_init__(self) -> None:
        """Assign a unique id and initialise the loop index tracking list."""
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
        if any(var.is_same_var(v) for v in self.iter_ref_vars()):
            return True
        return any(child.has_reference_to(var) for child in self.iter_children())

    def has_assignment_to(self, var: OpVar) -> bool:
        """Return ``True`` if ``var`` is assigned within this node."""
        if any(var.is_same_var(v) for v in self.iter_assign_vars()):
            return True
        return any(child.has_assignment_to(var) for child in self.iter_children())

    def has_access_to(self, var: OpVar) -> bool:
        """Return ``True`` if ``var`` is accessed within this node."""
        if self.has_assignment_to(var):
            return True
        if self.has_reference_to(var):
            return True
        return False

    def has_partial_access_to(self, var: str) -> bool:
        """Return ``True`` if ``var`` is accessed with index within this node."""
        for v in self.iter_ref_vars():
            if (
                v.name == var
                and v.index is not None
                and any(i is not None for i in v.index)
            ):
                return True
        for v in self.iter_assign_vars():
            if (
                v.name == var
                and v.index is not None
                and any(i is not None for i in v.index)
            ):
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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        """Return AD converted nodes."""
        return []

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List["Node"]:
        """Return nodes adjusted for return/exit/cycle handling.

        The base implementation simply returns ``self`` unchanged; subclasses
        override this to insert conditional blocks around exit or cycle
        statements.
        """
        return [self]

    def has_returnexitcycle_flags(self) -> bool:
        for child in self.iter_children():
            if child.has_returnexitcycle_flags():
                return True
        return False

    def _save_vars(self, var: OpVar, saved_vars: List[OpVar]) -> SaveAssignment:
        """Insert a ``SaveAssignment`` before this node and return a loader.

        The current value of ``var`` is stored so it can be restored later, and
        the temporary variable name is recorded in ``saved_vars``.
        """
        id = self.get_id()
        save = SaveAssignment(var, id)
        # place the save instruction before the current node
        self.parent.insert_before(id, save)
        # generate the corresponding load statement used after the mutation
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

    def find_decl(self, var: OpVar) -> Optional[Declaration]:
        if not (isinstance(self, Module) or isinstance(self, Routine)):
            raise NotImplementedError(f"class: {type(self)}")
        name = var.name
        if var.ref_var is not None:
            decl = self.find_decl(var.ref_var)
            if decl is None or decl.type_def is None:
                raise RuntimeError(f"type_def not found: {decl}")
            for decl in decl.type_def.iter_children():
                if decl.name == name:
                    return decl
            raise ValueError(f"name not found: {name} {decl.type_def}")
        decl = self.decls.find_by_name(name)
        if decl is None and getattr(self, "decl_map", None) is not None:
            decl = self.decl_map.get(name)
        return decl

    def find_by_id(self, node_id: int) -> Optional["Node"]:
        """Return the node with ``node_id`` from this subtree or ``None``."""
        if self.__id == node_id:
            return self
        for child in self.iter_children():
            found = child.find_by_id(node_id)
            if found is not None:
                return found
        return None

    def insert_before(self, id: int, node: "Node") -> None:
        """Insert ``node`` before the node with ``id``."""
        raise NotImplementedError(f"class: {type(self)}")

    def insert_begin(self, node: "Node") -> None:
        """Insert ``node`` at the beginning of this node's children."""
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

    def assigned_vars(
        self,
        vars: Optional[VarList] = None,
        without_savevar: bool = False,
        check_init_advars: bool = False,
    ) -> VarList:
        """Return variables assigned within this node and children."""
        flag = False
        for child in self.iter_children():
            vars = child.assigned_vars(
                vars,
                without_savevar=without_savevar,
                check_init_advars=check_init_advars,
            )
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

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        """Return variables needed before executing this node.

        ``vars`` is the ```VarList`` which consists of variables that must be defined *after* this
        node has executed.  The default implementation simply returns ``vars``
        unchanged.  Subclasses override this to remove variables that are
        assigned and to add any variables referenced by this node.
        """
        if vars is None:
            vars = VarList()
        for child in reversed(list(node for node in self.iter_children())):
            vars = child.required_vars(vars, no_accumulate, without_savevar)
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

    def collect_vars(
        self, without_refvar: bool = False, without_index: bool = False
    ) -> List[OpVar]:
        """Return variables used in this node."""
        vars = []
        for child in self.iter_children():
            _extend_unique(
                vars,
                child.collect_vars(
                    without_refvar=without_refvar, without_index=without_index
                ),
            )
        for var in self.iter_ref_vars():
            _append_unique(vars, var)
        for var in self.iter_assign_vars():
            _append_unique(vars, var)
        return vars

    def collect_exitcycle(self) -> List["Node"]:
        """Return ``exit`` and ``cycle`` nodes in this node."""
        nodes = []
        for child in self.iter_children():
            nodes.extend(child.collect_exitcycle())
        return nodes

    def collect_return(self) -> List["ReturnStmt"]:
        """Return ``return`` statements in this node."""
        nodes: List[ReturnStmt] = []
        for child in self.iter_children():
            nodes.extend(child.collect_return())
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

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional["Node"]:
        """Return a copy of this node with only code needed for ``targets``."""
        if any(node for node in self.iter_children()):
            raise NotImplementedError(
                "prune_for must be implemented for class having children"
            )
        for var in self.assigned_vars():
            if var in targets:
                return self.deep_clone()
        return None

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        """Remove self-add from the first assignment if safe.
        This method is for only AD variables.
        """
        if any(node for node in self.iter_children()):
            raise NotImplementedError(
                f"check_initial must be implemented for class having children"
            )
        return assigned_vars

    # ------------------------------------------------------------------
    # other helpers
    # ------------------------------------------------------------------
    def _save_var_name(
        self, name: str, id: int, no_suffix: bool = False, pushpop: bool = False
    ) -> str:
        if pushpop:
            ext = "_pushpop"
        else:
            ext = ""
        name = name.replace("%", "_")
        if no_suffix:
            return f"{name}_save_{id}{ext}"
        else:
            return f"{name}_save_{id}{ext}{AD_SUFFIX}"

    _pattern: ClassVar[Pattern[str]] = re.compile(
        r"([a-zA-Z][a-zA-Z0-9_]*)_save_([0-9]+)"
    )

    @classmethod
    def is_savevar(cls, name: str) -> bool:
        return bool(cls._pattern.match(name))

    @classmethod
    def get_arg_info(
        cls,
        routine: Union[OpFuncUser, CallStatement],
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
    ) -> Optional[dict]:
        """Return argument information for the given name."""
        if routine_map is None:
            return None
        name = routine.name
        arg_info = routine_map.get(name)
        if arg_info is not None:
            nargs = len(routine.args)
            if isinstance(routine, OpFuncUser) or routine.result is not None:
                nargs += 1
            if not arg_info.get("skip") and (
                arg_info["args"] is None or len(arg_info["args"]) != nargs
            ):
                raise RuntimeError(
                    f"Argument length mismatch for {name}: "
                    f"{len(arg_info['args'])} != {len(routine.args)} "
                    f"({arg_info['args']}) ({[v.name for v in routine.args]})"
                )
            return arg_info
        argtypes: List[str] = []
        argkinds: List[Optional[int]] = []
        argdims: List[Optional[int]] = []
        for arg in routine.args:
            if not isinstance(arg, Operator):
                raise ValueError(f"Unsupported argument type: {type(arg)}")
            argtypes.append(arg.var_type.typename)
            kind = arg.var_type.kind
            if isinstance(kind, Kind):
                argkinds.append(kind.val)
            else:
                argkinds.append(kind)
            if isinstance(arg, OpVar) and arg.dims:
                if arg.index:
                    if len(arg.index) != len(arg.dims):
                        raise RuntimeError(
                            f"rank is not consistent: {arg.index} {arg.dims}"
                        )
                    ndims = 0
                    for idx in arg.index:
                        if idx is None or isinstance(idx, OpRange):
                            ndims += 1
                    argdims.append(ndims if ndims > 0 else None)
                else:
                    argdims.append(len(arg.dims))
            else:
                argdims.append(None)

        if arg_info is None and generic_map and name in generic_map:
            for cand in generic_map[name]:
                if cand not in routine_map:
                    raise RuntimeError(f"Not found in routine_map: {cand}")
                cand_info = routine_map[cand]
                cand_types = list(cand_info.get("type", []))
                cand_kinds = (
                    list(cand_info.get("kind", [])) if cand_info.get("kind") else []
                )
                cand_dims = (
                    list(cand_info.get("dims", [])) if cand_info.get("dims") else []
                )
                cand_dims_lens = [len(d) if d else None for d in cand_dims]
                if len(cand_types) == len(argtypes) + 1:
                    cand_types = cand_types[:-1]
                    cand_kinds = cand_kinds[:-1]
                    cand_dims_lens = cand_dims_lens[:-1]
                if cand_types == argtypes and cand_kinds == argkinds:
                    dims_match = True
                    for cd, ad in zip(cand_dims_lens, argdims):
                        if cd is None or cd == ad:
                            continue
                        if (
                            cand_info.get("module") == "mpi"
                            and cd == 1
                            and ad is not None
                            and ad > 1
                        ):
                            continue
                        dims_match = False
                        break
                    if dims_match:
                        return cand_info
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
        rhs = rhs.deep_clone()  # Work on a copy of the expression
        assigns: List[Node] = []
        # Find user defined function calls inside the right-hand side
        funcs = rhs.find_userfunc()
        for n, ufunc in enumerate(funcs):
            if routine_map is None:
                raise RuntimeError("routine_map is necessary for CallStatement")
            arg_info = Node.get_arg_info(ufunc, routine_map, generic_map)
            if arg_info is None:
                raise RuntimeError(f"Not found in routime_map: {ufunc.name}")
            if rhs == ufunc and len(funcs) == 1:
                # When the whole expression is a single function call, reuse lhs
                result = lhs
            else:
                # Save the intermediate result of the function call
                name = self._save_var_name(
                    f"{ufunc.name}{n}", self.get_id(), no_suffix=True
                )
                result = OpVar(name, var_type=VarType(arg_info["type"][-1]))
                saved_vars.append(result)
                saved_vars.append(
                    OpVar(
                        f"{name}{AD_SUFFIX}",
                        var_type=VarType(
                            arg_info["type"][-1],
                            kind=arg_info["kind"][-1],
                        ),
                        dims=arg_info["dims"][-1],
                    )
                )
            intents = ufunc.intents
            if intents is None and ufunc.name in routine_map:
                intents = routine_map[ufunc.name]["intents"]
                if len(intents) > len(ufunc.args):
                    intents = intents[:-1]
            # Recursively generate AD code for the function call
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
                # The call produced the final result
                return assigns
            # Replace the function call with its result variable for further processing
            rhs = rhs.replace_with(ufunc, result)

        if lhs.ad_target:
            grad_lhs = lhs.add_suffix(AD_SUFFIX)
            if (
                grad_lhs.kind is None
                and lhs.var_type.typename.lower() == "double precision"
            ):
                grad_lhs.kind = Kind(OpInt(8), val=8, use_kind_keyword=False)
            ad_info = self.info.get("code") if self.info is not None else None

            # Allow certain intrinsic functions to provide their own AD handler
            if isinstance(rhs, OpFunc):
                handler = rhs.special_handler(lhs, rhs.args, AD_SUFFIX, reverse=False)
                if handler is not None:
                    assigns.append(Assignment(grad_lhs, handler, ad_info=ad_info))
                    return assigns

            vars = rhs.collect_vars(without_index=True, without_refvar=True)
            expr = None
            # Build the forward-mode derivative expression by summing contributions
            for var in vars:
                if not var.ad_target:
                    continue
                v = var.add_suffix(AD_SUFFIX)
                if assigned_advars is not None and not v in assigned_advars:
                    continue
                dev = rhs.derivative(
                    var, target=grad_lhs, info=self.info, warnings=warnings
                )
                term = v * dev
                if not grad_lhs.is_array() and term.is_array():
                    term = OpFunc("sum", args=[term])
                if expr is None:
                    expr = term
                else:
                    expr = expr + term
            if expr is None:
                # No contributing variables -> derivative is cleared
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
        saved_vars: List[OpVar],
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        rhs = rhs.deep_clone()
        extras: List[Node] = []

        # Handle user defined functions appearing in the expression first
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
                # Save intermediate function results so they can be reused in
                # the reverse sweep
                name = self._save_var_name(
                    f"{ufunc.name}{n}", self.get_id(), no_suffix=True
                )
                result = OpVar(name, var_type=VarType(arg_info["type"][-1]))
                saved_vars.append(result)
                saved_vars.append(
                    OpVar(
                        f"{name}{AD_SUFFIX}",
                        var_type=VarType(
                            arg_info["type"][-1],
                            kind=arg_info["kind"][-1],
                        ),
                        dims=arg_info["dims"][-1],
                    )
                )
            callstmt = CallStatement(
                name=ufunc.name,
                args=ufunc.args,
                intents=ufunc.intents,
                result=result,
                info=self.info,
                org_node=self,
            )
            callstmt.parent = self.parent
            # Generate reverse-mode AD for the function call
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
            if (
                grad_lhs.kind is None
                and lhs.var_type.typename.lower() == "double precision"
            ):
                grad_lhs.kind = Kind(OpInt(8), val=8, use_kind_keyword=False)
            ad_info = self.info.get("code") if self.info is not None else None

            # Intrinsic functions may provide their own reverse-mode handler
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

            rhs_vars = rhs.collect_vars(without_index=True, without_refvar=True)

            # Special handling for assignments between slices of the same array or
            # potentially aliased arrays (e.g., pointer associations).  Copy each
            # element of the adjoint of the left-hand slice to a temporary scalar,
            # clear the source element, and accumulate it into the destination
            # element inside explicit loops.
            flag = False
            if lhs.index is not None:
                for var in rhs_vars:
                    if not var.ad_target:
                        continue
                    if (
                        isinstance(var, OpVar)
                        and self._may_alias(lhs, var)
                        and var.index is not None
                        and not lhs == var
                    ):
                        if lhs.name == var.name:
                            for dim in range(len(lhs.index)):
                                idx1 = lhs.index[dim]
                                idx2 = var.index[dim]
                                if isinstance(idx1, OpRange) or isinstance(
                                    idx2, OpRange
                                ):
                                    flag = True
                                    break
                                diff = idx1 - idx2
                                if not (
                                    isinstance(diff, OpInt)
                                    or (
                                        isinstance(diff, OpNeg)
                                        and isinstance(diff.args[0], OpInt)
                                    )
                                ):
                                    flag = True
                                    break
                            break
                        flag = True
                        break
            if flag:
                tmp_var = OpVar(
                    self._save_var_name("tmp", self.get_id()),
                    var_type=VarType(
                        grad_lhs.var_type.typename,
                        kind=grad_lhs.var_type.kind,
                    ),
                )
                saved_vars.append(tmp_var)
                lhs_index: List[Operator] = []
                index_vars: List[Tuple[OpVar, Tuple[Operation]]] = []
                for dim in range(len(lhs.index)):
                    ldim = lhs.index[dim]
                    if not isinstance(ldim, OpRange):
                        lhs_index.append(ldim)
                        continue
                    if ldim[2] is not None and ldim[2] != OpInt(1):
                        raise NotImplementedError(
                            f"stride access is not supported: {lhs}"
                        )
                    ldim0 = ldim[0]
                    ldim1 = ldim[1]
                    if ldim0 is None:
                        ldim0 = OpFunc(
                            "lbound", [grad_lhs.change_index(None), OpInt(dim + 1)]
                        )
                    if ldim1 is None:
                        ldim1 = OpFunc(
                            "ubound", [grad_lhs.change_index(None), OpInt(dim + 1)]
                        )
                    idx_var = OpVar(
                        f"n{dim+1}_{self.get_id()}_ad",
                        var_type=VarType("integer"),
                    )
                    saved_vars.append(idx_var)
                    index_vars.append((idx_var, (ldim0, ldim1)))
                    lhs_index.append(idx_var)
                gl = grad_lhs.change_index(lhs_index)
                body: Block = Block(
                    [
                        Assignment(tmp_var, gl, ad_info=ad_info),
                        ClearAssignment(gl, ad_info=ad_info),
                    ]
                )
                for var in rhs_vars:
                    if not var.ad_target:
                        continue
                    rhs_index: List[Operator] = []
                    idx = 0
                    for dim in range(len(var.index)):
                        rdim = var.index[dim]
                        if not isinstance(rdim, OpRange):
                            rhs_index.append(rdim)
                            continue
                        if rdim[2] is not None and rdim[2] != OpInt(1):
                            raise NotImplementedError(
                                f"stride access is not supported: {var}"
                            )
                        idx_var, ldim = index_vars[idx]
                        idx += 1
                        rdim0 = rdim[0]
                        if rdim0 is None:
                            rdim0 = OpFunc(
                                "lbound",
                                [
                                    var.add_suffix(AD_SUFFIX).change_index(None),
                                    OpInt(dim + 1),
                                ],
                            )
                        index = idx_var
                        if rdim0 == ldim[0]:
                            index = idx_var
                        else:
                            index = rdim0 + idx_var - ldim[0]
                        rhs_index.append(index)
                    if idx != len(index_vars):
                        raise RuntimeError(
                            f"The number of range index is different: {lhs} {var} {len(index_vars)} {idx}"
                        )
                    var.index = AryIndex(rhs_index)  # override variables in rhs
                for var in rhs_vars:
                    grad_rhs = var.add_suffix(AD_SUFFIX)
                    dev = rhs.derivative(
                        var, target=grad_lhs, info=self.info, warnings=warnings
                    )
                    assig = Assignment(
                        grad_rhs,
                        tmp_var * dev,
                        accumulate=(grad_rhs != grad_lhs),
                        ad_info=ad_info,
                    )
                    body.append(assig)

                for idx_var, ldim in index_vars:
                    rng = OpRange([ldim[0], ldim[1]])
                    body = Block([DoLoop(body, idx_var, rng)])

                assigns = list(body.iter_children())
                assigns.extend(extras)
                return assigns

            # not the special case
            grad_lhs = lhs.add_suffix(AD_SUFFIX)
            if (
                grad_lhs.kind is None
                and lhs.var_type.typename.lower() == "double precision"
            ):
                grad_lhs.kind = Kind(OpInt(8), val=8, use_kind_keyword=False)
            ad_info = self.info.get("code") if self.info is not None else None

            if lhs in rhs_vars:
                # Ensure the derivative for lhs is computed last
                rhs_vars.remove(lhs)
                rhs_vars.append(lhs)
            for var in rhs_vars:
                if not var.ad_target:
                    continue
                dev = rhs.derivative(
                    var, target=grad_lhs, info=self.info, warnings=warnings
                )
                grad_rhs = var.add_suffix(AD_SUFFIX)
                res = grad_lhs * dev
                if not grad_rhs.is_array() and res.is_array():
                    res = OpFunc("sum", args=[res])
                assigns.append(
                    Assignment(
                        grad_rhs,
                        res,
                        accumulate=(grad_rhs != grad_lhs),
                        ad_info=ad_info,
                    )
                )
            if lhs not in rhs_vars:
                # If lhs does not appear in rhs, its gradient is cleared
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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> List[Node]:
        """Generate AD code for each child node in the block."""
        ad_code: List[Node] = []
        iterator = self._children
        # Reverse mode processes statements in reverse order
        if reverse:
            iterator = reversed(iterator)
        for node in iterator:
            if reverse and (exitcycle_flags or return_flags):
                ec_flags = [ec.flag() for ec in node.collect_exitcycle()]
                ret_flags = [r.flag() for r in node.collect_return()]
            else:
                ec_flags = None
                ret_flags = None
            nodes = node.generate_ad(
                saved_vars,
                reverse,
                assigned_advars,
                routine_map,
                generic_map,
                mod_vars,
                ec_flags,
                ret_flags,
                warnings,
            )
            nodes = [n for n in nodes if node and not node.is_effectively_empty()]
            if (
                reverse
                and (exitcycle_flags or return_flags)
                and not isinstance(node, (ExitStmt, CycleStmt, ReturnStmt))
            ):
                flags = []
                if exitcycle_flags:
                    flags.extend(exitcycle_flags)
                if return_flags:
                    flags.extend(return_flags)
                if ec_flags:
                    flags = [flag for flag in flags if flag not in ec_flags]
                if ret_flags:
                    flags = [flag for flag in flags if flag not in ret_flags]
                if flags:
                    cond = reduce(lambda x, y: x & y, flags)
                    ad_code.append(IfBlock([(cond, Block(nodes))]))
                else:
                    ad_code.extend(nodes)
            else:
                ad_code.extend(nodes)
        return ad_code

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List[Node]:
        nodes: List[Node] = []
        if set_return_cond and return_flags:
            flags = return_flags
        else:
            flags = []
        if set_exitcycle_cond and exitcycle_flags:
            flags = flags + exitcycle_flags
        if flags:
            cond = reduce(lambda x, y: x & y, flags)
            block = []
            for node in self.iter_children():
                ret_flags = node.collect_return()
                ce_flags = node.collect_exitcycle()
                if ret_flags or ce_flags:
                    node_new = node.set_for_returnexitcycle(
                        return_flags,
                        exitcycle_flags,
                        set_return_cond=False,
                        set_exitcycle_cond=False,
                        set_do_index=set_do_index,
                        label=label,
                        label_map=label_map,
                        keep=keep,
                    )
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
                nodes.extend(
                    node.set_for_returnexitcycle(
                        return_flags,
                        exitcycle_flags,
                        set_return_cond=False,
                        set_exitcycle_cond=False,
                        set_do_index=set_do_index,
                        label=label,
                        label_map=label_map,
                        keep=keep,
                    )
                )
        return nodes

    def find_by_name(self, name: str) -> Optional[Declaration]:
        for child in self.iter_children():
            if isinstance(child, Declaration) and child.name == name:
                return child
        return None

    def remove_child(self, child: Node) -> None:
        self._children.remove(child)

    def first(self) -> Optional[Node]:
        """Return the first element."""
        if len(self._children) > 0:
            return self._children[0]
        return None

    def last(self) -> Optional[Node]:
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

    def insert_before(self, id: int, node: Node) -> None:
        if isinstance(node, Block):
            for ch in reversed(node._children):
                self.insert_before(id, ch)
            return
        for i, child in enumerate(self._children):
            if child.get_id() == id:
                node.build_do_index_list(self.do_index_list)
                self._set_parent(node)
                return self._children.insert(i, node)
        raise ValueError("id is not found")

    def insert_after(self, id: int, node: Node) -> None:
        if isinstance(node, Block):
            for ch in node._children:
                self.insert_after(id, ch)
                id = ch.get_id()
            return
        for i, child in enumerate(self._children):
            if child.get_id() == id:
                node.build_do_index_list(self.do_index_list)
                self._set_parent(node)
                return self._children.insert(i + 1, node)
        raise ValueError("id is not found")

    def insert_begin(self, node: Node) -> None:
        if isinstance(node, Block):
            for ch in reversed(node._children):
                self.insert_begin(ch)
            return
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

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        for child in reversed(self._children):
            vars = child.required_vars(vars, no_accumulate, without_savevar)
        if vars is None:
            vars = VarList()
        return vars

    def recurrent_vars(self) -> List[str]:
        required_vars = self.required_vars()
        assigned_vars = self.assigned_vars()
        common_var_names = sorted(
            set(required_vars.names()) & set(assigned_vars.names())
        )
        do_index_list = set(self.do_index_list)
        var_names: List[str] = []
        for name in common_var_names:
            flag = False
            for index in required_vars[name]:
                if not (index is not None and do_index_list <= set(index.list())):
                    flag = True
                    break
            if not flag:
                for index in assigned_vars[name]:
                    if not (index is not None and do_index_list <= set(index.list())):
                        flag = True
                        break
            if flag:
                var_names.append(name)
        return var_names

    def conflict_vars(self) -> List[str]:
        required_vars = self.required_vars()
        assigned_vars = self.assigned_vars()
        common_var_names = sorted(
            set(required_vars.names()) & set(assigned_vars.names())
        )
        do_index_list = set(self.do_index_list)
        var_names: List[str] = []
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

    def remove_push(self) -> "Block":
        children_new = []
        for child in self.iter_children():
            if not isinstance(child, PushPop):
                children_new.append(child)
        return Block(children_new)

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "Block":
        if base_targets is None:
            base_targets = targets.copy()
        # Drop any statements following the first direct return
        children_all: List[Node] = []
        for child in self._children:
            children_all.append(child)
            if isinstance(child, ReturnStmt):
                break
        new_children: List[Node] = []
        for child in reversed(children_all):
            pruned = child.prune_for(targets, mod_vars, decl_map, base_targets)
            if pruned is not None and not pruned.is_effectively_empty():
                new_children.insert(0, pruned)
                targets = pruned.required_vars(targets)
                if isinstance(pruned, ReturnStmt):
                    targets = base_targets.copy()
        children = new_children
        if len(children) >= 2:
            i = 0
            while i < len(children) - 1:
                child1 = children[i]
                if (
                    isinstance(child1, SaveAssignment)
                    and not child1.load
                    and not child1.pushpop
                ):
                    var = child1.var
                    flag = False
                    for child2 in children[i + 1 :]:
                        if (
                            isinstance(child2, SaveAssignment)
                            and child2.var == var
                            and child2.id == child1.id
                            and child2.load
                        ):
                            children.remove(child2)
                            children.remove(child1)
                            flag = True
                            break
                        if var in child2.assigned_vars():
                            i += 1
                            flag = True
                            break
                    if flag:
                        continue
                if isinstance(child1, PointerAssignment):
                    lhs = child1.lhs
                    lhs_name = lhs.name_ext()
                    flag = False
                    for child2 in children[i + 1 :]:
                        if isinstance(child2, PointerClear):
                            if child2.var == lhs:
                                children.remove(child1)
                                children.remove(child2)
                                flag = True
                                break
                        if isinstance(child2, PointerAssignment):
                            if child2.rhs == lhs and child2.lhs == child1.rhs:
                                children.remove(child1)
                                children.remove(child2)
                                flag = True
                                break
                        if any(v.name_ext() == lhs_name for v in child2.collect_vars()):
                            i += 1
                            flag = True
                            break
                    if flag:
                        continue
                i += 1

            i = 0
            while i < len(children) - 1:
                child1 = children[i]
                if isinstance(child1, PushPop) and child1.pointer and child1.load:
                    var = child1.var
                    name = var.name_ext()
                    flag = False
                    for child2 in children[i + 1 :]:
                        if any(v.name_ext() == name for v in child2.collect_vars()):
                            i += 1
                            flag = True
                            break
                    if flag:
                        continue
                    children.remove(child1)
                    continue
                i += 1

            new_children = []
            for child in children:
                if len(new_children) > 0:
                    last = new_children[-1]
                    if isinstance(child, DoLoop) and isinstance(last, DoLoop):
                        item1 = child._body[0]
                        item2 = last._body[-1]
                        while (
                            isinstance(item1, SaveAssignment)
                            and not item1.pushpop
                            and isinstance(item2, SaveAssignment)
                            and not item2.pushpop
                            and item1.var.name == item2.var.name
                            and item1.id == item2.id
                            and item1.load != item2.load
                        ):
                            child._body.remove_child(item1)
                            last._body.remove_child(item2)
                            if (
                                child._body.is_effectively_empty()
                                or last._body.is_effectively_empty()
                            ):
                                break
                            item1 = child._body[0]
                            item2 = last._body[-1]
                        if last.is_effectively_empty():
                            new_children.remove(last)
                        if not child.is_effectively_empty():
                            new_children.append(child)
                        continue
                    if isinstance(child, IfBlock) and isinstance(last, IfBlock):
                        cb1 = child.cond_blocks[0]
                        cb2 = last.cond_blocks[0]
                        if (
                            cb1[0] == cb2[0]
                            and cb1[1][0]
                            and isinstance(cb1[1][0], SaveAssignment)
                            and not cb1[1][0].pushpop
                            and cb2[1][-1]
                            and isinstance(cb2[1][-1], SaveAssignment)
                            and not cb2[1][-1].pushpop
                            and cb1[1][0].var.name == cb2[1][-1].var.name
                            and cb1[1][0].id == cb2[1][-1].id
                            and cb1[1][0].load != cb2[1][-1].load
                        ):
                            cb1[1].remove_child(cb1[1][0])
                            cb2[1].remove_child(cb2[1][-1])
                            flag = True
                            if last.is_effectively_empty():
                                new_children.remove(last)
                            if not child.is_effectively_empty():
                                new_children.append(child)
                            continue
                        if (
                            cb1[0] == cb2[0]
                            and len(child.cond_blocks) == 1
                            and len(last.cond_blocks)
                            and not last.has_returnexitcycle_flags()
                        ):  # merge
                            last.cond_blocks[0][1].extend(child.cond_blocks[0][1])
                            continue
                    if (
                        isinstance(last, Assignment)
                        and isinstance(last.rhs, OpTrue)
                        and isinstance(child, IfBlock)
                        and len(child.cond_blocks) == 1
                        and child.cond_blocks[0][0] == last.lhs
                    ):
                        block = child.cond_blocks[0][1]
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
class StopStmt(Statement):
    """Representation of a STOP or ERROR STOP statement."""

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        return [StopStmt(self.body)]

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "StopStmt":
        return StopStmt(self.body)

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List["Node"]:
        return []


@dataclass
class PreprocessorLine(Node):
    """A single preprocessor directive line."""

    text: str

    def copy(self) -> "PreprocessorLine":
        return PreprocessorLine(self.text)

    def deep_clone(self) -> "PreprocessorLine":
        return PreprocessorLine(self.text)

    def render(self, indent: int = 0) -> List[str]:
        text = self.text.strip()
        return [f"{text}\n"]

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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        return [PreprocessorLine(self.text)]

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List[Node]:
        return []

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "PreprocessorLine":
        return self


@dataclass
class PreprocessorIfBlock(Node):
    """Representation of ``#if`` style preprocessor conditionals."""

    cond_blocks: List[Tuple[str, Block]] = field(default_factory=list)
    macro_tables: List[Dict[str, str]] = field(default_factory=list)

    def __post_init__(self) -> None:
        super().__post_init__()
        if self.macro_tables and len(self.macro_tables) != len(self.cond_blocks):
            raise ValueError("macro table count mismatch")
        if not self.macro_tables:
            self.macro_tables = [{} for _ in self.cond_blocks]
        for cond, block in self.cond_blocks:
            if not isinstance(cond, str):
                raise ValueError("condition must be a string")
            if not isinstance(block, Block):
                raise ValueError("body must be a Block")
            block.set_parent(self)

    def copy(self) -> "PreprocessorIfBlock":
        return PreprocessorIfBlock(
            self.cond_blocks, [m.copy() for m in self.macro_tables]
        )

    def deep_clone(self) -> "PreprocessorIfBlock":
        cond_blocks: List[Tuple[str, Block]] = []
        macro_tables: List[Dict[str, str]] = []
        for (cond, block), table in zip(self.cond_blocks, self.macro_tables):
            cond_blocks.append((cond, block.deep_clone()))
            macro_tables.append(table.copy())
        return PreprocessorIfBlock(cond_blocks, macro_tables)

    def iter_children(self) -> Iterator[Node]:
        for _, block in self.cond_blocks:
            yield block

    def render(self, indent: int = 0) -> List[str]:
        lines: List[str] = []
        for cond, block in self.cond_blocks:
            lines.append(f"#{cond}\n")
            lines.extend(block.render(indent))
        lines.append("#endif\n")
        return lines

    def is_effectively_empty(self) -> bool:
        return all(block.is_effectively_empty() for _, block in self.cond_blocks)

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List["Node"]:
        cond_blocks: List[Tuple[str, Block]] = []
        macro_tables: List[Dict[str, str]] = []
        has_content = False
        for (cond, block), table in zip(self.cond_blocks, self.macro_tables):
            nodes = block.generate_ad(
                saved_vars,
                reverse,
                assigned_advars,
                routine_map,
                generic_map,
                mod_vars,
                exitcycle_flags,
                return_flags,
                type_map,
                warnings,
            )
            nodes = [n for n in nodes if n and not n.is_effectively_empty()]
            if nodes:
                has_content = True
            cond_blocks.append((cond, Block(nodes)))
            macro_tables.append(table)
        if not has_content:
            return []
        return [PreprocessorIfBlock(cond_blocks, macro_tables)]

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List[Node]:
        cond_blocks: List[Tuple[str, Block]] = []
        macro_tables: List[Dict[str, str]] = []
        has_content = False
        for (cond, block), table in zip(self.cond_blocks, self.macro_tables):
            nodes = block.set_for_returnexitcycle(
                return_flags,
                exitcycle_flags,
                set_return_cond,
                set_exitcycle_cond,
                set_do_index,
                label,
                label_map,
                keep,
            )
            nodes = [n for n in nodes if n and not n.is_effectively_empty()]
            if nodes:
                has_content = True
            cond_blocks.append((cond, Block(nodes)))
            macro_tables.append(table)
        if not has_content:
            return []
        return [PreprocessorIfBlock(cond_blocks, macro_tables)]

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        vars_list = VarList()
        for _, block in self.cond_blocks:
            vars_list.merge(block.required_vars(vars, no_accumulate, without_savevar))
        return vars_list

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars_list = VarList()
        for _, block in self.cond_blocks:
            vars_list.merge(block.unrefered_advars(vars))
        return vars_list

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, Declaration]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional["PreprocessorIfBlock"]:
        cond_blocks: List[Tuple[str, Block]] = []
        macro_tables: List[Dict[str, str]] = []
        flag = False
        for (cond, block), table in zip(self.cond_blocks, self.macro_tables):
            block = block.prune_for(targets, mod_vars, decl_map, base_targets)
            cond_blocks.append((cond, block))
            macro_tables.append(table)
            if not block.is_effectively_empty():
                flag = True
        if not flag:
            return None
        return PreprocessorIfBlock(cond_blocks, macro_tables)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        vars_list = VarList()
        for _, block in self.cond_blocks:
            vars_list.merge(block.check_initial(assigned_vars))
        return vars_list


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

    def collect_return(self) -> List[ReturnStmt]:
        return []

    def is_effectively_empty(self) -> bool:
        return False

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "ExitCycle":
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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        if reverse:
            label = f" {self.label}" if self.label else ""
            return [Assignment(self.flag(), OpTrue(), ad_info=f"{self.name}{label}")]
        return [self]

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List[Node]:
        nodes: List[Node] = []
        if set_do_index and isinstance(self, ExitStmt):
            nodes.append(Assignment(set_do_index[0], set_do_index[1]))
        if exitcycle_flags:
            nodes.append(Assignment(self.flag(), OpFalse()))
        if exitcycle_flags is None or keep:
            if label and label_map:
                label = next(
                    (
                        label_new
                        for label_org, label_new in label_map
                        if label_org == self.label
                    ),
                    label_map[0][1],
                )
                nodes.append(self.__class__(label))
            else:
                nodes.append(self)
        return nodes

    def flag(self) -> OpVar:
        return OpVar(
            f"{self.name}_flag_{self.get_id()}_ad", var_type=VarType("logical")
        )


@dataclass
class ExitStmt(ExitCycle):
    """Representation of an ``exit`` statement."""

    name: ClassVar[str] = "exit"


@dataclass
class CycleStmt(ExitCycle):
    """Representation of a ``cycle`` statement."""

    name: ClassVar[str] = "cycle"


@dataclass
class ReturnStmt(Node):
    """Representation of a ``return`` statement."""

    name: ClassVar[str] = "return"

    def copy(self) -> "ReturnStmt":
        return ReturnStmt()

    def deep_clone(self) -> "ReturnStmt":
        return ReturnStmt()

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        return [f"{space}{self.name}\n"]

    def collect_exitcycle(self) -> List[Node]:
        return []

    def collect_return(self) -> List["ReturnStmt"]:
        flag = False
        parent = self.parent
        while parent:
            if isinstance(parent, BranchBlock):
                flag = True
                break
            parent = parent.parent
        if flag:
            return [self]
        else:
            return []

    def is_effectively_empty(self) -> bool:
        return False

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, Declaration]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "ReturnStmt":
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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        if reverse:
            flag = False
            parent = self.parent
            while parent:
                if isinstance(parent, BranchBlock):
                    flag = True
                    break
                parent = parent.parent
            if flag:
                return [Assignment(self.flag(), OpTrue(), ad_info=self.name)]
            else:
                return []
        return [self]

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List[Node]:
        nodes: List[Node] = []
        if return_flags:
            nodes.append(Assignment(self.flag(), OpFalse()))
        return nodes

    def iter_ref_vars(self) -> Iterator[OpVar]:
        return iter(())

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        return iter(())

    def flag(self) -> OpVar:
        return OpVar(
            f"{self.name}_flag_{self.get_id()}_ad", var_type=VarType("logical")
        )


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

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "Use":
        return self

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


NONDIFF_STD_ROUTINES = {"get_command_argument"}


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
    org_node: Optional[Node] = None

    def __post_init__(self) -> None:
        super().__post_init__()
        # if not _NAME_RE.fullmatch(self.name):
        #    raise ValueError(f"invalid Fortran routine name: {self.name}")
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
        return CallStatement(
            self.name,
            self.args,
            self.arg_keys,
            self.intents,
            self.result,
            self.info,
            self.ad_info,
        )

    def deep_clone(self) -> "CallStatement":
        args = [arg.deep_clone() for arg in self.args]
        arg_keys = list(self.arg_keys) if self.arg_keys else None
        intents = list(self.intents) if self.intents else None
        result = self.result.deep_clone() if self.result else None
        return CallStatement(
            name=self.name,
            args=args,
            arg_keys=arg_keys,
            intents=intents,
            result=result,
            info=self.info,
            ad_info=self.ad_info,
            associated_vars=self.associated_vars,
            donot_prune=self.donot_prune,
        )

    def _iter_vars(
        self, intents: List[str], kinds: Tuple[str], without_index: bool = False
    ) -> Iterator[OpVar]:
        for arg, intent in zip(self.args, intents):
            if intent in kinds:
                for var in arg.collect_vars(without_index=without_index):
                    yield var

    def iter_ref_vars(self) -> Iterator[OpVar]:
        intents = self.intents or ["inout"] * len(self.args)
        yield from self._iter_vars(intents, ("in", "inout"))

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        intents = self.intents or ["inout"] * len(self.args)
        yield from self._iter_vars(intents, ("out", "inout"), without_index=True)
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

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        intents = self.intents or ["inout"] * len(self.args)
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            for arg, intent in zip(self.args, intents):
                if intent in ("out", "inout"):
                    for var in arg.collect_vars(without_index=True):
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
    def rename_args(
        cls,
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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        if self.name in NONDIFF_STD_ROUTINES:
            return [self]
        if routine_map is None:
            raise RuntimeError("routine_map is necessary for CallStatement")
        name = self.name
        arg_info = Node.get_arg_info(self, routine_map, generic_map)
        if arg_info is None:
            # print(routine_map)
            raise RuntimeError(f"Not found in routime_map: {name}")

        name_key = "name_rev_ad" if reverse else "name_fwd_ad"
        if arg_info.get("skip") or arg_info.get(name_key) is None:
            if reverse:
                return [Statement(f"! {name} is skiped")]
            else:
                return [self]

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
                name = self._save_var_name(
                    f"{self.name}_arg{i}", self.get_id(), no_suffix=True
                )
                tmp = OpVar(name, var_type=VarType("real"))
                tmp_vars.append((tmp, arg))
                args_new.append(tmp)
                saved_vars.append(tmp)
                saved_vars.append(
                    OpVar(
                        f"{tmp.name}{AD_SUFFIX}",
                        var_type=VarType(
                            "real",
                            kind=arg_info["kind"][i],
                        ),
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
        ad_args = CallStatement.rename_args(
            ordered, arg_info["args"], arg_info[args_key], args_new, reverse
        )
        if self.associated_vars is None:
            associated_vars = None
        else:
            associated_vars = []
            for var in self.associated_vars:
                if not reverse:
                    associated_vars.append(var)
                associated_vars.append(var.add_suffix(AD_SUFFIX))

        ad_call = CallStatement(
            name=arg_info[name_key],
            args=ad_args,
            intents=arg_info[intents_key],
            ad_info=self.info["code"],
            associated_vars=associated_vars,
        )
        if not reverse:
            for i, arg in enumerate(ad_args):
                if arg_info["intents_fwd_ad"][i] in ("out", "inout"):
                    assigned_advars.push(arg)
        ad_nodes = []
        init_nodes = []
        if tmp_vars:
            if reverse:
                for lhs, rhs in tmp_vars:
                    init_nodes.append(
                        Assignment(
                            lhs.add_suffix(AD_SUFFIX),
                            OpReal("0.0", kind=lhs.kind),
                        )
                    )
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
            ad_nodes = init_nodes + [ad_call] + ad_nodes
            loads = []
            blocks = []
            for var in self.assigned_vars():
                if not var.name.endswith(AD_SUFFIX):
                    if self.org_node is not None:
                        node = self.org_node
                    else:
                        node = self
                    load = node._save_vars(var, saved_vars)
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

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional["CallStatement"]:
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
                for var in arg.collect_vars(without_index=True):
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
    uses: Optional[Block] = None
    body: Optional[Block] = None
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
        if self.uses is not None:
            lines.extend(self.uses.render(indent + 1))
        lines.extend(f"{space}  implicit none\n")
        if self.decls is not None:
            lines.append("\n")
            lines.extend(self.decls.render(indent + 1))
        if self.body is not None:
            lines.append("\n")
            lines.extend(self.body.render(indent + 1))
        lines.append("\n")
        lines.append(f"{space}contains\n")
        lines.append("\n")
        for routine in self.routines:
            lines.extend(routine.render(indent + 1))
            lines.append("\n")
        lines.append(f"{space}end module {self.name}\n")
        return lines

    def find_use_modules(self) -> List[str]:
        mods: List[str] = []
        if self.uses is not None:
            for child in self.uses.iter_children():
                if isinstance(child, Use):
                    mods.append(child.name)
        for routine in self.routines:
            for child in routine.decls.iter_children():
                if isinstance(child, Use):
                    mods.append(child.name)
        return mods


@dataclass
class Program(Module):
    """Representation of a Fortran main program."""

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}program {self.name}\n"]
        if self.uses is not None:
            lines.extend(self.uses.render(indent + 1))
        lines.extend(f"{space}  implicit none\n")
        if self.decls is not None:
            lines.append("\n")
            lines.extend(self.decls.render(indent + 1))
        if self.body is not None:
            lines.append("\n")
            lines.extend(self.body.render(indent + 1))
        lines.append("\n")
        lines.append(f"{space}contains\n")
        lines.append("\n")
        for routine in self.routines:
            lines.extend(routine.render(indent + 1))
            lines.append("\n")
        lines.append(f"{space}end program {self.name}\n")
        return lines


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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Routine]:
        raise RuntimeError("generate_ad for Routine is not allowed")

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        args = ", ".join(self.args)
        lines = [f"{space}{self.kind} {self.name}({args})\n"]
        lines.extend(self.decls.render(indent + 1))
        lines.append("\n")
        last_block: Optional[Block] = None
        if not self.content.is_effectively_empty():
            lines.extend(self.content.render(indent + 1))
            lines.append("\n")
            last_block = self.content
        if self.ad_init is not None and not self.ad_init.is_effectively_empty():
            lines.extend(self.ad_init.render(indent + 1))
            lines.append("\n")
            last_block = self.ad_init
        if self.ad_content is not None and not self.ad_content.is_effectively_empty():
            lines.extend(self.ad_content.render(indent + 1))
            lines.append("\n")
            last_block = self.ad_content
        if (
            last_block
            and last_block._children
            and isinstance(last_block._children[-1], ReturnStmt)
        ):
            lines.pop()
            ret_line = lines.pop()
            lines.append("\n")
            lines.append(ret_line)
        else:
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
            var_type=decl.var_type.copy(),
            dims=decl.dims,
            intent=intent,
            ad_target=None,
            is_constant=decl.parameter or getattr(decl, "constant", False),
            allocatable=decl.allocatable,
            pointer=decl.pointer,
            optional=decl.optional,
            target=decl.target,
            save=decl.save,
            value=decl.value,
            volatile=decl.volatile,
            asynchronous=decl.asynchronous,
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

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        for block in reversed(self._all_blocks()):
            vars = block.required_vars(vars, no_accumulate, without_savevar)
        if vars is None:
            vars = VarList()
        return vars

    def expand_decls(self, decls: Block) -> "Routine":
        self.decls.expand(decls)
        return self

    def deep_clone(self) -> "Routine":
        decls = self.decls.deep_clone()
        content = self.content.deep_clone()
        ad_init = self.ad_init.deep_clone() if self.ad_init is not None else None
        ad_content = (
            self.ad_content.deep_clone() if self.ad_content is not None else None
        )
        clone = type(self)(
            name=self.name,
            args=list(self.args),
            result=self.result,
            decls=decls,
            content=content,
            ad_init=ad_init,
            ad_content=ad_content,
        )
        clone.directives = dict(self.directives)
        if self.decl_map is not None:
            clone.decl_map = dict(self.decl_map)
        return clone

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "Routine":
        if base_targets is None:
            base_targets = targets.copy()
        ad_content = self.ad_content.prune_for(
            targets, mod_vars, decl_map, base_targets
        )
        targets = ad_content.required_vars(targets)
        ad_init = self.ad_init.prune_for(targets, mod_vars, decl_map, base_targets)
        targets = ad_init.required_vars(targets)
        content = self.content.prune_for(targets, mod_vars, decl_map, base_targets)
        all_vars = content.collect_vars()
        _extend_unique(all_vars, ad_init.collect_vars())
        _extend_unique(all_vars, ad_content.collect_vars())
        decls = self.decls.prune_for(
            VarList(all_vars), mod_vars, decl_map, base_targets
        )
        return type(self)(
            name=self.name,
            args=self.args,
            result=self.result,
            decls=decls,
            content=content,
            ad_init=ad_init,
            ad_content=ad_content,
        )


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
class Declaration(Node):
    """A variable declaration."""

    name: str
    var_type: VarType
    dims: Optional[Union[Tuple[str], str]] = None
    intent: Optional[str] = None
    parameter: bool = False
    constant: bool = False
    init_val: Optional[str] = None
    access: Optional[str] = None
    allocatable: bool = False
    pointer: bool = False
    optional: bool = False
    target: bool = False
    save: bool = False
    value: bool = False
    volatile: bool = False
    asynchronous: bool = False
    type_def: Optional[TypeDef] = None
    declared_in: Optional[str] = None

    def __init__(
        self,
        name: str,
        var_type: VarType,
        dims: Optional[Union[Tuple[str], str]] = None,
        intent: Optional[str] = None,
        parameter: bool = False,
        constant: bool = False,
        init_val: Optional[str] = None,
        access: Optional[str] = None,
        allocatable: bool = False,
        pointer: bool = False,
        optional: bool = False,
        target: bool = False,
        save: bool = False,
        value: bool = False,
        volatile: bool = False,
        asynchronous: bool = False,
        type_def: Optional[TypeDef] = None,
        declared_in: Optional[str] = None,
    ):
        self.name = name
        self.var_type = var_type
        self.dims = dims
        self.intent = intent
        self.parameter = parameter
        self.constant = constant
        self.init_val = init_val
        self.access = access
        self.allocatable = allocatable
        self.pointer = pointer
        self.optional = optional
        self.target = target
        self.save = save
        self.value = value
        self.volatile = volatile
        self.asynchronous = asynchronous
        self.type_def = type_def
        self.declared_in = declared_in
        self.__post_init__()

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.var_type, VarType):
            raise ValueError(f"var_type must be VarType: {type(self.var_type)}")
        if self.dims is not None and (
            not (isinstance(self.dims, tuple) or self.dims == "*")
            or any(not isinstance(dim, str) for dim in self.dims)
        ):
            raise ValueError(
                f"dims must be tuple of str or '*': {type(self.dims)} {self.dims}"
            )
        if self.intent is not None and not isinstance(self.intent, str):
            raise ValueError(f"intent must be str: {type(self.intent)}")

    def copy(self) -> "Declaration":
        return Declaration(
            name=self.name,
            var_type=self.var_type.copy(),
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
            save=self.save,
            value=self.value,
            volatile=self.volatile,
            asynchronous=self.asynchronous,
            type_def=self.type_def,
            declared_in=self.declared_in,
        )

    def deep_clone(self) -> "Declaration":
        dims = tuple(self.dims) if self.dims else None
        clone = Declaration(
            name=self.name,
            var_type=self.var_type.copy(),
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
            save=self.save,
            value=self.value,
            volatile=self.volatile,
            asynchronous=self.asynchronous,
            type_def=self.type_def,
            declared_in=self.declared_in,
        )
        clone.donot_prune = getattr(self, "donot_prune", False)
        return clone

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        if self.intent in ("in", "inout") or self.save:
            yield OpVar(
                name=self.name,
                var_type=self.var_type.copy(),
                is_constant=self.parameter or self.constant,
                allocatable=self.allocatable,
                pointer=self.pointer,
                optional=self.optional,
                target=self.target,
                save=self.save,
                value=self.value,
                volatile=self.volatile,
                asynchronous=self.asynchronous,
                declared_in=self.declared_in,
            )
        else:
            return iter(())

    def is_effectively_empty(self) -> bool:
        return False

    def collect_vars(
        self,
        without_refvar: bool = False,
        without_index: bool = False,
    ) -> List[OpVar]:
        var = OpVar(
            name=self.name,
            var_type=self.var_type.copy(),
            is_constant=self.parameter or self.constant,
            allocatable=self.allocatable,
            pointer=self.pointer,
            optional=self.optional,
            target=self.target,
            save=self.save,
            value=self.value,
            volatile=self.volatile,
            asynchronous=self.asynchronous,
            dims=self.dims,
            ad_target=self.ad_target(),
            intent=self.intent,
            declared_in=self.declared_in,
        )
        vars = [var]
        if self.type_def is not None:
            for decl in self.type_def.iter_children():
                for v in decl.collect_vars():
                    v.ref_var = var
                    vars.append(v)
        return vars

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        line = f"{space}{self.var_type}"
        pat = ""
        if self.parameter:
            line += ", parameter"
        if self.access is not None:
            line += f", {self.access}"
        if self.intent is not None:
            if self.intent == "in":
                pat = " "
            line += f", intent({self.intent})"
        if self.allocatable:
            line += ", allocatable"
        if self.pointer:
            line += ", pointer"
        if self.optional:
            line += ", optional"
        if self.target:
            line += ", target"
        if self.save:
            line += ", save"
        if self.value:
            line += ", value"
        if self.volatile:
            line += ", volatile"
        if self.asynchronous:
            line += ", asynchronous"
        line += f"{pat} :: {self.name}"
        if self.dims is not None:
            dims = ",".join(self.dims)
            line += f"({dims})"
        if self.init_val is not None:
            line += f" = {self.init_val}"
        line += "\n"
        return [line]

    def ad_target(self) -> bool:
        if self.constant or self.parameter:
            return False
        typename = self.var_type.typename.lower()
        if (
            typename.startswith("real")
            or typename.startswith("double")
            or typename.startswith("complex")
        ):
            return True
        if self.type_def is not None:
            for decl in self.type_def.iter_children():
                if decl.ad_target():
                    return True
        return False

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            return VarList()
        if self.intent in ("in", "inout") or self.save:
            vars = vars.copy()
            vars.remove(OpVar(self.name))
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        if vars is None:
            vars = VarList()
        if self.intent in ("in", "inout") or self.save:
            if self.name.endswith(AD_SUFFIX):
                vars = vars.copy()
                vars.push(
                    OpVar(
                        self.name,
                        var_type=self.var_type.copy(),
                        is_constant=self.parameter or self.constant,
                        allocatable=self.allocatable,
                        pointer=self.pointer,
                        optional=self.optional,
                        target=self.target,
                        save=self.save,
                        value=self.value,
                        volatile=self.volatile,
                        asynchronous=self.asynchronous,
                        declared_in=self.declared_in,
                    )
                )
        return vars

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[Dict] = None,
        generic_map: Optional[Dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> List[Node]:
        if self.constant or self.parameter or not self.ad_target():
            return []
        name = f"{self.name}{AD_SUFFIX}"
        if self.type_def is None:
            typename = self.var_type.typename
            type_def = None
        else:
            type_def = type_map[self.type_def.name]
            typename = f"type({type_def.name})"
        init_val = (
            str(OpReal("0.0", kind=self.var_type.kind))
            if not (self.allocatable or self.pointer)
            else None
        )
        decl = Declaration(
            name=name,
            var_type=VarType(
                typename,
                kind=self.var_type.kind,
            ),
            dims=self.dims,
            init_val=init_val,
            allocatable=self.allocatable,
            pointer=self.pointer,
            target=self.target,
            save=self.save,
            value=self.value,
            volatile=self.volatile,
            asynchronous=self.asynchronous,
            type_def=self.type_def,
            declared_in=self.declared_in,
        )
        return [decl]

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional["Declaration"]:
        if getattr(self, "donot_prune", False):
            return self.deep_clone()
        target_names = targets.names()
        if self.intent is not None or self.name in target_names:
            return self.deep_clone()
        return None


@dataclass
class Interface(Node):
    """Class for interface"""

    name: str
    module_procs: Optional[List[str]] = field(default=None)  # module procedures

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
    bind: Optional[str] = None
    abstract: bool = False
    sequence: bool = False
    map: Dict[str, Declaration] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        self.map = {}
        for decl in self.components:
            if not isinstance(decl, Declaration):
                raise ValueError(
                    f"components must be a list of Declaration: {self.components}"
                )
            self.map[decl.name] = decl

    def __getitem__(self, name: str) -> Optional[Declaration]:
        return self.map.get(name)

    def iter_children(self) -> Iterator[Declaration]:
        return iter(self.components)

    def copy(self) -> "TypeDef":
        return TypeDef(
            self.name,
            self.components,
            self.procs,
            self.access,
            bind=self.bind,
            abstract=self.abstract,
            sequence=self.sequence,
        )

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines: List[str] = []
        line = f"{space}type"
        if self.abstract:
            line += ", abstract"
        if self.bind is not None:
            line += f", bind({self.bind})"
        line += f" :: {self.name}\n"
        lines.append(line)
        if self.sequence:
            lines.append(f"{space}  sequence\n")
        for decl in self.components:
            lines.extend(decl.render(indent + 1))
        lines.append(f"{space}end type {self.name}\n")
        return lines

    def collect_vars(
        self, without_refvar: bool = False, without_index: bool = False
    ) -> List[OpVar]:
        return []

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[Dict] = None,
        generic_map: Optional[Dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> List[Node]:
        if self.name.endswith("_t"):
            name = f"{self.name.removesuffix('_t')}{AD_SUFFIX}_t"
        else:
            name = f"{self.name}{AD_SUFFIX}"
        components = []
        for decl in self.components:
            decl = decl.copy()
            decl.name = f"{decl.name}{AD_SUFFIX}"
            components.append(decl)
        procs = []
        for proc in self.procs:
            procs.append(list(proc))
        access = self.access
        return [
            TypeDef(
                name=name,
                components=components,
                procs=procs,
                access=access,
                bind=self.bind,
                abstract=self.abstract,
                sequence=self.sequence,
            )
        ]


@dataclass
class Assignment(Node):
    """An assignment statement ``lhs = rhs``."""

    pointer_alias_pairs: ClassVar[Set[Tuple[str, str]]] = set()

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
        self._rhs_vars = list(self.rhs.collect_vars(without_refvar=True))
        self._ufuncs = self.rhs.find_userfunc()

    def copy(self) -> "Assignment":
        return Assignment(self.lhs, self.rhs, self.accumulate, self.info, self.ad_info)

    def deep_clone(self):
        lhs = self.lhs.deep_clone()
        rhs = self.rhs.deep_clone()
        return Assignment(lhs, rhs, self.accumulate, self.info, self.ad_info)

    @staticmethod
    def _may_alias(lhs: OpVar, rhs: OpVar) -> bool:
        name_l = lhs.name_ext()
        name_r = rhs.name_ext()
        if name_l == name_r:
            return True
        if lhs.pointer and rhs.pointer:
            return True
        pairs = Assignment.pointer_alias_pairs
        if (name_l, name_r) in pairs or (name_r, name_l) in pairs:
            return True
        return False

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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
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

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional["Assignment"]:
        lhs = self.lhs
        if lhs in targets:
            return self.deep_clone()
        if (
            not lhs.ad_target
            and not lhs.name.endswith(AD_SUFFIX)
            and lhs.add_suffix(AD_SUFFIX) in targets
        ):  # e.g., requests_ad for mpi
            return Assignment(lhs.add_suffix(AD_SUFFIX), self.rhs)
        return None

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        lhs = self.lhs
        if vars is None:
            vars = VarList()
        else:
            if not isinstance(vars, VarList):
                raise ValueError(f"Must be VarList: {type(vars)}")
            vars = vars.copy()
            vars.remove(lhs)
        for var in lhs.collect_vars(without_refvar=True):  # variables in indexes
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

    def has_returnexitcycle_flags(self) -> bool:
        if self.lhs.name.startswith(("return_flag", "exit_flag", "cycle_flag")):
            return True
        return False


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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
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

    def assigned_vars(
        self,
        vars: Optional[VarList] = None,
        without_savevar: bool = False,
        check_init_advars: bool = False,
    ) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            if check_init_advars:
                vars.remove(self.lhs)
        if not check_init_advars:
            vars.push(self.lhs)
        return vars

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        lhs = self.lhs
        if vars is None:
            vars = VarList()
        else:
            if not isinstance(vars, VarList):
                raise ValueError(f"Must be VarList: {type(vars)}")
            vars = vars.copy()
            vars.remove(lhs)
        for var in lhs.collect_vars():  # variables in indexes
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
    tmpvar: Optional[OpVar] = field(repr=False, default=None)
    load: bool = False
    lhs: OpVar = field(init=False, repr=False)
    rhs: OpVar = field(init=False, repr=False)
    pushpop: ClassVar[bool] = False

    def __post_init__(self):
        super().__post_init__()
        name = self.var.name_ext()
        if re.search(rf"save_\d+{AD_SUFFIX}", name):
            raise RuntimeError(f"Variable has aleady saved: {name}")
        if self.tmpvar is None:
            self.var = self.var.deep_clone()
            dims = []
            v = self.var
            while v:
                d = v.dims
                if d:
                    dims.extend(d)
                v = v.ref_var
            if dims:
                dims = tuple(dims)
            else:
                dims = None
            self.tmpvar = OpVar(
                self._save_var_name(name, self.id, pushpop=self.pushpop),
                index=self.var.concat_index(),
                dims=dims,
                var_type=self.var.var_type.copy() if self.var.var_type else None,
                ad_target=self.var.ad_target,
                is_constant=self.var.is_constant,
                reference=self.var,
                allocatable=self.var.allocatable,
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
        # if without_savevar and self.lhs == self.tmpvar:
        if without_savevar:
            return iter(())
        else:
            yield self.lhs

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines: List[str] = []
        if self.lhs.allocatable and not self.pushpop and not self.load:
            lhs0 = self.lhs.change_index(None)
            rhs0 = self.rhs.change_index(None)
            if self.var.declared_in == "routine":
                lines.append(f"{space}allocate({lhs0}, mold={rhs0})\n")
            else:
                lines.append(f"{space}if (.not. allocated({lhs0})) then\n")
                lines.append(f"{space}  allocate({lhs0}, mold={rhs0})\n")
                lines.append(f"{space}end if\n")
        lines.append(f"{space}{self.lhs} = {self.rhs}\n")
        if self.load and self.rhs.allocatable and not self.pushpop:
            rhs0 = self.rhs.change_index(None)
            lines.append(f"{space}if (allocated({rhs0})) then\n")
            lines.append(f"{space}  deallocate({rhs0})\n")
            lines.append(f"{space}end if\n")
        return lines

    def is_effectively_empty(self) -> bool:
        return False

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            vars.remove(self.lhs)
        rhs = self.rhs
        if (not without_savevar) or rhs == self.var:  # if rhs is not saved var
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

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[SaveAssignment]:
        if isinstance(self, PushPop) and self.pointer and self.load:
            return self
        found = False
        for var in self.iter_assign_vars():
            if var in targets:
                found = True
                break
        if not found:
            return None
        if self.load and self.var.ref_var is None:
            name = self.var.name_ext()
            index = self.var.concat_index()
            if not name in targets.names():
                print(targets)
                print(self.var)
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
                if isinstance(idx, OpInt) or (
                    isinstance(idx, OpRange)
                    and isinstance(idx[0], OpInt)
                    and idx[0] == idx[1]
                ):
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
    pointer: bool = False

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        stack = self._stack_name()
        var = self.var
        if self.pointer:
            var = var.change_index(None)
        if stack == "fautodiff_stack_r":
            op = "fautodiff_stack_pop_r" if self.load else "fautodiff_stack_push_r"
            return [f"{space}call {op}({var})\n"]
        op = "pop" if self.load else "push"
        return [f"{space}call {stack}%{op}({var})\n"]

    def _stack_name(self) -> str:
        if self.pointer:
            return "fautodiff_stack_p"
        typ = self.var.var_type.typename if self.var.var_type else None
        kind = self.var.var_type.kind if self.var.var_type else None
        if typ is None or typ == "unknown":
            p = self.parent
            while p is not None and not isinstance(p, Routine):
                p = p.parent
            if p is not None:
                v = p.get_var(self.var.name)
                if v is not None:
                    typ = v.var_type.typename if v.var_type else None
                    kind = v.var_type.kind if v.var_type else None
        typ = typ.lower() if typ else ""
        if typ.startswith("logical"):
            return "fautodiff_stack_l"
        if typ.startswith("integer"):
            return "fautodiff_stack_i"
        if typ.startswith("real") or typ.startswith("double"):
            return "fautodiff_stack_r"
        return "fautodiff_stack_r"

    def to_load(self) -> "PushPop":
        return PushPop(
            self.var, id=self.id, tmpvar=self.tmpvar, pointer=self.pointer, load=True
        )


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
            return [f"{space}call fautodiff_stack_l%push({self.flag})\n"]

    def iter_assign_vars(self, without_savevar=False):
        return iter(())

    def iter_ref_vars(self):
        return iter(())

    def iter_children(self):
        return iter(())

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            return VarList([])
        return vars

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> "PushPopL":
        return self

    def to_load(self):
        raise NotImplementedError


@dataclass
class Allocate(Node):
    """An ``allocate`` statement."""

    vars: List[OpVar] = field(default_factory=list)
    mold: Optional[OpVar] = None

    def __post_init__(self):
        super().__post_init__()
        for v in self.vars:
            if not isinstance(v, OpVar):
                raise ValueError(f"vars must be OpVar: {type(v)}")

    def copy(self) -> "Allocate":
        return Allocate(self.vars, mold=self.mold)

    def deep_clone(self) -> "Allocate":
        return Allocate(self.vars.deep_clone(), mold=self.mold.deep_clone())

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
        if self.mold is not None:
            opts = f", mold={self.mold.change_index(None)}"
        else:
            opts = ""
        return [f"{space}allocate({names}{opts})\n"]

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        nodes: List[Node] = []
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        for var in self.vars:
            is_mod_var = var.is_module_var(mod_var_names)
            ad_var = var.add_suffix(AD_SUFFIX)
            if reverse:
                if var.ad_target:
                    nodes.append(
                        Allocate._add_if(
                            Deallocate([ad_var.change_index(None)], ad_code=True),
                            ad_var,
                            is_mod_var,
                        )
                    )
                if not is_mod_var:
                    nodes.append(Deallocate([var.change_index(None)], ad_code=True))
            else:
                if not is_mod_var:
                    nodes.append(Allocate([var]))
                if var.ad_target:
                    nodes.append(
                        Allocate._add_if(Allocate([ad_var]), ad_var, is_mod_var)
                    )
                    if not var.var_type.typename.startswith(("type", "class")):
                        nodes.append(ClearAssignment(ad_var.change_index(None)))

        return nodes

    def is_effectively_empty(self) -> bool:
        return not self.vars

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
        for var in self.vars:
            vars.remove(var)
            index = var.concat_index()
            if index:
                for idx in index.collect_vars():
                    vars.push(idx)
        return vars

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[Allocate]:
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        vars = []
        for var in self.vars:
            is_base_mod = var.is_module_var(mod_var_names)
            is_any_mod = var.is_module_var(mod_var_names, check_ad=True)
            if not is_base_mod and (
                is_any_mod or (decl_map is None or var.name in decl_map)
            ):
                vars.append(var)
        if vars:
            return Allocate(vars, mold=self.mold)
        return None

    @classmethod
    def _add_if(cls, node: Node, var: OpVar, is_mod_var: bool) -> Node:
        """Wrap ``node`` in a conditional block when needed."""

        # Pointer arguments and module variables might already be
        # allocated/associated outside of the current routine.  Guard the
        # (de)allocation so we do not operate on them twice.  Local pointer
        # variables have a well defined state so do not require this check.
        if isinstance(node, Deallocate):
            return node
        check = is_mod_var or (
            var.intent in ("in", "inout") and (var.allocatable or var.pointer)
        )
        if check:
            func = "associated" if var.pointer else "allocated"
            cond = OpFunc(func, args=[var.change_index(None)])
            body = Block([node])
            if isinstance(node, Allocate):
                cond = OpNot([cond])
            else:
                if var.ref_var is not None and var.ref_var.allocatable:
                    cond = OpLogic(
                        ".and.",
                        args=[
                            OpFunc(func, args=[var.ref_var.change_index(None)]),
                            cond,
                        ],
                    )
            return IfBlock([(cond, body)])
        return node


@dataclass
class Deallocate(Node):
    """A ``deallocate`` statement."""

    vars: List[OpVar] = field(default_factory=list)
    index: Optional[List[str]] = field(default=None)
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
        lines: List[str] = []
        for v in self.vars:
            func = "associated" if v.pointer else "allocated"
            name = str(v)
            cond = f"{func}({name})"
            ref = v.ref_var
            while isinstance(ref, OpVar):
                func_ref = "associated" if ref.pointer else "allocated"
                cond = f"{func_ref}({ref.change_index(None)}) .and. {cond}"
                ref = ref.ref_var
            lines.append(f"{space}if ({cond}) then\n")
            lines.append(f"{space}  deallocate({name})\n")
            lines.append(f"{space}end if\n")
        return lines

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        nodes: List[Node] = []
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        for var in self.vars:
            is_mod_var = var.is_module_var(mod_var_names)
            ad_var = var.add_suffix(AD_SUFFIX)
            if reverse:
                if var.ad_target:
                    mold = None
                    if is_mod_var:
                        if var.var_type.typename.startswith(("type", "class")):
                            index = []
                            ndim = len(var.index)
                            var = var.change_index(None)
                            for n in range(ndim):
                                index.append(OpFunc("size", args=[var, OpInt(n + 1)]))
                            index = AryIndex(index)
                        else:
                            mold = var
                            index = None
                        ad_var = ad_var.change_index(index)
                    elif self.index is not None:
                        ad_var = ad_var.change_index(self.index)
                    nodes.append(
                        Allocate._add_if(
                            Allocate([ad_var], mold=mold), ad_var, is_mod_var
                        )
                    )
                    if not var.var_type.typename.startswith(("type", "class")):
                        nodes.append(ClearAssignment(ad_var.change_index(None)))

            else:
                if var.ad_target:
                    ad_clean = ad_var.change_index(None)
                    nodes.append(
                        Allocate._add_if(
                            Deallocate([ad_clean], ad_code=True), ad_clean, is_mod_var
                        )
                    )
                var_clean = var.change_index(None)
                nodes.append(
                    Allocate._add_if(
                        Deallocate([var_clean], ad_code=True), var_clean, is_mod_var
                    )
                )
        return nodes

    def is_effectively_empty(self) -> bool:
        return not self.vars

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
        for var in self.vars:
            index = None if var.index is None else AryIndex([None] * len(var.index))
            vars.remove(var.change_index(index))
        return vars

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional["Deallocate"]:
        if not self.ad_code:
            return None
        mod_var_names = [var.name for var in mod_vars] if mod_vars is not None else []
        vars = []
        for var in self.vars:
            is_base_mod = var.is_module_var(mod_var_names)
            is_any_mod = var.is_module_var(mod_var_names, check_ad=True)
            if not is_base_mod and (
                is_any_mod or (decl_map is None or var.name in decl_map)
            ):
                vars.append(var)
        if vars:
            return Deallocate(vars, ad_code=True)
        return None


@dataclass
class PointerAssignment(Node):
    """A pointer assignment statement ``lhs => rhs``."""

    lhs: OpVar
    rhs: OpVar
    info: Optional[dict] = field(repr=False, default=None)
    ad_info: Optional[str] = field(repr=False, default=None)
    _rhs_vars: List[OpVar] = field(init=False, repr=False)

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.lhs, OpVar):
            raise ValueError(f"lhs must be OpVar: {type(self.lhs)}")
        if not isinstance(self.rhs, OpVar):
            raise ValueError(f"rhs must be OpVar: {type(self.rhs)}")
        self._rhs_vars = list(self.rhs.collect_vars(without_refvar=True))

    def copy(self) -> "PointerAssignment":
        return PointerAssignment(self.lhs, self.rhs, self.info, self.ad_info)

    def deep_clone(self):
        lhs = self.lhs.deep_clone()
        rhs = self.rhs.deep_clone()
        return PointerAssignment(lhs, rhs, self.info, self.ad_info)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for var in self._rhs_vars:
            yield var
        for var in self.lhs.collect_vars():
            if var != self.lhs:
                yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        yield self.lhs

    def is_effectively_empty(self) -> bool:
        return False

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        ad_comment = ""
        if self.ad_info is not None:
            ad_comment = f" ! {self.ad_info}"
        return [f"{space}{self.lhs} => {self.rhs}{ad_comment}\n"]

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        nodes = []
        if self.lhs.ad_target:
            ad_info = self.info.get("code") if self.info is not None else None
            lhs = self.lhs
            lhs_ad = lhs.add_suffix(AD_SUFFIX)
            rhs = self.rhs
            rhs_ad = self.rhs.add_suffix(AD_SUFFIX)
            if reverse:
                id = self.get_id()
                self.parent.insert_before(
                    id,
                    PointerAssignment(lhs_ad, rhs_ad, info=self.info, ad_info=ad_info),
                )
                nodes.append(
                    PointerClear(lhs, previous=rhs, info=self.info, ad_info=ad_info)
                )
                nodes.append(
                    PointerClear(
                        lhs_ad, previous=rhs_ad, info=self.info, ad_info=ad_info
                    )
                )
            else:
                nodes.append(
                    PointerAssignment(lhs_ad, rhs_ad, info=self.info, ad_info=ad_info)
                )
                nodes.append(self)
                if isinstance(rhs_ad, OpVar) and assigned_advars is not None:
                    if rhs_ad in assigned_advars:
                        assigned_advars.push(lhs_ad)
        return nodes

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        lhs = self.lhs
        if vars is None:
            vars = VarList()
        else:
            if not isinstance(vars, VarList):
                raise ValueError(f"Must be VarList: {type(vars)}")
            vars = vars.copy()
            vars.remove(lhs)
        for var in self._rhs_vars:
            vars.push(var)
        return vars

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[Node]:
        target_names = targets.names()
        if self.lhs.name_ext() in target_names or self.rhs.name_ext() in target_names:
            return self.deep_clone()
        return None

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            return VarList()
        if self.lhs.name.endswith(AD_SUFFIX):
            names = assigned_vars.names()
            lhs_name = self.lhs.name_ext()
            rhs_name = self.rhs.name_ext()
            if lhs_name in names and rhs_name not in names:
                assigned_vars.vars[rhs_name] = list(assigned_vars.vars[lhs_name])
                assigned_vars.dims[rhs_name] = list(assigned_vars.dims[lhs_name])
            if rhs_name in names and lhs_name not in names:
                assigned_vars.vars[lhs_name] = list(assigned_vars.vars[rhs_name])
                assigned_vars.dims[lhs_name] = list(assigned_vars.dims[rhs_name])
        return assigned_vars


@dataclass
class PointerClear(Node):

    var: OpVar
    previous: Optional[OpVar]
    info: Optional[dict] = field(repr=False, default=None)
    ad_info: Optional[str] = field(repr=False, default=None)

    def __post_init__(self):
        super().__post_init__()

    def copy(self) -> "PointerClear":
        return PointerClear(self.var, self.previous, self.info, self.ad_info)

    def deep_clone(self):
        var = self.var.deep_clone()
        previous = self.previous.deep_clone() if self.previous is not None else None
        return PointerClear(var, previous, self.info, self.ad_info)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        return iter(())

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        return iter(())

    def is_effectively_empty(self) -> bool:
        return False

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        ad_comment = ""
        if self.ad_info is not None:
            ad_comment = f" ! {self.ad_info}"
        return [f"{space}{self.var} => null(){ad_comment}\n"]

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> List[Node]:
        nodes: List[Node] = []
        if self.var.ad_target:
            ad_info = self.info.get("code") if self.info is not None else None
            var = self.var
            var_ad = var.add_suffix(AD_SUFFIX)
            typename = var.var_type.typename
            kind = var.var_type.kind
            index = var.index
            dims = var.dims
            if isinstance(self.previous, OpVar):
                previous_ad = self.previous.add_suffix(AD_SUFFIX)
            else:
                previous_ad = self.previous  # None
            if reverse:
                id = self.get_id()
                if self.previous is None:
                    tmpvar = OpVar(
                        self._save_var_name(var.name, id),
                        var_type=VarType(typename, kind=kind),
                        index=index,
                        dims=dims,
                        pointer=True,
                        reference=var,
                    )
                    self.parent.insert_before(
                        id,
                        PointerAssignment(tmpvar, var, info=self.info, ad_info=ad_info),
                    )
                    saved_vars.append(tmpvar)
                    tmpvar_ad = OpVar(
                        self._save_var_name(var_ad.name, id),
                        var_type=VarType(typename, kind=kind),
                        index=index,
                        dims=dims,
                        pointer=True,
                        reference=var_ad,
                    )
                    self.parent.insert_before(
                        id,
                        PointerAssignment(
                            tmpvar_ad, var_ad, info=self.info, ad_info=ad_info
                        ),
                    )
                    saved_vars.append(tmpvar_ad)
                    nodes.append(
                        PointerAssignment(
                            var_ad, tmpvar_ad, info=self.info, ad_info=ad_info
                        )
                    )
                    nodes.append(
                        PointerAssignment(var, tmpvar, info=self.info, ad_info=ad_info)
                    )
                else:
                    self.parent.insert_after(
                        id,
                        PointerClear(
                            var_ad, previous_ad, info=self.info, ad_info=ad_info
                        ),
                    )
                    nodes.append(
                        PointerAssignment(
                            var_ad, previous_ad, info=self.info, ad_info=ad_info
                        )
                    )
                    nodes.append(
                        PointerAssignment(
                            var, self.previous, info=self.info, ad_info=ad_info
                        )
                    )
            else:
                nodes.append(
                    PointerClear(var_ad, previous_ad, info=self.info, ad_info=ad_info)
                )
                nodes.append(self)
                if assigned_advars is not None:
                    assigned_advars.remove(var_ad)
        return nodes

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            vars.remove(self.var)
        if isinstance(self.previous, OpVar):
            vars.push(self.var)
        return vars

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[Node]:
        if (
            isinstance(self.previous, OpVar)
            and self.previous.name_ext() in targets.names()
        ):
            return self.deep_clone()
        return None


@dataclass
class BranchBlock(Node):
    """An abstract class for ``if`` and ``select case`` branch stracture."""

    cond_blocks: List[Tuple[Union[Operator, Tuple[Operator]], Block]] = field(
        default_factory=list
    )

    def __post_init__(self):
        super().__post_init__()
        if not isinstance(self.cond_blocks, list):
            raise ValueError(f"cond_blocks must be a list: {type(self.cond_blocks)}")
        for tup in self.cond_blocks:
            if not isinstance(tup, tuple):
                raise ValueError(f"item in cond_blocks must be a tuple: {type(tup)}")
            cond, block = tup
            if isinstance(self, (IfBlock, WhereBlock)):
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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        cond_blocks = []
        for cond, block in self.cond_blocks:
            nodes = block.generate_ad(
                saved_vars,
                reverse,
                assigned_advars,
                routine_map,
                generic_map,
                mod_vars,
                exitcycle_flags,
                return_flags,
                warnings,
            )
            if reverse:
                nodes_new = block.set_for_returnexitcycle(return_flags, exitcycle_flags)
                if nodes_new:
                    if exitcycle_flags:
                        for flag in exitcycle_flags:
                            nodes_new.insert(0, Assignment(flag, OpTrue()))
                    if return_flags:
                        for flag in return_flags:
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
        elif isinstance(self, WhereBlock):
            block = WhereBlock(cond_blocks)
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

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List[Node]:
        cond_blocks: List[tuple] = []
        for cond, block in self.cond_blocks:
            nodes = block.set_for_returnexitcycle(
                return_flags,
                exitcycle_flags,
                set_return_cond,
                set_exitcycle_cond,
                set_do_index,
                label,
                label_map,
                keep,
            )
            body = Block(nodes)
            cond_blocks.append((cond, body))
        if isinstance(self, IfBlock):
            return [IfBlock(cond_blocks)]
        elif isinstance(self, SelectBlock):
            return [SelectBlock(cond_blocks, expr=self.expr)]
        else:  # WhereBlock
            return [WhereBlock(cond_blocks)]

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
        vars_new = VarList()
        cover_all = False  # if else or case default exists
        to_remove = set()
        origin_savevars = {name for name in vars.names() if Node.is_savevar(name)}

        def _check(cond: Operator) -> bool:
            if isinstance(cond, OpLogic):
                return _check(cond.args[0]) and _check(cond.args[1])
            if isinstance(cond, OpFunc) and cond.name in ("allocated", "associated"):
                return True
            return False

        for cond, block in self.cond_blocks:
            vs = block.required_vars(vars, no_accumulate, without_savevar)
            vars_new.merge(vs)
            advars = {name for name in vs.names() if Node.is_savevar(name)}
            to_remove = to_remove | (origin_savevars - advars)
            if (
                isinstance(self, (IfBlock, WhereBlock))
                and len(block) > 0
                and isinstance(block[0], Deallocate)
            ):
                if cond is not None and _check(cond):
                    to_remove = to_remove | {v.name_ext() for v in block[0].vars}
            if cond is None:
                cover_all = True
        if not cover_all:
            vars_new.merge(vars)
        for name in to_remove:
            if vars_new.vars and name in vars_new.vars:
                del vars_new.vars[name]
        for cond, _ in self.cond_blocks:
            if cond is not None:
                if isinstance(self, (IfBlock, WhereBlock)):
                    if not _check(cond):
                        for var in cond.collect_vars(without_checkfunc=True):
                            vars_new.push(var)
                elif isinstance(self, SelectBlock):
                    for co in cond:
                        for var in co.collect_vars(without_checkfunc=True):
                            vars_new.push(var)
                else:
                    raise ValueError
        return vars_new

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars_list = VarList()
        for block in self.iter_children():
            for v in block.unrefered_advars(vars):
                if v.name.endswith(AD_SUFFIX):
                    vars_list.push(v)
        return vars_list

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[Node]:
        if base_targets is None:
            base_targets = targets.copy()
        new_condblocks: List[tuple] = []
        flag = False
        for cond, block in self.cond_blocks:
            block_targets = targets
            returns = block.collect_return()
            if returns:
                block_targets = base_targets
            new_block = block.prune_for(block_targets, mod_vars, decl_map, base_targets)
            new_condblocks.append((cond, new_block))
            if new_block is not None:
                flag = True
        if not flag or len(new_condblocks) == 0:
            return None
        if isinstance(self, IfBlock):
            node = IfBlock(new_condblocks)
        elif isinstance(self, SelectBlock):
            node = SelectBlock(new_condblocks, expr=self.expr)
        elif isinstance(self, WhereBlock):
            node = WhereBlock(new_condblocks)
        else:
            raise RuntimeError(f"Invalid class type: {type(self)}")
        return node

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
                lines.extend(block.render(indent + 1))
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
                conds = ", ".join([str(co) for co in cond])
                lines.append(f"{space}{case} ({conds})\n")
            else:
                lines.append(f"{space}{case} default\n")
            lines.extend(block.render(indent + 1))
        lines.append(f"{space}end select\n")
        return lines


@dataclass
class WhereBlock(BranchBlock):
    """A ``where`` construct with optional ``elsewhere`` blocks."""

    def copy(self) -> "WhereBlock":
        return WhereBlock(self.cond_blocks)

    def deep_clone(self) -> "WhereBlock":
        cond_blocks = []
        for cond, block in self.cond_blocks:
            cond = cond.deep_clone() if cond else None
            block = block.deep_clone()
            cond_blocks.append((cond, block))
        return WhereBlock(cond_blocks)

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for cond, _ in self.cond_blocks:
            if cond is not None:
                for var in cond.collect_vars():
                    yield var

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        cond, block = self.cond_blocks[0]
        lines = [f"{space}where ({cond})\n"]
        lines.extend(block.render(indent + 1))
        for cond, block in self.cond_blocks[1:]:
            if cond is None:
                lines.append(f"{space}elsewhere\n")
            else:
                lines.append(f"{space}elsewhere ({cond})\n")
            lines.extend(block.render(indent + 1))
        lines.append(f"{space}end where\n")
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
        vars: List[Node] = []
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
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:

        exitcycle_flags = None
        if reverse:
            exitcycles = self._body.collect_exitcycle()
            returns = self._body.collect_return()
            exitcycle_flags = [node.flag() for node in exitcycles + returns]
        else:
            for vname in self.recurrent_vars():
                assigned_advars.push(OpVar(name=f"{vname}{AD_SUFFIX}"))

        nodes = self._body.generate_ad(
            saved_vars,
            reverse,
            assigned_advars,
            routine_map,
            generic_map,
            mod_vars,
            exitcycle_flags,
            return_flags,
            warnings,
        )
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
            fwd_body = self._body.set_for_returnexitcycle(
                return_flags, exitcycle_flags, set_exitcycle_cond=True, label=label
            )

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
                    if (
                        cvar.name.endswith(AD_SUFFIX)
                        or cvar == self.index
                        or cvar.name in conflict_vars
                    ):
                        continue
                    save = SaveAssignment(cvar, self._body.get_id())
                    self._body.insert_begin(save)
                    load = save.to_load()
                    saved_vars.append(save.tmpvar)
                    new_body.append(load)
                    loads.insert(0, load)

            pushed = []
            recurrent_vars = self.recurrent_vars()
            for node in [node for node in self._body.iter_children()]:
                if isinstance(node, PointerClear):
                    if node.previous is None:
                        save = PushPop(node.var, node.get_id(), pointer=True)
                        self._body.insert_begin(save)
                        new_body.append(save.to_load())
                        var_ad = node.var.add_suffix(AD_SUFFIX)
                        save_ad = PushPop(var_ad, node.get_id(), pointer=True)
                        self._body.insert_begin(save_ad)
                        new_body.append(save_ad.to_load())
                    continue
                if isinstance(node, PointerAssignment):
                    continue
                for var in node.assigned_vars():
                    if var.name in recurrent_vars and (
                        not self.do or var.name in conflict_vars
                    ):
                        if not var in pushed:
                            save = PushPop(var, node.get_id())
                            self._body.insert_begin(save)
                            new_body.append(save.to_load())
                            pushed.append(var)

            if (
                isinstance(fwd_body[-1], IfBlock)
                and isinstance(nodes[0], IfBlock)
                and len(fwd_body[-1].cond_blocks) == 1
                and len(nodes[0].cond_blocks) == 1
                and fwd_body[-1].cond_blocks[0][0] == nodes[0].cond_blocks[0][0]
                and not fwd_body[-1].collect_exitcycle()
                and not nodes[0].collect_exitcycle()
            ):
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
                cond = OpFuncUser(
                    "fautodiff_stack_l%get", [], var_type=VarType("logical")
                )

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
        return DoLoop(
            self._body.deep_clone(),
            self.index.deep_clone(),
            self.range.deep_clone(),
            self.label,
        )

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
        for var in self._body.collect_vars(without_refvar=True, without_index=True):
            index = var.concat_index()
            if index is not None:
                for i, idx in enumerate(index):
                    if isinstance(idx, OpVar) and idx == self.index:
                        index_map[var.name_ext()] = (i, len(index))
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

    def has_modified_indices(self) -> bool:
        """Return ``True`` if any array access modifies the loop index.

        This scans the loop body for array index expressions that depend on
        the loop index variable but are not a simple use of that index.  It
        also tracks variables assigned from the loop index (e.g. ``ip = i``)
        and treats them as aliases.  Aliases that are subsequently modified
        (``ip = ip + 1``) are recognised as dependencies when used as array
        indices.
        """

        dep_vars = {self.index.name}
        simple_aliases = {self.index.name}

        def _walk(node: Node) -> bool:
            nonlocal dep_vars, simple_aliases

            # Check array index expressions in referenced and assigned vars
            vars = node.collect_vars(without_refvar=True, without_index=True)
            for var in vars:
                if var.index is None:
                    continue
                for dim in var.index:
                    if dim is None:
                        continue
                    names = [v.name for v in dim.collect_vars()]
                    if any(name in dep_vars for name in names):
                        if isinstance(dim, OpVar) and dim.name in simple_aliases:
                            continue
                        return True

            # Track variable assignments originating from dependent vars
            if isinstance(node, Assignment):
                lhs = node.lhs.name
                rhs_vars = node.rhs.collect_vars()
                if rhs_vars and any(v.name in dep_vars for v in rhs_vars):
                    if (
                        len(rhs_vars) == 1
                        and isinstance(node.rhs, OpVar)
                        and rhs_vars[0].name in simple_aliases
                    ):
                        dep_vars.add(lhs)
                        simple_aliases.add(lhs)
                    else:
                        dep_vars.add(lhs)
                        if lhs in simple_aliases:
                            simple_aliases.remove(lhs)

            for child in node.iter_children():
                if _walk(child):
                    return True
            return False

        for child in self._body.iter_children():
            if _walk(child):
                return True
        return False

    def recurrent_vars(self) -> List[str]:
        required_vars = self._body.required_vars()
        assigned_vars = self._body.assigned_vars()
        common_var_names = sorted(
            set(required_vars.names()) & set(assigned_vars.names())
        )
        do_index_list = set(self.do_index_list)
        var_names = []
        for name in common_var_names:
            flag = False
            for index in required_vars[name]:
                if not (index is not None and do_index_list <= set(index.list())):
                    flag = True
                    break
            if not flag:
                for index in assigned_vars[name]:
                    if not (index is not None and do_index_list <= set(index.list())):
                        flag = True
                        break
            if flag:
                var_names.append(name)
        return var_names

    def conflict_vars(self) -> List[str]:
        required_vars = self._body.required_vars()
        assigned_vars = self._body.assigned_vars()
        common_var_names = sorted(
            set(required_vars.names()) & set(assigned_vars.names())
        )
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

    def set_for_returnexitcycle(
        self,
        return_flags: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        set_return_cond: bool = False,
        set_exitcycle_cond: bool = False,
        set_do_index: Optional[Tuple[OpVar, OpVar]] = None,
        label: Optional[str] = None,
        label_map: Optional[List[Tuple[str, str]]] = None,
        keep: bool = False,
    ) -> List[Node]:
        exitcycles = self._body.collect_exitcycle()
        if exitcycles and any(isinstance(ec, ExitStmt) for ec in exitcycles):
            nodes: List[Node] = []
            exit_do_start = self.exit_do_start()
            if set_do_index is None:
                nodes.append(Assignment(exit_do_start, self.range[1]))
                set_do_index = (exit_do_start, self.index)
            if label:  # this means that this node is in ad_code
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
            body = Block(
                self._body.set_for_returnexitcycle(
                    return_flags,
                    exitcycle_flags,
                    set_return_cond,
                    set_exitcycle_cond,
                    set_do_index,
                    self.label,
                    label_map,
                    keep=True,
                )
            )
            nodes.append(DoLoop(body, self.index, self.range, label))
            return nodes
        return [self]

    def exit_do_start(self) -> OpVar:
        return OpVar(f"exit_do_start_{self.get_id()}_ad", var_type=VarType("integer"))

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        label = f"{self.label}: " if self.label else ""
        header = f"{space}{label}do {self.index} = {self.range[0]}, {self.range[1]}"
        if self.range[2] is not None:
            header = f"{header}, {self.range[2]}"
        lines = [f"{header}\n"]
        lines.extend(self._body.render(indent + 1))
        label = f" {self.label}" if self.label else ""
        lines.append(f"{space}end do{label}\n")
        return lines

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        if vars is None:
            vars = VarList()
        else:
            vars = vars.copy()
            vars.update_index_downward(self._build_index_map(), self.index)
        vars = self._body.required_vars(vars, no_accumulate, without_savevar)

        if self.index in vars:
            vars.remove(self.index)
        index_map = self._build_index_map()
        if (
            self.range[2] is None
            or (isinstance(self.range[2], OpInt) and self.range[2].val == 1)
            or (
                isinstance(self.range[2], OpNeg)
                and isinstance(self.range[2].args[0], OpInt)
                and self.range[2].args[0].val == 1
            )
        ):
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

    def assigned_vars(
        self,
        vars: Optional[VarList] = None,
        without_savevar: bool = False,
        check_init_advars: bool = False,
    ) -> VarList:
        vars = self._body.assigned_vars(
            vars, without_savevar=without_savevar, check_init_advars=check_init_advars
        )
        vars.update_index_upward(self._build_index_map(), range=self.range)
        if not check_init_advars:
            vars.push(self.index)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        vars = self._body.unrefered_advars(vars)
        vars.update_index_upward(self._build_index_map(), range=self.range)
        return vars

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[Node]:
        targets = targets.copy()
        flag = True
        if base_targets is None:
            base_targets = targets.copy()
        while flag:
            flag = False
            new_body = self._body.prune_for(targets, mod_vars, decl_map, base_targets)
            for var in new_body.required_vars(targets):
                if var.name == self.index.name:
                    continue
                if var.index is not None and set(self.do_index_list) <= set(
                    var.index_list()
                ):
                    continue
                if var not in targets:
                    targets.push(var)
                    flag = True
        new_body = self._body.prune_for(targets, mod_vars, decl_map, base_targets)

        if new_body.is_effectively_empty():
            return None
        return DoLoop(new_body, self.index, self.range, self.label)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        else:
            assigned_vars = assigned_vars.copy()
        assigned_vars.update_index_downward(
            self._build_index_map(), do_index_var=self.index
        )
        # assigned_vars.merge(self._body.assigned_vars(check_init_advars = True))
        for var in self._body.assigned_vars(check_init_advars=True):
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
        lines.extend(self._body.render(indent + 1))
        label = f" {self.label}" if self.label else ""
        lines.append(f"{space}end do{label}\n")
        return lines

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
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

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Node:
        if base_targets is None:
            base_targets = targets.copy()
        new_body = self._body.prune_for(targets, mod_vars, decl_map, base_targets)
        targets = targets.copy()
        targets.merge(new_body.required_vars(targets))
        for var in self.cond.collect_vars():
            targets.push(var)
        new_body = self._body.prune_for(targets, mod_vars, decl_map, base_targets)
        if new_body.is_effectively_empty():
            return Block([])
        return DoWhile(new_body, self.cond, self.label)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if assigned_vars is None:
            assigned_vars = VarList()
        else:
            assigned_vars = assigned_vars.copy()
        assigned_vars.merge(self._body.assigned_vars(check_init_advars=True))
        assigned_vars = self._body.check_initial(assigned_vars)
        return assigned_vars


@dataclass
class BlockConstruct(Node):
    """A ``block`` construct with its own declarations."""

    decls: Block = field(default_factory=Block)
    body: Block = field(default_factory=Block)

    def __post_init__(self):
        super().__post_init__()
        self.decls.set_parent(self)
        self.body.set_parent(self)

    def copy(self) -> "BlockConstruct":
        return BlockConstruct(self.decls, self.body)

    def deep_clone(self) -> "BlockConstruct":
        return BlockConstruct(self.decls.deep_clone(), self.body.deep_clone())

    def iter_children(self) -> Iterator[Node]:
        yield self.decls
        yield self.body

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        lines = [f"{space}block\n"]
        if not self.decls.is_effectively_empty():
            lines.extend(self.decls.render(indent + 1))
            lines.append("\n")
        if not self.body.is_effectively_empty():
            lines.extend(self.body.render(indent + 1))
        lines.append(f"{space}end block\n")
        return lines

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[List[str]] = None,
    ) -> List[Node]:
        decls = self.decls.deep_clone()
        for node in self.decls.generate_ad(
            saved_vars,
            reverse,
            assigned_advars,
            routine_map,
            generic_map,
            mod_vars,
            exitcycle_flags,
            return_flags,
            type_map,
            warnings,
        ):
            decls.append(node)
        body_nodes = self.body.generate_ad(
            saved_vars,
            reverse,
            assigned_advars,
            routine_map,
            generic_map,
            mod_vars,
            exitcycle_flags,
            return_flags,
            type_map,
            warnings,
        )
        body = Block(body_nodes)
        return [BlockConstruct(decls, body)]

    # ------------------------------------------------------------------
    # helper utilities
    # ------------------------------------------------------------------

    def _local_names(self) -> List[str]:
        names: List[str] = []
        for node in self.decls.iter_children():
            if isinstance(node, Declaration):
                names.append(node.name)
        return names

    @staticmethod
    def _save_remove(vars: Optional[VarList], names: List[str]):
        if vars is None:
            return {}, {}, {}
        saved = {}
        saved_dims = {}
        saved_ex = {}
        for name in names:
            if name in vars.vars:
                saved[name] = vars.vars.pop(name)
                saved_dims[name] = vars.dims.pop(name)
                if name in vars.exclude:
                    saved_ex[name] = vars.exclude.pop(name)
        return saved, saved_dims, saved_ex

    @staticmethod
    def _remove_names(vars: Optional[VarList], names: List[str]):
        if vars is None:
            return
        for name in names:
            if name in vars.vars:
                vars.vars.pop(name)
                vars.dims.pop(name, None)
                vars.exclude.pop(name, None)

    @staticmethod
    def _restore(vars: VarList, saved, saved_dims, saved_ex):
        for name in saved:
            vars.vars[name] = saved[name]
            vars.dims[name] = saved_dims[name]
            if name in saved_ex:
                vars.exclude[name] = saved_ex[name]

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        local_names = self._local_names()
        saved, saved_dims, saved_ex = self._save_remove(vars, local_names)
        vars = self.body.required_vars(vars, no_accumulate, without_savevar)
        vars = self.decls.required_vars(vars, no_accumulate, without_savevar)
        self._remove_names(vars, local_names)
        self._restore(vars, saved, saved_dims, saved_ex)
        return vars

    def assigned_vars(
        self,
        vars: Optional[VarList] = None,
        without_savevar: bool = False,
        check_init_advars: bool = False,
    ) -> VarList:
        local_names = self._local_names()
        saved, saved_dims, saved_ex = self._save_remove(vars, local_names)
        vars = self.body.assigned_vars(
            vars, without_savevar=without_savevar, check_init_advars=check_init_advars
        )
        vars = self.decls.assigned_vars(
            vars, without_savevar=without_savevar, check_init_advars=check_init_advars
        )
        self._remove_names(vars, local_names)
        self._restore(vars, saved, saved_dims, saved_ex)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        local_names = self._local_names()
        saved, saved_dims, saved_ex = self._save_remove(vars, local_names)
        vars = self.decls.unrefered_advars(vars)
        vars = self.body.unrefered_advars(vars)
        self._remove_names(vars, local_names)
        self._restore(vars, saved, saved_dims, saved_ex)
        return vars

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[Node]:
        if base_targets is None:
            base_targets = targets.copy()
        local_names = self._local_names()
        t_local = targets.copy()
        self._remove_names(t_local, local_names)
        body = self.body.prune_for(t_local, mod_vars, decl_map, base_targets)
        targets_new = body.required_vars(t_local.copy())
        targets_new.merge(body.assigned_vars())
        decls = self.decls.prune_for(targets_new, mod_vars, decl_map, base_targets)
        if decls.is_effectively_empty() and body.is_effectively_empty():
            return None
        return BlockConstruct(decls, body)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        local_names = self._local_names()
        saved, saved_dims, saved_ex = self._save_remove(assigned_vars, local_names)
        vars = self.decls.check_initial(assigned_vars)
        vars = self.body.check_initial(vars)
        self._remove_names(vars, local_names)
        self._restore(vars, saved, saved_dims, saved_ex)
        return vars

    # ------------------------------------------------------------------
    # reference and assignment helpers with scope handling
    # ------------------------------------------------------------------

    def iter_ref_vars(self) -> Iterator[OpVar]:
        local_names = self._local_names()
        for var in self.decls.iter_ref_vars():
            v = var
            while v.ref_var:
                v = v.ref_var
            if v.name not in local_names:
                yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        local_names = self._local_names()
        for var in self.decls.iter_assign_vars(without_savevar=without_savevar):
            v = var
            while v.ref_var:
                v = v.ref_var
            if v.name not in local_names:
                yield var

    def has_reference_to(self, var: OpVar) -> bool:
        v = var
        while v.ref_var:
            v = v.ref_var
        if v.name in self._local_names():
            return False
        return self.decls.has_reference_to(var) or self.body.has_reference_to(var)

    def has_assignment_to(self, var: OpVar) -> bool:
        v = var
        while v.ref_var:
            v = v.ref_var
        if v.name in self._local_names():
            return False
        return self.decls.has_assignment_to(var) or self.body.has_assignment_to(var)


@dataclass
class OmpDirective(Node):
    """Representation of an OpenMP directive."""

    directive: str
    clauses: List[Union[str, Dict[str, List[Any]]]] = field(default_factory=list)
    body: Optional[Node] = None

    def __post_init__(self):
        super().__post_init__()
        self.clauses = [
            self._parse_clause(c) if isinstance(c, str) else c for c in self.clauses
        ]
        if self.body is not None:
            self.body.set_parent(self)

    @staticmethod
    def _parse_clause(clause: str) -> Union[str, Dict[str, List[Any]]]:
        m = re.match(r"\s*(\w+)\s*\((.*)\)\s*", clause)
        if not m:
            return clause.strip()
        key = m.group(1)
        body = m.group(2).strip()
        low_key = key.lower()
        if low_key == "reduction":
            m2 = re.match(r"([^:]+):(.*)", body)
            if m2:
                op = m2.group(1).strip()
                vars = [v.strip() for v in m2.group(2).split(",") if v.strip()]
                return {key: [op, vars]}
            return clause.strip()
        if low_key in {
            "private",
            "firstprivate",
            "lastprivate",
            "shared",
            "copyin",
            "copyprivate",
        }:
            vars = [v.strip() for v in body.split(",") if v.strip()]
            return {key: vars}
        return clause.strip()

    def iter_children(self) -> Iterator[Node]:
        if self.body is not None:
            yield self.body

    def copy(self) -> "OmpDirective":
        return OmpDirective(self.directive, copy.deepcopy(self.clauses), self.body)

    def deep_clone(self) -> "OmpDirective":
        body = self.body.deep_clone() if self.body is not None else None
        return OmpDirective(self.directive, copy.deepcopy(self.clauses), body)

    def insert_before(self, id: int, node: "Node"):
        if self.body is None:
            raise NotImplementedError("OmpDirective has no body")
        if isinstance(self.body, Block):
            self.body.insert_before(id, node)
            return
        if self.body.get_id() == id:
            self.body = Block([node, self.body])
            self.body.set_parent(self)
            return
        self.body.insert_before(id, node)

    def insert_begin(self, node: "Node"):
        if self.body is None:
            self.body = Block([node])
            self.body.set_parent(self)
            return
        if isinstance(self.body, Block):
            self.body.insert_begin(node)
            return
        self.body = Block([node, self.body])
        self.body.set_parent(self)

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        parts: List[str] = []
        for c in self.clauses:
            if isinstance(c, str):
                parts.append(c)
                continue
            key, values = next(iter(c.items()))
            low_key = key.lower()
            if low_key == "reduction":
                op, vars = values[0], values[1]
                parts.append(f"{key}({op}:{', '.join(vars)})")
            else:
                parts.append(f"{key}({', '.join(values)})")
        clause = f" {' '.join(parts)}" if parts else ""
        lines = [f"{space}!$omp {self.directive}{clause}\n"]
        if self.body is not None:
            lines.extend(self.body.render(indent))
            lines.append(f"{space}!$omp end {self.directive}\n")
        return lines

    def build_do_index_list(self, index_list: List[str]) -> None:
        self.do_index_list = index_list
        if self.body is not None:
            self.body.build_do_index_list(index_list)

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        if self.body is None:
            return [self]
        nodes = self.body.generate_ad(
            saved_vars,
            reverse,
            assigned_advars,
            routine_map,
            generic_map,
            mod_vars,
            exitcycle_flags,
            return_flags,
            warnings,
        )
        if not nodes:
            return []
        body = Block(nodes)
        new_clauses: List[Union[str, Dict[str, List[Any]]]] = []
        for clause in self.clauses:
            if isinstance(clause, str):
                new_clauses.append(clause)
                continue
            key, values = next(iter(clause.items()))
            low_key = key.lower()
            if low_key in ("private", "shared", "firstprivate"):
                vars = list(values)
                for v in list(vars):
                    ad = f"{v}{AD_SUFFIX}"
                    if ad not in vars:
                        vars.append(ad)
                new_clauses.append({key: vars})
                continue
            if low_key == "reduction":
                if reverse:
                    continue
                op, vars = values[0], list(values[1])
                for v in list(vars):
                    ad = f"{v}{AD_SUFFIX}"
                    if ad not in vars:
                        vars.append(ad)
                new_clauses.append({key: [op, vars]})
                continue
            new_clauses.append(clause)
        node = OmpDirective(self.directive, new_clauses, body)
        return [node]

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional["Node"]:
        if self.body is None:
            return self
        body = self.body.prune_for(targets, mod_vars, decl_map, base_targets)
        if body is None:
            return None

        used = {v.name for v in body.collect_vars()}
        new_clauses: List[Union[str, Dict[str, List[Any]]]] = []
        for clause in self.clauses:
            if isinstance(clause, str):
                new_clauses.append(clause)
                continue
            key, values = next(iter(clause.items()))
            low_key = key.lower()
            if low_key == "reduction":
                op, vars = values[0], [v for v in values[1] if v in used]
                if vars:
                    new_clauses.append({key: [op, vars]})
                continue
            elif low_key in {
                "private",
                "firstprivate",
                "lastprivate",
                "shared",
                "copyin",
                "copyprivate",
            }:
                vars = [v for v in values if v in used]
                if vars:
                    new_clauses.append({key: vars})
                continue
            new_clauses.append(clause)
        return OmpDirective(self.directive, new_clauses, body)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        if self.body is None:
            return assigned_vars
        return self.body.check_initial(assigned_vars)


@dataclass
class ForallBlock(Node):
    """A ``forall`` construct."""

    _body: Block
    index_specs: List[Tuple[OpVar, OpRange]]
    mask: Optional[Operator] = None

    def __post_init__(self):
        super().__post_init__()
        self._body.set_parent(self)

    def copy(self) -> "ForallBlock":
        return ForallBlock(self._body, self.index_specs, self.mask)

    def deep_clone(self) -> "ForallBlock":
        body = self._body.deep_clone()
        specs = [(idx.deep_clone(), rng.deep_clone()) for idx, rng in self.index_specs]
        mask = self.mask.deep_clone() if self.mask is not None else None
        return ForallBlock(body, specs, mask)

    def iter_children(self) -> Iterator[Node]:
        yield self._body

    def iter_ref_vars(self) -> Iterator[OpVar]:
        for idx, rng in self.index_specs:
            for var in rng.collect_vars():
                yield var
        if self.mask is not None:
            for var in self.mask.collect_vars():
                yield var

    def iter_assign_vars(self, without_savevar: bool = False) -> Iterator[OpVar]:
        for idx, _ in self.index_specs:
            yield idx

    def render(self, indent: int = 0) -> List[str]:
        space = "  " * indent
        parts = []
        for idx, rng in self.index_specs:
            part = f"{idx}={rng[0]}:{rng[1]}"
            if rng[2] is not None:
                part += f":{rng[2]}"
            parts.append(part)
        if self.mask is not None:
            parts.append(str(self.mask))
        lines = [f"{space}forall ({', '.join(parts)})\n"]
        lines.extend(self._body.render(indent + 1))
        lines.append(f"{space}end forall\n")
        return lines

    def generate_ad(
        self,
        saved_vars: List[OpVar],
        reverse: bool = False,
        assigned_advars: Optional[VarList] = None,
        routine_map: Optional[dict] = None,
        generic_map: Optional[dict] = None,
        mod_vars: Optional[List[OpVar]] = None,
        exitcycle_flags: Optional[List[OpVar]] = None,
        return_flags: Optional[List[OpVar]] = None,
        type_map: Optional[dict] = None,
        warnings: Optional[list[str]] = None,
    ) -> List[Node]:
        nodes = self._body.generate_ad(
            saved_vars,
            reverse,
            assigned_advars,
            routine_map,
            generic_map,
            mod_vars,
            exitcycle_flags,
            return_flags,
            warnings,
        )
        body = Block(nodes)
        return [ForallBlock(body, self.index_specs, self.mask)]

    def required_vars(
        self,
        vars: Optional[VarList] = None,
        no_accumulate: bool = False,
        without_savevar: bool = False,
    ) -> VarList:
        vars = self._body.required_vars(vars, no_accumulate, without_savevar)
        for idx, _ in self.index_specs:
            if idx in vars:
                vars.remove(idx)
        for idx, rng in self.index_specs:
            for var in rng.collect_vars():
                vars.push(var)
        if self.mask is not None:
            for var in self.mask.collect_vars():
                vars.push(var)
        return vars

    def assigned_vars(
        self,
        vars: Optional[VarList] = None,
        without_savevar: bool = False,
        check_init_advars: bool = False,
    ) -> VarList:
        vars = self._body.assigned_vars(
            vars, without_savevar=without_savevar, check_init_advars=check_init_advars
        )
        for idx, rng in self.index_specs:
            for var in rng.collect_vars(without_refvar=True, without_index=True):
                vars.push(var)
            if not check_init_advars:
                vars.push(idx)
        return vars

    def unrefered_advars(self, vars: Optional[VarList] = None) -> VarList:
        return self._body.unrefered_advars(vars)

    def prune_for(
        self,
        targets: VarList,
        mod_vars: Optional[List[OpVar]] = None,
        decl_map: Optional[Dict[str, "Declaration"]] = None,
        base_targets: Optional[VarList] = None,
    ) -> Optional[Node]:
        new_body = self._body.prune_for(targets, mod_vars, decl_map, base_targets)
        if new_body.is_effectively_empty():
            return None
        return ForallBlock(new_body, self.index_specs, self.mask)

    def check_initial(self, assigned_vars: Optional[VarList] = None) -> VarList:
        return self._body.check_initial(assigned_vars)


def render_program(node: Node, indent: int = 0) -> str:
    """Return formatted Fortran code for the entire ``node`` tree."""

    lines = node.render(indent)
    return "".join(lines)
