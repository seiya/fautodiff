import sys
import textwrap
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.code_tree import (
    AD_SUFFIX,
    Allocate,
    Assignment,
    Block,
    BlockConstruct,
    CallStatement,
    ClearAssignment,
    Deallocate,
    Declaration,
    DoLoop,
    DoWhile,
    IfBlock,
    OmpDirective,
    PushPop,
    ReturnStmt,
    SaveAssignment,
    Statement,
    Subroutine,
    render_program,
)
from fautodiff.operators import OpFunc, OpInt, OpRange, OpReal, OpVar, VarType
from fautodiff.var_list import VarList


class TestOpVar(unittest.TestCase):
    def test_scalar(self):
        var = OpVar("x", var_type=VarType("real"))
        self.assertEqual(var.name, "x")
        self.assertEqual(var.var_type.typename, "real")
        self.assertFalse(var.is_array())

    def test_array(self):
        var = OpVar(
            "a",
            var_type=VarType("real"),
            dims=(OpVar("n"),),
            dims_raw=("n",),
        )
        self.assertTrue(var.is_array())
        self.assertEqual(str(var.dims[0]), "n")

    def test_invalid_name(self):
        with self.assertRaises(ValueError):
            OpVar("a(i)", var_type=VarType("real"))


class TestRenderProgram(unittest.TestCase):
    def test_simple_assignment(self):
        prog = Block([Assignment(OpVar("a"), OpInt(1))])
        self.assertEqual(render_program(prog), "a = 1\n")

    def test_if_else_block(self):
        code = textwrap.dedent(
            """\
        if (a > 0) then
          b = 1
        else
          b = 2
        end if
        """
        )
        b = OpVar("b")
        cond1 = OpVar("a") > OpInt(0)
        body1 = Block([Assignment(b, OpInt(1))])
        cond2 = None
        body2 = Block([Assignment(b, OpInt(2))])
        prog = Block([IfBlock([(cond1, body1), (cond2, body2)])])
        self.assertEqual(render_program(prog), code)

    def test_if_elif_block(self):
        code = textwrap.dedent(
            """\
        if (a > 0) then
          b = 1
        else if (a < 0) then
          b = 2
        else
          b = 3
        end if
        """
        )
        b = OpVar("b")
        cond1 = OpVar("a") > OpInt(0)
        body1 = Block([Assignment(b, OpInt(1))])
        cond2 = OpVar("a") < OpInt(0)
        body2 = Block([Assignment(b, OpInt(2))])
        cond3 = None
        body3 = Block([Assignment(b, OpInt(3))])
        prog = Block([IfBlock([(cond1, body1), (cond2, body2), (cond3, body3)])])
        expected = ()
        self.assertEqual(render_program(prog), code)


class TestNodeMethods(unittest.TestCase):
    def test_block_empty(self):
        blk = Block([])
        self.assertTrue(blk.is_effectively_empty())

    def test_has_assignment_to(self):
        blk = Block(
            [
                Assignment(OpVar("a"), OpInt(1)),
            ]
        )
        self.assertTrue(blk.has_assignment_to("a"))
        self.assertFalse(blk.has_assignment_to("b"))

        inner = DoLoop(
            Block(
                [
                    Assignment(
                        OpVar("a", index=[OpVar("i"), OpVar("j")]),
                        OpVar("b", index=[OpVar("i"), OpVar("j")]) + OpVar("c"),
                    )
                ]
            ),
            index=OpVar("i"),
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        outer = DoLoop(
            Block([inner]), index=OpVar("j"), range=OpRange([OpInt(1), OpVar("m")])
        )
        self.assertTrue(outer.has_assignment_to("a"))
        self.assertFalse(outer.has_assignment_to("b"))
        self.assertFalse(outer.has_assignment_to("c"))

    def test_has_reference_to(self):
        inner = Block(
            [
                Assignment(OpVar("b"), OpVar("a")),
            ]
        )
        loop = DoLoop(inner, index=OpVar("i"), range=OpRange([OpInt(1), OpInt(10)]))
        outer = Block([loop])
        self.assertTrue(outer.has_reference_to("a"))
        self.assertFalse(outer.has_reference_to("c"))
        self.assertFalse(outer.has_reference_to("b"))

    def test_ids_and_clone(self):
        blk = Block([Assignment(OpVar("a"), OpInt(1))])
        for child in blk.iter_children():
            child_id = child.get_id()
            break
        clone = blk.deep_clone()
        for child in clone.iter_children():
            clone_child_id = child.get_id()
            break
        self.assertNotEqual(clone.get_id(), blk.get_id())
        self.assertNotEqual(clone_child_id, child_id)
        self.assertEqual(render_program(clone), render_program(blk))

    def test_find_and_remove(self):
        a = Assignment(OpVar("a"), OpInt(1))
        b = Assignment(OpVar("b"), OpInt(2))
        blk = Block([a, b])
        self.assertIs(blk.find_by_id(b.get_id()), b)
        blk.remove_by_id(a.get_id())
        self.assertEqual(len(blk), 1)
        child0 = None
        for child in blk.iter_children():
            child0 = child
            break
        self.assertIs(child0, b)

    def test_var_analysis(self):
        a = OpVar("a")
        b = OpVar("b")
        c = OpVar("c")
        cond1 = a > 0
        body1 = Block([Assignment(c, b)])
        cond2 = None
        body2 = Block([Assignment(b, c)])
        blk = Block(
            [
                Assignment(a, OpInt(1)),
                Assignment(b, a),
                IfBlock([(cond1, body1), (cond2, body2)]),
            ]
        )
        self.assertEqual({str(v) for v in blk.assigned_vars()}, {"a", "b", "c"})
        self.assertEqual({str(v) for v in blk.required_vars()}, {"c"})

        sub = Subroutine(
            "foo",
            "",
            decls=Block(
                [
                    Declaration(name="a", var_type=VarType("real"), intent="in"),
                    Declaration(name="b", var_type=VarType("real")),
                ]
            ),
            content=Block([Assignment(OpVar("b"), OpVar("a"))]),
        )
        self.assertEqual({str(v) for v in sub.assigned_vars()}, {"a", "b"})
        self.assertEqual({str(v) for v in sub.required_vars()}, set())

    def test_block_scope(self):
        decls = Block([Declaration(name="a", var_type=VarType("real"))])
        body = Block([Assignment(OpVar("b"), OpVar("a"))])
        blk = BlockConstruct(decls, body)
        self.assertFalse(blk.has_reference_to("a"))
        self.assertFalse(blk.has_assignment_to("a"))
        self.assertTrue(blk.has_assignment_to("b"))
        vars = VarList([OpVar("a")])
        vars = blk.required_vars(vars)
        self.assertEqual({str(v) for v in vars}, {"a"})
        vars = VarList([OpVar("a")])
        vars = blk.assigned_vars(vars)
        self.assertEqual({str(v) for v in vars}, {"a", "b"})

    def test_block_unrefered_advars(self):
        decls = Block([Declaration(name="a_ad", var_type=VarType("real"))])
        blk = BlockConstruct(decls, Block([]))
        vars = VarList([OpVar("b_ad")])
        vars = blk.unrefered_advars(vars)
        self.assertEqual({str(v) for v in vars}, {"b_ad"})

    def test_do_loop_list(self):
        a = Assignment(OpVar("a"), OpInt(0))
        blk = Block([a])
        self.assertEqual(a.do_index_list, [])
        doblk = DoLoop(blk, index=OpVar("i"), range=OpRange([OpInt(1), OpVar("n")]))
        self.assertEqual(a.do_index_list, ["i"])
        blk2 = Block([doblk])
        doblk = DoLoop(blk2, index=OpVar("j"), range=OpRange([OpInt(1), OpVar("m")]))
        self.assertEqual(a.do_index_list, ["i", "j"])
        b = Assignment(OpVar("b"), OpInt(0))
        blk.append(b)
        self.assertEqual(b.do_index_list, ["i", "j"])

    def test_prune_for(self):
        a = OpVar("a")
        b = OpVar("b")
        c = OpVar("c")
        blk = Block(
            [
                Assignment(a, OpInt(2)),
                Assignment(c, a),
                Assignment(a, OpInt(1)),
                Assignment(b, a),
            ]
        )
        pruned = blk.prune_for(VarList([b]))
        self.assertEqual(
            render_program(pruned),
            "a = 1\n" "b = a\n",
        )

    def test_prune_after_return(self):
        a = OpVar("a")
        b = OpVar("b")
        blk = Block(
            [
                Assignment(a, OpInt(1)),
                ReturnStmt(),
                Assignment(b, OpInt(2)),
            ]
        )
        pruned = blk.prune_for(VarList([a]))
        self.assertEqual(
            render_program(pruned),
            "a = 1\n" "return\n",
        )

    def test_prune_with_recurent_variable_in_loop(self):
        code = textwrap.dedent(
            """\
        do while (a < 10)
          count = count + 1
          a = a + 1
        end do
        """
        )
        loop = DoWhile(
            Block(
                [
                    Assignment(OpVar("count"), OpVar("count") + OpInt(1)),
                    Assignment(OpVar("a"), OpVar("a") + OpInt(1)),
                ]
            ),
            cond=OpVar("a") < OpInt(10),
        )
        pruned = loop.prune_for(VarList([OpVar("count")]))
        self.assertEqual(render_program(pruned), code)

    def test_prune_assignment_to_same_var(self):
        code = textwrap.dedent(
            """\
        do i = n, 2, - 1
          x(i) = x_save_1_ad(i)
          x_ad(i) = x_ad(i) * x(i)
        end do
        """
        )
        i = OpVar("i")
        n = OpVar("n")
        x = OpVar("x", index=[i])
        x_ad = OpVar("x_ad", index=[i])
        sa1 = SaveAssignment(x, id=1)
        sa2 = SaveAssignment(x, id=2)
        loop = DoLoop(
            Block(
                [
                    sa1.to_load(),
                    sa2,
                    Assignment(x, x * x),
                    sa2.to_load(),
                    Assignment(x_ad, x_ad * x),
                    sa1.to_load(),
                ]
            ),
            index=i,
            range=OpRange([n, OpInt(2), -OpInt(1)]),
        )
        pruned = loop.prune_for(VarList([OpVar("x_ad"), OpVar("x", index=[2])]))
        self.assertEqual(render_program(pruned), code)

    def test_conditional_return_resets_targets(self):
        a = OpVar("a")
        b = OpVar("b")
        cond = OpVar("flag")
        blk = Block(
            [
                IfBlock([(cond, Block([Assignment(b, OpInt(2)), ReturnStmt()]))]),
                Assignment(a, b),
            ]
        )
        pruned = blk.prune_for(VarList([a]))
        self.assertEqual(
            render_program(pruned),
            "if (flag) then\n" "  return\n" "end if\n" "a = b\n",
        )

    def test_assignment_accumulate(self):
        # detection without flag
        x = OpVar("x")
        x_da = OpVar("x_da")
        y = OpVar("y")
        a = Assignment(x_da, x_da + y)
        self.assertEqual({str(v) for v in a.required_vars()}, {"x_da", "y"})

        b = Assignment(x, x_da + y)
        z = OpVar("z")
        self.assertEqual(
            {str(v) for v in b.required_vars(VarList([z]))}, {"z", "x_da", "y"}
        )

        c = Assignment(x, x_da + y)
        self.assertEqual({str(v) for v in c.required_vars(VarList([x]))}, {"x_da", "y"})

        # explicit accumulate
        d = Assignment(x_da, y, accumulate=True)
        self.assertEqual(
            render_program(Block([d])),
            "x_da = y + x_da\n",
        )
        self.assertEqual({str(v) for v in b.required_vars()}, {"x_da", "y"})

    def test_required_vars_in_simple_loop(self):
        a = OpVar("a")
        b = OpVar("b")
        c = OpVar("c")
        assign = Assignment(a, b + c)
        self.assertEqual({str(v) for v in assign.required_vars()}, {"b", "c"})

        blk = Block([Assignment(a, c), Assignment(b, a)])
        self.assertEqual({str(v) for v in blk.required_vars()}, {"c"})

        i = OpVar("i")
        xa = OpVar("x", index=[OpRange([None])])
        xi = OpVar("x", index=[i])
        yi = OpVar("y", index=[i])
        blk = Block([Assignment(xa, OpInt(0)), Assignment(yi, xi)])
        self.assertEqual({str(v) for v in blk.required_vars()}, {"i"})

        n = OpVar("n")
        ya = OpVar("y", index=[OpRange([1, n])])
        blk = Block(
            [
                Assignment(xa, OpInt(0)),
                DoLoop(
                    Block([Assignment(yi, xi)]),
                    index=i,
                    range=OpRange([1, n]),
                ),
            ]
        )
        self.assertEqual({str(v) for v in blk.required_vars(VarList([ya]))}, {"n"})

    def test_required_vars_in_nested_loop(self):
        i = OpVar("i")
        j = OpVar("j")
        n = OpVar("n")
        m = OpVar("m")
        k = OpVar("k")
        a = OpVar("a")
        x = OpVar("x", index=[i])
        y = OpVar("y", index=[i])
        v = OpVar("v", index=[k])
        v1 = OpVar("v", index=[1])
        v2 = OpVar("v", index=[2])
        w = OpVar("w", index=[None])
        w1 = OpVar("w", index=[1])
        w2 = OpVar("w", index=[2])
        inner = DoLoop(
            Block([Assignment(v, k)]), index=k, range=OpRange([OpInt(1), OpInt(2)])
        )
        outer = DoLoop(
            Block(
                [
                    Assignment(a, i),
                    Assignment(w, x),
                    inner,
                    Assignment(y, v1 + v2 + w1 + w2 + a),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), n]),
        )
        self.assertEqual({str(v) for v in outer.required_vars()}, {"x(1:n)", "n"})

        x_ad = OpVar("x_ad", index=[i])
        y_ad = OpVar("y_ad", index=[i])
        v_ad = OpVar("v_ad", index=[k])
        v1_ad = OpVar("v_ad", index=[1])
        v2_ad = OpVar("v_ad", index=[2])
        w1_ad = OpVar("w_ad", index=[1])
        w2_ad = OpVar("w_ad", index=[2])
        save_assign1 = SaveAssignment(w1, id=1)
        save_assign2 = SaveAssignment(w2, id=1)
        inner = DoLoop(
            Block([Assignment(x_ad, v_ad, accumulate=True)]),
            index=k,
            range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
        )
        outer = DoLoop(
            Block(
                [
                    save_assign1,
                    save_assign2,
                    Assignment(v1_ad, y_ad, accumulate=True),
                    Assignment(v2_ad, y_ad, accumulate=True),
                    Assignment(w1_ad, y_ad, accumulate=True),
                    Assignment(w2_ad, y_ad, accumulate=True),
                    inner,
                    Assignment(x_ad, w1_ad + w2_ad, accumulate=True),
                    save_assign1.to_load(),
                    save_assign2.to_load(),
                ]
            ),
            index=i,
            range=OpRange([OpVar("n"), OpInt(1), OpInt(-1)]),
        )
        self.assertEqual(
            {
                str(v)
                for v in outer.required_vars(no_accumulate=True, without_savevar=True)
            },
            {"y_ad(1:n)", "w(1:2)", "n"},
        )

        a_ad = OpVar("a_ad", index=[i, j])
        b_ad = OpVar("b_ad", index=[i, j])
        one = OpInt(1)
        inner = DoLoop(
            Block(
                [
                    Assignment(a_ad, b_ad, accumulate=True),
                ]
            ),
            index=i,
            range=OpRange([n, one, -one]),
        )
        outer = DoLoop(Block([inner]), index=j, range=OpRange([m, one, -one]))
        self.assertEqual(
            {str(v) for v in outer.required_vars()},
            {"a_ad(1:n,1:m)", "b_ad(1:n,1:m)", "m", "n"},
        )

    def test_required_vars_loop_with_guarded_neighbors(self):
        i = OpVar("i", is_read_only=True)
        n = OpVar("n")
        one = OpInt(1)
        z = OpVar("z", index=[i], dims=(OpRange([one,n]),))
        x = OpVar("x", index=[i - one], dims=(OpRange([one,n]),))
        y = OpVar("y", index=[i + one], dims=(OpRange([one,n]),))
        loop = DoLoop(
            Block([
                Assignment(OpVar("z", index=[i]), OpReal("0.0")),
                IfBlock([(i >= OpInt(2), Block([Assignment(z, x + z)]))]),
                IfBlock([(i <= n - OpInt(1), Block([Assignment(z, y + z)]))]),
            ]),
            index=i,
            range=OpRange([OpInt(1), n]),
        )

        required = {str(v) for v in loop.required_vars()}
        self.assertEqual(
            set(required),
            {"n", "x(1:n - 1)", "y(2:n)"},
        )

    def test_required_vars_in_if(self):
        i = OpVar("i", is_read_only=True)
        a = OpVar("a")
        b = OpVar("b")
        c = OpVar("c")
        cond1 = i < 0
        block1 = Block([Assignment(a, c)])
        cond2 = i > 0
        block2 = Block([Assignment(a, b)])
        cond3 = None
        block3 = Block([Assignment(b, c)])
        ifblk = IfBlock([(cond1, block1), (cond2, block2), (cond3, block3)])
        self.assertEqual(
            {str(v) for v in ifblk.required_vars(VarList([a]))}, {"i", "c", "b", "a"}
        )

    def test_required_vars_guarded_assignment(self):
        a = OpVar("a", is_read_only=True)
        b = OpVar("b")
        c = OpVar("c")
        cond1 = a > OpInt(0)
        init_b = IfBlock([(cond1, Block([Assignment(b, OpReal("1.0"))]))])
        assign_c = Assignment(c, OpReal("1.0"))
        use_b = IfBlock([(cond1, Block([Assignment(c, c + b)]))])
        program = Block([init_b, assign_c, use_b])

        self.assertEqual({str(v) for v in program.required_vars()}, {"a"})

    def test_required_vars_multiple_guarded_assignment(self):
        a = OpVar("a", is_read_only=True)
        b = OpVar("b")
        c = OpVar("c")
        d = OpVar("d")
        cond1 = a > OpInt(0)
        cond2 = a < OpInt(0)
        init_b = IfBlock([(cond1, Block([Assignment(b, OpReal("1.0"))]))])
        init_d = IfBlock([(cond2, Block([Assignment(d, OpReal("1.0"))]))])
        assign_c = Assignment(c, OpReal("1.0"))
        use_b = IfBlock([(cond1, Block([Assignment(c, c + b)]))])
        use_d = IfBlock([(cond2, Block([Assignment(c, c + d)]))])
        program = Block([init_b, init_d, assign_c, use_b, use_d])

        self.assertEqual({str(v) for v in program.required_vars()}, {"a"})

    def test_required_vars_guard_mismatch(self):
        a = OpVar("a", is_read_only=True)
        b = OpVar("b")
        c = OpVar("c")
        cond_assign = a > OpInt(0)
        cond_use_same = a > OpInt(0)
        cond_use_diff = a > OpInt(-1)
        init_b = IfBlock([(cond_assign, Block([Assignment(b, OpReal("1.0"))]))])
        assign_c = Assignment(c, OpReal("1.0"))
        use_same = IfBlock([(cond_use_same, Block([Assignment(c, c + b)]))])
        use_diff = IfBlock([(cond_use_diff, Block([Assignment(c, c * b)]))])
        program = Block([init_b, assign_c, use_same, use_diff])
        self.assertEqual({str(v) for v in program.required_vars()}, {"a", "b"})

    def test_required_vars_guarded_array_components(self):
        i = OpVar("i", is_read_only=True)
        istart = OpVar("istart", is_read_only=True)
        iend = OpVar("iend", is_read_only=True)
        cond = (i >= istart) & (i <= iend)
        flux1 = OpVar("flux", index=[OpInt(1)])
        flux2 = OpVar("flux", index=[OpInt(2)])
        dhdt = OpVar("dhdt", index=[i])
        assign1 = Assignment(flux1, dhdt)
        assign2 = Assignment(flux2, dhdt)
        read1 = Assignment(OpVar("u", index=[i]), flux1)
        read2 = Assignment(OpVar("h", index=[i]), flux2)
        program = Block(
            [
                IfBlock([(cond, Block([assign1]))]),
                IfBlock([(cond, Block([assign2]))]),
                IfBlock([(cond, Block([read1]))]),
                IfBlock([(cond, Block([read2]))]),
            ]
        )
        self.assertEqual(
            {str(v) for v in program.required_vars()}, {"dhdt(i)", "i", "istart", "iend"}
        )

    def test_required_vars_guarded_array_components_with_dims(self):
        i = OpVar("i", is_read_only=True)
        istart = OpVar("istart", is_read_only=True)
        iend = OpVar("iend", is_read_only=True)
        cond = (i >= istart) & (i <= iend)

        flux_dims = (OpRange([OpInt(1), OpInt(2)]),)
        flux1 = OpVar(
            "flux",
            index=[OpInt(1)],
            dims=flux_dims,
            var_type=VarType("real"),
        )
        flux2 = OpVar(
            "flux",
            index=[OpInt(2)],
            dims=flux_dims,
            var_type=VarType("real"),
        )
        dhdt = OpVar("dhdt", index=[i])
        assign1 = Assignment(flux1, dhdt)
        assign2 = Assignment(flux2, dhdt)
        read1 = Assignment(OpVar("u", index=[i]), flux1)
        read2 = Assignment(OpVar("h", index=[i]), flux2)
        program = Block(
            [
                IfBlock([(cond, Block([assign1]))]),
                IfBlock([(cond, Block([assign2]))]),
                IfBlock([(cond, Block([read1]))]),
                IfBlock([(cond, Block([read2]))]),
            ]
        )
        self.assertEqual(
            {str(v) for v in program.required_vars()}, {"dhdt(i)", "i", "istart", "iend"}
        )

    def test_check_initial_simple_accumulate_block(self):
        code = textwrap.dedent(
            """\
        a_ad = b_ad
        a_ad = c_ad + a_ad
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        c_ad = OpVar("c_ad")
        blk = Block(
            [
                Assignment(a_ad, b_ad, accumulate=True),
                Assignment(a_ad, c_ad, accumulate=True),
            ]
        )
        blk.check_initial()
        self.assertEqual(render_program(blk), code)

    def test_check_initial_loop_two_accumulates(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          a_ad = b_ad + a_ad
          a_ad = c_ad + a_ad
        end do
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        c_ad = OpVar("c_ad")
        i = OpVar("i")
        n = OpVar("n")
        loop = DoLoop(
            Block(
                [
                    Assignment(a_ad, b_ad, accumulate=True),
                    Assignment(a_ad, c_ad, accumulate=True),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), n]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_accum_and_clear(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          a_ad = b_ad
          b_ad = a_ad + b_ad
          a_ad = 0.0
        end do
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        i = OpVar("i")
        n = OpVar("n")
        loop = DoLoop(
            Block(
                [
                    Assignment(a_ad, b_ad, accumulate=True),
                    Assignment(b_ad, a_ad, accumulate=True),
                    ClearAssignment(a_ad),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), n]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_mutual_accumulates(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          a_ad = b_ad + a_ad
          b_ad = 0.0
          b_ad = a_ad
          a_ad = 0.0
          a_ad = b_ad
          b_ad = 0.0
        end do
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        i = OpVar("i")
        n = OpVar("n")
        loop = DoLoop(
            Block(
                [
                    Assignment(a_ad, b_ad, accumulate=True),
                    ClearAssignment(b_ad),
                    Assignment(b_ad, a_ad, accumulate=True),
                    ClearAssignment(a_ad),
                    Assignment(a_ad, b_ad, accumulate=True),
                    ClearAssignment(b_ad),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), n]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_prunes_self_assign(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          a_ad = b_ad + a_ad
        end do
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        loop = DoLoop(
            Block([Assignment(a_ad, a_ad), Assignment(a_ad, b_ad, accumulate=True)]),
            index=OpVar("i"),
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_array_loop_with_clear(self):
        code = textwrap.dedent(
            """\
        y_ad(:) = 0.0
        do i = 1, n
          x_ad(i) = y_ad(i)
          y_ad(i) = c_ad + y_ad(i)
          x_ad(i) = c_ad + x_ad(i)
          x_ad(i) = 0.0
        end do
        """
        )
        i = OpVar("i")
        n = OpVar("n")
        c_ad = OpVar("c_ad")
        x_ad = OpVar("x_ad", index=[i])
        y_ad = OpVar("y_ad", index=[i])
        body = Block(
            [
                Assignment(OpVar("y_ad", index=[None]), OpReal("0.0")),
                DoLoop(
                    Block(
                        [
                            Assignment(x_ad, y_ad, accumulate=True),
                            Assignment(y_ad, c_ad, accumulate=True),
                            Assignment(x_ad, c_ad, accumulate=True),
                            ClearAssignment(x_ad),
                        ]
                    ),
                    index=i,
                    range=OpRange([OpInt(1), n]),
                ),
            ]
        )
        body.check_initial()
        self.assertEqual(render_program(body), code)

    def test_check_initial_loop_two_index_targets(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          x_ad(i) = y_ad(i) + x_ad(i)
          x_ad(ip) = y_ad(i) + x_ad(ip)
        end do
        """
        )
        i = OpVar("i")
        n = OpVar("n")
        ip = OpVar("ip")
        y_ad = OpVar("y_ad", index=[i])
        xi_ad = OpVar("x_ad", index=[i])
        xip_ad = OpVar("x_ad", index=[ip])
        loop = DoLoop(
            Block(
                [
                    Assignment(xi_ad, y_ad, accumulate=True),
                    Assignment(xip_ad, y_ad, accumulate=True),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), n]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_neighbor_accumulate_true(self):
        # Ensure accumulate remains True for neighbor-updates after check_initial
        code = textwrap.dedent(
            """\
        do i = n - 2, 2, - 1
          u_ad(i) = du_ad(i) * (u(i - 1) * u(i + 1)) + u_ad(i)
          u_ad(i - 1) = du_ad(i) * u(i) + u_ad(i - 1)
          u_ad(i + 1) = du_ad(i) * u(i) + u_ad(i + 1)
        end do
        """
        )
        i = OpVar("i")
        n = OpVar("n")
        u_i = OpVar("u", index=[i])
        u_im1 = OpVar("u", index=[i - OpInt(1)])
        u_ip1 = OpVar("u", index=[i + OpInt(1)])
        u_ad_i = OpVar("u_ad", index=[i])
        u_ad_im1 = OpVar("u_ad", index=[i - OpInt(1)])
        u_ad_ip1 = OpVar("u_ad", index=[i + OpInt(1)])
        du_ad_i = OpVar("du_ad", index=[i])
        a1 = Assignment(u_ad_i, du_ad_i * (u_im1 + u_ip1), accumulate=True)
        a2 = Assignment(u_ad_im1, du_ad_i * u_i, accumulate=True)
        a3 = Assignment(u_ad_ip1, du_ad_i * u_i, accumulate=True)
        loop = DoLoop(
            Block([a1, a2, a3]),
            index=i,
            range=OpRange([n - OpInt(1), OpInt(2), -OpInt(1)]),
        )
        loop.check_initial()
        self.assertTrue(a1.accumulate)
        self.assertTrue(a2.accumulate)
        self.assertTrue(a3.accumulate)

    def test_check_initial_nested_loops_scalar_and_array_clears(self):
        code = textwrap.dedent(
            """\
        do j = 1, m
          do i = 1, n
            x_ad(i,j) = y_ad(i,j)
            x_ad(i,j) = 0.0
            c_ad = y_ad(i,j)
            c_ad = 0.0
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        n = OpVar("n")
        m = OpVar("m")
        index = [i, j]
        x_ad = OpVar("x_ad", index=index)
        y_ad = OpVar("y_ad", index=index)
        c_ad = OpVar("c_ad")
        inner = DoLoop(
            Block(
                [
                    Assignment(x_ad, y_ad, accumulate=True),
                    ClearAssignment(x_ad),
                    Assignment(c_ad, y_ad, accumulate=True),
                    ClearAssignment(c_ad),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), n]),
        )
        outer = DoLoop(Block([inner]), index=j, range=OpRange([OpInt(1), m]))
        outer.check_initial()
        self.assertEqual(render_program(outer), code)

    def test_check_initial_if_same_target(self):
        code = textwrap.dedent(
            """\
        if (a > 0) then
          a_ad = b_ad
        else
          a_ad = c_ad
        end if
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        c_ad = OpVar("c_ad")
        cond1 = OpVar("a") > 0
        body1 = Block([Assignment(a_ad, b_ad, accumulate=True)])
        cond2 = None
        body2 = Block([Assignment(a_ad, c_ad, accumulate=True)])
        cond_blk = Block([IfBlock([(cond1, body1), (cond2, body2)])])
        cond_blk.check_initial()
        self.assertEqual(render_program(cond_blk), code)

    def test_check_initial_if_different_target(self):
        code = textwrap.dedent(
            """\
        if (a > 0) then
          a_ad = b_ad
        else
          c_ad = b_ad
        end if
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        c_ad = OpVar("c_ad")
        cond1 = OpVar("a") > 0
        body1 = Block([Assignment(a_ad, b_ad, accumulate=True)])
        cond2 = None
        body2 = Block([Assignment(c_ad, b_ad, accumulate=True)])
        cond_blk2 = Block([IfBlock([(cond1, body1), (cond2, body2)])])
        cond_blk2.check_initial()
        self.assertEqual(render_program(cond_blk2), code)

    def test_check_initial_recurent_var_in_while(self):
        code = textwrap.dedent(
            """\
        do while (a > 0)
          a_ad = b_ad * b + a_ad
        end do
        """
        )
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        b = OpVar("b")
        cond = OpVar("a") > 0
        body = Block([Assignment(a_ad, b_ad * b, accumulate=True)])
        dowhile = DoWhile(body, cond)
        dowhile.check_initial()
        self.assertEqual(render_program(dowhile), code)

    def test_check_initial_loop_private_1d_pattern1_nonaccumulate(self):
        code = textwrap.dedent(
            """\
        do i = n, 1, - 1
          work_ad(2) = z_ad * 2.0
          work_ad(1) = z_ad * 1.0
          do k = 2, 1, - 1
            work_ad(k) = z_ad + work_ad(k)
            var_ad(k,i) = work_ad(k)
            work_ad(k) = 0.0
          end do
        end do
        """
        )
        i = OpVar("i")
        k = OpVar("k")
        var_ad = OpVar("var_ad", index=[k, i])
        work_ad = OpVar("work_ad", index=[k])
        work_ad1 = OpVar("work_ad", index=[1])
        work_ad2 = OpVar("work_ad", index=[2])
        z_ad = OpVar("z_ad")
        loop = DoLoop(
            Block(
                [
                    Assignment(work_ad2, z_ad * OpReal("2.0"), accumulate=True),
                    Assignment(work_ad1, z_ad * OpReal("1.0"), accumulate=True),
                    DoLoop(
                        Block(
                            [
                                Assignment(work_ad, z_ad, accumulate=True),
                                Assignment(var_ad, work_ad, accumulate=True),
                                ClearAssignment(work_ad),
                            ]
                        ),
                        index=k,
                        range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                    ),
                ]
            ),
            index=i,
            range=OpRange([OpVar("n"), OpInt(1), OpInt(-1)]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_private_1d_pattern2_nonaccumulate(self):
        code = textwrap.dedent(
            """\
        do i = n, 1, - 1
          do k = 2, 1, - 1
            work_ad(k) = z_ad * k
          end do
          do k = 2, 1, - 1
            work_ad(k) = z_ad + work_ad(k)
            var_ad(k,i) = work_ad(k)
            work_ad(k) = 0.0
          end do
        end do
        """
        )
        i = OpVar("i")
        k = OpVar("k")
        var_ad = OpVar("var_ad", index=[k, i])
        work_ad = OpVar("work_ad", index=[k])
        z_ad = OpVar("z_ad")
        loop = DoLoop(
            Block(
                [
                    DoLoop(
                        Block([Assignment(work_ad, z_ad * k, accumulate=True)]),
                        index=k,
                        range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                    ),
                    DoLoop(
                        Block(
                            [
                                Assignment(work_ad, z_ad, accumulate=True),
                                Assignment(var_ad, work_ad, accumulate=True),
                                ClearAssignment(work_ad),
                            ]
                        ),
                        index=k,
                        range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                    ),
                ]
            ),
            index=i,
            range=OpRange([OpVar("n"), OpInt(1), OpInt(-1)]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_private_1d_pattern3_nonaccumulate(self):
        code = textwrap.dedent(
            """\
        do i = n, 1, - 1
          do k = 2, 1, - 1
            work_ad(k) = z_ad * k
          end do
          var_ad(k,i) = sum(work_ad(:))
          work_ad(:) = 0.0
        end do
        """
        )
        i = OpVar("i")
        k = OpVar("k")
        var_ad = OpVar("var_ad", index=[k, i])
        work_ad = OpVar("work_ad", index=[k])
        work_ad1 = OpVar("work_ad", index=[1])
        work_ad2 = OpVar("work_ad", index=[2])
        work_ada = OpVar("work_ad", index=[None])
        z_ad = OpVar("z_ad")
        loop = DoLoop(
            Block(
                [
                    DoLoop(
                        Block([Assignment(work_ad, z_ad * k, accumulate=True)]),
                        index=k,
                        range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                    ),
                    Assignment(var_ad, OpFunc("sum", [work_ada]), accumulate=True),
                    ClearAssignment(work_ada),
                ]
            ),
            index=i,
            range=OpRange([OpVar("n"), OpInt(1), OpInt(-1)]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_private_2d_pattern1_nonaccumulate(self):
        code = textwrap.dedent(
            """\
        do j = m, 1, - 1
          do i = n, 1, - 1
            work_ad(2,i) = z_ad * 2.0
            work_ad(1,i) = z_ad * 1.0
          end do
          do i = n, 1, - 1
            var_ad(i,j) = sum(work_ad(:,i))
            work_ad(:,i) = 0.0
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        n = OpVar("n")
        m = OpVar("m")
        work_ad1 = OpVar("work_ad", index=[1, i])
        work_ad2 = OpVar("work_ad", index=[2, i])
        work_ada = OpVar("work_ad", index=[None, i])
        var_ad = OpVar("var_ad", index=[i, j])
        z_ad = OpVar("z_ad")
        loop = DoLoop(
            Block(
                [
                    DoLoop(
                        Block(
                            [
                                Assignment(
                                    work_ad2, z_ad * OpReal("2.0"), accumulate=True
                                ),
                                Assignment(
                                    work_ad1, z_ad * OpReal("1.0"), accumulate=True
                                ),
                            ]
                        ),
                        index=i,
                        range=OpRange([n, OpInt(1), OpInt(-1)]),
                    ),
                    DoLoop(
                        Block(
                            [
                                Assignment(
                                    var_ad, OpFunc("sum", [work_ada]), accumulate=True
                                ),
                                ClearAssignment(work_ada),
                            ]
                        ),
                        index=i,
                        range=OpRange([n, OpInt(1), OpInt(-1)]),
                    ),
                ]
            ),
            index=j,
            range=OpRange([m, OpInt(1), OpInt(-1)]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_private_2d_pattern2_nonaccumulate(self):
        code = textwrap.dedent(
            """\
        do j = m, 1, - 1
          do i = n, 1, - 1
            do k = 2, 1, - 1
              work_ad(k,i) = z_ad * k
            end do
          end do
          do i = n, 1, - 1
            var_ad(i,j) = sum(work_ad(:,i))
            work_ad(:,i) = 0.0
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        n = OpVar("n")
        m = OpVar("m")
        work_ad = OpVar("work_ad", index=[k, i])
        work_ad1 = OpVar("work_ad", index=[1, i])
        work_ad2 = OpVar("work_ad", index=[2, i])
        work_ada = OpVar("work_ad", index=[None, i])
        var_ad = OpVar("var_ad", index=[i, j])
        z_ad = OpVar("z_ad")
        loop = DoLoop(
            Block(
                [
                    DoLoop(
                        Block(
                            [
                                DoLoop(
                                    Block(
                                        [Assignment(work_ad, z_ad * k, accumulate=True)]
                                    ),
                                    index=k,
                                    range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                                )
                            ]
                        ),
                        index=i,
                        range=OpRange([n, OpInt(1), OpInt(-1)]),
                    ),
                    DoLoop(
                        Block(
                            [
                                Assignment(
                                    var_ad, OpFunc("sum", [work_ada]), accumulate=True
                                ),
                                ClearAssignment(work_ada),
                            ]
                        ),
                        index=i,
                        range=OpRange([n, OpInt(1), OpInt(-1)]),
                    ),
                ]
            ),
            index=j,
            range=OpRange([m, OpInt(1), OpInt(-1)]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_check_initial_loop_private_2d_pattern3_nonaccumulate(self):
        code = textwrap.dedent(
            """\
        do j = m, 1, - 1
          do i = n, 1, - 1
            do k = 2, 1, - 1
              work_ad(k,i) = z_ad * k
            end do
          end do
          do i = n, 1, - 1
            do k = 2, 1, - 1
              var_ad(i,j) = work_ad(k,i) + var_ad(i,j)
              work_ad(k,i) = 0.0
            end do
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        n = OpVar("n")
        m = OpVar("m")
        work_ad = OpVar("work_ad", index=[k, i])
        var_ad = OpVar("var_ad", index=[i, j])
        z_ad = OpVar("z_ad")
        loop = DoLoop(
            Block(
                [
                    DoLoop(
                        Block(
                            [
                                DoLoop(
                                    Block(
                                        [Assignment(work_ad, z_ad * k, accumulate=True)]
                                    ),
                                    index=k,
                                    range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                                )
                            ]
                        ),
                        index=i,
                        range=OpRange([n, OpInt(1), OpInt(-1)]),
                    ),
                    DoLoop(
                        Block(
                            [
                                DoLoop(
                                    Block(
                                        [
                                            Assignment(
                                                var_ad, work_ad, accumulate=True
                                            ),
                                            ClearAssignment(work_ad),
                                        ]
                                    ),
                                    index=k,
                                    range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                                )
                            ]
                        ),
                        index=i,
                        range=OpRange([n, OpInt(1), OpInt(-1)]),
                    ),
                ]
            ),
            index=j,
            range=OpRange([m, OpInt(1), OpInt(-1)]),
        )
        loop.check_initial()
        self.assertEqual(render_program(loop), code)


class TestPushPop(unittest.TestCase):
    def test_push(self):
        var = OpVar("a")
        node = PushPop(var, 100)
        self.assertEqual(
            render_program(Block([node])), "call fautodiff_stack_push_r(a)\n"
        )
        self.assertEqual({str(v) for v in node.iter_ref_vars()}, {"a"})

    def test_pop(self):
        var = OpVar("a")
        node = PushPop(var, 100).to_load()
        self.assertEqual(
            render_program(Block([node])), "call fautodiff_stack_pop_r(a)\n"
        )
        self.assertEqual({str(v) for v in node.iter_assign_vars()}, {"a"})


class TestCallStatement(unittest.TestCase):
    def test_render_and_vars(self):
        a = OpVar("a")
        b = OpVar("b")
        node = CallStatement("foo", [a, b])
        self.assertEqual(render_program(Block([node])), "call foo(a, b)\n")
        self.assertEqual({str(v) for v in node.iter_ref_vars()}, {"a", "b"})
        self.assertEqual({str(v) for v in node.iter_assign_vars()}, {"a", "b"})

    def test_intent(self):
        a = OpVar("a")
        b = OpVar("b")
        node = CallStatement("foo", [a, b], intents=["in", "out"])
        self.assertEqual(render_program(Block([node])), "call foo(a, b)\n")
        self.assertEqual({str(v) for v in node.iter_ref_vars()}, {"a"})
        self.assertEqual({str(v) for v in node.iter_assign_vars()}, {"b"})

    def test_keyword_render(self):
        node = CallStatement("foo", [OpInt(1), OpInt(2)], arg_keys=["a", "b"])
        self.assertEqual(render_program(Block([node])), "call foo(a=1, b=2)\n")

    def test_required_vars_explicit_shape_pointer(self):
        m = OpVar("m")
        i = OpVar("i")
        actual = OpVar("a", index=[i])
        node = CallStatement("foo", [m, actual], intents=["in", "inout"])
        node.arg_info = {
            "args": ["n", "a"],
            "dims": [None, ("n",)],
            "intents": ["in", "inout"],
        }
        required = {str(v) for v in node.required_vars()}
        self.assertIn("a(i:i + m - 1)", required)


class TestRenderWrapping(unittest.TestCase):
    def test_assignment_alignment(self):
        terms = [f"term{i}" for i in range(20)]
        body = "result = " + " + ".join(terms)
        stmt = Statement(body)
        code = render_program(stmt, indent=1)
        lines = code.splitlines()
        self.assertGreater(len(lines), 1)
        for idx, line in enumerate(lines):
            self.assertLessEqual(len(line), 132)
            if idx < len(lines) - 1:
                self.assertTrue(line.endswith("&"))
            else:
                self.assertFalse(line.endswith("&"))

        first = lines[0]
        eq_idx = first.index("=")
        rhs_start = eq_idx + 1
        while rhs_start < len(first) and first[rhs_start] == " ":
            rhs_start += 1

        expected_single_line = "  " + body
        reconstructed = first.rstrip("&").rstrip()
        for cont in lines[1:]:
            indent_width = len(cont) - len(cont.lstrip(" "))
            self.assertEqual(indent_width, rhs_start)
            reconstructed += " " + cont.lstrip()
        self.assertEqual(reconstructed, expected_single_line)

    def test_non_assignment_uses_block_indent(self):
        args = [f"arg{i}" for i in range(20)]
        body = "call foo(" + ", ".join(args) + ")"
        stmt = Statement(body)
        code = render_program(stmt, indent=1)
        lines = code.splitlines()
        self.assertGreater(len(lines), 1)
        for idx, line in enumerate(lines):
            self.assertLessEqual(len(line), 132)
            if idx < len(lines) - 1:
                self.assertTrue(line.endswith("&"))
            else:
                self.assertFalse(line.endswith("&"))

        reconstructed = lines[0].rstrip("&").rstrip()
        for cont in lines[1:]:
            indent_width = len(cont) - len(cont.lstrip(" "))
            self.assertEqual(indent_width, 2)
            reconstructed += " " + cont.lstrip()
        self.assertEqual(reconstructed, "  " + body)

    def test_inline_comment_wrap_drops_ampersand(self):
        args = [f"arg{i}" for i in range(10)]
        comment = "! " + "comment text " * 12
        body = "call foo(" + ", ".join(args) + ") " + comment
        stmt = Statement(body)
        lines = render_program(stmt, indent=1).splitlines()
        self.assertGreater(len(lines), 1)
        self.assertFalse(lines[0].endswith("&"))
        for line in lines[1:]:
            stripped = line.lstrip()
            self.assertTrue(stripped.startswith("!"))
            self.assertLessEqual(len(line), 132)

    def test_inline_comment_with_existing_continuation_marker(self):
        stmt = Statement("foo & ! comment")
        lines = render_program(stmt, indent=1).splitlines()
        self.assertEqual(lines[0], "  foo &")
        self.assertTrue(lines[1].lstrip().startswith("!"))

    def test_comment_only_wraps_with_bang_prefix(self):
        comment = "!" + " long-comment" * 15
        stmt = Statement(comment)
        lines = render_program(stmt, indent=1).splitlines()
        self.assertGreater(len(lines), 1)
        for line in lines:
            stripped = line.lstrip()
            self.assertTrue(stripped.startswith("!"))
            self.assertLessEqual(len(line), 132)

    def test_inline_comment_preserves_continuation_for_followup_line(self):
        block = Block(
            [
                Statement("call foo(a, b) & ! comment"),
                Statement("& baz"),
            ]
        )
        lines = render_program(block, indent=1).splitlines()
        self.assertEqual(lines[0], "  call foo(a, b) &")
        self.assertTrue(lines[1].lstrip().startswith("!"))
        self.assertEqual(lines[2], "  & baz")

    def test_openmp_directive_wrap_keeps_continuation(self):
        directive = "!$omp parallel do " + "private(x, y, z, very_long_name) " * 6
        stmt = Statement(directive)
        lines = render_program(stmt, indent=1).splitlines()

        self.assertGreater(len(lines), 1)
        for idx, line in enumerate(lines):
            self.assertLessEqual(len(line), 132)
            if idx < len(lines) - 1:
                self.assertTrue(line.endswith("&"))
            else:
                self.assertFalse(line.endswith("&"))

        first = lines[0]
        self.assertTrue(first.strip().lower().startswith("!$omp"))
        for continuation in lines[1:]:
            stripped = continuation.lstrip()
            self.assertTrue(stripped.lower().startswith("!$omp&"))

    def test_wrap_prefers_low_precedence_breaks(self):
        terms = [f"very_long_variable_{idx} * other_operand_{idx}" for idx in range(8)]
        body = "res = " + " + ".join(terms)
        stmt = Statement(body)
        lines = render_program(stmt, indent=1).splitlines()
        self.assertGreater(len(lines), 1)
        for line in lines[1:]:
            self.assertTrue(line.lstrip().startswith("+"))
        reconstructed = lines[0].rstrip("&").rstrip()
        for cont in lines[1:]:
            stripped = cont.lstrip()
            if stripped.endswith("&"):
                stripped = stripped[:-1].rstrip()
            reconstructed += " " + stripped
        self.assertEqual(reconstructed, "  " + body)


class TestLoopAnalysis(unittest.TestCase):
    def test_simple_loop(self):
        i = OpVar("i")
        body = Block([Assignment(OpVar("a", index=[i]), OpVar("b", index=[i]))])
        loop = DoLoop(body, index=i, range=OpRange([OpInt(1), OpVar("n")]))
        self.assertEqual({str(v) for v in loop.required_vars()}, {"b(1:n)", "n"})
        self.assertEqual(set(loop.recurrent_vars()), set())

    def test_loop_with_accumulate(self):
        i = OpVar("i")
        a_ad = OpVar("a_ad", index=[i])
        b_ad = OpVar("b_ad", index=[i])
        c = OpVar("c")
        body = Block(
            [Assignment(a_ad, c, accumulate=True), Assignment(b_ad, c, accumulate=True)]
        )
        loop = DoLoop(body, index=i, range=OpRange([OpInt(1), OpVar("n")]))
        self.assertEqual(
            {str(v) for v in loop.required_vars()}, {"a_ad(1:n)", "b_ad(1:n)", "c", "n"}
        )
        self.assertEqual(
            {str(v) for v in loop.required_vars(no_accumulate=True)}, {"c", "n"}
        )
        self.assertEqual(set(loop.recurrent_vars()), set())

    def test_self_reference_loop(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        loop = DoLoop(
            Block([Assignment(a, a + OpVar("c"))]),
            index=i,
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a(1:n)", "c", "n"})
        self.assertEqual(set(loop.recurrent_vars()), set())

    def test_no_recurrent_loop_with_scalar(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        c = OpVar("c")
        body = Block(
            [
                Assignment(c, a + i),
                Assignment(a, c + OpInt(1)),
            ]
        )
        loop = DoLoop(body, index=i, range=OpRange([OpInt(1), OpVar("n")]))
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a(1:n)", "n"})
        self.assertEqual(set(loop.recurrent_vars()), set())

    def test_recurrent_loop_with_scalar(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        c = OpVar("c")
        body = Block(
            [
                Assignment(a, c + OpInt(1)),
                Assignment(c, a + i),
            ]
        )
        loop = DoLoop(body, index=i, range=OpRange([OpInt(1), OpVar("n")]))
        self.assertEqual({str(v) for v in loop.required_vars()}, {"c", "n"})
        self.assertEqual(set(loop.recurrent_vars()), {"c"})


class TestRemoveRedundantAllocates(unittest.TestCase):
    def _decl_alloc_real(self, name: str) -> Declaration:
        return Declaration(
            name=name, var_type=VarType("real"), allocatable=True, dims=(None,)
        )

    def test_remove_alloc_dealloc_without_access(self):
        # allocate(a); deallocate(a) with no access -> both removed
        decls = Block([self._decl_alloc_real("a")])
        body = Block([Allocate([OpVar("a")]), Deallocate([OpVar("a")])])
        sub = Subroutine("foo", [], decls=decls, content=body)
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        self.assertNotIn("allocate(a)", code)
        self.assertNotIn("deallocate(a)", code)

    def test_keep_first_alloc_on_access_drop_dealloc_and_second_alloc(self):
        # allocate(a); use a; deallocate(a); allocate(a) -> drop deallocate and 2nd allocate
        decls = Block(
            [
                self._decl_alloc_real("a"),
                Declaration(name="x", var_type=VarType("real")),
            ]
        )
        body = Block(
            [
                Allocate([OpVar("a")]),
                Assignment(OpVar("x"), OpVar("a")),  # access a
                Deallocate([OpVar("a")]),
                Allocate([OpVar("a")]),
            ]
        )
        sub = Subroutine("foo", [], decls=decls, content=body)
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        # first allocate remains; deallocate and second allocate removed
        self.assertIn("allocate(a)\n", code)
        self.assertEqual(code.count("allocate(a)\n"), 1)
        self.assertNotIn("deallocate(a)", code)

    def test_drop_first_pair_without_access_keep_second_alloc(self):
        # allocate(a); deallocate(a); allocate(a) and no access before dealloc -> drop first pair
        decls = Block([self._decl_alloc_real("a")])
        body = Block(
            [
                Allocate([OpVar("a")]),
                Deallocate([OpVar("a")]),
                Allocate([OpVar("a")]),
            ]
        )
        sub = Subroutine("foo", [], decls=decls, content=body)
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        self.assertIn("allocate(a)\n", code)
        self.assertEqual(code.count("allocate(a)\n"), 1)
        self.assertNotIn("deallocate(a)", code)

    def test_loop_context_is_separate(self):
        # Outer allocate/deallocate without access should be removed; loop alloc/use/dealloc remains
        i = OpVar("i")
        decls = Block(
            [
                self._decl_alloc_real("a"),
                Declaration(name="x", var_type=VarType("real")),
                Declaration(name="n", var_type=VarType("integer")),
            ]
        )
        loop_body = Block(
            [
                Allocate([OpVar("a")]),
                Assignment(OpVar("x"), OpVar("a")),
                Deallocate([OpVar("a")]),
            ]
        )
        body = Block(
            [
                Allocate([OpVar("a")]),
                Deallocate([OpVar("a")]),  # no access -> removed
                DoLoop(loop_body, index=i, range=OpRange([OpInt(1), OpVar("n")])),
            ]
        )
        sub = Subroutine("foo", [], decls=decls, content=body)
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        # Outer pair removed; only the loop's allocate/deallocate remain
        alloc_lines = sum(
            1 for ln in code.splitlines(True) if ln.strip() == "allocate(a)"
        )
        dealloc_lines = sum(
            1 for ln in code.splitlines(True) if ln.strip() == "deallocate(a)"
        )
        self.assertEqual(alloc_lines, 1)
        self.assertEqual(dealloc_lines, 1)

    def test_allocate_then_conditional_return_then_use(self):
        # allocate; if (...) return (with inserted deallocate); later use and final deallocate
        # The initial allocate must NOT be removed.
        from fautodiff.code_tree import IfBlock, ReturnStmt

        decls = Block(
            [
                self._decl_alloc_real("a"),
                Declaration(name="n", var_type=VarType("integer")),
                Declaration(name="x", var_type=VarType("real")),
            ]
        )

        # Body: allocate(a); if (n<=0) then res=0; deallocate(a); return; end if; x=a(1); deallocate(a)
        cond = OpVar("n") <= OpInt(0)
        if_body = Block(
            [
                Assignment(OpVar("x"), OpReal("0.0")),
                Deallocate([OpVar("a")]),
                ReturnStmt(),
            ]
        )
        body = Block(
            [
                Allocate([OpVar("a")]),
                IfBlock([(cond, if_body)]),
                Assignment(OpVar("x"), OpVar("a", index=[OpInt(1)])),
                Deallocate([OpVar("a")]),
            ]
        )

        sub = Subroutine("foo", [], decls=decls, content=body)
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        # Must still contain the first allocate and the final deallocate
        self.assertIn("allocate(a)\n", code)
        self.assertIn("deallocate(a)", code)

    def test_if_branch_alloc_then_later_alloc_kept(self):
        # if (flag) allocate(a); end if; allocate(a) -> keep the post-branch allocate
        from fautodiff.code_tree import IfBlock

        decls = Block(
            [
                self._decl_alloc_real("a"),
                Declaration(name="flag", var_type=VarType("logical")),
            ]
        )
        ifblk = IfBlock([(OpVar("flag"), Block([Allocate([OpVar("a")])]))])
        body = Block([ifblk, Allocate([OpVar("a")])])
        sub = Subroutine("foo", [], decls=decls, content=body)
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        # both allocates should remain (one inside branch, one after)
        alloc_lines = sum(
            1 for ln in code.splitlines(True) if ln.strip() == "allocate(a)"
        )
        self.assertEqual(alloc_lines, 2)

    def test_if_branch_deallocate_not_pair_with_outer_allocate(self):
        # allocate(a); if (flag) deallocate(a); x=a(1); deallocate(a)
        # outer allocate must remain, and final deallocate remain
        from fautodiff.code_tree import IfBlock

        decls = Block(
            [
                self._decl_alloc_real("a"),
                Declaration(name="flag", var_type=VarType("logical")),
                Declaration(name="x", var_type=VarType("real")),
            ]
        )
        ifblk = IfBlock([(OpVar("flag"), Block([Deallocate([OpVar("a")])]))])
        body = Block(
            [
                Allocate([OpVar("a")]),
                ifblk,
                Assignment(OpVar("x"), OpVar("a", index=[OpInt(1)])),
                Deallocate([OpVar("a")]),
            ]
        )
        sub = Subroutine("foo", [], decls=decls, content=body)
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        self.assertIn("allocate(a)\n", code)
        self.assertIn("deallocate(a)", code)

    def test_then_else_cross_pair_not_removed(self):
        # if (flag) then allocate(a) else deallocate(a) end if
        # Cross-branch pair must not cancel each other.
        from fautodiff.code_tree import IfBlock

        decls = Block(
            [
                self._decl_alloc_real("a"),
                Declaration(name="flag", var_type=VarType("logical")),
            ]
        )
        then_b = Block([Allocate([OpVar("a")])])
        else_b = Block([Deallocate([OpVar("a")])])
        ifblk = IfBlock([(OpVar("flag"), then_b), (None, else_b)])
        sub = Subroutine("foo", [], decls=decls, content=Block([ifblk]))
        sub.remove_redundant_allocates([])
        code = "".join(sub.render())
        self.assertIn("if (flag) then\n", code)
        self.assertIn("allocate(a)\n", code)
        self.assertIn("deallocate(a)", code)

    def test_recurrent_loop_with_different_index(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          ip = i + 1
          a(i) = a(i) + a(ip) + c
        end do
        """
        )
        i = OpVar("i")
        ip = OpVar("ip")
        loop = DoLoop(
            Block(
                [
                    Assignment(OpVar("ip"), i + OpInt(1)),
                    Assignment(
                        OpVar("a", index=[i]),
                        OpVar("a", index=[i]) + OpVar("a", index=[ip]) + OpVar("c"),
                    ),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        self.assertEqual("".join(loop.render()), code)
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a", "c", "n"})
        self.assertEqual(set(loop.recurrent_vars()), {"a"})

    def test_recurrent_loop_with_self_reference_and_different_index(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          ip = i + 1
          a(i) = c
          a(ip) = a(ip) + c
        end do
        """
        )
        i = OpVar("i")
        ip = OpVar("ip")
        a1 = OpVar("a", index=[i])
        a2 = OpVar("a", index=[ip])
        c = OpVar("c")
        loop = DoLoop(
            Block(
                [
                    Assignment(OpVar("ip"), i + OpInt(1)),
                    Assignment(a1, c),
                    Assignment(a2, a2 + c),
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        self.assertEqual("".join(loop.render()), code)
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a", "c", "n"})
        self.assertEqual(set(loop.recurrent_vars()), {"a"})

    def test_has_modified_indices_simple(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        b = OpVar("b", index=[i + OpInt(1)])
        body = Block([Assignment(a, b)])
        loop = DoLoop(body, i, OpRange([OpInt(1), OpVar("n")]))
        self.assertTrue(loop.has_modified_indices())

    def test_has_modified_indices_alias(self):
        i = OpVar("i")
        ip = OpVar("ip")
        body = Block(
            [
                Assignment(ip, i + OpInt(1)),
                Assignment(OpVar("a", index=[i]), OpVar("b", index=[ip])),
            ]
        )
        loop = DoLoop(body, i, OpRange([OpInt(1), OpVar("n")]))
        self.assertTrue(loop.has_modified_indices())

    def test_has_modified_indices_none(self):
        i = OpVar("i")
        body = Block([Assignment(OpVar("a", index=[i]), OpVar("b", index=[i]))])
        loop = DoLoop(body, i, OpRange([OpInt(1), OpVar("n")]))
        self.assertFalse(loop.has_modified_indices())

    def test_has_modified_indices_simple_alias(self):
        i = OpVar("i")
        ip = OpVar("ip")
        body = Block(
            [
                Assignment(ip, i),
                Assignment(OpVar("a", index=[ip]), OpVar("b", index=[ip])),
            ]
        )
        loop = DoLoop(body, i, OpRange([OpInt(1), OpVar("n")]))
        self.assertFalse(loop.has_modified_indices())

    def test_nested_loop(self):
        i = OpVar("i")
        j = OpVar("j")
        inner = DoLoop(
            Block(
                [
                    Assignment(
                        OpVar("a", index=[i, j]), OpVar("b", index=[i, j]) + OpVar("c")
                    )
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        outer = DoLoop(Block([inner]), index=j, range=OpRange([OpInt(1), OpVar("m")]))
        self.assertEqual(
            {str(v) for v in outer.required_vars()}, {"b(1:n,1:m)", "c", "n", "m"}
        )
        self.assertEqual(set(outer.recurrent_vars()), set())

    def test_nested_loop_with_private_array(self):
        code = textwrap.dedent(
            """\
        do i = 1, n
          do k = 1, 2
            c(k,i) = k
            d(k,i) = k
          end do
          a(i) = c(1,i) + c(2,i)
          do k = 1, 2
            b(i) = b(i) + d(k,i)
          end do
        end do
        """
        )
        i = OpVar("i")
        k = OpVar("k")
        a = OpVar("a", index=[i])
        b = OpVar("b", index=[i])
        c = OpVar("c", index=[k, i])
        d = OpVar("d", index=[k, i])
        inner1 = DoLoop(
            Block([Assignment(c, k), Assignment(d, k)]),
            index=k,
            range=OpRange([OpInt(1), OpInt(2)]),
        )
        inner2 = DoLoop(
            Block(
                [
                    Assignment(b, b + d),
                ]
            ),
            index=k,
            range=OpRange([OpInt(1), OpInt(2)]),
        )
        outer = DoLoop(
            Block(
                [
                    inner1,
                    Assignment(a, OpVar("c", index=[1, i]) + OpVar("c", index=[2, i])),
                    inner2,
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual({str(v) for v in outer.required_vars()}, {"b(1:n)", "n"})
        self.assertEqual(set(outer.recurrent_vars()), set())

    def test_nested_loop_with_different_index(self):
        code = textwrap.dedent(
            """\
        do j = 1, m
          do i = 1, n
            a(i,j) = b(i,k) + c
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        inner = DoLoop(
            Block(
                [
                    Assignment(
                        OpVar("a", index=[i, j]), OpVar("b", index=[i, k]) + OpVar("c")
                    )
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        outer = DoLoop(Block([inner]), index=j, range=OpRange([OpInt(1), OpVar("m")]))
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual(
            {str(v) for v in outer.required_vars()}, {"b(1:n,k)", "k", "c", "n", "m"}
        )
        self.assertEqual(set(outer.recurrent_vars()), set())

    def test_nested_recurrent_loop_with_different_index(self):
        code = textwrap.dedent(
            """\
        do j = 1, m
          do i = 1, n
            a(j,k) = b(i,k) + c
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        inner = DoLoop(
            Block(
                [
                    Assignment(
                        OpVar("a", index=[j, k]), OpVar("b", index=[i, k]) + OpVar("c")
                    )
                ]
            ),
            index=i,
            range=OpRange([OpInt(1), OpVar("n")]),
        )
        outer = DoLoop(Block([inner]), index=j, range=OpRange([OpInt(1), OpVar("m")]))
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual(
            {str(v) for v in outer.required_vars()}, {"b(1:n,k)", "k", "c", "n", "m"}
        )
        self.assertEqual(set(outer.recurrent_vars()), set())

    def test_nested_loop_with_private_advars(self):
        code = textwrap.dedent(
            """\
        do j = m, 1, - 1
          do i = n, 1, - 1
            work_ad(1) = x_ad(i,j) + work_ad(1)
            work_ad(2) = y_ad(i,j) + work_ad(2)
            x_ad(i,j) = work_ad(1) * x(i,j) + x_ad(i,j)
            y_ad(i,j) = work_ad(2) * y(i,j) + y_ad(i,j)
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        x = OpVar("x", index=[i, j])
        x_ad = OpVar("x_ad", index=[i, j])
        y = OpVar("y", index=[i, j])
        y_ad = OpVar("y_ad", index=[i, j])
        work1_ad = OpVar("work_ad", index=[1])
        work2_ad = OpVar("work_ad", index=[2])
        inner = DoLoop(
            Block(
                [
                    Assignment(work1_ad, x_ad, accumulate=True),
                    Assignment(work2_ad, y_ad, accumulate=True),
                    Assignment(x_ad, work1_ad * x, accumulate=True),
                    Assignment(y_ad, work2_ad * y, accumulate=True),
                ]
            ),
            index=i,
            range=OpRange([OpVar("n"), OpInt(1), OpInt(-1)]),
        )
        outer = DoLoop(
            Block([inner]), index=j, range=OpRange([OpVar("m"), OpInt(1), OpInt(-1)])
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual(
            {str(v) for v in outer.required_vars()},
            {
                "x(1:n,1:m)",
                "y(1:n,1:m)",
                "x_ad(1:n,1:m)",
                "y_ad(1:n,1:m)",
                "work_ad(1:2)",
                "n",
                "m",
            },
        )
        self.assertEqual(set(outer.recurrent_vars()), set())

        code = textwrap.dedent(
            """\
        do j = m, 1, - 1
          do i = n, 1, - 1
            do k = 2, 1, - 1
              work(k) = k
              work_ad(k) = x_ad(i,j) * k + work_ad(k)
            end do
            x_ad(i,j) = work_ad(1) * work(1) + x_ad(i,j)
            work_ad(1) = 0.0
            y_ad(i,j) = work_ad(2) * work(2) + y_ad(i,j)
            work_ad(2) = 0.0
          end do
        end do
        """
        )
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        x = OpVar("x", index=[i, j])
        x_ad = OpVar("x_ad", index=[i, j])
        y = OpVar("y", index=[i, j])
        y_ad = OpVar("y_ad", index=[i, j])
        work = OpVar("work", index=[k])
        work1 = OpVar("work", index=[1])
        work2 = OpVar("work", index=[2])
        work_ad = OpVar("work_ad", index=[k])
        work1_ad = OpVar("work_ad", index=[1])
        work2_ad = OpVar("work_ad", index=[2])
        save1 = SaveAssignment(work1, id=1)
        save2 = SaveAssignment(work2, id=1)
        inner = DoLoop(
            Block(
                [
                    DoLoop(
                        Block(
                            [
                                Assignment(work, k),
                                Assignment(work_ad, x_ad * k, accumulate=True),
                            ]
                        ),
                        index=k,
                        range=OpRange([OpInt(2), OpInt(1), OpInt(-1)]),
                    ),
                    Assignment(x_ad, work1_ad * work1, accumulate=True),
                    Assignment(work1_ad, OpReal("0.0"), accumulate=False),
                    Assignment(y_ad, work2_ad * work2, accumulate=True),
                    Assignment(work2_ad, OpReal("0.0"), accumulate=False),
                ]
            ),
            index=i,
            range=OpRange([OpVar("n"), OpInt(1), OpInt(-1)]),
        )
        outer = DoLoop(
            Block([save1, save2, inner, save1.to_load(), save2.to_load()]),
            index=j,
            range=OpRange([OpVar("m"), OpInt(1), OpInt(-1)]),
        )
        self.assertEqual(
            {str(v) for v in outer.required_vars()},
            {"x_ad(1:n,1:m)", "y_ad(1:n,1:m)", "work(1:2)", "work_ad(1:2)", "n", "m"},
        )
        outer = outer.prune_for(VarList([x_ad, y_ad]))
        self.assertEqual(set(outer.recurrent_vars()), set())
        self.assertEqual("".join(outer.render()), code)


class TestDoWhile(unittest.TestCase):
    def test_basic_render_and_required(self):
        code = textwrap.dedent(
            """\
        do while (cond)
          a = b
        end do
        """
        )
        cond = OpVar("cond")
        body = Block([Assignment(OpVar("a"), OpVar("b"))])
        loop = DoWhile(body, cond)
        self.assertEqual(render_program(loop), code)
        self.assertEqual({str(v) for v in loop.required_vars()}, {"b", "cond"})

    def test_iter_ref_vars(self):
        body = Block([Assignment(OpVar("a"), OpVar("b"))])
        loop = DoWhile(body, OpVar("cond"))
        self.assertEqual({str(v) for v in loop.iter_ref_vars()}, {"cond"})

    def test_check_initial(self):
        code = textwrap.dedent(
            """\
        do while (flag)
          a_ad = b_ad
        end do
        """
        )
        body = Block([Assignment(OpVar("a_ad"), OpVar("b_ad"))])
        loop = DoWhile(body, OpVar("flag"))
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_prune_for(self):
        body = Block([Assignment(OpVar("a"), OpVar("b"))])
        loop = DoWhile(body, OpVar("flag"))
        pruned = loop.prune_for(VarList([OpVar("a")]))
        self.assertEqual(render_program(pruned), render_program(loop))
        pruned2 = loop.prune_for(VarList([OpVar("c")]))
        self.assertTrue(pruned2.is_effectively_empty())


class TestOmpDirective(unittest.TestCase):
    def test_prune_for_removes_unused_vars(self):
        a = OpVar("a")
        b = OpVar("b")
        c = OpVar("c")
        d = OpVar("d")
        body = Block(
            [
                Assignment(c, c + OpInt(1)),
                Assignment(a, OpInt(1)),
                Assignment(b, OpInt(2)),
            ]
        )
        omp = OmpDirective(
            "parallel",
            [{"private": ["a", "b"]}, {"reduction": ["+", ["c", "d"]]}],
            body,
        )
        pruned = omp.prune_for(VarList([a]))
        self.assertEqual(pruned.clauses, [{"private": ["a"]}])
        self.assertEqual(
            render_program(pruned),
            "!$omp parallel private(a)\n" "a = 1\n" "!$omp end parallel\n",
        )


class TestNodeDelete(unittest.TestCase):
    def test_delete_from_block(self):
        a = Assignment(OpVar("a"), OpInt(1))
        b = Assignment(OpVar("b"), OpInt(2))
        blk = Block([a, b])
        # Sanity precondition
        self.assertEqual(render_program(blk), "a = 1\n" "b = 2\n")
        # Delete first statement
        a.delete()
        self.assertEqual(len(blk), 1)
        self.assertEqual(render_program(blk), "b = 2\n")

    def test_delete_last_stmt_keeps_empty_branch(self):
        # if (a > 0) then;  b = 1; else; b = 2; end if
        b = OpVar("b")
        cond1 = OpVar("a") > OpInt(0)
        body1 = Block([Assignment(b, OpInt(1))])
        body2 = Block([Assignment(b, OpInt(2))])
        ifblk = IfBlock([(cond1, body1), (None, body2)])
        prog = Block([ifblk])
        # Delete the only statement in the first branch body; structure must remain
        body1.first().delete()
        self.assertEqual(
            render_program(prog),
            "if (a > 0) then\n" "else\n" "  b = 2\n" "end if\n",
        )

    def test_delete_branch_bodies_then_remove_if(self):
        # Place IfBlock under a parent block so it can be removed when empty
        b = OpVar("b")
        cond1 = OpVar("a") > OpInt(0)
        body1 = Block([Assignment(b, OpInt(1))])
        body2 = Block([Assignment(b, OpInt(2))])
        ifblk = IfBlock([(cond1, body1), (None, body2)])
        prog = Block([ifblk])
        # Delete entire branch bodies via their Block.delete()
        body1.delete()
        # IfBlock still present because second branch has content
        self.assertIn("end if\n", render_program(prog))
        body2.delete()
        # With both branches empty, the IfBlock should be deleted from prog
        self.assertEqual(len(prog), 0)
        self.assertEqual(render_program(prog), "")


if __name__ == "__main__":
    unittest.main()
