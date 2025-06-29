import sys
from pathlib import Path
import unittest
import textwrap

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.code_tree import (
    Variable,
    Subroutine,
    Declaration,
    Block,
    Assignment,
    ClearAssignment,
    SaveAssignment,
    PushPop,
    DoLoop,
    DoWhile,
    IfBlock,
    render_program
)
from fautodiff.operators import (
    OpInt,
    OpReal,
    OpVar,
    OpRange
)

from fautodiff.var_list import (
    VarList
)

class TestVariable(unittest.TestCase):
    def test_scalar(self):
        var = Variable("x", "real")
        self.assertEqual(var.name, "x")
        self.assertEqual(var.typename, "real")
        self.assertFalse(var.is_array())

    def test_array(self):
        var = Variable("a", "real", dims=("n",))
        self.assertTrue(var.is_array())
        self.assertEqual(var.dims, ("n",))
        self.assertEqual(str(var), "a(n)")

    def test_invalid_name(self):
        with self.assertRaises(ValueError):
            Variable("a(i)", "real")


class TestRenderProgram(unittest.TestCase):
    def test_simple_assignment(self):
        prog = Block([Assignment(OpVar("a"), OpInt(1))])
        self.assertEqual(render_program(prog), "a = 1\n")

    def test_if_else_block(self):
        code = textwrap.dedent("""\
        if (a > 0) then
          b = 1
        else
          b = 2
        end if
        """)
        b = OpVar("b")
        cond1 = OpVar("a") > OpInt(0)
        body1 = Block([Assignment(b, OpInt(1))])
        cond2 = None
        body2 = Block([Assignment(b, OpInt(2))])
        prog = Block([IfBlock([(cond1, body1), (cond2, body2)])])
        self.assertEqual(render_program(prog), code)

    def test_if_elif_block(self):
        code = textwrap.dedent("""\
        if (a > 0) then
          b = 1
        else if (a < 0) then
          b = 2
        else
          b = 3
        end if
        """)
        b = OpVar("b")
        cond1 = OpVar("a") > OpInt(0)
        body1 = Block([Assignment(b, OpInt(1))])
        cond2 = OpVar("a") < OpInt(0)
        body2 = Block([Assignment(b, OpInt(2))])
        cond3 = None
        body3 = Block([Assignment(b, OpInt(3))])
        prog = Block([IfBlock([(cond1, body1), (cond2, body2), (cond3, body3)])])
        expected = (
        )
        self.assertEqual(render_program(prog), code)


class TestNodeMethods(unittest.TestCase):
    def test_block_empty(self):
        blk = Block([])
        self.assertTrue(blk.is_effectively_empty())

    def test_has_assignment_to(self):
        blk = Block([
            Assignment(OpVar("a"), OpInt(1)),
        ])
        self.assertTrue(blk.has_assignment_to("a"))
        self.assertFalse(blk.has_assignment_to("b"))

        inner = DoLoop(
            Block([
                Assignment(
                    OpVar(
                        "a", index=[OpVar("i"), OpVar("j")]
                    ),
                    OpVar(
                        "b", index=[OpVar("i"), OpVar("j")]
                    )
                    + OpVar("c")
                )
            ]),
            index=OpVar("i"),
            start=OpInt(1),
            end=OpVar("n"),
        )
        outer = DoLoop(
            Block([inner]),
            index=OpVar("j"),
            start=OpInt(1),
            end=OpVar("m"),
        )
        self.assertTrue(outer.has_assignment_to("a"))
        self.assertFalse(outer.has_assignment_to("b"))
        self.assertFalse(outer.has_assignment_to("c"))

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
        self.assertEqual(
            render_program(clone), render_program(blk)
        )

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
        blk = Block([
            Assignment(a, OpInt(1)),
            Assignment(b, a),
            IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual({str(v) for v in blk.assigned_vars()}, {"a", "b", "c"})
        self.assertEqual({str(v) for v in blk.required_vars()}, {"c"})

        sub = Subroutine(
            "foo",
            "",
            decls=Block([
                Declaration(name="a", typename="real", intent="in"),
                Declaration(name="b", typename="real"),
            ]),
            content=Block([Assignment(OpVar("b"), OpVar("a"))]),
        )
        self.assertEqual({str(v) for v in sub.assigned_vars()}, {"a", "b"})
        self.assertEqual({str(v) for v in sub.required_vars()}, set())

    def test_do_loop_list(self):
        a = Assignment(OpVar("a"), OpInt(0))
        blk = Block([a])
        self.assertEqual(a.do_index_list, [])
        doblk = DoLoop(blk, index=OpVar("i"), start=OpInt(1), end=OpVar("n"))
        self.assertEqual(a.do_index_list, ["i"])
        blk2 = Block([doblk])
        doblk = DoLoop(blk2,
                                  index=OpVar("j"), start=OpInt(1), end=OpVar("m")
                                  )
        self.assertEqual(a.do_index_list, ["i","j"])
        b = Assignment(OpVar("b"), OpInt(0))
        blk.append(b)
        self.assertEqual(b.do_index_list, ["i","j"])

    def test_prune_for(self):
        a = OpVar("a")
        b = OpVar("b")
        c = OpVar("c")
        blk = Block([
            Block([
                Assignment(a, OpInt(2)),
                Assignment(c, a),
            ]),
            Block([
                Assignment(a, OpInt(1)),
                Assignment(b, a),
            ]),
        ])
        pruned = blk.prune_for(VarList([b]))
        self.assertEqual(
            render_program(pruned),
            "a = 1\n" "b = a\n",
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
        self.assertEqual({str(v) for v in b.required_vars(VarList([z]))}, {"z", "x_da", "y"})

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

        blk = Block([
            Assignment(a, c),
            Assignment(b, a)
        ])
        self.assertEqual({str(v) for v in blk.required_vars()}, {"c"})
        
        i = OpVar("i")
        xa = OpVar("x", index=[OpRange([None])])
        xi = OpVar("x", index=[i])
        yi = OpVar("y", index=[i])
        blk = Block([
            Assignment(xa, OpInt(0)),
            Assignment(yi, xi)
        ])
        self.assertEqual({str(v) for v in blk.required_vars()}, {"i"})

        ya = OpVar("y", index=[OpRange([None])])
        blk = Block([
            Assignment(xa, OpInt(0)),
            DoLoop(
                Block([Assignment(yi, xi)]),
                index=i, start=OpInt(1), end=OpVar("n")
            )
        ])
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
            Block([Assignment(v, k)]),
            index=k, start=OpInt(1), end=OpInt(2)
        )
        outer = DoLoop(
            Block([
                Assignment(a, i),
                Assignment(w, x),
                inner,
                Assignment(y, v1 + v2 + w1 + w2 + a)
            ]),
            index=i, start=OpInt(1), end=n
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
            index=k, start=OpInt(2), end=OpInt(1), step=OpInt(-1)
        )
        outer = DoLoop(
            Block([
                save_assign1,
                save_assign2,
                Assignment(v1_ad, y_ad, accumulate=True),
                Assignment(v2_ad, y_ad, accumulate=True),
                Assignment(w1_ad, y_ad, accumulate=True),
                Assignment(w2_ad, y_ad, accumulate=True),
                inner,
                Assignment(x_ad, w1_ad + w2_ad, accumulate=True),
                save_assign1.to_load(),
                save_assign2.to_load()
            ]),
            index=i, start=OpVar("n"), end=OpInt(1), step=OpInt(-1)
        )
        self.assertEqual({str(v) for v in outer.required_vars(no_accumulate=True, without_savevar=True)}, {"y_ad(1:n)","n"})

        a_ad = OpVar("a_ad", index=[i,j])
        b_ad = OpVar("b_ad", index=[i,j])
        one = OpInt(1)
        inner = DoLoop(
            Block([
                Assignment(a_ad, b_ad, accumulate=True),
            ]),
            index=i, start=n, end=one, step=-one)
        outer = DoLoop(
            Block([inner]),
            index=j, start=m, end=one, step=-one)
        self.assertEqual({str(v) for v in outer.required_vars()}, {"a_ad(1:n,1:m)","b_ad(1:n,1:m)", "m", "n"})

    def test_required_vars_in_if(self):
        i = OpVar("i")
        a = OpVar("a")
        b = OpVar("b")
        c = OpVar("c")
        cond1 = i < 0
        block1 = Block([Assignment(a, c)])
        cond2 = i > 0
        block2 = Block([Assignment(a, b)])
        cond3 = None
        block3 = Block([Assignment(b, c)])
        ifblk = IfBlock([
            (cond1, block1), (cond2, block2), (cond3, block3)
        ])
        self.assertEqual({str(v) for v in ifblk.required_vars(VarList([a]))}, {"i", "c", "b", "a"})

    def test_check_initial(self):
        code = textwrap.dedent("""\
        a_ad = b_ad
        a_ad = c_ad + a_ad
        """)
        a_ad = OpVar("a_ad")
        b_ad = OpVar("b_ad")
        c_ad = OpVar("c_ad")
        i = OpVar("i")
        n = OpVar("n")

        blk = Block([
            Assignment(a_ad, b_ad, accumulate=True),
            Assignment(a_ad, c_ad, accumulate=True)
        ])
        blk.check_initial()
        self.assertEqual(render_program(blk), code)

        code = textwrap.dedent("""\
        do i = 1, n
          a_ad = b_ad + a_ad
          a_ad = c_ad + a_ad
        end do
        """)
        loop = DoLoop(Block([
            Assignment(a_ad, b_ad, accumulate=True),
            Assignment(a_ad, c_ad, accumulate=True)
        ]),
        index=i, start=OpInt(1), end=n)
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

        code = textwrap.dedent("""\
        do i = 1, n
          a_ad = b_ad
          b_ad = a_ad + b_ad
          a_ad = 0.0
        end do
        """)
        loop = DoLoop(Block([
            Assignment(a_ad, b_ad, accumulate=True),
            Assignment(b_ad, a_ad, accumulate=True),
            ClearAssignment(a_ad)
        ]),
        index=i, start=OpInt(1), end=n)
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

        code = textwrap.dedent("""\
        do i = 1, n
          a_ad = b_ad + a_ad
          b_ad = a_ad + b_ad
          a_ad = b_ad + a_ad
        end do
        """)
        loop = DoLoop(Block([
            Assignment(a_ad, b_ad, accumulate=True),
            Assignment(b_ad, a_ad, accumulate=True),
            Assignment(a_ad, b_ad, accumulate=True)
        ]),
        index=i, start=OpInt(1), end=n)
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

        code = textwrap.dedent("""\
        do i = 1, n
          a_ad = b_ad + a_ad
        end do
        """)
        loop = DoLoop(Block([
            Assignment(a_ad, a_ad),
            Assignment(a_ad, b_ad, accumulate=True)
        ]),
        index=OpVar("i"), start=OpInt(1), end=OpVar("n"))
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

        code = textwrap.dedent("""\
        y_ad(:) = 0.0
        do i = 1, n
          x_ad(i) = y_ad(i)
          y_ad(i) = c_ad + y_ad(i)
          x_ad(i) = c_ad + x_ad(i)
          x_ad(i) = 0.0
        end do
        """)
        x_ad = OpVar("x_ad", index=[i])
        y_ad = OpVar("y_ad", index=[i])
        body = Block([
            Assignment(OpVar("y_ad", index=[None]), OpReal("0.0")),
            DoLoop(Block([
                Assignment(x_ad, y_ad, accumulate=True),
                Assignment(y_ad, c_ad, accumulate=True),
                Assignment(x_ad, c_ad, accumulate=True),
                ClearAssignment(x_ad)
            ]),
                   index=OpVar("i"), start=OpInt(1), end=OpVar("n"))
        ])
        body.check_initial()
        self.assertEqual(render_program(body), code)

        code = textwrap.dedent("""\
        do i = 1, n
          x_ad(i) = y_ad(i) + x_ad(i)
          x_ad(ip) = y_ad(i) + x_ad(ip)
        end do
        """)
        ip = OpVar("ip")
        xi_ad = OpVar("x_ad", index=[i])
        xip_ad = OpVar("x_ad", index=[ip])
        loop = DoLoop(Block([
            Assignment(xi_ad, y_ad, accumulate=True),
            Assignment(xip_ad, y_ad, accumulate=True),
        ]),
        index=i, start=OpInt(1), end=n)
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

        code = textwrap.dedent("""\
        do j = 1, m
          do i = 1, n
            x_ad(i,j) = y_ad(i,j)
            x_ad(i,j) = 0.0
            c_ad = y_ad(i,j)
            c_ad = 0.0
          end do
        end do
        """)
        index = [OpVar("i"), OpVar("j")]
        x_ad = OpVar("x_ad", index=index)
        y_ad = OpVar("y_ad", index=index)
        c_ad = OpVar("c_ad")
        inner = DoLoop(
            Block([
                Assignment(x_ad, y_ad, accumulate=True),
                ClearAssignment(x_ad),
                Assignment(c_ad, y_ad, accumulate=True),
                ClearAssignment(c_ad)
            ]),
            index=i, start=OpInt(1), end=n)
        outer = DoLoop(
            Block([inner]),
            index=OpVar("j"),
            start=OpInt(1),
            end=OpVar("m"),
        )
        outer.check_initial()
        self.assertEqual(render_program(outer), code)

        code = textwrap.dedent("""\
        if (a > 0) then
          a_ad = b_ad
        else
          a_ad = c_ad
        end if
        """)
        cond1 = OpVar("a") > 0
        body1 = Block([Assignment(a_ad, b_ad, accumulate=True)])
        cond2 = None
        body2 = Block([Assignment(a_ad, c_ad, accumulate=True)])
        cond_blk = Block([
            IfBlock([(cond1, body1), (cond2, body2)])
        ])
        cond_blk.check_initial()
        self.assertEqual(render_program(cond_blk), code)

        code = textwrap.dedent("""\
        if (a > 0) then
          a_ad = b_ad
        else
          c_ad = b_ad
        end if
        """)
        cond1 = OpVar("a") > 0
        body1 = Block([Assignment(a_ad, b_ad, accumulate=True)])
        cond2 = None
        body2 = Block([Assignment(c_ad, b_ad, accumulate=True)])
        cond_blk2 = Block([
            IfBlock([(cond1, body1), (cond2, body2)])
        ])
        cond_blk2.check_initial()
        self.assertEqual(render_program(cond_blk2), code)

class TestPushPop(unittest.TestCase):
    def test_push(self):
        var = OpVar("a")
        node = PushPop(var)
        self.assertEqual(render_program(Block([node])), "call push(a)\n")
        self.assertEqual([str(v) for v in node.iter_ref_vars()], ["a"])
        self.assertEqual(list(node.iter_assign_vars()), [])

    def test_pop(self):
        var = OpVar("a")
        node = PushPop(var).to_load()
        self.assertEqual(render_program(Block([node])), "call pop(a)\n")
        self.assertEqual(list(node.iter_ref_vars()), [])
        self.assertEqual([str(v) for v in node.iter_assign_vars()], ["a"])

class TestLoopAnalysis(unittest.TestCase):
    def test_simple_loop(self):
        i = OpVar("i")
        body = Block([
            Assignment(
                OpVar("a", index=[i]),
                OpVar("b", index=[i])
            )
        ])
        loop = DoLoop(
            body,
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual({str(v) for v in loop.required_vars()}, {"b(1:n)", "n"})
        #self.assertEqual({str(v) for v in loop.private_vars()}, set())

    def test_loop_with_accumulate(self):
        i = OpVar("i")
        a_ad = OpVar("a_ad", index=[i])
        b_ad = OpVar("b_ad", index=[i])
        c = OpVar("c")
        body = Block([
            Assignment(a_ad, c, accumulate=True),
            Assignment(b_ad, c, accumulate=True)
        ])
        loop = DoLoop(
            body,
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a_ad(1:n)", "b_ad(1:n)", "c", "n"})
        self.assertEqual({str(v) for v in loop.required_vars(no_accumulate=True)}, {"c", "n"})
        #self.assertEqual({str(v) for v in loop.private_vars()}, set())


    def test_self_reference_loop(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        body = Block([
            Assignment(a, a + OpVar("c"))
        ])
        loop = DoLoop(
            body,
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a(1:n)", "c", "n"})
        #self.assertEqual({str(v) for v in loop.private_vars()}, set())

    def test_no_recurrent_loop_with_scalar(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        c = OpVar("c")
        body = Block([
            Assignment(c, a + i),
            Assignment(a, c + OpInt(1)),
        ])
        loop = DoLoop(
            body,
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a(1:n)", "n"})
        #self.assertEqual({str(v) for v in loop.private_vars()}, {"c"})

    def test_recurrent_loop_with_scalar(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        c = OpVar("c")
        body = Block([
            Assignment(a, c + OpInt(1)),
            Assignment(c, a + i),
        ])
        loop = DoLoop(
            body,
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual({str(v) for v in loop.required_vars()}, {"c", "n"})
        #self.assertEqual({str(v) for v in loop.private_vars()}, set())

    def test_recurrent_loop_with_different_index(self):
        code = textwrap.dedent("""\
        do i = 1, n
          ip = i + 1
          a(i) = b(i) + c
          a(ip) = b(i) + c
        end do
        """)
        i = OpVar("i")
        ip = OpVar("ip")
        loop = DoLoop(
            Block([
                Assignment(
                    OpVar("ip"),
                    i + OpInt(1)
                ),
                Assignment(
                    OpVar("a", index=[i]),
                    OpVar("b", index=[i]) + OpVar("c")
                ),
                Assignment(
                    OpVar("a", index=[ip]),
                    OpVar("b", index=[i]) + OpVar("c")
                )
            ]),
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual("".join(loop.render()), code)
        self.assertEqual({str(v) for v in loop.required_vars()}, {"b(1:n)", "c", "n"})
        #self.assertEqual({str(v) for v in loop.private_vars()}, {"ip"})

    def test_recurrent_loop_with_self_reference_and_different_index(self):
        code = textwrap.dedent("""\
        do i = 1, n
          ip = i + 1
          a(i) = c
          a(ip) = a(ip) + c
        end do
        """)
        i = OpVar("i")
        ip = OpVar("ip")
        a1 = OpVar("a", index=[i])
        a2 = OpVar("a", index=[ip])
        c = OpVar("c")
        loop = DoLoop(
            Block([
                Assignment(OpVar("ip"), i + OpInt(1)),
                Assignment(a1, c),
                Assignment(a2, a2 + c)
            ]),
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual("".join(loop.render()), code)
        self.assertEqual({str(v) for v in loop.required_vars()}, {"a(1:n)", "c", "n"})
        #self.assertEqual({str(v) for v in loop.private_vars()}, {"ip"})

    def test_nested_loop(self):
        i = OpVar("i")
        j = OpVar("j")
        inner = DoLoop(
            Block([
                Assignment(
                    OpVar("a", index=[i,j]),
                    OpVar("b", index=[i,j]) + OpVar("c")
                )
            ]),
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        outer = DoLoop(
            Block([inner]),
            index=j,
            start=OpInt(1),
            end=OpVar("m"),
        )
        self.assertEqual({str(v) for v in outer.required_vars()}, {"b(1:n,1:m)", "c", "n", "m"})
        #self.assertEqual({str(v) for v in outer.private_vars()}, set())

    def test_nested_loop_with_private_array(self):
        code = textwrap.dedent("""\
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
        """)
        i = OpVar("i")
        k = OpVar("k")
        a = OpVar("a", index=[i])
        b = OpVar("b", index=[i])
        c = OpVar("c", index=[k,i])
        d = OpVar("d", index=[k,i])
        inner1 = DoLoop(
            Block([
                Assignment(c, k),
                Assignment(d, k)
            ]),
            index=k,
            start=OpInt(1),
            end=OpInt(2)
        )
        inner2 = DoLoop(
            Block([
                Assignment(b, b + d),
            ]),
            index=k,
            start=OpInt(1),
            end=OpInt(2)
        )
        outer = DoLoop(
            Block([
                inner1,
                Assignment(a,
                                     OpVar("c", index=[1, i])
                                     + OpVar("c", index=[2, i])),
                inner2,
            ]),
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual({str(v) for v in outer.required_vars()}, {"b(1:n)", "n"})
        #self.assertEqual({str(v) for v in outer.private_vars()}, set())

    def test_nested_loop_with_different_index(self):
        code = textwrap.dedent("""\
        do j = 1, m
          do i = 1, n
            a(i,j) = b(i,k) + c
          end do
        end do
        """)
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        inner = DoLoop(
            Block([
                Assignment(
                    OpVar("a", index=[i,j]),
                    OpVar("b", index=[i,k]) + OpVar("c")
                )
            ]),
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        outer = DoLoop(
            Block([inner]),
            index=j,
            start=OpInt(1),
            end=OpVar("m"),
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual({str(v) for v in outer.required_vars()}, {"b(1:n,k)", "k", "c", "n", "m"})
        #self.assertEqual({str(v) for v in outer.private_vars()}, set())

    def test_nested_recurrent_loop_with_different_index(self):
        code = textwrap.dedent("""\
        do j = 1, m
          do i = 1, n
            a(j,k) = b(i,k) + c
          end do
        end do
        """)
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        inner = DoLoop(
            Block([
                Assignment(
                    OpVar("a", index=[j,k]),
                    OpVar("b", index=[i,k]) + OpVar("c")
                )
            ]),
            index=i,
            start=OpInt(1),
            end=OpVar("n"),
        )
        outer = DoLoop(
            Block([inner]),
            index=j,
            start=OpInt(1),
            end=OpVar("m"),
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual({str(v) for v in outer.required_vars()}, {"b(1:n,k)", "k", "c", "n", "m"})
        #self.assertEqual({str(v) for v in outer.private_vars()}, set())

    def test_nested_loop_with_private_advars(self):
        code = textwrap.dedent("""\
        do j = m, 1, - 1
          do i = n, 1, - 1
            work_ad(1) = x_ad(i,j)
            work_ad(2) = y_ad(i,j)
            x_ad(i,j) = work_ad(1) * x(i,j) + x_ad(i,j)
            y_ad(i,j) = work_ad(2) * y(i,j) + y_ad(i,j)
          end do
        end do
        """)
        i = OpVar("i")
        j = OpVar("j")
        x = OpVar("x", index=[i,j])
        x_ad = OpVar("x_ad", index=[i,j])
        y = OpVar("y", index=[i,j])
        y_ad = OpVar("y_ad", index=[i,j])
        work1_ad = OpVar("work_ad", index=[1])
        work2_ad = OpVar("work_ad", index=[2])
        inner = DoLoop(
            Block([
                Assignment(work1_ad, x_ad, accumulate=True),
                Assignment(work2_ad, y_ad, accumulate=True),
                Assignment(x_ad, work1_ad * x, accumulate=True),
                Assignment(y_ad, work2_ad * y, accumulate=True),
            ]),
            index=i,
            start=OpVar("n"),
            end=OpInt(1),
            step=OpInt(-1),
        )
        outer = DoLoop(
            Block([inner]),
            index=j,
            start=OpVar("m"),
            end=OpInt(1),
            step=OpInt(-1),
        )
        #self.assertEqual({str(v) for v in outer.required_vars()}, {"y(:,:)", "y_ad(:,:)", "x(:,:)", "x_ad(:,:)", "n", "m")
        self.assertEqual({str(v) for v in outer.required_vars()}, {"x(1:n,1:m)", "y(1:n,1:m)", "x_ad(1:n,1:m)", "y_ad(1:n,1:m)", "work_ad(1:2)", "n", "m"})
        #private_vars = outer.private_vars()
        #self.assertEqual({str(v) for v in private_vars}, {"work_ad(1)", "work_ad(2)"})
        #for var in private_vars:
        #    outer.check_initial(str(var), force=True)
        #self.assertEqual("".join(outer.render()), code)


        code = textwrap.dedent("""\
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
        """)
        i = OpVar("i")
        j = OpVar("j")
        k = OpVar("k")
        x = OpVar("x", index=[i,j])
        x_ad = OpVar("x_ad", index=[i,j])
        y = OpVar("y", index=[i,j])
        y_ad = OpVar("y_ad", index=[i,j])
        work = OpVar("work", index=[k])
        work1 = OpVar("work", index=[1])
        work2 = OpVar("work", index=[2])
        work_ad = OpVar("work_ad", index=[k])
        work1_ad = OpVar("work_ad", index=[1])
        work2_ad = OpVar("work_ad", index=[2])
        save1 = SaveAssignment(work1, id=1)
        save2 = SaveAssignment(work2, id=1)
        inner = DoLoop(
            Block([
                DoLoop(
                    Block([
                        Assignment(work, k),
                        Assignment(work_ad, x_ad * k, accumulate=True),
                    ]),
                    index=k, start=OpInt(2), end=OpInt(1), step=OpInt(-1)),
                Assignment(x_ad, work1_ad * work1, accumulate=True),
                Assignment(work1_ad, OpReal("0.0"), accumulate=False),
                Assignment(y_ad, work2_ad * work2, accumulate=True),
                Assignment(work2_ad, OpReal("0.0"), accumulate=False)
            ]),
            index=i,
            start=OpVar("n"),
            end=OpInt(1),
            step=OpInt(-1),
        )
        outer = DoLoop(
            Block([
                save1, save2,
                inner,
                save1.to_load(), save2.to_load()
            ]),
            index=j,
            start=OpVar("m"),
            end=OpInt(1),
            step=OpInt(-1),
        )
        #self.assertEqual({str(v) for v in outer.required_vars()}, {"y_ad(:,:)", "x_ad(:,:)", "n", "m"})
        self.assertEqual({str(v) for v in outer.required_vars()}, {"x_ad(1:n,1:m)", "y_ad(1:n,1:m)", "work(1:2)", "work_ad(1:2)", "n", "m"})
        #private_vars = outer.private_vars()
        #self.assertEqual({str(v) for v in private_vars}, {"work_ad(1)", "work(1)", "work_ad(2)", "work(2)")
        #self.assertEqual({str(v) for v in private_vars}, {"work(1)", "work(2)"})
        outer = outer.prune_for(VarList([x_ad, y_ad]))
        self.assertEqual("".join(outer.render()), code)


class TestDoWhile(unittest.TestCase):
    def test_basic_render_and_required(self):
        code = textwrap.dedent("""\
        do while (cond)
          a = b
        end do
        """)
        cond = OpVar('cond')
        body = Block([
            Assignment(OpVar('a'), OpVar('b'))
        ])
        loop = DoWhile(body, cond)
        self.assertEqual(render_program(loop), code)
        self.assertEqual({str(v) for v in loop.required_vars()}, {"b", "cond"})

    def test_check_initial(self):
        code = textwrap.dedent("""\
        do while (flag)
          a_ad = b_ad
        end do
        """)
        body = Block([
            Assignment(OpVar("a_ad"), OpVar("b_ad"))
        ])
        loop = DoWhile(body, OpVar('flag'))
        loop.check_initial()
        self.assertEqual(render_program(loop), code)

    def test_prune_for(self):
        body = Block([
            Assignment(OpVar('a'), OpVar('b'))
        ])
        loop = DoWhile(body, OpVar('flag'))
        pruned = loop.prune_for(VarList([OpVar('a')]))
        self.assertEqual(render_program(pruned), render_program(loop))
        pruned2 = loop.prune_for(VarList([OpVar('c')]))
        self.assertTrue(pruned2.is_effectively_empty())


if __name__ == '__main__':
    unittest.main()




if __name__ == "__main__":
    unittest.main()
