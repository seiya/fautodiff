import sys
from pathlib import Path
import unittest
import textwrap

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree
from fautodiff import operators


class TestVariable(unittest.TestCase):
    def test_scalar(self):
        var = code_tree.Variable("x", "real")
        self.assertEqual(var.name, "x")
        self.assertEqual(var.typename, "real")
        self.assertFalse(var.is_array())

    def test_array(self):
        var = code_tree.Variable("a", "real", dims=("n",))
        self.assertTrue(var.is_array())
        self.assertEqual(var.dims, ("n",))
        self.assertEqual(str(var), "a(n)")

    def test_invalid_name(self):
        with self.assertRaises(ValueError):
            code_tree.Variable("a(i)", "real")


class TestRenderProgram(unittest.TestCase):
    def test_simple_assignment(self):
        prog = code_tree.Block([code_tree.Assignment(operators.OpVar("a"), operators.OpInt(1))])
        self.assertEqual(code_tree.render_program(prog), "a = 1\n")

    def test_if_else_block(self):
        b = operators.OpVar("b")
        cond1 = operators.OpVar("a") > operators.OpInt(0)
        body1 = code_tree.Block([code_tree.Assignment(b, operators.OpInt(1))])
        cond2 = None
        body2 = code_tree.Block([code_tree.Assignment(b, operators.OpInt(2))])
        prog = code_tree.Block([code_tree.IfBlock([(cond1, body1), (cond2, body2)])])
        expected = (
            "if (a > 0) then\n"
            "  b = 1\n"
            "else\n"
            "  b = 2\n"
            "end if\n"
        )
        self.assertEqual(code_tree.render_program(prog), expected)

    def test_if_elif_block(self):
        b = operators.OpVar("b")
        cond1 = operators.OpVar("a") > operators.OpInt(0)
        body1 = code_tree.Block([code_tree.Assignment(b, operators.OpInt(1))])
        cond2 = operators.OpVar("a") < operators.OpInt(0)
        body2 = code_tree.Block([code_tree.Assignment(b, operators.OpInt(2))])
        cond3 = None
        body3 = code_tree.Block([code_tree.Assignment(b, operators.OpInt(3))])
        prog = code_tree.Block([code_tree.IfBlock([(cond1, body1), (cond2, body2), (cond3, body3)])])
        expected = (
            "if (a > 0) then\n"
            "  b = 1\n"
            "else if (a < 0) then\n"
            "  b = 2\n"
            "else\n"
            "  b = 3\n"
            "end if\n"
        )
        self.assertEqual(code_tree.render_program(prog), expected)


class TestNodeMethods(unittest.TestCase):
    def test_block_empty(self):
        blk = code_tree.Block([])
        self.assertTrue(blk.is_effectively_empty())

    def test_has_assignment_to(self):
        blk = code_tree.Block([
            code_tree.Assignment(operators.OpVar("a"), operators.OpInt(1)),
        ])
        self.assertTrue(blk.has_assignment_to("a"))
        self.assertFalse(blk.has_assignment_to("b"))

        inner = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(
                    operators.OpVar(
                        "a", index=[operators.OpVar("i"), operators.OpVar("j")]
                    ),
                    operators.OpVar(
                        "b", index=[operators.OpVar("i"), operators.OpVar("j")]
                    )
                    + operators.OpVar("c")
                )
            ]),
            index=operators.OpVar("i"),
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        outer = code_tree.DoLoop(
            code_tree.Block([inner]),
            index=operators.OpVar("j"),
            start=operators.OpInt(1),
            end=operators.OpVar("m"),
        )
        self.assertTrue(outer.has_assignment_to("a"))
        self.assertFalse(outer.has_assignment_to("b"))
        self.assertFalse(outer.has_assignment_to("c"))

    def test_ids_and_clone(self):
        blk = code_tree.Block([code_tree.Assignment(operators.OpVar("a"), operators.OpInt(1))])
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
            code_tree.render_program(clone), code_tree.render_program(blk)
        )

    def test_find_and_remove(self):
        a = code_tree.Assignment(operators.OpVar("a"), operators.OpInt(1))
        b = code_tree.Assignment(operators.OpVar("b"), operators.OpInt(2))
        blk = code_tree.Block([a, b])
        self.assertIs(blk.find_by_id(b.get_id()), b)
        blk.remove_by_id(a.get_id())
        self.assertEqual(len(blk), 1)
        child0 = None
        for child in blk.iter_children():
            child0 = child
            break
        self.assertIs(child0, b)

    def test_var_analysis(self):
        a = operators.OpVar("a")
        b = operators.OpVar("b")
        c = operators.OpVar("c")
        cond1 = a > 0
        body1 = code_tree.Block([code_tree.Assignment(c, b)])
        cond2 = None
        body2 = code_tree.Block([code_tree.Assignment(b, c)])
        blk = code_tree.Block([
            code_tree.Assignment(a, operators.OpInt(1)),
            code_tree.Assignment(b, a),
            code_tree.IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual([str(v) for v in blk.assigned_vars()], ["a", "b", "c"])
        self.assertEqual([str(v) for v in blk.required_vars()], ["c"])

        sub = code_tree.Subroutine(
            "foo",
            "",
            decls=code_tree.Block([
                code_tree.Declaration(name="a", typename="real", intent="in"),
                code_tree.Declaration(name="b", typename="real"),
            ]),
            content=code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpVar("a"))]),
        )
        self.assertEqual([str(v) for v in sub.assigned_vars()], ["a", "b"])
        self.assertEqual([str(v) for v in sub.required_vars()], [])

    def test_do_loop_list(self):
        a = code_tree.Assignment(operators.OpVar("a"), operators.OpInt(0))
        blk = code_tree.Block([a])
        self.assertEqual(a.do_index_list, [])
        doblk = code_tree.DoLoop(blk, index=operators.OpVar("i"), start=operators.OpInt(1), end=operators.OpVar("n"))
        self.assertEqual(a.do_index_list, ["i"])
        blk2 = code_tree.Block([doblk])
        doblk = code_tree.DoLoop(blk2,
                                  index=operators.OpVar("j"), start=operators.OpInt(1), end=operators.OpVar("m")
                                  )
        self.assertEqual(a.do_index_list, ["i","j"])
        b = code_tree.Assignment(operators.OpVar("b"), operators.OpInt(0))
        blk.append(b)
        self.assertEqual(b.do_index_list, ["i","j"])

    def test_prune_for(self):
        a = operators.OpVar("a")
        b = operators.OpVar("b")
        c = operators.OpVar("c")
        blk = code_tree.Block([
            code_tree.Block([
                code_tree.Assignment(a, operators.OpInt(2)),
                code_tree.Assignment(c, a),
            ]),
            code_tree.Block([
                code_tree.Assignment(a, operators.OpInt(1)),
                code_tree.Assignment(b, a),
            ]),
        ])
        pruned = blk.prune_for([b])
        self.assertEqual(
            code_tree.render_program(pruned),
            "a = 1\n" "b = a\n",
        )

    def test_assignment_accumulate(self):
        # detection without flag
        x = operators.OpVar("x")
        x_da = operators.OpVar("x_da")
        y = operators.OpVar("y")
        a = code_tree.Assignment(x_da, x_da + y)
        self.assertEqual([str(v) for v in a.required_vars()], ["x_da", "y"])

        b = code_tree.Assignment(x, x_da + y)
        z = operators.OpVar("z")
        self.assertEqual([str(v) for v in b.required_vars([z])], ["z", "x_da", "y"])

        c = code_tree.Assignment(x, x_da + y)
        self.assertEqual([str(v) for v in c.required_vars([x])], ["x_da", "y"])

        # explicit accumulate
        d = code_tree.Assignment(x_da, y, accumulate=True)
        self.assertEqual(
            code_tree.render_program(code_tree.Block([d])),
            "x_da = y + x_da\n",
        )
        self.assertEqual([str(v) for v in b.required_vars()], ["x_da", "y"])

    def test_required_vars(self):
        a = operators.OpVar("a")
        b = operators.OpVar("b")
        c = operators.OpVar("c")
        assign = code_tree.Assignment(a, b + c)
        self.assertEqual([str(v) for v in assign.required_vars()], ["b", "c"])

        blk = code_tree.Block([
            code_tree.Assignment(a, c),
            code_tree.Assignment(b, a)
        ])
        self.assertEqual([str(v) for v in blk.required_vars()], ["c"])
        
        i = operators.OpVar("i")
        xa = operators.OpVar("x", index=[operators.OpRange([None])])
        xi = operators.OpVar("x", index=[i])
        yi = operators.OpVar("y", index=[i])
        blk = code_tree.Block([
            code_tree.Assignment(xa, operators.OpInt(0)),
            code_tree.Assignment(yi, xi)
        ])
        self.assertEqual([str(v) for v in blk.required_vars()], ["i"])

        ya = operators.OpVar("y", index=[operators.OpRange([None])])
        blk = code_tree.Block([
            code_tree.Assignment(xa, operators.OpInt(0)),
            code_tree.DoLoop(
                code_tree.Block([code_tree.Assignment(yi, xi)]),
                index=i, start=operators.OpInt(1), end=operators.OpVar("n")
            )
        ])
        self.assertEqual([str(v) for v in blk.required_vars([ya])], ["n"])

        cond1 = i < 0
        block1 = code_tree.Block([code_tree.Assignment(a, c)])
        cond2 = i > 0
        block2 = code_tree.Block([code_tree.Assignment(a, b)])
        cond3 = None
        block3 = code_tree.Block([code_tree.Assignment(b, c)])
        ifblk = code_tree.IfBlock([
            (cond1, block1), (cond2, block2), (cond3, block3)
        ])
        self.assertEqual([str(v) for v in ifblk.required_vars([a])], ["i", "c", "b", "a"])

    def test_check_initial(self):
        a_ad = operators.OpVar("a_ad")
        b_ad = operators.OpVar("b_ad")
        c_ad = operators.OpVar("c_ad")
        i = operators.OpVar("i")
        n = operators.OpVar("n")

        blk = code_tree.Block([
            code_tree.Assignment(a_ad, b_ad, accumulate=True),
            code_tree.Assignment(a_ad, c_ad, accumulate=True)
        ])
        self.assertEqual(blk.check_initial("a_ad"), 2)
        self.assertEqual(code_tree.render_program(blk), "a_ad = b_ad\na_ad = c_ad + a_ad\n")

        loop = code_tree.Block([
            code_tree.DoLoop(code_tree.Block([
                code_tree.Assignment(a_ad, b_ad, accumulate=True),
                code_tree.Assignment(a_ad, c_ad, accumulate=True)
            ]),
            index=i, start=operators.OpInt(1), end=n)
        ])
        self.assertEqual(loop.check_initial("a_ad"), -1)
        self.assertEqual(
            code_tree.render_program(loop),
            "do i = 1, n\n  a_ad = b_ad + a_ad\n  a_ad = c_ad + a_ad\nend do\n",
        )

        loop = code_tree.Block([
            code_tree.DoLoop(code_tree.Block([
                code_tree.Assignment(a_ad, b_ad, accumulate=True),
                code_tree.Assignment(b_ad, a_ad, accumulate=True)
            ]),
            index=i, start=operators.OpInt(1), end=n)
        ])
        self.assertEqual(loop.check_initial("a_ad"), 2)
        self.assertEqual(loop.check_initial("b_ad"), -1)
        self.assertEqual(
            code_tree.render_program(loop),
            "do i = 1, n\n  a_ad = b_ad\n  b_ad = a_ad + b_ad\nend do\n",
        )

        loop = code_tree.Block([
            code_tree.DoLoop(code_tree.Block([
                code_tree.Assignment(a_ad, b_ad, accumulate=True),
                code_tree.Assignment(b_ad, a_ad, accumulate=True),
                code_tree.Assignment(a_ad, b_ad, accumulate=True)
            ]),
            index=i, start=operators.OpInt(1), end=n)
        ])
        self.assertEqual(loop.check_initial("a_ad"), -1)
        self.assertEqual(loop.check_initial("b_ad"), -1)
        self.assertEqual(
            code_tree.render_program(loop),
            "do i = 1, n\n  a_ad = b_ad + a_ad\n  b_ad = a_ad + b_ad\n  a_ad = b_ad + a_ad\nend do\n",
        )

        loop = code_tree.Block([
            code_tree.DoLoop(code_tree.Block([
                code_tree.Assignment(a_ad, a_ad),
                code_tree.Assignment(a_ad, b_ad, accumulate=True)
            ]),
            index=operators.OpVar("i"), start=operators.OpInt(1), end=operators.OpVar("n"))
        ])
        self.assertEqual(loop.check_initial("a_ad"), -1)
        self.assertEqual(
            code_tree.render_program(loop),
            "do i = 1, n\n  a_ad = b_ad + a_ad\nend do\n",
        )

        index = [i]
        x_ad = operators.OpVar("x_ad", index=index)
        y_ad = operators.OpVar("y_ad", index=index)
        loop = code_tree.Block([
            code_tree.DoLoop(code_tree.Block([
                code_tree.Assignment(x_ad, y_ad, accumulate=True),
                code_tree.Assignment(y_ad, c_ad, accumulate=True),
                code_tree.Assignment(x_ad, c_ad, accumulate=True)
            ]),
            index=operators.OpVar("i"), start=operators.OpInt(1), end=operators.OpVar("n"))
        ])
        self.assertEqual(loop.check_initial("x_ad"), 2)
        self.assertEqual(loop.check_initial("y_ad"), -1)
        self.assertEqual(
            code_tree.render_program(loop),
            "do i = 1, n\n  x_ad(i) = y_ad(i)\n  y_ad(i) = c_ad + y_ad(i)\n  x_ad(i) = c_ad + x_ad(i)\nend do\n",
        )

        ip = operators.OpVar("ip")
        xi_ad = operators.OpVar("x_ad", index=[i])
        xip_ad = operators.OpVar("x_ad", index=[ip])
        loop = code_tree.Block([
            code_tree.DoLoop(code_tree.Block([
                code_tree.Assignment(xi_ad, y_ad, accumulate=True),
                code_tree.Assignment(xip_ad, y_ad, accumulate=True),
            ]),
            index=i, start=operators.OpInt(1), end=n)
        ])
        self.assertEqual(loop.check_initial("x_ad"), -1)

        index = [operators.OpVar("i"), operators.OpVar("j")]
        x_ad = operators.OpVar("x_ad", index=index)
        y_ad = operators.OpVar("y_ad", index=index)
        c_ad = operators.OpVar("c_ad")
        inner = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(x_ad, y_ad, accumulate=True),
                code_tree.Assignment(c_ad, y_ad, accumulate=True)
            ]),
            index=i, start=operators.OpInt(1), end=n)
        outer = code_tree.DoLoop(
            code_tree.Block([inner]),
            index=operators.OpVar("j"),
            start=operators.OpInt(1),
            end=operators.OpVar("m"),
        )
        self.assertEqual(outer.check_initial("x_ad"), 2)
        self.assertEqual(outer.check_initial("y_ad"), 0)
        self.assertEqual(outer.check_initial("c_ad"), -1)

        cond1 = operators.OpVar("a") > 0
        body1 = code_tree.Block([code_tree.Assignment(a_ad, b_ad, accumulate=True)])
        cond2 = None
        body2 = code_tree.Block([code_tree.Assignment(a_ad, c_ad, accumulate=True)])
        cond_blk = code_tree.Block([
            code_tree.IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual(cond_blk.check_initial("a_ad"), 2)
        self.assertEqual(
            code_tree.render_program(cond_blk),
            "if (a > 0) then\n  a_ad = b_ad\nelse\n  a_ad = c_ad\nend if\n",
        )

        cond1 = operators.OpVar("a") > 0
        body1 = code_tree.Block([code_tree.Assignment(a_ad, b_ad, accumulate=True)])
        cond2 = None
        body2 = code_tree.Block([code_tree.Assignment(c_ad, b_ad, accumulate=True)])
        cond_blk2 = code_tree.Block([
            code_tree.IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual(cond_blk2.check_initial("a_ad"), -1)
        self.assertEqual(cond_blk2.check_initial("c_ad"), -1)


class TestLoopAnalysis(unittest.TestCase):
    def test_simple_loop(self):
        i = operators.OpVar("i")
        body = code_tree.Block([
            code_tree.Assignment(
                operators.OpVar("a", index=[i]),
                operators.OpVar("b", index=[i])
            )
        ])
        loop = code_tree.DoLoop(
            body,
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual([str(v) for v in loop.required_vars()], ["b(:)", "n"])

    def test_loop_with_accumulate(self):
        i = operators.OpVar("i")
        a = operators.OpVar("a", index=[i])
        b = operators.OpVar("b", index=[i])
        c = operators.OpVar("c")
        body = code_tree.Block([
            code_tree.Assignment(a, c, accumulate=True),
            code_tree.Assignment(b, c, accumulate=True)
        ])
        loop = code_tree.DoLoop(
            body,
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual([str(v) for v in loop.required_vars()], ["c", "b(:)", "a(:)", "n"])
        self.assertEqual([str(v) for v in loop.required_vars(no_accumulate=True)], ["c", "n"])


    def test_self_reference_loop(self):
        i = operators.OpVar("i")
        a = operators.OpVar("a", index=[i])
        body = code_tree.Block([
            code_tree.Assignment(a, a + operators.OpVar("c"))
        ])
        loop = code_tree.DoLoop(
            body,
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual([str(v) for v in loop.required_vars()], ["a(:)", "c", "n"])

    def test_no_recurrent_loop_with_scalar(self):
        i = operators.OpVar("i")
        a = operators.OpVar("a", index=[i])
        c = operators.OpVar("c")
        body = code_tree.Block([
            code_tree.Assignment(c, a + i),
            code_tree.Assignment(a, c + operators.OpInt(1)),
        ])
        loop = code_tree.DoLoop(
            body,
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual([str(v) for v in loop.required_vars()], ["a(:)", "n"])

    def test_recurrent_loop_with_scalar(self):
        i = operators.OpVar("i")
        a = operators.OpVar("a", index=[i])
        c = operators.OpVar("c")
        body = code_tree.Block([
            code_tree.Assignment(a, c + operators.OpInt(1)),
            code_tree.Assignment(c, a + i),
        ])
        loop = code_tree.DoLoop(
            body,
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual([str(v) for v in loop.required_vars()], ["c", "n"])

    def test_recurrent_loop_with_different_index(self):
        code = textwrap.dedent("""\
        do i = 1, n
          ip = i + 1
          a(i) = b(i) + c
          a(ip) = b(i) + c
        end do
        """)
        i = operators.OpVar("i")
        ip = operators.OpVar("ip")
        loop = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(
                    operators.OpVar("ip"),
                    i + operators.OpInt(1)
                ),
                code_tree.Assignment(
                    operators.OpVar("a", index=[i]),
                    operators.OpVar("b", index=[i]) + operators.OpVar("c")
                ),
                code_tree.Assignment(
                    operators.OpVar("a", index=[ip]),
                    operators.OpVar("b", index=[i]) + operators.OpVar("c")
                )
            ]),
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual("".join(loop.render()), code)
        self.assertEqual([str(v) for v in loop.required_vars()], ["b(:)", "c", "n"])

    def test_recurrent_loop_with_self_reference_and_different_index(self):
        code = textwrap.dedent("""\
        do i = 1, n
          ip = i + 1
          a(i) = c
          a(ip) = a(ip) + c
        end do
        """)
        i = operators.OpVar("i")
        ip = operators.OpVar("ip")
        a1 = operators.OpVar("a", index=[i])
        a2 = operators.OpVar("a", index=[ip])
        c = operators.OpVar("c")
        loop = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(operators.OpVar("ip"), i + operators.OpInt(1)),
                code_tree.Assignment(a1, c),
                code_tree.Assignment(a2, a2 + c)
            ]),
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual("".join(loop.render()), code)
        self.assertEqual([str(v) for v in loop.required_vars()], ["a(ip)", "c", "n"])

    def test_nested_loop(self):
        i = operators.OpVar("i")
        j = operators.OpVar("j")
        inner = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(
                    operators.OpVar("a", index=[i,j]),
                    operators.OpVar("b", index=[i,j]) + operators.OpVar("c")
                )
            ]),
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        outer = code_tree.DoLoop(
            code_tree.Block([inner]),
            index=j,
            start=operators.OpInt(1),
            end=operators.OpVar("m"),
        )
        self.assertEqual([str(v) for v in outer.required_vars()], ["b(:,:)", "c", "n", "m"])

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
        i = operators.OpVar("i")
        k = operators.OpVar("k")
        a = operators.OpVar("a", index=[i])
        b = operators.OpVar("b", index=[i])
        c = operators.OpVar("c", index=[k,i])
        d = operators.OpVar("d", index=[k,i])
        inner1 = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(c, k),
                code_tree.Assignment(d, k)
            ]),
            index=k,
            start=operators.OpInt(1),
            end=operators.OpInt(2)
        )
        inner2 = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(b, b + d),
            ]),
            index=k,
            start=operators.OpInt(1),
            end=operators.OpInt(2)
        )
        outer = code_tree.DoLoop(
            code_tree.Block([
                inner1,
                code_tree.Assignment(a,
                                     operators.OpVar("c", index=[operators.OpInt(1), i])
                                     + operators.OpVar("c", index=[operators.OpInt(2), i])),
                inner2,
            ]),
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual([str(v) for v in outer.required_vars()], ["b(:)", "n"])

    def test_nested_loop_with_different_index(self):
        code = textwrap.dedent("""\
        do j = 1, m
          do i = 1, n
            a(i,j) = b(i,k) + c
          end do
        end do
        """)
        i = operators.OpVar("i")
        j = operators.OpVar("j")
        k = operators.OpVar("k")
        inner = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(
                    operators.OpVar("a", index=[i,j]),
                    operators.OpVar("b", index=[i,k]) + operators.OpVar("c")
                )
            ]),
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        outer = code_tree.DoLoop(
            code_tree.Block([inner]),
            index=j,
            start=operators.OpInt(1),
            end=operators.OpVar("m"),
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual([str(v) for v in outer.required_vars()], ["b(:,k)", "k", "c", "n", "m"])

    def test_nested_recurrent_loop_with_different_index(self):
        code = textwrap.dedent("""\
        do j = 1, m
          do i = 1, n
            a(j,k) = b(i,k) + c
          end do
        end do
        """)
        i = operators.OpVar("i")
        j = operators.OpVar("j")
        k = operators.OpVar("k")
        inner = code_tree.DoLoop(
            code_tree.Block([
                code_tree.Assignment(
                    operators.OpVar("a", index=[j,k]),
                    operators.OpVar("b", index=[i,k]) + operators.OpVar("c")
                )
            ]),
            index=i,
            start=operators.OpInt(1),
            end=operators.OpVar("n"),
        )
        outer = code_tree.DoLoop(
            code_tree.Block([inner]),
            index=j,
            start=operators.OpInt(1),
            end=operators.OpVar("m"),
        )
        self.assertEqual("".join(outer.render()), code)
        self.assertEqual([str(v) for v in outer.required_vars()], ["k", "b(:,k)", "c", "n", "m"])


if __name__ == "__main__":
    unittest.main()
