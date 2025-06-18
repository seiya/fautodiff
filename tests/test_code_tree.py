import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree
from fautodiff import operators


class TestRenderProgram(unittest.TestCase):
    def test_simple_assignment(self):
        prog = code_tree.Block([code_tree.Assignment(operators.OpVar("a"), operators.OpInt(1))])
        self.assertEqual(code_tree.render_program(prog), "a = 1\n")

    def test_if_else_block(self):
        cond1 = operators.OpVar("a") > operators.OpInt(0)
        body1 = code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpInt(1))])
        cond2 = None
        body2 = code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpInt(2))])
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
        cond1 = operators.OpVar("a") > operators.OpInt(0)
        body1 = code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpInt(1))])
        cond2 = operators.OpVar("a") < operators.OpInt(0)
        body2 = code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpInt(2))])
        cond3 = None
        body3 = code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpInt(3))])
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
        cond1 = operators.OpVar("a") > operators.OpInt(0)
        body1 = code_tree.Block([code_tree.Assignment(operators.OpVar("c"), operators.OpVar("b"))])
        cond2 = None
        body2 = code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpVar("c"))])
        blk = code_tree.Block([
            code_tree.Assignment(operators.OpVar("a"), operators.OpInt(1)),
            code_tree.Assignment(operators.OpVar("b"), operators.OpVar("a")),
            code_tree.IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual(blk.assigned_vars(), ["a", "b", "c"])
        self.assertEqual(blk.required_vars(), ["c"])

        sub = code_tree.Subroutine(
            "foo",
            "",
            decls=code_tree.Block([
                code_tree.Declaration(name="a", typename="real", intent="in"),
                code_tree.Declaration(name="b", typename="real"),
            ]),
            content=code_tree.Block([code_tree.Assignment(operators.OpVar("b"), operators.OpVar("a"))]),
        )
        self.assertEqual(sub.assigned_vars(), ["a", "b"])
        self.assertEqual(sub.required_vars(), [])

    def test_prune_for(self):
        blk = code_tree.Block([
            code_tree.Block([
                code_tree.Assignment(operators.OpVar("a"), operators.OpInt(2)),
                code_tree.Assignment(operators.OpVar("c"), operators.OpVar("a")),
            ]),
            code_tree.Block([
                code_tree.Assignment(operators.OpVar("a"), operators.OpInt(1)),
                code_tree.Assignment(operators.OpVar("b"), operators.OpVar("a")),
            ]),
        ])
        pruned = blk.prune_for(["b"])
        self.assertEqual(
            code_tree.render_program(pruned),
            "a = 1\n" "b = a\n",
        )

    def test_assignment_accumulate(self):
        # detection without flag
        a = code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("x_da") + operators.OpVar("y"))
        self.assertEqual(a.required_vars(), ["x_da", "y"])

        b = code_tree.Assignment(operators.OpVar("x"), operators.OpVar("x_da") + operators.OpVar("y"))
        self.assertEqual(b.required_vars(["z"]), ["z", "x_da", "y"])

        c = code_tree.Assignment(operators.OpVar("x"), operators.OpVar("x_da") + operators.OpVar("y"))
        self.assertEqual(c.required_vars(["x"]), ["x_da", "y"])

        # explicit accumulate
        d = code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("y"), accumulate=True)
        self.assertEqual(
            code_tree.render_program(code_tree.Block([d])),
            "x_da = y + x_da\n",
        )
        self.assertEqual(b.required_vars(), ["x_da", "y"])

    def test_assignment_rhs_names(self):
        assign = code_tree.Assignment(operators.OpVar("a"), operators.OpVar("b") + operators.OpVar("c"))
        self.assertEqual(assign.rhs_names, ["b", "c"])

    def test_required_vars_uses_cached_names(self):
        assign = code_tree.Assignment(operators.OpVar("a"), operators.OpVar("b") + operators.OpVar("c"))
        assign.rhs = "d"
        self.assertEqual(assign.required_vars(), ["b", "c"])

    def test_check_initial(self):
        blk = code_tree.Block([
            code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("x_da") + operators.OpVar("y")),
        ])
        res = blk.check_initial("x_da")
        self.assertEqual(res, -1)
        self.assertEqual(code_tree.render_program(blk), "x_da = x_da + y\n")

        blk = code_tree.Block([
            code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("y"), accumulate=True),
        ])
        res = blk.check_initial("x_da")
        self.assertEqual(res, 2)
        self.assertEqual(code_tree.render_program(blk), "x_da = y\n")

        loop = code_tree.Block([
            code_tree.DoLoop(body=code_tree.Block([
                code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("x_da") + operators.OpVar("y"))
            ]), index=operators.OpVar("i"), start=operators.OpInt(1), end=operators.OpVar("n"))
        ])
        loop.build_do_index_list([])
        self.assertEqual(loop.check_initial("x_da"), -1)
        self.assertEqual(
            code_tree.render_program(loop),
            "do i = 1, n\n  x_da = x_da + y\nend do\n",
        )

        cond1 = operators.OpVar("a") > 0
        body1 = code_tree.Block([code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("x_da") + operators.OpVar("a"))])
        cond2 = None
        body2 = code_tree.Block([
                    code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("x_da") + operators.OpVar("b"))
                ])
        cond_blk = code_tree.Block([
            code_tree.IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual(cond_blk.check_initial("x_da"), -1)
        self.assertEqual(
            code_tree.render_program(cond_blk),
            "if (a > 0) then\n  x_da = x_da + a\nelse\n  x_da = x_da + b\nend if\n",
        )

        cond1 = operators.OpVar("a") > 0
        body1 = code_tree.Block([code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("x_da") + operators.OpVar("a"))])
        cond2 = None
        body2 = code_tree.Block([])
        cond_blk2 = code_tree.Block([
            code_tree.IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual(cond_blk2.check_initial("x_da"), -1)

        cond1 = operators.OpVar("a") > 0
        body1 = code_tree.Block([code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("a"), accumulate=True)])
        cond2 = None
        body2 = code_tree.Block([
                    code_tree.Assignment(operators.OpVar("x_da"), operators.OpVar("b"), accumulate=True)
                ])
        cond_blk3 = code_tree.Block([
            code_tree.IfBlock([(cond1, body1), (cond2, body2)])
        ])
        self.assertEqual(cond_blk3.check_initial("x_da"), 2)


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


if __name__ == "__main__":
    unittest.main()
