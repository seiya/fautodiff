import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree


class TestRenderProgram(unittest.TestCase):
    def test_simple_assignment(self):
        prog = code_tree.Block([code_tree.Assignment("a", "1")])
        self.assertEqual(code_tree.render_program(prog), "a = 1\n")

    def test_if_else_block(self):
        prog = code_tree.Block(
            [
                code_tree.IfBlock(
                    "a > 0",
                    code_tree.Block([code_tree.Assignment("b", "1")]),
                    else_body=code_tree.Block([code_tree.Assignment("b", "2")]),
                )
            ]
        )
        expected = (
            "IF (a > 0) THEN\n"
            "  b = 1\n"
            "ELSE\n"
            "  b = 2\n"
            "END IF\n"
        )
        self.assertEqual(code_tree.render_program(prog), expected)

    def test_if_elif_block(self):
        prog = code_tree.Block(
            [
                code_tree.IfBlock(
                    "a > 0",
                    code_tree.Block([code_tree.Assignment("b", "1")]),
                    elif_blocks=[
                        ("a < 0", code_tree.Block([code_tree.Assignment("b", "2")]))
                    ],
                    else_body=code_tree.Block([code_tree.Assignment("b", "3")]),
                )
            ]
        )
        expected = (
            "IF (a > 0) THEN\n"
            "  b = 1\n"
            "ELSE IF (a < 0) THEN\n"
            "  b = 2\n"
            "ELSE\n"
            "  b = 3\n"
            "END IF\n"
        )
        self.assertEqual(code_tree.render_program(prog), expected)


class TestNodeMethods(unittest.TestCase):
    def test_block_empty(self):
        blk = code_tree.Block([code_tree.EmptyLine()])
        self.assertTrue(blk.is_effectively_empty())

    def test_has_assignment_to(self):
        blk = code_tree.Block([
            code_tree.Assignment("a", "1"),
            code_tree.EmptyLine(),
        ])
        self.assertTrue(blk.has_assignment_to("a"))
        self.assertFalse(blk.has_assignment_to("b"))

    def test_ids_and_clone(self):
        blk = code_tree.Block([code_tree.Assignment("a", "1")])
        child_id = blk.children[0].get_id()
        clone = blk.deep_clone()
        self.assertNotEqual(clone.get_id(), blk.get_id())
        self.assertNotEqual(clone.children[0].get_id(), child_id)
        self.assertEqual(
            code_tree.render_program(clone), code_tree.render_program(blk)
        )

    def test_find_and_remove(self):
        a = code_tree.Assignment("a", "1")
        b = code_tree.Assignment("b", "2")
        blk = code_tree.Block([a, b])
        self.assertIs(blk.find_by_id(b.get_id()), b)
        blk.remove_by_id(a.get_id())
        self.assertEqual(len(blk.children), 1)
        self.assertIs(blk.children[0], b)

    def test_var_analysis(self):
        blk = code_tree.Block([
            code_tree.Assignment("a", "1"),
            code_tree.Assignment("b", "a"),
            code_tree.IfBlock(
                "a > 0",
                code_tree.Block([code_tree.Assignment("c", "b")]),
                else_body=code_tree.Block([code_tree.Assignment("b", "c")]),
            ),
        ])
        self.assertEqual(blk.assigned_vars(), ["a", "b", "c"])
        self.assertEqual(blk.required_vars(), ["c"])

        sub = code_tree.Subroutine(
            "foo",
            "",
            decls=code_tree.Block([
                code_tree.Declaration("real", "a", "in"),
                code_tree.Declaration("real", "b"),
            ]),
            body=code_tree.Block([code_tree.Assignment("b", "a")]),
        )
        self.assertEqual(sub.defined_var_names(), ["a", "b"])
        self.assertEqual(sub.assigned_vars(), ["a", "b"])
        self.assertEqual(sub.required_vars(), [])

    def test_prune_for(self):
        blk = code_tree.Block([
            code_tree.Block([
                code_tree.Assignment("a", "2"),
                code_tree.Assignment("c", "a"),
            ]),
            code_tree.Block([
                code_tree.Assignment("a", "1"),
                code_tree.Assignment("b", "a"),
            ]),
        ])
        pruned = blk.prune_for(["b"])
        self.assertEqual(
            code_tree.render_program(pruned),
            "a = 1\n" "b = a\n",
        )

    def test_assignment_accumulate(self):
        # detection without flag
        a = code_tree.Assignment("x_da", "x_da + y")
        self.assertEqual(a.required_vars(["x_da"]), ["y"])

        # explicit accumulate
        b = code_tree.Assignment("x_da", "y", accumulate=True)
        self.assertEqual(
            code_tree.render_program(code_tree.Block([b])),
            "x_da = y + x_da\n",
        )
        self.assertEqual(b.required_vars(["x_da"]), ["y"])

        with self.assertRaises(ValueError):
            code_tree.Assignment("x_da", "x_da + y", accumulate=True)

    def test_remove_initial_self_add(self):
        blk = code_tree.Block([
            code_tree.Assignment("x_da", "x_da + y"),
        ])
        res = blk.remove_initial_self_add("x_da")
        self.assertEqual(res, 2)
        self.assertEqual(code_tree.render_program(blk), "x_da = x_da + y\n")

        blk = code_tree.Block([
            code_tree.Assignment("x_da", "y", accumulate=True),
        ])
        res = blk.remove_initial_self_add("x_da")
        self.assertEqual(res, 3)
        self.assertEqual(code_tree.render_program(blk), "x_da = y\n")

        loop = code_tree.Block([
            code_tree.DoLoop("DO i=1,n", code_tree.Block([
                code_tree.Assignment("x_da", "x_da + y")
            ]))
        ])
        self.assertEqual(loop.remove_initial_self_add("x_da"), 2)
        self.assertEqual(
            code_tree.render_program(loop),
            "DO i=1,n\n  x_da = x_da + y\nEND DO\n",
        )

        cond_blk = code_tree.Block([
            code_tree.IfBlock(
                "a>0",
                code_tree.Block([code_tree.Assignment("x_da", "x_da + a")]),
                else_body=code_tree.Block([
                    code_tree.Assignment("x_da", "x_da + b")
                ]),
            )
        ])
        self.assertEqual(cond_blk.remove_initial_self_add("x_da"), 2)
        self.assertEqual(
            code_tree.render_program(cond_blk),
            "IF (a>0) THEN\n  x_da = x_da + a\nELSE\n  x_da = x_da + b\nEND IF\n",
        )

        cond_blk2 = code_tree.Block([
            code_tree.IfBlock(
                "a>0",
                code_tree.Block([code_tree.Assignment("x_da", "x_da + a")]),
                else_body=code_tree.Block([]),
            )
        ])
        self.assertEqual(cond_blk2.remove_initial_self_add("x_da"), 2)


class TestVariable(unittest.TestCase):
    def test_scalar(self):
        var = code_tree.Variable("x", "real")
        self.assertEqual(var.name, "x")
        self.assertEqual(var.typename, "real")
        self.assertFalse(var.is_array())

    def test_array(self):
        var = code_tree.Variable("a", "real", dimension="(n)")
        self.assertTrue(var.is_array())
        self.assertEqual(var.dimension, "(n)")


if __name__ == "__main__":
    unittest.main()
