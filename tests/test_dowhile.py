import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import code_tree, operators


class TestDoWhile(unittest.TestCase):
    def test_basic_render_and_required(self):
        cond = operators.OpVar('cond')
        body = code_tree.Block([
            code_tree.Assignment(operators.OpVar('a'), operators.OpVar('b'))
        ])
        loop = code_tree.DoWhile(body, cond)
        expected = (
            "do while (cond)\n"
            "  a = b\n"
            "end do\n"
        )
        self.assertEqual(code_tree.render_program(loop), expected)
        self.assertEqual([str(v) for v in loop.required_vars()], ['b', 'cond'])

    def test_check_initial(self):
        body = code_tree.Block([
            code_tree.Assignment(operators.OpVar('a'), operators.OpVar('b'))
        ])
        loop = code_tree.DoWhile(body, operators.OpVar('flag'))
        self.assertEqual(loop.check_initial('a'), -1)
        self.assertEqual(loop.check_initial('b'), 0)

    def test_prune_for(self):
        body = code_tree.Block([
            code_tree.Assignment(operators.OpVar('a'), operators.OpVar('b'))
        ])
        loop = code_tree.DoWhile(body, operators.OpVar('flag'))
        pruned = loop.prune_for([operators.OpVar('a')])
        self.assertEqual(code_tree.render_program(pruned), code_tree.render_program(loop))
        pruned2 = loop.prune_for([operators.OpVar('c')])
        self.assertTrue(pruned2.is_effectively_empty())


if __name__ == '__main__':
    unittest.main()
