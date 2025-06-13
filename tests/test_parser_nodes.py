import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import parser
from fautodiff.code_tree import Assignment, Return
from fparser.two import Fortran2003


class TestExecPartToBlock(unittest.TestCase):
    def test_simple_block(self):
        ast = parser.parse_file('examples/simple_math.f90')
        sub = parser.walk(ast, Fortran2003.Subroutine_Subprogram)[0]
        _, exec_part = parser._routine_parts(sub)
        blk = parser.exec_part_to_block(exec_part)
        self.assertEqual(len(blk.children), 3)
        self.assertIsInstance(blk.children[0], Assignment)
        self.assertIsInstance(blk.children[1], Assignment)
        self.assertIsInstance(blk.children[2], Return)

    def test_block_roundtrip(self):
        ast = parser.parse_file('examples/simple_math.f90')
        sub = parser.walk(ast, Fortran2003.Subroutine_Subprogram)[0]
        _, exec_part = parser._routine_parts(sub)
        blk = parser.exec_part_to_block(exec_part)
        new_exec = parser.block_to_exec_part(blk)
        self.assertIsNotNone(new_exec)
        self.assertEqual(len(new_exec.content), len(exec_part.content))


if __name__ == '__main__':
    unittest.main()
