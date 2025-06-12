import sys
from pathlib import Path
import unittest

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator
from fautodiff import parser
from fautodiff import code_tree
from fautodiff.generator import _collect_names


def _validate_no_undefined(code):
    reader = FortranStringReader(code)
    p = ParserFactory().create(std="f2008")
    ast = p(reader)
    errors = []
    for sub in parser.walk(ast, Fortran2003.Subroutine_Subprogram):
        spec, exec_part = parser._routine_parts(sub)
        decl_map = parser._parse_decls(spec)
        declared = set(decl_map.keys())
        intents = {n: i for n, (_, i) in decl_map.items()}
        assigned = set()
        for stmt in getattr(exec_part, "content", []):
            if not isinstance(stmt, Fortran2003.Assignment_Stmt):
                continue
            lhs = str(stmt.items[0])
            lhs_name = lhs.split('(')[0]
            rhs = stmt.items[2]
            names = []
            _collect_names(rhs, names)
            for name in names:
                base = name.split('(')[0]
                if base not in declared:
                    errors.append(f"{base} used but not declared")
                elif base not in assigned:
                    intent = intents.get(base)
                    if intent == "out":
                        errors.append(f"intent(out) variable {name} used before assignment")
                    elif intent not in ("in", "inout"):
                        errors.append(f"local variable {name} used before assignment")
            assigned.add(lhs_name)
    return errors

def _check_inits(code):
    reader = FortranStringReader(code)
    p = ParserFactory().create(std="f2008")
    ast = p(reader)
    violations = []
    for sub in parser.walk(ast, Fortran2003.Subroutine_Subprogram):
        spec, exec_part = parser._routine_parts(sub)
        decl_map = parser._parse_decls(spec)
        intents = {n: i for n, (_, i) in decl_map.items()}
        inits = set()
        for stmt in getattr(exec_part, "content", []):
            if not isinstance(stmt, Fortran2003.Assignment_Stmt):
                continue
            lhs = str(stmt.items[0])
            lhs_name = lhs.split('(')[0]
            rhs = stmt.items[2].tofortran().strip()
            if rhs == "0.0":
                inits.add(lhs_name)
        for lhs in inits:
            intent = intents.get(lhs)
            if intent not in ("out", None):
                violations.append(f"{lhs} initialized but intent is {intent}")
    return violations

class TestGenerator(unittest.TestCase):
    def test_examples(self):
        examples = Path('examples')
        for src in sorted(examples.glob('*.f90')):
            if src.name.endswith('_ad.f90'):
                continue
            generated = generator.generate_ad(str(src), warn=False)
            expected = src.with_name(src.stem + '_ad.f90').read_text()
            self.assertEqual(generated, expected, msg=f"Mismatch for {src.name}")
            errors = _validate_no_undefined(generated)
            self.assertEqual(errors, [], msg=f"Undefined variables in {src.name}: {errors}")
            violations = _check_inits(generated)
            self.assertEqual(violations, [], msg=f"Unexpected init in {src.name}: {violations}")


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


if __name__ == '__main__':
    unittest.main()
