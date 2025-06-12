import sys
from pathlib import Path
import unittest

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator
from fautodiff import parser
from fautodiff.generator import _collect_names, _parse_decls, _routine_parts


def _validate_no_undefined(code):
    reader = FortranStringReader(code)
    p = ParserFactory().create(std="f2008")
    ast = p(reader)
    errors = []
    for sub in parser.walk(ast, Fortran2003.Subroutine_Subprogram):
        spec, exec_part = _routine_parts(sub)
        decl_map = _parse_decls(spec)
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
        spec, exec_part = _routine_parts(sub)
        decl_map = _parse_decls(spec)
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

if __name__ == '__main__':
    unittest.main()
