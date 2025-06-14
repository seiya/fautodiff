import sys
from pathlib import Path
import unittest

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import generator
from fautodiff import parser


def _collect_names(expr, names, unique=True):
    """Collect variable names found in ``expr`` preserving order."""
    if isinstance(expr, (Fortran2003.Intrinsic_Function_Reference, Fortran2003.Part_Ref)):
        name = expr.items[0].tofortran().lower()
        if name in DERIVATIVE_TEMPLATES:
            args = expr.items[1]
            for arg in getattr(args, "items", []):
                if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                    subexpr = arg.items[1]
                else:
                    subexpr = arg
                _collect_names(subexpr, names, unique=unique)
            return
        if isinstance(expr, Fortran2003.Part_Ref):
            if unique:
                if name not in names:
                    names.append(name)
            else:
                names.append(name)
            # also collect index variable names
            for arg in getattr(expr.items[1], "items", []):
                if isinstance(arg, Fortran2003.Actual_Arg_Spec):
                    subexpr = arg.items[1]
                else:
                    subexpr = arg
                _collect_names(subexpr, names, unique=unique)
            return
    if isinstance(expr, Fortran2003.Name):
        name = str(expr)
        if unique:
            if name not in names:
                names.append(name)
        else:
            names.append(name)
    for item in getattr(expr, "items", []):
        if not isinstance(item, str):
            _collect_names(item, names, unique=unique)


class TestGenerator(unittest.TestCase):
    def test_examples(self):
        examples = Path('examples')
        for src in sorted(examples.glob('*.f90')):
            if src.name.endswith('_ad.f90'):
                continue
            generated = generator.generate_ad(str(src), warn=False)
            expected = src.with_name(src.stem + '_ad.f90').read_text()
            self.assertEqual(generated, expected, msg=f"Mismatch for {src.name}")


if __name__ == '__main__':
    unittest.main()
