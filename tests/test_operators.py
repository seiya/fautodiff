import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff import operators


class TestOperatorsBasic(unittest.TestCase):
    def test_opint_negative(self):
        self.assertEqual(operators.OpInt(-1), -operators.OpInt(1))

    def test_opint_str_with_kind(self):
        target = operators.OpVar("x", is_real=True)
        self.assertEqual(str(operators.OpInt(2, target=target)), "2.0")
        target = operators.OpVar("y", is_real=True, kind="4")
        self.assertEqual(str(operators.OpInt(3, target=target)), "3.0")
        target = operators.OpVar("z", is_real=True, kind="8")
        self.assertEqual(str(operators.OpInt(4, target=target)), "4.0d0")
        target = operators.OpVar("t", is_real=True, kind="RP")
        self.assertEqual(str(operators.OpInt(5, target=target)), "5.0_RP")
        target = operators.OpVar("i", is_real=False)
        self.assertEqual(str(operators.OpInt(6, target=target)), "6")
        target = operators.OpVar("j", is_real=False, kind="4")
        self.assertEqual(str(operators.OpInt(7, target=target)), "7")
        target = operators.OpVar("k", is_real=False, kind="8")
        self.assertEqual(str(operators.OpInt(8, target=target)), "8_8")
        target = operators.OpVar("l", is_real=False, kind="IP")
        self.assertEqual(str(operators.OpInt(9, target=target)), "9_IP")

    def test_opvar_suffix_and_eq(self):
        v = operators.OpVar("a")
        v2 = v.add_suffix("_ad")
        self.assertEqual(str(v2), "a_ad")
        self.assertEqual(v, operators.OpVar("a"))
        self.assertNotEqual(v, operators.OpVar("b"))

    def test_basic_arithmetic_simplify(self):
        x = operators.OpVar("x", is_real=True)
        y = operators.OpVar("y")
        z = operators.OpVar("z")
        t = operators.OpVar("t")
        zero = operators.OpInt(0)
        one = operators.OpInt(1)
        none = - one
        two = operators.OpInt(2, target=x)
        self.assertIs(x + zero, x)
        self.assertIs(zero + x, x)
        self.assertIs(x - zero, x)
        self.assertEqual(str(zero - x), str(-x))
        self.assertIs(x * one, x)
        self.assertIs(one * x, x)
        self.assertEqual(x * none, -x)
        self.assertEqual(none * x, -x)
        self.assertIs(-(-x), x)
        self.assertEqual(str(x * zero), "0")
        self.assertEqual(str(zero * x), "0")
        self.assertEqual(str(x / one), str(x))
        self.assertEqual(str(zero / x), "0")
        self.assertEqual(str(one / two), "1.0 / 2.0")
        self.assertEqual(str(two * (x + two)), "2.0 * (x + 2.0)")
        self.assertEqual(str(two * (x * two)), "4.0 * x")
        self.assertEqual(str(two * (x * y)), "2.0 * x * y")
        self.assertEqual(str((x/y) / z), "x / (y * z)")
        self.assertEqual(str((x/y) / (z/t)), "x * t / (y * z)")

    def test_power_special_cases(self):
        x = operators.OpVar("x")
        self.assertEqual(str(x ** operators.OpInt(0)), "1")
        self.assertEqual(str(x ** operators.OpInt(1)), "x")
        self.assertEqual(str(x ** operators.OpInt(2)), "x**2")
        self.assertEqual(str(x ** operators.OpInt(2) / x), "x")

    def test_derivative_mul(self):
        x = operators.OpVar("x")
        y = operators.OpVar("y")
        expr = x * y
        dx = expr.derivative(x)
        dy = expr.derivative(y)
        self.assertEqual(str(dx), "y")
        self.assertEqual(str(dy), "x")

    def test_derivative_pow(self):
        x = operators.OpVar("x")
        expr = x ** operators.OpInt(2)
        d = expr.derivative(x)
        self.assertEqual(str(d), "2 * x")

    def test_collect_vars(self):
        i = operators.OpVar("i")
        a = operators.OpVar("a", index=[i])
        x = operators.OpVar("x")
        expr = operators.OpFunc("sin", args=[a + operators.OpInt(2)]) * x
        vars = expr.collect_vars()
        self.assertEqual(vars, [a, i, x])

    def test_oprange_str(self):
        one = operators.OpInt(1)
        n = operators.OpVar("n")
        rng = operators.OpRange(args=[one, n])
        self.assertEqual(str(rng), "1:n")
        rng = operators.OpRange(args=[None, n])
        self.assertEqual(str(rng), ":n")
        rng = operators.OpRange(args=[one, None])
        self.assertEqual(str(rng), "1:")
        rng = operators.OpRange(args=[None, None])
        self.assertEqual(str(rng), ":")
        rng = operators.OpRange(args=[None, None, None])
        self.assertEqual(str(rng), ":")
        rng = operators.OpRange(args=[None, None, n])
        self.assertEqual(str(rng), "::n")
        rng = operators.OpRange(args=[one, None, None])
        self.assertEqual(str(rng), "1:")


if __name__ == "__main__":
    unittest.main()
