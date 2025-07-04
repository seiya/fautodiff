import sys
from pathlib import Path
import unittest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from fautodiff.operators import (
    OpInt,
    OpVar,
    OpRange,
    OpFunc
)


class TestOperatorsBasic(unittest.TestCase):
    def test_opint_negative(self):
        self.assertEqual(OpInt(-1), -OpInt(1))

    def test_opint_str_with_kind(self):
        target = OpVar("x", typename="real")
        self.assertEqual(str(OpInt(2, target=target)), "2.0")
        target = OpVar("y", typename="real", kind="4")
        self.assertEqual(str(OpInt(3, target=target)), "3.0")
        target = OpVar("z", typename="real", kind="8")
        self.assertEqual(str(OpInt(4, target=target)), "4.0d0")
        target = OpVar("t", typename="real", kind="RP")
        self.assertEqual(str(OpInt(5, target=target)), "5.0_RP")
        target = OpVar("i", typename="integer")
        self.assertEqual(str(OpInt(6, target=target)), "6")
        target = OpVar("j", typename="integer", kind="4")
        self.assertEqual(str(OpInt(7, target=target)), "7")
        target = OpVar("k", typename="integer", kind="8")
        self.assertEqual(str(OpInt(8, target=target)), "8_8")
        target = OpVar("l", typename="integer", kind="IP")
        self.assertEqual(str(OpInt(9, target=target)), "9_IP")

    def test_opvar_suffix_and_eq(self):
        v = OpVar("a")
        v2 = v.add_suffix("_ad")
        self.assertEqual(str(v2), "a_ad")
        self.assertEqual(v, OpVar("a"))
        self.assertNotEqual(v, OpVar("b"))

    def test_basic_arithmetic_simplify(self):
        x = OpVar("x", typename="real")
        y = OpVar("y")
        z = OpVar("z")
        t = OpVar("t")
        zero = OpInt(0)
        one = OpInt(1)
        none = - one
        two = OpInt(2, target=x)
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

    def test_sub(self):
        n = OpVar("n")
        zero = OpInt(0)
        one = OpInt(1)
        self.assertEqual(n-n, zero)
        self.assertEqual((n+1)-n, one)
        self.assertEqual((n-1)-n, -one)
        self.assertEqual(n-(n-1), one)
        self.assertEqual(n-(n+1), -one)
        self.assertEqual((one-n)+n, one)
        self.assertEqual((-one-n)+n, -one)
        self.assertEqual(n+(one-n), one)
        self.assertEqual(n+(-one-n), -one)

    def test_power_special_cases(self):
        x = OpVar("x")
        self.assertEqual(str(x ** OpInt(0)), "1")
        self.assertEqual(str(x ** OpInt(1)), "x")
        self.assertEqual(str(x ** OpInt(2)), "x**2")
        self.assertEqual(str(x ** OpInt(2) / x), "x")

    def test_derivative_mul(self):
        x = OpVar("x")
        y = OpVar("y")
        expr = x * y
        dx = expr.derivative(x)
        dy = expr.derivative(y)
        self.assertEqual(str(dx), "y")
        self.assertEqual(str(dy), "x")

    def test_derivative_pow(self):
        x = OpVar("x")
        expr = x ** OpInt(2)
        d = expr.derivative(x)
        self.assertEqual(str(d), "2 * x")

    def test_collect_vars(self):
        i = OpVar("i")
        a = OpVar("a", index=[i])
        x = OpVar("x")
        expr = OpFunc("sin", args=[a + OpInt(2)]) * x
        vars = expr.collect_vars()
        self.assertEqual(vars, [a, i, x])

    def test_oprange_str(self):
        one = OpInt(1)
        n = OpVar("n")
        rng = OpRange([one, n])
        self.assertEqual(str(rng), "1:n")
        rng = OpRange(args=[None, n])
        self.assertEqual(str(rng), ":n")
        rng = OpRange(args=[one, None])
        self.assertEqual(str(rng), "1:")
        rng = OpRange(args=[None, None])
        self.assertEqual(str(rng), ":")
        rng = OpRange(args=[None, None, None])
        self.assertEqual(str(rng), ":")
        rng = OpRange(args=[None, None, n])
        self.assertEqual(str(rng), "::n")
        rng = OpRange(args=[one, None, None])
        self.assertEqual(str(rng), "1:")

    def test_oprange_in(self):
        one = OpInt(1)
        two = OpInt(2)
        three = OpInt(3)
        n = OpVar("n")
        rng = OpRange([two, n])
        self.assertFalse(one in rng)
        self.assertTrue(two in rng)
        self.assertTrue(three in rng)
        self.assertTrue(n in rng)
        self.assertTrue(n-1 in rng)
        self.assertFalse(n+1 in rng)


if __name__ == "__main__":
    unittest.main()
