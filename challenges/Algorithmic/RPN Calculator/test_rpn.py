"""Tests for enhanced RPN evaluator."""

from __future__ import annotations

import unittest

import postifx_evaluator as rpn


class TestRPNEvaluator(unittest.TestCase):
    def eval(self, expr: str):
        return rpn.evaluate_rpn(expr)

    def test_add_mul(self):
        self.assertEqual(self.eval("3 4 + 2 *"), 14)

    def test_power_and_floor(self):
        self.assertEqual(self.eval("2 3 ^ 5 //"), (2**3) // 5)

    def test_unary_neg(self):
        self.assertEqual(self.eval("5 neg"), -5)

    def test_factorial(self):
        self.assertEqual(self.eval("5 !"), 120)

    def test_constants(self):
        res = self.eval("pi e +")
        self.assertAlmostEqual(res, 3.141592653589793 + 2.718281828459045, places=9)

    def test_trig_degrees(self):
        cfg = rpn.EvalConfig(degrees=True)
        self.assertAlmostEqual(rpn.evaluate_rpn("90 sin", config=cfg), 1.0, places=6)

    def test_error_unknown(self):
        with self.assertRaises(rpn.RPNError):
            self.eval("2 2 foo")

    def test_error_div_zero(self):
        with self.assertRaises(rpn.RPNError):
            self.eval("3 0 /")

    def test_error_sqrt_negative(self):
        with self.assertRaises(rpn.RPNError):
            self.eval("-1 sqrt")

    def test_error_log_zero(self):
        with self.assertRaises(rpn.RPNError):
            self.eval("0 log")

    def test_malformed(self):
        with self.assertRaises(rpn.RPNError):
            self.eval("1 2 + 3")


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
