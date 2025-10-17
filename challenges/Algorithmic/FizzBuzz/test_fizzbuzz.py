"""Unit tests for modernized fizzbuzz implementation."""

from __future__ import annotations

import json
import unittest
from typing import List

import fizzbuzz as fb


class TestFizzBuzzCore(unittest.TestCase):
    def test_default_first_15(self):
        got = list(fb.fizzbuzz_stream(15))
        self.assertEqual(
            got,
            [
                "1",
                "2",
                "Fizz",
                "4",
                "Buzz",
                "Fizz",
                "7",
                "8",
                "Fizz",
                "Buzz",
                "11",
                "Fizz",
                "13",
                "14",
                "FizzBuzz",
            ],
        )

    def test_custom_rule_addition(self):
        rules = (
            fb.FizzBuzzRule(2, "Foo"),
            fb.FizzBuzzRule(3, "Bar"),
        )
        got = list(fb.fizzbuzz_stream(6, rules))
        self.assertEqual(got, ["1", "Foo", "Bar", "Foo", "5", "FooBar"])

    def test_no_numbers(self):
        rules = (fb.FizzBuzzRule(2, "X"),)
        got = list(fb.fizzbuzz_stream(5, rules, include_numbers=False))
        self.assertEqual(got, ["X", "X"])

    def test_run_plain(self):
        out = fb.run(5, rules=None, fmt="plain")
        self.assertIn("Fizz", out)

    def test_run_json(self):
        out = fb.run(5, fmt="json")
        data = json.loads(out)
        self.assertEqual(data[2], "Fizz")

    def test_invalid_limit(self):
        with self.assertRaises(ValueError):
            list(fb.fizzbuzz_stream(0))

    def test_invalid_rule(self):
        with self.assertRaises(ValueError):
            fb.FizzBuzzRule(0, "Bad")


if __name__ == "__main__":  # pragma: no cover
    unittest.main(verbosity=2)
