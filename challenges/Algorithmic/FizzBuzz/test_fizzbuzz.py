"""Unit tests for modernized fizzbuzz implementation."""

from __future__ import annotations

import json
import unittest

import fizzbuzz as fb
import fizzbuzz_visualizer as viz


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


class TestFizzBuzzVisualizer(unittest.TestCase):
    def test_metadata_matches_stream(self):
        entries = viz.generate_rule_metadata(6)
        emitted_outputs = [entry.output for entry in entries if entry.emitted]
        self.assertEqual(emitted_outputs, list(fb.fizzbuzz_stream(6)))

        third = entries[2]
        self.assertEqual(third.value, 3)
        self.assertEqual(third.applied_rules, ("Fizz",))
        self.assertTrue(third.emitted)

        fifth = entries[4]
        self.assertEqual(fifth.value, 5)
        self.assertEqual(fifth.applied_rules, ("Buzz",))

    def test_metadata_with_number_suppression(self):
        rules = (
            fb.FizzBuzzRule(2, "Foo"),
            fb.FizzBuzzRule(3, "Bar"),
        )

        entries = viz.generate_rule_metadata(6, rules=rules, include_numbers=False)
        emitted_values = [entry.value for entry in entries if entry.emitted]
        self.assertEqual(emitted_values, [2, 3, 4, 6])
        emitted_outputs = [entry.output for entry in entries if entry.emitted]
        self.assertEqual(
            emitted_outputs, list(fb.fizzbuzz_stream(6, rules, include_numbers=False))
        )

        combined = entries[5]
        self.assertEqual(combined.value, 6)
        self.assertEqual(combined.applied_rules, ("Foo", "Bar"))
        self.assertTrue(combined.emitted)

    def test_metadata_rejects_invalid_limit(self):
        with self.assertRaises(ValueError):
            viz.generate_rule_metadata(0)


if __name__ == "__main__":  # pragma: no cover
    unittest.main(verbosity=2)
