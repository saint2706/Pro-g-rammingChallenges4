"""Tests for rnp.py random name picker."""

from __future__ import annotations

import os
import tempfile
import unittest

import rnp


class TestRandomNamePicker(unittest.TestCase):
    def make_temp_file(self, content: str) -> str:
        td = tempfile.mkdtemp()
        path = os.path.join(td, "names.txt")
        with open(path, "w", encoding="utf-8") as f:
            f.write(content)
        return path

    def test_basic_unique_pick(self):
        path = self.make_temp_file("Alice\nBob\nCharlie\n")
        names, weights = rnp.parse_names_file(path)
        self.assertIsNone(weights)
        cfg = rnp.NamePickerConfig(path=path, count=2)
        cfg.validate(len(names))
        rng = rnp.random.Random(123)
        picked = rnp.pick_names(
            names, 2, with_replacement=False, weights=weights, rng=rng
        )
        self.assertEqual(len(picked), 2)
        self.assertEqual(len(set(picked)), 2)

    def test_with_replacement(self):
        path = self.make_temp_file("Alice\nBob\n")
        names, weights = rnp.parse_names_file(path)
        rng = rnp.random.Random(5)
        picked = rnp.pick_names(
            names, 5, with_replacement=True, weights=weights, rng=rng
        )
        self.assertEqual(len(picked), 5)
        self.assertTrue(any(picked.count(n) > 1 for n in names))

    def test_weighted_selection(self):
        path = self.make_temp_file("Alice,10\nBob,1\nCharlie,1\n")
        names, weights = rnp.parse_names_file(path)
        rng = rnp.random.Random(42)
        picked = rnp.pick_names(
            names, 2, with_replacement=False, weights=weights, rng=rng
        )
        # Alice should have higher chance to appear
        self.assertIn("Alice", picked)

    def test_invalid_count(self):
        path = self.make_temp_file("Alice\nBob\n")
        names, _ = rnp.parse_names_file(path)
        cfg = rnp.NamePickerConfig(path=path, count=3)
        with self.assertRaises(ValueError):
            cfg.validate(len(names))

    def test_empty_file_error(self):
        path = self.make_temp_file("")
        with self.assertRaises(ValueError):
            rnp.parse_names_file(path)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
