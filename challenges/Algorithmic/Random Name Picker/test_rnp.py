"""Tests for rnp.py random name picker."""

from __future__ import annotations

import json
import os
import tempfile
import unittest

import rnp
import rnp_visualizer as viz


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

    def test_visualizer_probability_summary(self):
        path = self.make_temp_file("Alice,2\nBob,1\nCharlie,1\n")
        cfg = viz.SimulationConfig(
            path=path,
            count=2,
            trials=10,
            with_replacement=True,
            seed=7,
            json_output=True,
            json_indent=2,
            no_show=True,
            save=None,
        )
        result = viz.run_simulation(cfg)

        self.assertEqual(result.names, ["Alice", "Bob", "Charlie"])
        self.assertAlmostEqual(result.normalized_weights[0], 0.5)
        self.assertAlmostEqual(result.normalized_weights[1], 0.25)
        self.assertAlmostEqual(result.normalized_weights[2], 0.25)
        self.assertEqual(result.sample_counts, {"Alice": 13, "Bob": 5, "Charlie": 2})
        self.assertAlmostEqual(result.sample_probabilities[0], 0.65)
        self.assertAlmostEqual(result.sample_probabilities[1], 0.25)
        self.assertAlmostEqual(result.sample_probabilities[2], 0.1)

        payload = json.loads(result.to_json(indent=2))
        self.assertEqual(payload["sample_counts"], result.sample_counts)
        self.assertEqual(payload["sample_probabilities"], result.sample_probabilities)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
