"""Tests for passgen.py password generator and analytics visualiser."""

from __future__ import annotations

import json
import random
import re
import unittest
from unittest import mock

import passgen
import passgen_visualizer


class TestPasswordGenerator(unittest.TestCase):
    def test_basic_generation_letters_digits(self):
        spec = passgen.PasswordSpec(length=16, letters=True, digits=True, symbols=False)
        pw = passgen.generate_passwords(spec)[0]
        self.assertEqual(len(pw), 16)
        self.assertRegex(pw, r"[A-Za-z]")
        self.assertRegex(pw, r"[0-9]")

    def test_symbols_included(self):
        spec = passgen.PasswordSpec(length=20, letters=True, digits=False, symbols=True)
        pw = passgen.generate_passwords(spec)[0]
        self.assertRegex(pw, r"[A-Za-z]")
        self.assertRegex(pw, r"[^A-Za-z0-9]")

    def test_ambiguous_removed(self):
        spec = passgen.PasswordSpec(
            length=30, letters=True, digits=True, symbols=False, exclude_ambiguous=True
        )
        pw = passgen.generate_passwords(spec)[0]
        self.assertNotRegex(pw, r"[O0l1I|`'\"{}\[\]()\\/]")

    def test_entropy_estimate(self):
        spec = passgen.PasswordSpec(length=12, letters=True, digits=True, symbols=False)
        pool = passgen.build_pool(spec)
        ent = passgen.estimate_entropy_bits(len(pool), spec.length)
        self.assertGreater(ent, 12 * 4)  # > 4 bits per char threshold

    def test_invalid_length(self):
        spec = passgen.PasswordSpec(length=4, letters=True)
        with self.assertRaises(ValueError):
            spec.validate()

    def test_empty_pool(self):
        spec = passgen.PasswordSpec(
            length=12, letters=False, digits=False, symbols=False
        )
        with self.assertRaises(ValueError):
            spec.validate()


class TestVisualizerSummary(unittest.TestCase):
    def test_summary_json_deterministic(self):
        spec = passgen.PasswordSpec(
            length=8, letters=True, digits=True, symbols=False, count=2
        )

        rng = random.Random(123)

        def deterministic_choice(seq):
            return seq[rng.randrange(len(seq))]

        class DeterministicSystemRandom:
            def shuffle(self, seq):
                rng.shuffle(seq)

        with mock.patch.object(
            passgen.secrets, "choice", side_effect=deterministic_choice
        ), mock.patch.object(
            passgen.secrets, "SystemRandom", return_value=DeterministicSystemRandom()
        ):
            summary = passgen_visualizer.summarize_generation(spec, batches=2)

        expected_frequencies = {
            "1": 2,
            "2": 1,
            "4": 1,
            "5": 1,
            "7": 2,
            "A": 1,
            "H": 1,
            "J": 1,
            "Q": 1,
            "S": 1,
            "X": 2,
            "c": 2,
            "d": 1,
            "f": 2,
            "g": 2,
            "j": 1,
            "k": 2,
            "p": 1,
            "q": 1,
            "r": 1,
            "t": 1,
            "u": 1,
            "v": 3,
        }

        self.assertEqual(summary["character_frequencies"], expected_frequencies)
        self.assertEqual(
            summary["category_frequencies"], {"letters": 25, "digits": 7, "symbols": 0}
        )
        self.assertAlmostEqual(summary["estimated_entropy_bits"], 47.6336)
        self.assertAlmostEqual(summary["baseline_entropy_bits"], 52.4367)

        payload = json.loads(passgen_visualizer.summary_to_json(summary, indent=0))
        self.assertEqual(payload["character_frequencies"], expected_frequencies)
        self.assertAlmostEqual(payload["estimated_entropy_bits"], 47.6336)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
