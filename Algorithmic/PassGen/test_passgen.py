"""Tests for passgen.py password generator."""

from __future__ import annotations

import re
import unittest

import passgen


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


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
