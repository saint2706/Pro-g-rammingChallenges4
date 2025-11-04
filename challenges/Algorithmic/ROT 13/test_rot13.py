"""Tests for rot13.py."""

from __future__ import annotations

import os
import tempfile
import unittest

import rot13


class TestROT13(unittest.TestCase):
    def test_round_trip(self):
        s = "Hello World!"
        encoded = rot13.rot13(s)
        self.assertEqual(rot13.rot13(encoded), s)

    def test_punctuation_unchanged(self):
        s = "1234!?.,"
        self.assertEqual(rot13.rot13(s), s)

    def test_file_io(self):
        with tempfile.TemporaryDirectory() as td:
            src = os.path.join(td, "in.txt")
            with open(src, "w", encoding="utf-8") as f:
                f.write("abcXYZ")
            # Simulate CLI usage by calling main with args
            rot13.main(["--file", src])  # prints, but we only ensure no error
            self.assertEqual(rot13.rot13("abcXYZ"), "nopKLM")

    def test_invalid_utf8_file(self):
        with tempfile.NamedTemporaryFile(delete=False) as tmp:
            tmp.write(b"\xff\xfe\xff")
            tmp_path = tmp.name
        try:
            cfg = rot13.CLIConfig(file=tmp_path)
            with self.assertRaises(ValueError):
                rot13.resolve_input(cfg)
        finally:
            os.remove(tmp_path)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
