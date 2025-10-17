"""Unit tests for mbr.py parser.

These tests validate the dummy MBR generation and parsing logic.
They avoid relying on any external disk images.
"""

from __future__ import annotations

import json
import os
import tempfile
import unittest
from typing import List

import mbr


class TestMBRParser(unittest.TestCase):
    def test_dummy_creation_and_parse(self):
        with tempfile.TemporaryDirectory() as td:
            path = os.path.join(td, "dummy.bin")
            mbr.create_dummy_mbr_file(path, overwrite=True)
            raw = mbr.read_mbr(path)
            result = mbr.parse_mbr(raw)
            self.assertTrue(result.signature_valid)
            self.assertEqual(2, len(result.partitions))
            p1, p2 = result.partitions
            self.assertTrue(p1.bootable)
            self.assertFalse(p2.bootable)
            self.assertEqual(0x0C, p1.type_code)
            self.assertEqual(0x83, p2.type_code)
            self.assertEqual(2048, p1.start_lba)
            self.assertEqual(1_002_048, p2.start_lba)
            self.assertEqual(1_000_000, p1.sectors)
            self.assertEqual(4_000_000, p2.sectors)

    def test_json_serialization_round_trip(self):
        with tempfile.TemporaryDirectory() as td:
            path = os.path.join(td, "dummy.bin")
            mbr.create_dummy_mbr_file(path, overwrite=True)
            raw = mbr.read_mbr(path)
            result = mbr.parse_mbr(raw)
            data = json.loads(json.dumps(result.to_dict()))
            self.assertIn("signature", data)
            self.assertEqual("0xaa55", data["signature"])
            self.assertEqual(2, len(data["partitions"]))
            self.assertEqual("0x0c", data["partitions"][0]["type_code"])

    def test_invalid_size_raises(self):
        with self.assertRaises(ValueError):
            mbr.parse_mbr(b"not512")

    def test_partition_entry_all_zero_is_none(self):
        # Build a mostly empty MBR with signature only
        m = bytearray(512)
        m[510:512] = (0x55, 0xAA)
        res = mbr.parse_mbr(bytes(m))
        self.assertEqual(0, len(res.partitions))


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
