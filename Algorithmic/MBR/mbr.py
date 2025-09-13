"""mbr.py - Educational Master Boot Record (MBR) partition table parser.

This module provides a small, well‑documented parser for the 512‑byte
Master Boot Record sector found at the beginning of traditional MBR
partitioned disks. It focuses on clarity and approachability for new
developers while still demonstrating robust, testable design.

Features
========
* Dataclass models for partition entries and overall MBR parse result
* Safe parsing with validation & helpful error messages
* CHS (Cylinder / Head / Sector) decoding (legacy – informational only)
* Human friendly size formatting and optional JSON output
* Simple CLI built with argparse
* Utility to create a reproducible dummy MBR for demonstrations / tests

Example (human readable):
    python mbr.py --file disk.img

Example (JSON):
    python mbr.py --file disk.img --json

Create and parse a dummy image:
    python mbr.py --create-dummy demo_mbr.bin --json

Programmatic usage:
    from mbr import read_mbr, parse_mbr
    raw = read_mbr('disk.img')
    result = parse_mbr(raw)
    for p in result.partitions:
        print(p.index, p.type_description, p.size_human)

This code intentionally stays within the Python standard library.
"""

from __future__ import annotations

import argparse
import json
import os
import struct
import sys
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Tuple

SECTOR_BYTES_DEFAULT = 512
MBR_SIZE = 512
PARTITION_TABLE_OFFSET = 446
PARTITION_ENTRY_SIZE = 16
PARTITION_ENTRY_COUNT = 4
MBR_SIGNATURE_OFFSET = 510
MBR_SIGNATURE = 0xAA55

# Common partition type codes (not exhaustive – extend as needed)
PARTITION_TYPE_MAP: Dict[int, str] = {
    0x00: "Unused",
    0x01: "FAT12",
    0x04: "FAT16 <32M",
    0x05: "Extended",
    0x06: "FAT16",
    0x07: "NTFS/ExFAT/HPFS",
    0x0B: "FAT32",
    0x0C: "FAT32 LBA",
    0x0E: "FAT16 LBA",
    0x0F: "Extended LBA",
    0x82: "Linux Swap",
    0x83: "Linux Filesystem",
    0x84: "Hibernate",
    0x85: "Linux Extended",
    0x8E: "Linux LVM",
    0xA5: "FreeBSD",
    0xA8: "macOS UFS",
    0xAB: "macOS Boot",
    0xAF: "macOS HFS/HFS+",
    0xEE: "GPT Protective",
    0xEF: "EFI System",
}


def _decode_chs(triple: bytes) -> Tuple[int, int, int]:
    """Decode 3 CHS bytes into (cylinder, head, sector).

    CHS layout (legacy):
        Byte 0: Head (0–255)
        Byte 1: Sector (bits 0–5, 1–63) | high 2 bits of Cylinder (bits 6–7)
        Byte 2: Low 8 bits of Cylinder
    """
    if len(triple) != 3:
        return (0, 0, 0)
    head = triple[0]
    sector = triple[1] & 0x3F  # lower 6 bits
    cylinder = ((triple[1] & 0xC0) << 2) | triple[2]
    return (cylinder, head, sector)


def _human_size(num_bytes: int) -> str:
    """Return a human friendly size string using binary prefixes."""
    if num_bytes <= 0:
        return "0 B"
    units = ["B", "KiB", "MiB", "GiB", "TiB"]
    value = float(num_bytes)
    for unit in units:
        if value < 1024 or unit == units[-1]:
            return f"{value:.2f} {unit}"
        value /= 1024
    return f"{value:.2f} {units[-1]}"  # fallback (unreachable)


@dataclass(slots=True)
class PartitionEntry:
    """Represents a single 16‑byte MBR partition table entry."""

    index: int
    bootable: bool
    status_raw: int
    type_code: int
    start_chs: Tuple[int, int, int]
    end_chs: Tuple[int, int, int]
    start_lba: int
    sectors: int
    sector_bytes: int = SECTOR_BYTES_DEFAULT

    @property
    def size_bytes(self) -> int:
        return self.sectors * self.sector_bytes

    @property
    def size_human(self) -> str:
        return _human_size(self.size_bytes)

    @property
    def type_description(self) -> str:
        return PARTITION_TYPE_MAP.get(self.type_code, "Unknown")

    def to_dict(self) -> Dict[str, object]:
        return {
            "index": self.index,
            "bootable": self.bootable,
            "status_raw": f"0x{self.status_raw:02x}",
            "type_code": f"0x{self.type_code:02x}",
            "type_description": self.type_description,
            "start_chs": {
                "cylinder": self.start_chs[0],
                "head": self.start_chs[1],
                "sector": self.start_chs[2],
            },
            "end_chs": {
                "cylinder": self.end_chs[0],
                "head": self.end_chs[1],
                "sector": self.end_chs[2],
            },
            "start_lba": self.start_lba,
            "sectors": self.sectors,
            "size_bytes": self.size_bytes,
            "size_human": self.size_human,
        }


@dataclass(slots=True)
class MBRParseResult:
    """Container for overall MBR parse outcome."""

    signature: int
    signature_valid: bool
    partitions: List[PartitionEntry]
    sector_bytes: int = SECTOR_BYTES_DEFAULT

    def to_dict(self) -> Dict[str, object]:
        return {
            "signature": f"0x{self.signature:04x}",
            "signature_valid": self.signature_valid,
            "sector_bytes": self.sector_bytes,
            "partitions": [p.to_dict() for p in self.partitions],
        }


def parse_partition_entry(
    entry_bytes: bytes, index: int, sector_bytes: int = SECTOR_BYTES_DEFAULT
) -> Optional[PartitionEntry]:
    """Parse a single 16‑byte partition entry.

    Returns None for an all‑zero (unused) entry.
    """
    if len(entry_bytes) != PARTITION_ENTRY_SIZE:
        raise ValueError("Partition entry must be 16 bytes")
    if all(b == 0 for b in entry_bytes):
        return None

    status = entry_bytes[0]
    start_chs = _decode_chs(entry_bytes[1:4])
    type_code = entry_bytes[4]
    end_chs = _decode_chs(entry_bytes[5:8])
    start_lba, sectors = struct.unpack_from("<II", entry_bytes, 8)

    return PartitionEntry(
        index=index,
        bootable=(status == 0x80),
        status_raw=status,
        type_code=type_code,
        start_chs=start_chs,
        end_chs=end_chs,
        start_lba=start_lba,
        sectors=sectors,
        sector_bytes=sector_bytes,
    )


def parse_mbr(
    mbr_data: bytes, *, sector_bytes: int = SECTOR_BYTES_DEFAULT
) -> MBRParseResult:
    """Parse raw 512‑byte MBR sector into structured result."""
    if len(mbr_data) != MBR_SIZE:
        raise ValueError(f"MBR must be exactly {MBR_SIZE} bytes; got {len(mbr_data)}")

    signature = struct.unpack_from("<H", mbr_data, MBR_SIGNATURE_OFFSET)[0]
    signature_valid = signature == MBR_SIGNATURE

    table = mbr_data[
        PARTITION_TABLE_OFFSET : PARTITION_TABLE_OFFSET
        + PARTITION_ENTRY_COUNT * PARTITION_ENTRY_SIZE
    ]
    partitions: List[PartitionEntry] = []
    for i in range(PARTITION_ENTRY_COUNT):
        start = i * PARTITION_ENTRY_SIZE
        entry_bytes = table[start : start + PARTITION_ENTRY_SIZE]
        part = parse_partition_entry(entry_bytes, i + 1, sector_bytes)
        if part is not None:
            partitions.append(part)

    return MBRParseResult(
        signature=signature,
        signature_valid=signature_valid,
        partitions=partitions,
        sector_bytes=sector_bytes,
    )


def read_mbr(path: str) -> bytes:
    """Read 512 bytes from a file path.

    Raises FileNotFoundError / IOError on failure.
    """
    with open(path, "rb") as f:
        data = f.read(MBR_SIZE)
    if len(data) != MBR_SIZE:
        raise IOError(
            f"File '{path}' shorter than {MBR_SIZE} bytes; cannot be a full MBR sector"
        )
    return data


def create_dummy_mbr_file(filepath: str, *, overwrite: bool = False) -> None:
    """Create a deterministic dummy MBR with two sample partitions.

    Partition layout:
        #1 bootable FAT32 LBA starting at LBA 2048, size 1,000,000 sectors (~476.84 MiB)
        #2 non‑bootable Linux filesystem starting at 1,002,048, size 4,000,000 sectors (~1.91 GiB)
    """
    if os.path.exists(filepath) and not overwrite:
        raise FileExistsError(
            f"File '{filepath}' already exists. Pass --force to overwrite."
        )
    mbr = bytearray(MBR_SIZE)

    # Helper to build a partition entry; CHS fields left zero for simplicity.
    def build(status: int, type_code: int, start_lba: int, sectors: int) -> bytes:
        return struct.pack(
            "<B3sB3sII",
            status,
            b"\x00\x00\x00",
            type_code,
            b"\x00\x00\x00",
            start_lba,
            sectors,
        )

    part1 = build(0x80, 0x0C, 2048, 1_000_000)
    part2 = build(0x00, 0x83, 1_002_048, 4_000_000)

    mbr[PARTITION_TABLE_OFFSET : PARTITION_TABLE_OFFSET + PARTITION_ENTRY_SIZE] = part1
    mbr[
        PARTITION_TABLE_OFFSET
        + PARTITION_ENTRY_SIZE : PARTITION_TABLE_OFFSET
        + 2 * PARTITION_ENTRY_SIZE
    ] = part2
    mbr[MBR_SIGNATURE_OFFSET : MBR_SIGNATURE_OFFSET + 2] = struct.pack(
        "<H", MBR_SIGNATURE
    )

    with open(filepath, "wb") as f:
        f.write(mbr)


def _print_human(result: MBRParseResult, *, show_empty: bool = False) -> None:
    print(
        f"MBR Signature: 0x{result.signature:04x} {'(valid)' if result.signature_valid else '(INVALID)'}"
    )
    if not result.partitions and not show_empty:
        print("No partitions present.")
        return
    print("\nPartition Table:")
    if not result.partitions and show_empty:
        for i in range(1, PARTITION_ENTRY_COUNT + 1):
            print(f"  [Entry {i}] <empty>")
        return
    for p in result.partitions:
        print(
            f"  [Entry {p.index}] {'BOOT ' if p.bootable else '     '}Type {p.type_description} ({p.type_code:#04x})"
        )
        print(
            f"      Start LBA: {p.start_lba:,}  Sectors: {p.sectors:,}  Size: {p.size_human}"
        )
        cyl_s, head_s, sec_s = p.start_chs
        cyl_e, head_e, sec_e = p.end_chs
        if any(
            [cyl_s, head_s, sec_s, cyl_e, head_e, sec_e]
        ):  # only show if not all zero
            print(
                f"      CHS: start (C={cyl_s} H={head_s} S={sec_s}) end (C={cyl_e} H={head_e} S={sec_e})"
            )


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Parse an MBR (Master Boot Record) sector."
    )
    parser.add_argument(
        "--file",
        "-f",
        help="Path to disk image containing an MBR (default: dummy_mbr.bin)",
    )
    parser.add_argument(
        "--json", action="store_true", help="Output JSON instead of human readable text"
    )
    parser.add_argument(
        "--show-empty",
        action="store_true",
        help="Show empty entries explicitly in human readable output",
    )
    parser.add_argument(
        "--create-dummy",
        metavar="PATH",
        help="Create a dummy MBR at PATH and exit (unless --file also used to parse)",
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Overwrite when using --create-dummy if file exists",
    )
    parser.add_argument(
        "--sector-size",
        type=int,
        default=SECTOR_BYTES_DEFAULT,
        help="Logical sector size in bytes (default 512)",
    )
    return parser


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)

    # Optional dummy creation
    if args.create_dummy:
        try:
            create_dummy_mbr_file(args.create_dummy, overwrite=args.force)
            print(f"Created dummy MBR at '{args.create_dummy}'", file=sys.stderr)
        except FileExistsError as e:
            print(e, file=sys.stderr)
            return 1
        # If only creating dummy and no file specified, parse that
        if not args.file:
            args.file = args.create_dummy

    path = args.file or "dummy_mbr.bin"
    if not os.path.exists(path):
        # Auto-generate dummy if using default path and absent.
        if path == "dummy_mbr.bin":
            try:
                create_dummy_mbr_file(path, overwrite=True)
                print("Generated default dummy_mbr.bin", file=sys.stderr)
            except Exception as e:  # pragma: no cover - very unlikely
                print(f"Failed to create dummy MBR: {e}", file=sys.stderr)
                return 1
        else:
            print(f"File not found: {path}", file=sys.stderr)
            return 1

    try:
        raw = read_mbr(path)
        result = parse_mbr(raw, sector_bytes=args.sector_size)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    if args.json:
        print(json.dumps(result.to_dict(), indent=2))
    else:
        _print_human(result, show_empty=args.show_empty)
    return 0 if result.signature_valid else 2


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
