import struct

import pytest

from challenges.Algorithmic.MBR import mbr, mbr_visualizer


def _build_mbr(partitions):
    data = bytearray(mbr.MBR_SIZE)
    for index, (status, type_code, start_lba, sectors) in enumerate(partitions):
        entry = struct.pack(
            "<B3sB3sII",
            status,
            b"\x00\x00\x00",
            type_code,
            b"\x00\x00\x00",
            start_lba,
            sectors,
        )
        start = mbr.PARTITION_TABLE_OFFSET + index * mbr.PARTITION_ENTRY_SIZE
        data[start : start + mbr.PARTITION_ENTRY_SIZE] = entry
    data[mbr.MBR_SIGNATURE_OFFSET : mbr.MBR_SIGNATURE_OFFSET + 2] = struct.pack(
        "<H", mbr.MBR_SIGNATURE
    )
    return bytes(data)


def test_dummy_layout_matches_expected_ratios():
    raw = _build_mbr(
        [
            (0x80, 0x0C, 2048, 1_000_000),
            (0x00, 0x83, 1_002_048, 4_000_000),
        ]
    )
    metadata = mbr_visualizer.describe_mbr(raw)

    total = 5_002_048
    assert metadata["disk_sectors"] == total

    kinds = [segment["kind"] for segment in metadata["segments"]]
    assert kinds == ["gap", "partition", "partition"]

    gap, part1, part2 = metadata["segments"]

    assert gap["sectors"] == 2048
    assert gap["length_ratio"] == pytest.approx(2048 / total)

    assert part1["start_lba"] == 2048
    assert part1["bootable"] is True
    assert part1["type_description"] == "FAT32 LBA"
    assert part1["length_ratio"] == pytest.approx(1_000_000 / total)
    assert part1["start_ratio"] == pytest.approx(2048 / total)
    assert part1["end_ratio"] == pytest.approx((2048 + 1_000_000) / total)

    assert part2["start_lba"] == 1_002_048
    assert part2["bootable"] is False
    assert part2["type_description"] == "Linux Filesystem"
    assert part2["length_ratio"] == pytest.approx(4_000_000 / total)
    assert part2["start_ratio"] == pytest.approx(1_002_048 / total)
    assert part2["end_ratio"] == pytest.approx(total / total)

    assert sum(
        segment["length_ratio"] for segment in metadata["segments"]
    ) == pytest.approx(1.0)


def test_segments_include_internal_gap_and_ordering():
    raw = _build_mbr(
        [
            (0x00, 0x07, 63, 100),
            (0x00, 0x07, 400, 50),
        ]
    )
    metadata = mbr_visualizer.describe_mbr(raw)

    segments = metadata["segments"]
    total = 450
    assert metadata["disk_sectors"] == total

    # Expect: leading gap (0-63), partition #1 (63-163), middle gap (163-400), partition #2 (400-450)
    expected_sectors = [63, 100, 237, 50]
    assert [seg["sectors"] for seg in segments] == expected_sectors

    # Ratios should follow the same ordering and sum to ~1.0
    ratios = [seg["length_ratio"] for seg in segments]
    assert sum(ratios) == pytest.approx(1.0)
    assert ratios[0] == pytest.approx(63 / total)
    assert ratios[2] == pytest.approx(237 / total)

    # Verify partition start positions are correctly normalised.
    first_partition = segments[1]
    second_partition = segments[3]
    assert first_partition["start_ratio"] == pytest.approx(63 / total)
    assert second_partition["start_ratio"] == pytest.approx(400 / total)
    assert second_partition["end_ratio"] == pytest.approx(450 / total)
