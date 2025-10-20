import importlib.util
import json
from pathlib import Path

import pytest

MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "basic text encoding"
    / "encoding_visualizer.py"
)

spec = importlib.util.spec_from_file_location("encoding_visualizer", MODULE_PATH)
visualizer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(visualizer)


def _bits(binary_strings):
    return [[int(bit) for bit in bits] for bits in binary_strings]


def test_generate_visualization_data_ascii_roundtrip():
    data = visualizer.generate_visualization_data("Hi", encoding="utf-8").to_json_dict()

    assert data["hex_values"] == ["48", "69"]
    assert data["byte_values"] == [72, 105]
    assert data["bit_matrix"] == _bits(["01001000", "01101001"])
    assert data["roundtrip"]["hex_matches"] is True
    assert data["roundtrip"]["bin_matches"] is True

    ascii_comparison = data["comparisons"]
    assert ascii_comparison["supported"] is True
    assert all(entry["delta"] == 0 for entry in ascii_comparison["differences"][:2])

    # Ensure the structure is JSON serialisable for downstream tooling.
    json.dumps(data)


@pytest.mark.parametrize(
    "text, hex_values",
    [
        ("⚡", ["e2", "9a", "a1"]),
        ("é", ["c3", "a9"]),
    ],
)
def test_generate_visualization_data_non_ascii(text, hex_values):
    data = visualizer.generate_visualization_data(text, encoding="utf-8").to_json_dict()

    assert data["hex_values"] == hex_values
    assert data["metadata"]["byte_count"] == len(hex_values)
    assert data["comparisons"]["supported"] is False


def test_utf16_visualization_metadata():
    data = visualizer.generate_visualization_data("Hi", encoding="utf-16").to_json_dict()

    assert data["metadata"]["byte_count"] == len(data["byte_values"])
    assert all(len(row) == 8 for row in data["bit_matrix"])
    assert data["roundtrip"]["hex_matches"] is True
    assert data["roundtrip"]["bin_matches"] is True

    ascii_differences = data["comparisons"]["differences"]
    assert len(ascii_differences) == max(len(data["byte_values"]), 2)
