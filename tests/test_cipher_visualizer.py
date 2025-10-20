"""Unit tests for the cipher visualization helper."""
from __future__ import annotations

import json
import sys
from pathlib import Path

VISUALIZER_DIR = (
    Path(__file__).resolve().parents[1]
    / "challenges"
    / "Algorithmic"
    / "Caesar Cipher"
)
if str(VISUALIZER_DIR) not in sys.path:
    sys.path.insert(0, str(VISUALIZER_DIR))

import cipher_visualizer as cv  # type: ignore  # noqa: E402


def _counts_map(freq_section: dict) -> dict:
    return dict(zip(freq_section["alphabet"], freq_section["counts"]))


def test_caesar_mapping_and_frequencies() -> None:
    cfg = cv.VisualizationConfig(cipher="caesar", mode="encrypt", text="ABC", shift=3)
    data = cv.generate_visualization_data(cfg)

    permutation = data["mapping"]["permutation"]
    assert permutation["A"] == "D"
    assert permutation["Z"] == "C"

    output_counts = _counts_map(data["frequencies"]["output"])
    assert output_counts["D"] == 1
    assert output_counts["E"] == 1
    assert output_counts["F"] == 1
    assert sum(output_counts.values()) == data["frequencies"]["output"]["total_letters"]

    json.dumps(data)


def test_vigenere_key_mapping_matrix_dimensions() -> None:
    cfg = cv.VisualizationConfig(
        cipher="vigenere",
        mode="encrypt",
        text="ATTACKATDAWN",
        key="LEMON",
    )
    data = cv.generate_visualization_data(cfg)

    key_mappings = data["mapping"]["key_mappings"]
    assert len(key_mappings) == len(cv.sanitize_key("LEMON"))
    assert key_mappings[0]["mapping"]["A"] == "L"

    matrix = data["mapping"]["matrix"]
    assert len(matrix) == len(cv.ALPHABET)
    assert all(len(row) == len(cv.ALPHABET) for row in matrix)


def test_affine_bruteforce_section_respects_limit() -> None:
    plaintext = "Affine"
    cfg = cv.VisualizationConfig(
        cipher="affine",
        mode="decrypt",
        text=plaintext,
        a=5,
        b=8,
        include_bruteforce=True,
        max_bruteforce_results=3,
    )
    data = cv.generate_visualization_data(cfg)

    brute = data["affine_bruteforce"]
    assert brute["reported"] == 3
    assert brute["total_considered"] == len(cv.VALID_A_VALUES) * 26
    assert len(brute["results"]) == 3


def test_rot13_mapping_is_involutive() -> None:
    cfg = cv.VisualizationConfig(cipher="rot13", mode="encrypt", text="Hello")
    data = cv.generate_visualization_data(cfg)

    permutation = data["mapping"]["permutation"]
    for letter in cv.ALPHABET:
        assert permutation[permutation[letter]] == letter


