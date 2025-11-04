import importlib.util
import sys
from pathlib import Path

import numpy as np


MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "Ulam Spiral"
    / "ulam.py"
)

spec = importlib.util.spec_from_file_location("ulam", MODULE_PATH)
ulam = importlib.util.module_from_spec(spec)
sys.modules.setdefault(spec.name, ulam)
spec.loader.exec_module(ulam)  # type: ignore[attr-defined]


def test_even_size_two_marks_single_prime():
    grid = ulam.generate_ulam_spiral(2)

    expected = np.array([[0, 1], [0, 0]], dtype=np.uint8)
    assert np.array_equal(grid, expected)


def test_even_size_four_marks_primes_within_bounds():
    grid = ulam.generate_ulam_spiral(4)

    expected = np.array(
        [
            [1, 0, 1, 0],
            [0, 0, 1, 1],
            [1, 0, 0, 0],
            [0, 0, 0, 0],
        ],
        dtype=np.uint8,
    )
    assert np.array_equal(grid, expected)
