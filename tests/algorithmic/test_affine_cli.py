"""Tests for the Rumkin affine cipher CLI helpers."""

from __future__ import annotations

import sys
from pathlib import Path

import pytest


AFFINE_DIR = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "Rumkin Ciphers"
)
if str(AFFINE_DIR) not in sys.path:
    sys.path.insert(0, str(AFFINE_DIR))

import affine  # type: ignore  # noqa: E402


@pytest.mark.parametrize("bad_b", [-1, 26, 99])
def test_cli_rejects_out_of_range_key_b(
    bad_b: int, capsys: pytest.CaptureFixture[str]
) -> None:
    exit_code = affine.main(
        [
            "--mode",
            "encrypt",
            "-a",
            "5",
            "-b",
            str(bad_b),
            "--text",
            "abc",
        ]
    )
    err = capsys.readouterr().err
    assert exit_code == 1
    assert "Key b must satisfy 0 <= b < 26" in err
