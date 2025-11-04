"""Tests for the Character Counter challenge utilities."""

from __future__ import annotations

import importlib.util
from collections import Counter
from pathlib import Path


MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "Character Counter"
    / "charcount.py"
)


def load_module(name: str):
    spec = importlib.util.spec_from_file_location(name, MODULE_PATH)
    if spec is None or spec.loader is None:  # pragma: no cover - defensive guard
        raise ImportError(f"Unable to load module from {MODULE_PATH}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


charcount = load_module("charcount_module")


def test_case_sensitive_counts_preserve_original_characters():
    result = charcount.get_char_counts("AaA")
    assert result == Counter({"A": 2, "a": 1})


def test_case_insensitive_counts_use_casefolding_for_sharp_s():
    result = charcount.get_char_counts("Straße", case_sensitive=False)
    assert result == Counter({"s": 3, "t": 1, "r": 1, "a": 1, "e": 1})
