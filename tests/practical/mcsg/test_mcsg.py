"""Tests for the Markov Chain Sentence Generator practical challenge."""
from __future__ import annotations

import importlib.util
import random
import sys
from pathlib import Path

import pytest


_MODULE_PATH = (
    Path(__file__).resolve().parents[3]
    / "challenges"
    / "Practical"
    / "Markov Chain Sentence Generator"
    / "mcsg.py"
)

_spec = importlib.util.spec_from_file_location("mcsg", _MODULE_PATH)
if _spec is None or _spec.loader is None:  # pragma: no cover - defensive
    pytest.skip("Unable to load mcsg module", allow_module_level=True)
mcsg = importlib.util.module_from_spec(_spec)
sys.modules[_spec.name] = mcsg
_spec.loader.exec_module(mcsg)

MarkovGenerator = mcsg.MarkovGenerator


def test_start_states_align_with_sentence_boundaries() -> None:
    gen = MarkovGenerator(state_size=2)
    gen.train("First second third. Fourth fifth sixth.")

    assert set(gen._start_states) == {
        ("first", "second"),
        ("fourth", "fifth"),
    }


def test_generated_sentence_starts_with_sentence_lead() -> None:
    gen = MarkovGenerator(state_size=2)
    gen.train("Alpha beta gamma. Delta epsilon zeta. Eta theta.")

    random.seed(0)
    sentence = gen.generate(length=5)

    first_word = sentence.split()[0]
    assert first_word in {"alpha", "delta", "eta"}


def test_sentence_start_fragments_recorded_for_short_sentences() -> None:
    gen = MarkovGenerator(state_size=3)
    gen.train("Alpha beta gamma delta. Zeta.")

    assert ("zeta",) in gen._start_fragments
    assert gen._start_tokens[-1] == "zeta"
