"""mcsg.py - Markov Chain Sentence Generator

Modernized Features:
  * Dataclass MarkovConfig for reproducible generation parameters
  * Flexible tokenizer: strips surrounding punctuation while preserving internal apostrophes
  * Optional lowercasing toggle (--no-lower to preserve original casing)
  * Seed control for deterministic output (--seed)
  * JSON export of generated sentences & model stats (--json)
  * Support generating multiple sentences via helper function generate_sentences
  * Graceful handling of short corpora and dead ends
  * Summary stats (unique states, transitions, start states)
  * Exit codes: 0 success, 1 file/model error, 2 argument error

Example:
  python mcsg.py book.txt -n 10 -l 20 --state-size 2 --seed 123 --json out.json
"""

from __future__ import annotations

import argparse
import json
import os
import random
import re
import sys
from collections import defaultdict, deque
from dataclasses import dataclass
from pathlib import Path
from typing import Deque, Dict, Iterable, List, Optional, Sequence, Tuple

# ----------------------------- Configuration ----------------------------- #


@dataclass(slots=True)
class MarkovConfig:
    length: int = 15
    num: int = 5
    state_size: int = 2
    start: Optional[str] = None
    lower: bool = True
    seed: Optional[int] = None

    def validate(self) -> None:
        if self.length < 1:
            raise ValueError("length must be >= 1")
        if self.num < 1:
            raise ValueError("num must be >= 1")
        if self.state_size < 1:
            raise ValueError("state_size must be >= 1")


# ----------------------------- Core Generator ----------------------------- #


class MarkovGenerator:
    """A Markov Chain generator for creating sentences based on a source text."""

    def __init__(self, state_size: int = 1, lower: bool = True):
        if state_size < 1:
            raise ValueError("state size must be at least 1")
        self.state_size = state_size
        self.lower = lower
        self.model: Dict[Tuple[str, ...], List[str]] = defaultdict(list)
        self._start_states: List[Tuple[str, ...]] = []
        self._token_count: int = 0

    TOKEN_SPLIT_RE = re.compile(r"\s+")
    STRIP_PUNCT_RE = re.compile(r"^[\W_]+|[\W_]+$")  # leading / trailing punctuation

    def _tokenize(self, text: str) -> List[str]:
        # Basic whitespace split, then strip punctuation at boundaries, keep internal apostrophes
        raw = [t for t in self.TOKEN_SPLIT_RE.split(text) if t]
        tokens: List[str] = []
        for tok in raw:
            stripped = self.STRIP_PUNCT_RE.sub("", tok)
            if not stripped:
                continue
            tokens.append(stripped.lower() if self.lower else stripped)
        return tokens

    def train(self, text: str) -> None:
        """Build the Markov model from a given source text."""
        tokens = self._tokenize(text)
        self._token_count = len(tokens)
        if len(tokens) <= self.state_size:
            print(
                "Warning: Source text is too short for the given state size.",
                file=sys.stderr,
            )
            return
        state: Deque[str] = deque(tokens[: self.state_size], maxlen=self.state_size)
        self._start_states.append(tuple(state))
        for next_word in tokens[self.state_size :]:
            self.model[tuple(state)].append(next_word)
            if state[0].endswith("."):
                self._start_states.append(tuple(state))
            state.append(next_word)

    # ---------------- Generation ---------------- #
    def generate(self, length: int, start_key: Optional[str] = None) -> str:
        if not self.model:
            return "(Model empty – train() with a larger corpus)"
        if length < self.state_size:
            length = self.state_size
        if start_key:
            start_candidates = [
                s for s in self._start_states if s[0].lower() == start_key.lower()
            ]
            if not start_candidates:
                start_state = random.choice(self._start_states)
            else:
                start_state = random.choice(start_candidates)
        else:
            start_state = random.choice(self._start_states)
        words = list(start_state)
        current = start_state
        for _ in range(length - self.state_size):
            options = self.model.get(current)
            if not options:
                break
            nxt = random.choice(options)
            words.append(nxt)
            current = (*current[1:], nxt)
        return " ".join(words)

    def generate_sentences(self, cfg: MarkovConfig) -> List[str]:
        return [self.generate(cfg.length, cfg.start) for _ in range(cfg.num)]

    # ---------------- Stats ---------------- #
    def stats(self) -> Dict[str, int]:
        transitions = sum(len(v) for v in self.model.values())
        return {
            "state_size": self.state_size,
            "unique_states": len(self.model),
            "start_states": len(self._start_states),
            "transitions": transitions,
            "tokens": self._token_count,
        }


# ----------------------------- CLI ----------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Generate sentences using a Markov Chain model."
    )
    p.add_argument("file", help="Path to source text file")
    p.add_argument(
        "-l", "--length", type=int, default=15, help="Words per sentence (default 15)"
    )
    p.add_argument(
        "-n", "--num", type=int, default=5, help="Number of sentences (default 5)"
    )
    p.add_argument("-s", "--start", help="Optional starting word")
    p.add_argument(
        "--state-size", type=int, default=2, help="Order of Markov chain (default 2)"
    )
    p.add_argument(
        "--no-lower",
        action="store_true",
        help="Preserve original casing (default lowers)",
    )
    p.add_argument("--seed", type=int, help="Random seed for reproducibility")
    p.add_argument("--json", type=Path, help="Write JSON output (sentences + stats)")
    return p


def parse_args(
    argv: Optional[Sequence[str]],
) -> Tuple[MarkovConfig, argparse.Namespace]:
    parser = build_parser()
    a = parser.parse_args(argv)
    cfg = MarkovConfig(
        length=a.length,
        num=a.num,
        state_size=a.state_size,
        start=a.start,
        lower=not a.no_lower,
        seed=a.seed,
    )
    try:
        cfg.validate()
    except ValueError as e:
        print(f"Argument error: {e}", file=sys.stderr)
        raise SystemExit(2)
    return cfg, a


# ----------------------------- Orchestration ----------------------------- #


def main(argv: Optional[Sequence[str]] = None) -> int:
    cfg, args = parse_args(argv)
    if cfg.seed is not None:
        random.seed(cfg.seed)
    try:
        with open(args.file, "r", encoding="utf-8") as f:
            text = f.read()
    except FileNotFoundError:
        print(f"Error: Source text file not found at '{args.file}'", file=sys.stderr)
        return 1
    except OSError as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        return 1

    gen = MarkovGenerator(state_size=cfg.state_size, lower=cfg.lower)
    gen.train(text)

    sentences = gen.generate_sentences(cfg)
    print(f"\n--- Generated {cfg.num} sentence(s) (length ~{cfg.length}) ---\n")
    for i, s in enumerate(sentences, 1):
        print(f"{i}. {s}\n")

    stats = gen.stats()
    print("Stats:")
    for k, v in stats.items():
        print(f"  {k}: {v}")

    if args.json:
        payload = {"sentences": sentences, "config": cfg.__dict__, "stats": stats}
        try:
            with open(args.json, "w", encoding="utf-8") as fh:
                json.dump(payload, fh, indent=2)
            print(f"JSON written to {args.json}")
        except OSError as e:
            print(f"Warning: Could not write JSON: {e}", file=sys.stderr)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
