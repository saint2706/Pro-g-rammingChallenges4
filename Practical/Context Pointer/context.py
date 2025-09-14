"""context.py – Linguistic feature explorer using spaCy.

Enhancements:
  * Dataclass Config + structured pipeline
  * CLI options: model, features, json export, no-color, verbose
  * Graceful model load errors & helpful download hint
  * Selectable token attributes (default: text,pos,dep,head_text,head_pos,lemma,ent)
  * Optional ANSI coloring for headers
  * JSON output for downstream tooling
  * Clear separation of parsing vs. rendering logic

Example:
  python context.py "The quick brown fox jumps over the lazy dog." --features text,pos,dep,lemma
  python context.py --model en_core_web_sm --json out.json
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from typing import List, Dict, Any, Sequence, Optional

try:  # dependency guard
    import spacy
except ImportError:  # pragma: no cover
    print("Error: The 'spacy' library is required.", file=sys.stderr)
    print("Install with: pip install spacy", file=sys.stderr)
    raise SystemExit(1)

# ----------------------------- Data Model ----------------------------- #


@dataclass(slots=True)
class Config:
    sentence: str
    model_name: str
    features: List[str]
    json_path: Optional[str]
    color: bool
    verbose: bool


DEFAULT_FEATURES = ["text", "pos", "dep", "head_text", "head_pos", "lemma", "ent"]
COLOR_HEADER = "\x1b[36m"  # cyan
COLOR_RESET = "\x1b[0m"

# ----------------------------- Core Parser ----------------------------- #


class SentenceParser:
    """Wrapper around spaCy model providing structured token feature extraction."""

    def __init__(self, model_name: str = "en_core_web_sm", verbose: bool = False):
        self.nlp = None
        self.verbose = verbose
        try:
            self.nlp = spacy.load(model_name)
            if verbose:
                print(f"Loaded spaCy model '{model_name}'.")
        except OSError:
            print(f"Error: spaCy model '{model_name}' not found.", file=sys.stderr)
            print(
                f"Download with: python -m spacy download {model_name}", file=sys.stderr
            )
            raise SystemExit(1)

    def parse(self, sentence: str) -> List[Dict[str, Any]]:
        if not self.nlp:
            raise RuntimeError("spaCy model not loaded")
        if not sentence.strip():
            return []
        doc = self.nlp(sentence)
        rows: List[Dict[str, Any]] = []
        for tok in doc:
            rows.append(
                {
                    "text": tok.text,
                    "pos": tok.pos_,
                    "dep": tok.dep_,
                    "head_text": tok.head.text,
                    "head_pos": tok.head.pos_,
                    "lemma": tok.lemma_,
                    "ent": tok.ent_type_ or "-",
                }
            )
        return rows


# ----------------------------- Display Logic ----------------------------- #


def filter_features(
    rows: List[Dict[str, Any]], features: List[str]
) -> List[Dict[str, Any]]:
    return [{f: r.get(f, "") for f in features} for r in rows]


def compute_col_widths(rows: List[Dict[str, Any]], features: List[str]) -> List[int]:
    widths = [len(f.upper()) for f in features]
    for r in rows:
        for i, f in enumerate(features):
            widths[i] = max(widths[i], len(str(r.get(f, ""))))
    return widths


def print_table(rows: List[Dict[str, Any]], features: List[str], color: bool) -> None:
    if not rows:
        print("No tokens (empty input).")
        return
    widths = compute_col_widths(rows, features)
    header_cells = [features[i].upper().ljust(widths[i]) for i in range(len(features))]
    header_line = " | ".join(header_cells)
    if color:
        header_line = f"{COLOR_HEADER}{header_line}{COLOR_RESET}"
    print(header_line)
    print("-" * len(header_line))
    for r in rows:
        line = " | ".join(
            str(r.get(f, "")).ljust(widths[i]) for i, f in enumerate(features)
        )
        print(line)


# ----------------------------- CLI ----------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Analyze a sentence: POS, dependencies, lemmas, entities"
    )
    p.add_argument(
        "sentence",
        nargs="?",
        default="Sachin hit the ball with a bat.",
        help="Sentence to analyze",
    )
    p.add_argument("--model", default="en_core_web_sm", help="spaCy model name")
    p.add_argument("--features", help="Comma-separated list of features to show")
    p.add_argument("--json", help="Write JSON output to path")
    p.add_argument(
        "--no-color", action="store_true", help="Disable ANSI color in header"
    )
    p.add_argument(
        "-v", "--verbose", action="store_true", help="Verbose model load messages"
    )
    return p


def parse_args(argv: Optional[Sequence[str]]) -> Config:
    parser = build_parser()
    args = parser.parse_args(argv)
    feats = (
        DEFAULT_FEATURES
        if not args.features
        else [f.strip() for f in args.features.split(",") if f.strip()]
    )
    return Config(
        sentence=args.sentence,
        model_name=args.model,
        features=feats,
        json_path=args.json,
        color=not args.no_color,
        verbose=args.verbose,
    )


# ----------------------------- Main Flow ----------------------------- #


def main(argv: Optional[Sequence[str]] = None) -> int:
    cfg = parse_args(argv)
    parser = SentenceParser(cfg.model_name, verbose=cfg.verbose)
    rows = parser.parse(cfg.sentence)
    rows = filter_features(rows, cfg.features)
    print(f"\n--- Analysis for: '{cfg.sentence}' ---\n")
    print_table(rows, cfg.features, cfg.color)
    if cfg.json_path:
        try:
            with open(cfg.json_path, "w", encoding="utf-8") as fh:
                json.dump(rows, fh, indent=2)
            if cfg.verbose:
                print(f"Wrote JSON to {cfg.json_path}")
        except OSError as e:
            print(f"Error writing JSON: {e}", file=sys.stderr)
            return 2
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
