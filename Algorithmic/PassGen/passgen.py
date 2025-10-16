"""passgen.py - Modern password generator.

This module provides a flexible, cryptographically-secure password generator
with a clean CLI, entropy estimation, and optional JSON output for scripting.

Highlights
==========
* Uses Python's `secrets` module (CSPRNG)
* Supports multiple generated passwords in one invocation
* Ensures inclusion of at least one character from each selected category
* Optional exclusion of ambiguous characters (e.g. O,0,l,1,I)
* Entropy (bits) reported for transparency / auditing
* JSON output mode for automation pipelines
* Minimal dependency footprint (standard library only)

Examples
--------
Generate one 16-character password with letters+digits:
    python passgen.py -l 16 --letters --digits

Generate 5 high-entropy passwords including symbols, no ambiguous chars:
    python passgen.py -l 24 -n 5 --letters --digits --symbols --no-ambiguous

JSON output for tooling:
    python passgen.py -l 20 --letters --digits --symbols --json
"""

from __future__ import annotations

import argparse
import json
import secrets
import string
import sys
from dataclasses import dataclass
from math import log2
from typing import Iterable, List, Optional


# ---------------------------- Data Model ---------------------------- #
@dataclass(slots=True)
class PasswordSpec:
    length: int = 16
    letters: bool = True
    digits: bool = True
    symbols: bool = False
    count: int = 1
    exclude_ambiguous: bool = False
    json_output: bool = False
    min_categories: int = 2  # enforce at least this many categories selected

    def validate(self) -> None:
        if self.length < 8:
            raise ValueError("Password length must be at least 8 for basic security")
        categories = sum([self.letters, self.digits, self.symbols])
        if categories == 0:
            raise ValueError("At least one character category must be enabled")
        if categories < self.min_categories:
            raise ValueError(
                "Selected character categories do not meet the minimum diversity "
                f"requirement of {self.min_categories}. Enable more categories or "
                "lower the --min-categories threshold."
            )
        if self.count <= 0:
            raise ValueError("Count must be >= 1")


AMBIGUOUS = set("O0l1I|`'\"{}[]()/\\")


def build_pool(spec: PasswordSpec) -> str:
    pool = ""
    if spec.letters:
        pool += string.ascii_letters
    if spec.digits:
        pool += string.digits
    if spec.symbols:
        pool += string.punctuation
    if spec.exclude_ambiguous:
        pool = "".join(ch for ch in pool if ch not in AMBIGUOUS)
    if not pool:
        raise ValueError("Character pool is empty; adjust options")
    return pool


def estimate_entropy_bits(pool_size: int, length: int) -> float:
    """Estimate entropy in bits for uniformly random selection (approx)."""
    return length * log2(pool_size)


def generate_one(spec: PasswordSpec, pool: str) -> str:
    # Guarantee at least one from each selected category
    required = []
    if spec.letters:
        required.append(secrets.choice(string.ascii_letters))
    if spec.digits:
        required.append(secrets.choice(string.digits))
    if spec.symbols:
        required.append(secrets.choice(string.punctuation))

    remaining = spec.length - len(required)
    if remaining < 0:
        raise ValueError("Length too short for chosen categories")
    body = [secrets.choice(pool) for _ in range(remaining)]
    chars = required + body
    secrets.SystemRandom().shuffle(chars)
    return "".join(chars)


def generate_passwords(spec: PasswordSpec) -> List[str]:
    pool = build_pool(spec)
    return [generate_one(spec, pool) for _ in range(spec.count)]


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Secure password generator (CSPRNG)")
    p.add_argument(
        "-l", "--length", type=int, default=16, help="Password length (min 8)"
    )
    p.add_argument(
        "-n", "--count", type=int, default=1, help="Number of passwords to generate"
    )
    p.add_argument("--letters", action="store_true", help="Include letters (a-zA-Z)")
    p.add_argument("--digits", action="store_true", help="Include digits (0-9)")
    p.add_argument("--symbols", action="store_true", help="Include punctuation symbols")
    p.add_argument(
        "--no-ambiguous",
        action="store_true",
        dest="no_ambiguous",
        help="Exclude ambiguous characters (O,0,l,1,I, etc.)",
    )
    p.add_argument(
        "--json", action="store_true", help="Output JSON (passwords + metadata)"
    )
    p.add_argument(
        "--min-categories",
        type=int,
        default=2,
        help="Recommended minimum distinct categories (soft)",
    )
    return p


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)

    # If user didn't specify any category flags, default to letters+digits (backward compat) unless they selected something.
    any_flag = args.letters or args.digits or args.symbols
    spec = PasswordSpec(
        length=args.length,
        letters=args.letters if any_flag else True,
        digits=args.digits if any_flag else True,
        symbols=args.symbols,
        count=args.count,
        exclude_ambiguous=args.no_ambiguous,
        json_output=args.json,
        min_categories=args.min_categories,
    )

    try:
        spec.validate()
        passwords = generate_passwords(spec)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    pool = build_pool(spec)
    entropy_bits = estimate_entropy_bits(len(pool), spec.length)

    if spec.json_output:
        payload = {
            "passwords": passwords,
            "length": spec.length,
            "count": spec.count,
            "letters": spec.letters,
            "digits": spec.digits,
            "symbols": spec.symbols,
            "excluded_ambiguous": spec.exclude_ambiguous,
            "pool_size": len(pool),
            "estimated_entropy_bits": round(entropy_bits, 2),
        }
        print(json.dumps(payload, indent=2))
    else:
        print("Generated Password(s):")
        for i, pw in enumerate(passwords, 1):
            print(f" {i:>2}: {pw}")
        print(f"\nPool size: {len(pool)} | Estimated entropy: {entropy_bits:.2f} bits")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
