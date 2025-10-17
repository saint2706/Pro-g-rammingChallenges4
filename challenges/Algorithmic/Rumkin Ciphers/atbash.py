"""atbash.py - Classical Atbash monoalphabetic substitution cipher.

The Atbash cipher reverses the alphabet so that:
    A <-> Z, B <-> Y, C <-> X, ...

It is involutive: applying Atbash twice returns the original text. This makes
it a teaching example for substitution ciphers, not a secure encryption method.

Features Added
==============
* Precomputed translation table (no per-call rebuild)
* argparse-based CLI with multiple input sources (text literal, file, stdin)
* Optional JSON output for scripting
* Save transformed output to a file
* Dataclass config for clarity and testability

Examples
--------
Inline text:
    python atbash.py --text "Hello World!"

File input -> output file:
    python atbash.py --file message.txt --save enc.txt

Pipeline usage:
    echo "Secret" | python atbash.py --stdin

JSON metadata:
    python atbash.py --text "Hello" --json
"""

from __future__ import annotations

import argparse
import json
import string
import sys
from dataclasses import dataclass
from typing import Iterable, Optional

_ALPHA_IN = string.ascii_lowercase + string.ascii_uppercase
_ALPHA_OUT = string.ascii_lowercase[::-1] + string.ascii_uppercase[::-1]
_TABLE = str.maketrans(_ALPHA_IN, _ALPHA_OUT)


def atbash_cipher(text: str) -> str:
    return text.translate(_TABLE)


@dataclass(slots=True)
class CLIConfig:
    text: Optional[str] = None
    file: Optional[str] = None
    stdin: bool = False
    save: Optional[str] = None
    json_output: bool = False


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Atbash cipher (A<->Z). Involutive transformation."
    )
    p.add_argument("--text", help="Inline text to transform")
    p.add_argument("--file", help="Read input text from UTF-8 file")
    p.add_argument("--stdin", action="store_true", help="Read entire STDIN as input")
    p.add_argument("--save", help="Write transformed output to file")
    p.add_argument("--json", action="store_true", help="Emit JSON metadata + sample")
    return p


def resolve_input(cfg: CLIConfig) -> str:
    sources = [bool(cfg.text), bool(cfg.file), cfg.stdin]
    if sum(sources) > 1:
        raise ValueError("Specify only one input source (--text / --file / --stdin)")
    if cfg.text is not None:
        return cfg.text
    if cfg.file:
        try:
            with open(cfg.file, "r", encoding="utf-8") as f:
                return f.read()
        except OSError as e:
            raise ValueError(f"Failed to read file: {e}")
    if cfg.stdin:
        return sys.stdin.read()
    # interactive fallback
    try:
        return input("Enter text: ")
    except (EOFError, KeyboardInterrupt):
        raise ValueError("No input provided")


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = CLIConfig(
        text=args.text,
        file=args.file,
        stdin=args.stdin,
        save=args.save,
        json_output=args.json,
    )

    try:
        original = resolve_input(cfg)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    transformed = atbash_cipher(original)

    if cfg.save:
        try:
            with open(cfg.save, "w", encoding="utf-8") as f:
                f.write(transformed)
        except OSError as e:
            print(f"Error writing file: {e}", file=sys.stderr)
            return 1

    if cfg.json_output:
        payload = {
            "input_length": len(original),
            "output_length": len(transformed),
            "sample_in": original[:80],
            "sample_out": transformed[:80],
            "round_trip_identity": atbash_cipher(transformed) == original,
        }
        print(json.dumps(payload, indent=2))
    else:
        print(transformed)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
