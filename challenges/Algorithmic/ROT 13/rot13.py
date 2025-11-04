"""rot13.py - Simple, modern ROT13 encoder/decoder.

ROT13 is a Caesar cipher with a rotation of 13 over the English alphabet.
Applying ROT13 twice returns the original text, making the transform its own
inverse. While not a secure cipher, it is still useful for light obfuscation
in puzzles or spoiler prevention.

Features Added in Refactor
==========================
* Reusable precomputed translation table (no recomputation per call)
* Command line interface using argparse
* Multiple input sources: direct argument, file, STDIN, or interactive prompt
* Optional JSON output mode for scripting pipelines
* Graceful handling of encoding errors (UTF-8 assumed)
* Unit-test friendly pure function `rot13` with idempotence property check

Examples
--------
Encode a string:
    python rot13.py --text "Hello World!"

Decode (ROT13 again):
    python rot13.py --text "Uryyb Jbeyq!"

From a file:
    python rot13.py --file message.txt --save encoded.txt

Via pipe:
    echo "secret" | python rot13.py --stdin

JSON output:
    python rot13.py --text "Hello" --json
"""

from __future__ import annotations

import argparse
import json
import string
import sys
from dataclasses import dataclass
from typing import Iterable, Optional

# Precompute translation table once (lowercase + uppercase)
_INPUT = string.ascii_lowercase + string.ascii_uppercase
_OUTPUT = (
    string.ascii_lowercase[13:]
    + string.ascii_lowercase[:13]
    + string.ascii_uppercase[13:]
    + string.ascii_uppercase[:13]
)
_ROT13_TABLE = str.maketrans(_INPUT, _OUTPUT)


def rot13(text: str) -> str:
    """Return ROT13 transform of text (letters rotated, others unchanged)."""
    return text.translate(_ROT13_TABLE)


@dataclass(slots=True)
class CLIConfig:
    text: Optional[str] = None
    file: Optional[str] = None
    stdin: bool = False
    save: Optional[str] = None
    json_output: bool = False


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="ROT13 encoder/decoder")
    p.add_argument("--text", help="Text to transform (bypasses interactive prompt)")
    p.add_argument("--file", help="Read input text from file (UTF-8)")
    p.add_argument("--stdin", action="store_true", help="Read entire STDIN as input")
    p.add_argument("--save", help="Write transformed output to file")
    p.add_argument("--json", action="store_true", help="Emit JSON with input & output")
    return p


def resolve_input(cfg: CLIConfig) -> str:
    sources = [bool(cfg.text), bool(cfg.file), cfg.stdin]
    if sum(sources) > 1:
        raise ValueError("Specify only one of --text / --file / --stdin")
    if cfg.text is not None:
        return cfg.text
    if cfg.file:
        try:
            with open(cfg.file, "r", encoding="utf-8") as f:
                return f.read()
        except (OSError, UnicodeDecodeError) as e:
            raise ValueError(f"Failed to read file: {e}")
    if cfg.stdin:
        return sys.stdin.read()
    # Interactive fallback
    try:
        return input("Enter text to apply ROT13 to: ")
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

    transformed = rot13(original)

    if cfg.save:
        try:
            with open(cfg.save, "w", encoding="utf-8") as f:
                f.write(transformed)
        except OSError as e:
            print(f"Error writing output file: {e}", file=sys.stderr)
            return 1

    if cfg.json_output:
        payload = {
            "input_length": len(original),
            "output_length": len(transformed),
            "same_length": len(original) == len(transformed),
            "input_sample": original[:80],
            "output_sample": transformed[:80],
            "rot13_twice_equals_original": rot13(transformed) == original,
        }
        print(json.dumps(payload, indent=2))
    else:
        print(transformed)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
