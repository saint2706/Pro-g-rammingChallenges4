"""affine.py - Classical Affine substitution cipher implementation.

Formulae (mod 26 for English alphabet):
  Encryption:   C = (a * P + b) mod 26
  Decryption:   P = a^{-1} * (C - b) mod 26  (where a^{-1} is modular inverse of a)

Security note: This cipher is strictly pedagogical and NOT secure by modern standards.

Enhancements Added
==================
* Dataclass configuration
* argparse CLI with encrypt/decrypt + brute force key search
* JSON output option with metadata
* File / stdin / inline text input sources
* Output file saving
* Robust validation + helpful error messages
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from dataclasses import dataclass
from typing import Iterable, List, Optional, Sequence, Tuple

ALPHABET_SIZE = 26
VALID_A_VALUES = [a for a in range(1, ALPHABET_SIZE) if math.gcd(a, ALPHABET_SIZE) == 1]


def modular_inverse(a: int, m: int) -> Optional[int]:
    if math.gcd(a, m) != 1:
        return None
    try:
        return pow(a, -1, m)
    except ValueError:
        return None


def _transform_char(ch: str, a: int, b: int, mode: str, inv_a: int) -> str:
    if not ch.isalpha():
        return ch
    base = ord("a") if ch.islower() else ord("A")
    offset = ord(ch) - base
    if mode == "encrypt":
        new_val = (a * offset + b) % ALPHABET_SIZE
    else:
        new_val = (inv_a * (offset - b)) % ALPHABET_SIZE
    return chr(base + new_val)


def affine_cipher(text: str, key_a: int, key_b: int, mode: str) -> str:
    if mode not in {"encrypt", "decrypt"}:
        raise ValueError("mode must be 'encrypt' or 'decrypt'")
    inv_a = modular_inverse(key_a, ALPHABET_SIZE)
    if inv_a is None:
        raise ValueError(
            f"Invalid key 'a'={key_a}; must be coprime with {ALPHABET_SIZE}"
        )
    return "".join(_transform_char(c, key_a, key_b, mode, inv_a) for c in text)


def brute_force_affine(ciphertext: str) -> List[Tuple[int, int, str]]:
    """Enumerate all possible decryptions for given ciphertext.

    Returns list of (a, b, plaintext) for all valid 'a', all b in [0,25].
    This is useful when key is unknown; user can manually inspect output
    or downstream filters (e.g., word frequency scoring) could be applied.
    """
    results = []
    for a in VALID_A_VALUES:
        inv_a = modular_inverse(a, ALPHABET_SIZE)
        assert inv_a is not None  # by construction
        for b in range(ALPHABET_SIZE):
            plain = "".join(
                _transform_char(c, a, b, "decrypt", inv_a) for c in ciphertext
            )
            results.append((a, b, plain))
    return results


@dataclass(slots=True)
class CLIConfig:
    mode: str
    a: Optional[int]
    b: Optional[int]
    text: Optional[str] = None
    file: Optional[str] = None
    stdin: bool = False
    brute_force: bool = False
    json_output: bool = False
    save: Optional[str] = None

    def validate(self) -> None:
        if self.brute_force:
            if any(v is not None for v in (self.a, self.b)):
                raise ValueError("Do not supply -a/-b with --bruteforce")
            if self.mode != "decrypt":
                raise ValueError("--bruteforce only valid with --mode decrypt")
            return
        if self.a is None or self.b is None:
            raise ValueError("-a and -b keys required unless --bruteforce used")
        if self.a not in VALID_A_VALUES:
            raise ValueError(f"Key a must be in {VALID_A_VALUES}")
        if self.mode not in {"encrypt", "decrypt"}:
            raise ValueError("mode must be encrypt or decrypt")


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Affine cipher tool")
    p.add_argument(
        "--mode",
        "-m",
        required=True,
        choices=["encrypt", "decrypt"],
        help="Operation mode",
    )
    p.add_argument("-a", type=int, help="Key a (must be coprime with 26)")
    p.add_argument("-b", type=int, help="Key b shift (0-25)")
    p.add_argument("--text", help="Inline text input")
    p.add_argument("--file", help="Input file path (UTF-8)")
    p.add_argument("--stdin", action="store_true", help="Read stdin as input text")
    p.add_argument(
        "--bruteforce",
        action="store_true",
        help="Try all key combinations (decrypt mode only)",
    )
    p.add_argument("--json", action="store_true", help="Emit JSON output")
    p.add_argument("--save", help="Write transformed / results text to file")
    return p


def resolve_input(cfg: CLIConfig) -> str:
    sources = [bool(cfg.text), bool(cfg.file), cfg.stdin]
    if sum(sources) > 1:
        raise ValueError("Specify only one of --text/--file/--stdin")
    if cfg.text is not None:
        return cfg.text
    if cfg.file:
        try:
            with open(cfg.file, "r", encoding="utf-8") as f:
                return f.read()
        except (OSError, UnicodeDecodeError) as e:
            raise ValueError(f"File read error: {e}")
    if cfg.stdin:
        return sys.stdin.read()
    # interactive fallback
    try:
        return input("Enter text: ")
    except (EOFError, KeyboardInterrupt):
        raise ValueError("No input provided")


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = CLIConfig(
        mode=args.mode,
        a=args.a,
        b=args.b,
        text=args.text,
        file=args.file,
        stdin=args.stdin,
        brute_force=args.bruteforce,
        json_output=args.json,
        save=args.save,
    )

    try:
        cfg.validate()
        data = resolve_input(cfg)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    if cfg.brute_force:
        combos = brute_force_affine(data)
        if cfg.json_output:
            payload = [
                {"a": a, "b": b, "plaintext": plain[:120]} for a, b, plain in combos
            ]
            print(json.dumps({"count": len(payload), "results": payload}, indent=2))
        else:
            for a, b, plain in combos:
                print(f"a={a:2d} b={b:2d}: {plain}")
        if cfg.save:
            try:
                with open(cfg.save, "w", encoding="utf-8") as f:
                    for a, b, plain in combos:
                        f.write(f"a={a} b={b}: {plain}\n")
            except OSError as e:
                print(f"Error writing file: {e}", file=sys.stderr)
                return 1
        return 0

    # Single transform mode
    assert cfg.a is not None and cfg.b is not None
    transformed = affine_cipher(data, cfg.a, cfg.b, cfg.mode)
    if cfg.save:
        try:
            with open(cfg.save, "w", encoding="utf-8") as f:
                f.write(transformed)
        except OSError as e:
            print(f"Error writing file: {e}", file=sys.stderr)
            return 1
    if cfg.json_output:
        inv_a = modular_inverse(cfg.a, ALPHABET_SIZE)
        assert inv_a is not None
        payload = {
            "mode": cfg.mode,
            "a": cfg.a,
            "b": cfg.b,
            "inverse_a": inv_a,
            "input_length": len(data),
            "output_length": len(transformed),
            "sample_in": data[:80],
            "sample_out": transformed[:80],
        }
        print(json.dumps(payload, indent=2))
    else:
        print(transformed)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
