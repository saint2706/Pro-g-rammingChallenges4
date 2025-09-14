"""Radix/Base conversion utilities.

Supports bases 2 through 36 (0-9 then A-Z). Provides both library functions
and a modern CLI interface. The module is safe for import; the CLI executes
only when run as a script.

Example (library use):
    >>> to_decimal("FF", 16)
    255
    >>> from_decimal(255, 16)
    'FF'
    >>> convert("1011", 2, 16)
    'B'

CLI examples:
    python radix.py --from 16 --to 2 FF
    python radix.py --list 42 10 2 8 16

Design notes:
  - Input sanitization & validation (raises ValueError for invalid digits)
  - Handles negative numbers (preserves leading '-')
  - Avoids large intermediate string allocations where possible
  - Precomputed digit lookup for speed in `from_decimal`
"""

from __future__ import annotations

from dataclasses import dataclass
import argparse
from typing import Iterable, Sequence

_DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
_CHAR_TO_VAL = {ch: i for i, ch in enumerate(_DIGITS)}


def _validate_base(base: int) -> None:
    if not (2 <= base <= 36):
        raise ValueError("Base must be between 2 and 36")


def to_decimal(number: str, base: int) -> int:
    """Convert a number string of base `base` to an integer (base 10).

    Supports optional leading '-' for negatives.
    """
    _validate_base(base)
    s = str(number).strip().upper()
    if not s:
        raise ValueError("Empty string")
    neg = s.startswith("-")
    if neg:
        s = s[1:]
    if not s:
        raise ValueError("No digits after sign")

    value = 0
    for ch in s:
        if ch not in _CHAR_TO_VAL:
            raise ValueError(f"Invalid character '{ch}' in input")
        v = _CHAR_TO_VAL[ch]
        if v >= base:
            raise ValueError(f"Invalid digit '{ch}' for base {base}")
        value = value * base + v
    return -value if neg else value


def from_decimal(number: int, base: int) -> str:
    """Convert a non-negative integer to a given base (2..36)."""
    _validate_base(base)
    if number == 0:
        return "0"
    neg = number < 0
    n = -number if neg else number
    out_chars: list[str] = []
    while n:
        n, rem = divmod(n, base)
        out_chars.append(_DIGITS[rem])
    if neg:
        out_chars.append("-")
    return "".join(reversed(out_chars))


def convert(number: str, from_base: int, to_base: int) -> str:
    """Convert `number` from `from_base` to `to_base` (both 2..36)."""
    return from_decimal(to_decimal(number, from_base), to_base)


@dataclass
class ConversionResult:
    input: str
    from_base: int
    to_base: int
    output: str


def batch_convert(
    value: str, bases_from: Sequence[int], bases_to: Sequence[int]
) -> list[ConversionResult]:
    """Convert a single value across combinations of from/to bases.

    Useful for quick comparison or demonstration tables.
    """
    results: list[ConversionResult] = []
    for fb in bases_from:
        for tb in bases_to:
            if fb == tb:
                continue
            try:
                results.append(ConversionResult(value, fb, tb, convert(value, fb, tb)))
            except ValueError:
                # Skip invalid representations silently (could expose flag for verbose)
                pass
    return results


def _parse_cli(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Radix / Base converter")
    parser.add_argument("value", nargs="?", help="The value to convert (string).")
    parser.add_argument("--from", dest="from_base", type=int, help="Source base (2-36)")
    parser.add_argument("--to", dest="to_base", type=int, help="Target base (2-36)")
    parser.add_argument(
        "--list",
        action="store_true",
        help="List conversions across common bases (2,8,10,16,36)",
    )
    parser.add_argument(
        "--bases",
        nargs="*",
        type=int,
        help="Custom list mode bases (overrides --list default set)",
    )
    parser.add_argument(
        "--no-header", action="store_true", help="Omit header row in list mode output"
    )
    parser.add_argument(
        "--pad",
        type=int,
        default=0,
        help="Pad output with leading zeros to this width (only for single conversion)",
    )
    parser.add_argument(
        "--upper",
        action="store_true",
        help="Force uppercase output (default) - provided for symmetry",
    )
    parser.add_argument("--lower", action="store_true", help="Force lowercase output")
    return parser.parse_args(argv)


def _format_output(out: str, lower: bool, pad: int) -> str:
    if lower:
        out = out.lower()
    if pad > 0:
        sign = ""
        if out.startswith("-"):
            sign, out = "-", out[1:]
        out = sign + out.rjust(pad, "0")
    return out


def _cli(argv: list[str] | None = None) -> int:
    ns = _parse_cli(argv)
    if ns.value is None:
        print("No value provided. Use --help for usage.")
        return 2

    # List mode: show conversions across multiple bases for same lexeme
    if ns.list:
        bases = ns.bases if ns.bases else [2, 8, 10, 16, 36]
        if not ns.no_header:
            print("from_base -> to_base : output")
        for fb in bases:
            for tb in bases:
                if fb == tb:
                    continue
                try:
                    res = convert(ns.value, fb, tb)
                    print(f"{fb:>3} -> {tb:>3} : {res}")
                except ValueError:
                    pass
        return 0

    if ns.from_base is None or ns.to_base is None:
        print("Both --from and --to are required unless using --list mode.")
        return 2

    try:
        result = convert(ns.value, ns.from_base, ns.to_base)
    except ValueError as e:
        print(f"Error: {e}")
        return 1

    formatted = _format_output(result, lower=ns.lower, pad=ns.pad)
    print(formatted)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(_cli())
