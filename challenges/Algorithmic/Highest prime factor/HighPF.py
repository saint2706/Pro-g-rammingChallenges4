"""Highest Prime Factor (modernized)
===================================

Compute the largest prime factor of one or more positive integers efficiently.

Enhancements vs original version:
 - Optimized 6k±1 trial division after stripping factors of 2 and 3.
 - Handles very large integers (limited only by memory/time) using pure Python integers.
 - Supports multiple inputs via CLI, JSON output, and optional timing.
 - Clear error handling and exit codes (non-zero on invalid input).
 - Well documented and type annotated for new contributors.

Time complexity: O( sqrt(n)/log n ) practical due to factor elimination (worst-case for large prime).

Example usage:
  python HighPF.py 13195 600851475143
  python HighPF.py --json 42 97 10
  echo 600851475143 | python HighPF.py --stdin --timing
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from dataclasses import dataclass
from typing import Iterable, List, Sequence


def highest_prime_factor(n: int) -> int:
    """Returns the largest prime factor of a given integer.

    This function uses an optimized trial division method. It first handles
    the factors of 2 and 3, and then checks for factors of the form 6k ± 1
    up to the square root of the remaining number.

    Args:
        n: An integer greater than 1.

    Returns:
        The largest prime factor of `n`.

    Raises:
        ValueError: If `n` is less than 2.
    """
    if n < 2:
        raise ValueError("Input must be an integer greater than 1.")

    # Track current best factor
    max_factor = 1  # will be replaced; n>=2 guarantees answer >=2

    # Remove all factors of 2
    while n % 2 == 0:
        n //= 2
        max_factor = 2
    # Remove all factors of 3
    while n % 3 == 0:
        n //= 3
        max_factor = 3  # overwrites if 3 divides

    # Now test potential factors of form 6k ± 1
    f = 5
    # We can stop when f^2 > n; when loop exits if n>1 it is prime.
    while f * f <= n:
        if n % f == 0:
            max_factor = f
            while n % f == 0:
                n //= f
        # Check companion f+2 (since between 6k-1 and 6k+1 there is one even and one divisible by 3)
        f2 = f + 2
        if n % f2 == 0:
            max_factor = f2
            while n % f2 == 0:
                n //= f2
        f += 6

    if n > 1:  # Remaining n is prime
        max_factor = n

    return int(max_factor)


@dataclass
class FactorResult:
    value: int
    highest_prime_factor: int
    elapsed_seconds: float | None = None


def process_numbers(numbers: Sequence[int], timing: bool = False) -> List[FactorResult]:
    """Processes a sequence of numbers to find their highest prime factors.

    Args:
        numbers: A sequence of integers to process.
        timing: If `True`, measures the time taken to process each number.

    Returns:
        A list of `FactorResult` objects.
    """
    results: List[FactorResult] = []
    for num in numbers:
        start = time.perf_counter() if timing else None
        hpf = highest_prime_factor(num)
        elapsed = (
            (time.perf_counter() - start) if timing and start is not None else None
        )
        results.append(FactorResult(num, hpf, elapsed))
    return results


def parse_stdin() -> List[int]:
    """Parses a list of integers from standard input.

    The input can be a single line of space- or comma-separated integers.

    Returns:
        A list of integers parsed from stdin.

    Raises:
        ValueError: If any of the tokens in the input cannot be converted to an integer.
    """
    data = sys.stdin.read().strip()
    if not data:
        return []
    out: List[int] = []
    for token in data.replace(",", " ").split():
        try:
            out.append(int(token))
        except ValueError:
            raise ValueError(f"Invalid integer in stdin input: {token}") from None
    return out


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Compute highest prime factor for one or more integers."
    )
    p.add_argument("numbers", nargs="*", type=int, help="Integers >= 2")
    p.add_argument(
        "--stdin",
        action="store_true",
        help="Also read whitespace/comma separated integers from stdin",
    )
    p.add_argument("--json", action="store_true", help="Output results as JSON array")
    p.add_argument(
        "--timing", action="store_true", help="Include elapsed seconds per number"
    )
    return p


def render_results(results: Sequence[FactorResult], as_json: bool) -> str:
    if as_json:
        return json.dumps(
            [
                {
                    "value": r.value,
                    "highest_prime_factor": r.highest_prime_factor,
                    **(
                        {"elapsed_seconds": r.elapsed_seconds}
                        if r.elapsed_seconds is not None
                        else {}
                    ),
                }
                for r in results
            ],
            indent=2,
        )
    # Plain text output
    lines = []
    for r in results:
        base = f"{r.value}: {r.highest_prime_factor}"
        if r.elapsed_seconds is not None:
            base += f" (elapsed {r.elapsed_seconds * 1000:.3f} ms)"
        lines.append(base)
    return "\n".join(lines)


def main(argv: Iterable[str] | None = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)

    numbers: List[int] = list(args.numbers)
    if args.stdin:
        try:
            numbers.extend(parse_stdin())
        except ValueError as e:
            print(f"[error] {e}", file=sys.stderr)
            return 2

    if not numbers:
        parser.print_usage(sys.stderr)
        print("No numbers provided (use positional args or --stdin).", file=sys.stderr)
        return 1

    # Validate inputs
    for n in numbers:
        if n < 2:
            print(f"[error] All numbers must be >= 2 (got {n}).", file=sys.stderr)
            return 3

    try:
        results = process_numbers(numbers, timing=args.timing)
    except ValueError as e:  # from highest_prime_factor, defensive
        print(f"[error] {e}", file=sys.stderr)
        return 4

    print(render_results(results, as_json=args.json))
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
