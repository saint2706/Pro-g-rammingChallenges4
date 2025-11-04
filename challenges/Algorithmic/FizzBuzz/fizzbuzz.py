"""FizzBuzz Modernized
=====================

An extensible and testable implementation of the classic FizzBuzz problem.

Key Features:
- Rule engine using a dataclass for easy extension (e.g., add 7 -> "Pop")
- Streaming generator for memory efficiency on large ranges
- Multiple output formats: plain (default), JSON, CSV
- Argparse-based CLI with input validation and helpful usage examples
- Optional zero-padding and suppression of numbers
- Fully type-annotated and documented for new developers

Example:
    Plain output up to 20 (default rules 3/5):
        python fizzbuzz.py -n 20
    JSON output with custom rules:
        python fizzbuzz.py -n 15 --rule 3:Fizz --rule 5:Buzz --format json
    CSV output, suppress raw numbers:
        python fizzbuzz.py -n 16 --no-numbers --format csv
    Add an extra rule (7 -> Pop):
        python fizzbuzz.py -n 35 --rule 3:Fizz --rule 5:Buzz --rule 7:Pop

Exit Codes:
    0 - Success
    2 - Invalid user input (argument parsing / validation errors)

This module can be imported and the core generator reused in other contexts.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from typing import Callable, Iterable, Iterator, List, Sequence, Tuple


# -----------------------------
# Rule Engine Definitions
# -----------------------------
@dataclass(frozen=True)
class FizzBuzzRule:
    """Represents a single FizzBuzz rule.

    Attributes:
        divisor: Positive integer divisor to test.
        label:   String to append when a number is divisible by divisor.
    """

    divisor: int
    label: str

    def applies(self, value: int) -> bool:
        """Return True if the rule applies to the provided value."""
        return value % self.divisor == 0

    def __post_init__(self) -> None:
        if self.divisor <= 0:
            raise ValueError(f"Divisor must be positive, got {self.divisor}")
        if not self.label:
            raise ValueError("Label must be a non-empty string")


# Default classic FizzBuzz rules
DEFAULT_RULES: Tuple[FizzBuzzRule, ...] = (
    FizzBuzzRule(3, "Fizz"),
    FizzBuzzRule(5, "Buzz"),
)

# -----------------------------
# Core Generator Logic
# -----------------------------


def fizzbuzz_stream(
    limit: int,
    rules: Sequence[FizzBuzzRule] = DEFAULT_RULES,
    *,
    include_numbers: bool = True,
) -> Iterator[str]:
    """Yields the FizzBuzz sequence up to a given limit.

    Args:
        limit: The upper bound of the sequence (inclusive). Must be a positive integer.
        rules: A sequence of `FizzBuzzRule` objects to apply. Defaults to classic FizzBuzz rules.
        include_numbers: If `True`, numbers that don't match any rules are yielded as strings.
                         If `False`, they are skipped.

    Yields:
        A string representing the FizzBuzz output for each number in the sequence.

    Raises:
        ValueError: If `limit` is not a positive integer.
    """
    if limit < 1:
        raise ValueError("Limit must be >= 1")

    for value in range(1, limit + 1):
        parts: List[str] = [rule.label for rule in rules if rule.applies(value)]
        if parts:
            yield "".join(parts)
        else:
            if include_numbers:
                yield str(value)
            else:
                # If numbers are suppressed and no rule matches, skip emitting
                continue


# -----------------------------
# Formatting Helpers
# -----------------------------


def to_json(sequence: Iterable[str]) -> str:
    """Convert a sequence to a JSON array string."""
    return json.dumps(list(sequence), ensure_ascii=False)


def to_csv(sequence: Iterable[str]) -> str:
    """Convert a sequence to a simple CSV string (one row)."""
    return ",".join(sequence)


OUTPUT_FORMATTERS: dict[str, Callable[[Iterable[str]], str]] = {
    "plain": lambda seq: "\n".join(seq),
    "json": to_json,
    "csv": to_csv,
}

# -----------------------------
# CLI Handling
# -----------------------------


def parse_rule(spec: str) -> FizzBuzzRule:
    """Parses a rule specification from a string.

    The string should be in the format "divisor:Label", e.g., "3:Fizz".

    Args:
        spec: The string specification of the rule.

    Returns:
        A `FizzBuzzRule` object.

    Raises:
        argparse.ArgumentTypeError: If the spec is not in the correct format
                                    or the values are invalid.
    """
    if ":" not in spec:
        raise argparse.ArgumentTypeError(
            "Rule must be in form DIVISOR:LABEL (e.g., 3:Fizz)"
        )
    div_part, label = spec.split(":", 1)
    try:
        divisor = int(div_part)
    except ValueError as exc:  # pragma: no cover - defensive
        raise argparse.ArgumentTypeError("Divisor must be an integer") from exc
    try:
        return FizzBuzzRule(divisor, label)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(str(exc)) from exc


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Extensible FizzBuzz generator with multiple output formats.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""Examples:\n  fizzbuzz.py -n 30\n  fizzbuzz.py -n 30 --format json\n  fizzbuzz.py -n 50 --rule 3:Fizz --rule 5:Buzz --rule 7:Pop\n  fizzbuzz.py -n 40 --no-numbers --format csv\n""",
    )
    parser.add_argument(
        "-n",
        "--limit",
        type=int,
        default=100,
        help="Upper limit (inclusive). Default: %(default)s",
    )
    parser.add_argument(
        "--rule",
        action="append",
        type=parse_rule,
        help="Add a custom rule in form DIVISOR:LABEL. Can be repeated. Overrides defaults if used.",
    )
    parser.add_argument(
        "--format",
        choices=sorted(OUTPUT_FORMATTERS.keys()),
        default="plain",
        help="Output format (default: %(default)s)",
    )
    parser.add_argument(
        "--no-numbers",
        action="store_true",
        help="Suppress printing of raw numbers when no rule matches.",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=str,
        help="Optional output file path. If omitted, prints to stdout.",
    )
    return parser


# -----------------------------
# Main Entrypoint
# -----------------------------


def run(
    limit: int,
    *,
    rules: Sequence[FizzBuzzRule] | None = None,
    fmt: str = "plain",
    include_numbers: bool = True,
) -> str:
    """Runs the FizzBuzz generator and returns the formatted result.

    This function is designed to be easily imported and used in other modules.

    Args:
        limit: The upper bound for the FizzBuzz sequence.
        rules: A sequence of `FizzBuzzRule` objects. Defaults to the classic rules.
        fmt: The output format. Can be "plain", "json", or "csv".
        include_numbers: Whether to include numbers that don't match any rules.

    Returns:
        A string containing the formatted FizzBuzz sequence.

    Raises:
        ValueError: If an unsupported format is specified.
    """
    if rules is None:
        chosen_rules = DEFAULT_RULES
    else:
        chosen_rules = tuple(rules)
    if fmt not in OUTPUT_FORMATTERS:
        raise ValueError(f"Unsupported format: {fmt}")
    stream = fizzbuzz_stream(limit, chosen_rules, include_numbers=include_numbers)
    return OUTPUT_FORMATTERS[fmt](stream)


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    if args.limit < 1:
        print("Error: limit must be >= 1", file=sys.stderr)
        return 2

    # If custom rules provided, they replace defaults; else use defaults
    rules = tuple(args.rule) if args.rule else DEFAULT_RULES

    try:
        result = run(
            args.limit,
            rules=rules,
            fmt=args.format,
            include_numbers=not args.no_numbers,
        )
    except ValueError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 2

    if args.output:
        try:
            with open(args.output, "w", encoding="utf-8") as f:
                f.write(result if args.format != "plain" else result + "\n")
        except OSError as exc:
            print(f"Error writing to file: {exc}", file=sys.stderr)
            return 2
    else:
        # Print directly for plain; for json/csv already formatted
        print(result)

    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
