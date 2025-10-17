"""triangle.py - Generate ASCII Sierpinski triangles using a bitwise method.

Mathematical Idea
-----------------
Point (x, y) belongs to the discrete Sierpinski triangle iff (x & y) == 0.
We iterate rows (y) top-down and columns (x) left-right respecting this rule
and print a symbol for interior points and spaces elsewhere.

Enhancements
------------
* Pure generation function returning list of lines (testable)
* Dataclass configuration with validation
* argparse CLI (size, character, JSON, save to file)
* Optional JSON metadata (width, height, density)
* Power-of-two advisory + graceful errors

Example
-------
    python triangle.py --size 16 --char '#'
    python triangle.py --size 32 --json --save tri.txt
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from dataclasses import dataclass
from typing import Iterable, List, Optional, Sequence


@dataclass(slots=True)
class TriangleConfig:
    size: int = 8
    char: str = "*"
    json_output: bool = False
    save: Optional[str] = None

    def validate(self) -> None:
        if self.size <= 0:
            raise ValueError("size must be positive")
        if len(self.char) != 1:
            raise ValueError("char must be a single character")
        # Advisory only: computed property style (no assignment due to slots)
        # Use helper property below when needed.

    @property
    def power_of_two(self) -> bool:  # derived helper
        return (self.size & (self.size - 1)) == 0


def generate_sierpinski_lines(size: int, char: str = "*") -> List[str]:
    """Generate lines for a Sierpinski triangle of given size.

    Returns a list of strings (each line without trailing spaces) suitable
    for printing or further processing.
    """
    lines: List[str] = []
    for y in range(size):
        calc_y = size - 1 - y  # invert for upright orientation
        line_chars: List[str] = []
        # Leading spaces for centering (each symbol plus trailing space counts width ~2)
        line_chars.append(" " * y)
        for x in range(size - y):
            if (x & calc_y) == 0:
                line_chars.append(char)
                line_chars.append(" ")  # spacing for visual proportion
            else:
                line_chars.append("  ")
        lines.append("".join(line_chars).rstrip())
    return lines


def generate_sierpinski(size: int, char: str = "*") -> str:
    return "\n".join(generate_sierpinski_lines(size, char))


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Generate an ASCII Sierpinski triangle")
    p.add_argument(
        "--size", type=int, default=8, help="Triangle height (power of 2 recommended)"
    )
    p.add_argument("--char", default="*", help="Fill character (default: *)")
    p.add_argument(
        "--json",
        action="store_true",
        help="Emit JSON metadata instead of raw triangle text",
    )
    p.add_argument("--save", help="Write output (text or JSON) to file")
    return p


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = TriangleConfig(
        size=args.size, char=args.char, json_output=args.json, save=args.save
    )

    try:
        cfg.validate()
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    lines = generate_sierpinski_lines(cfg.size, cfg.char)
    output_text = "\n".join(lines)

    if cfg.json_output:
        total_cells = sum(len(line.replace(" ", "")) for line in lines)
        payload = {
            "size": cfg.size,
            "char": cfg.char,
            "power_of_two": cfg.power_of_two,
            "lines": len(lines),
            "width_first_line": len(lines[0]) if lines else 0,
            "non_space_chars": total_cells,
            "density": total_cells / (cfg.size * cfg.size),  # rough metric
        }
        serialized = json.dumps(payload, indent=2)
        if cfg.save:
            try:
                with open(cfg.save, "w", encoding="utf-8") as f:
                    f.write(serialized)
            except OSError as e:
                print(f"Error writing file: {e}", file=sys.stderr)
                return 1
        print(serialized)
    else:
        if not cfg.power_of_two:
            print(
                "Warning: size is not a power of two; pattern may look irregular.",
                file=sys.stderr,
            )
        if cfg.save:
            try:
                with open(cfg.save, "w", encoding="utf-8") as f:
                    f.write(output_text + "\n")
            except OSError as e:
                print(f"Error writing file: {e}", file=sys.stderr)
                return 1
        print(output_text)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
