"""ToH.py - Towers of Hanoi solver utilities (recursive & iterative).

Provides:
  * Recursive move generator (`towers_of_hanoi`)
  * Iterative solver (`towers_of_hanoi_iterative`) using an explicit stack
  * State generator (`hanoi_state_generator`) for visualization
  * CLI with JSON output, count-only mode, iterative toggle, and step limiting

All functions are pure (no printing) enabling reuse in teaching, testing, or
visualization contexts. Complexity is O(2^n) moves; memory for iterative stack
is O(n).
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from typing import Dict, Generator, Iterable, Iterator, List, Optional


Move = str


def towers_of_hanoi(
    disks: int, source: str, target: str, auxiliary: str
) -> Iterator[Move]:
    """Recursive generator yielding textual moves."""
    if disks > 0:
        yield from towers_of_hanoi(disks - 1, source, auxiliary, target)
        yield f"Move disk {disks} from {source} to {target}"
        yield from towers_of_hanoi(disks - 1, auxiliary, target, source)


def towers_of_hanoi_iterative(
    disks: int, source: str, target: str, auxiliary: str
) -> Iterator[Move]:
    """Iterative (stack-based) equivalent of the recursive generator.

    Simulates recursion frames as tuples (n, src, tgt, aux, stage). Stage 0 indicates
    we still need to process children; stage 1 means we should emit the move and then
    process the second half. This avoids Python recursion depth concerns for larger n.
    """
    stack: List[tuple[int, str, str, str, int]] = [
        (disks, source, target, auxiliary, 0)
    ]
    while stack:
        n, src, tgt, aux, stage = stack.pop()
        if n == 0:
            continue
        if stage == 0:
            # Defer processing current move after left subtree
            stack.append((n, src, tgt, aux, 1))
            stack.append((n - 1, src, aux, tgt, 0))
        else:
            # Emit move then process right subtree
            yield f"Move disk {n} from {src} to {tgt}"
            stack.append((n - 1, aux, tgt, src, 0))


def hanoi_state_generator(
    num_disks: int, source: str, target: str, auxiliary: str
) -> Iterator[Dict[str, List[int]]]:
    """Yield full peg state after each move for visualization."""
    pegs: Dict[str, List[int]] = {
        source: list(range(num_disks, 0, -1)),
        auxiliary: [],
        target: [],
    }
    yield {k: v.copy() for k, v in pegs.items()}

    def solve(n: int, src: str, tgt: str, aux: str) -> Iterator[None]:
        if n > 0:
            yield from solve(n - 1, src, aux, tgt)
            disk = pegs[src].pop()
            pegs[tgt].append(disk)
            yield None
            yield from solve(n - 1, aux, tgt, src)

    for _ in solve(num_disks, source, target, auxiliary):
        yield {k: v.copy() for k, v in pegs.items()}


@dataclass(slots=True)
class CLIConfig:
    disks: int
    iterative: bool = False
    json_output: bool = False
    count_only: bool = False
    max_steps: Optional[int] = None

    def validate(self) -> None:
        if self.disks < 1:
            raise ValueError("disks must be >= 1")
        if self.max_steps is not None and self.max_steps < 1:
            raise ValueError("max-steps must be positive")


def solve_moves(cfg: CLIConfig) -> List[Move]:
    gen = (towers_of_hanoi_iterative if cfg.iterative else towers_of_hanoi)(
        cfg.disks, "A", "C", "B"
    )
    moves: List[Move] = []
    for i, mv in enumerate(gen, 1):
        moves.append(mv)
        if cfg.max_steps is not None and i >= cfg.max_steps:
            break
    return moves


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Towers of Hanoi solver")
    p.add_argument("--disks", "-n", type=int, default=4, help="Number of disks (>=1)")
    p.add_argument(
        "--iterative", action="store_true", help="Use iterative solver implementation"
    )
    p.add_argument("--json", action="store_true", help="Emit JSON output")
    p.add_argument("--count-only", action="store_true", help="Only output move count")
    p.add_argument(
        "--max-steps", type=int, help="Limit number of moves emitted (for preview)"
    )
    return p


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = CLIConfig(
        disks=args.disks,
        iterative=args.iterative,
        json_output=args.json,
        count_only=args.count_only,
        max_steps=args.max_steps,
    )
    try:
        cfg.validate()
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    total_moves = (2**cfg.disks) - 1
    moves = [] if cfg.count_only else solve_moves(cfg)

    if cfg.json_output:
        print(
            json.dumps(
                {
                    "disks": cfg.disks,
                    "iterative": cfg.iterative,
                    "total_moves": total_moves,
                    "emitted_moves": len(moves),
                    "moves": moves[
                        :1000
                    ],  # safety cap for extremely large n with max-steps
                },
                indent=2,
            )
        )
    else:
        if cfg.count_only:
            print(f"Total moves: {total_moves}")
        else:
            for i, mv in enumerate(moves, 1):
                print(f"{i}. {mv}")
            print(f"\nTotal moves required: {total_moves}")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
