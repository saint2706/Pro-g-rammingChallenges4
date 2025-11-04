"""Ulam Spiral generator.

Modernized version featuring:
  * Pure functions for sieve + spiral construction
  * Dataclass-style configuration (using a lightweight class for Python <3.10 compatibility in simple scripts)
  * Argparse driven CLI (no interactive input for automation friendliness)
  * Optional JSON metadata output (--json)
  * Save image to file (--save) and/or suppress window (--no-show)
  * Adjustable colormap (--cmap) and figure size (--figsize)

Example usages:
  python ulam.py --size 101
  python ulam.py -s 301 --cmap viridis --save ulam_301.png --no-show --json
  python ulam.py -s 201 --json

Notes:
  * The canonical Ulam spiral places 1 at the center, then enumerates integers
    by moving right, up, left, left, down, down, right, right, right, up, up, up ...
    (spiral arms: step lengths repeating twice and incrementing: 1,1,2,2,3,3,...)
  * Primes trace striking diagonal and radial patterns, hinting at underlying
    structure in their distribution.
  * For even ``size`` values the starting coordinate is nudged inward to
    ``size//2 - 1`` so the spiral begins inside the central 2×2 block rather
    than stepping out of bounds immediately.
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from dataclasses import dataclass
from typing import List, Optional, Tuple

import numpy as np

try:  # Matplotlib is optional for JSON / data-only usage
    import matplotlib.pyplot as plt  # type: ignore

    HAVE_MPL = True
except Exception:  # pragma: no cover - fallback when matplotlib absent
    HAVE_MPL = False


# ------------------------------ Core Algorithms ------------------------------ #


def sieve_of_eratosthenes(n: int) -> np.ndarray:
    """Return boolean array ``is_prime`` of length n+1 using a classic sieve.

    Complexity: O(n log log n). For modest sizes (<= 5e6) this is fast in pure NumPy.
    """
    if n < 1:
        return np.zeros(n + 1, dtype=bool)
    primes = np.ones(n + 1, dtype=bool)
    primes[:2] = False
    limit = int(math.isqrt(n))
    for p in range(2, limit + 1):
        if primes[p]:
            # Start eliminating from p*p; prior composites already handled.
            primes[p * p : n + 1 : p] = False
    return primes


def generate_ulam_spiral(size: int) -> np.ndarray:
    """Generate a square ``size x size`` binary array for the Ulam spiral.

    ``1`` indicates a prime number, ``0`` otherwise. The integer 1 (center) is
    not prime so center may be zero if size > 1.
    """
    if size < 1:
        raise ValueError("size must be >= 1")
    limit = size * size
    primes = sieve_of_eratosthenes(limit)

    grid = np.zeros((size, size), dtype=np.uint8)

    # Start at the center; even sizes offset into the central 2x2 block
    center = size // 2
    if size % 2 == 0:
        center -= 1
    x = y = center
    # Spiral parameters
    directions: List[Tuple[int, int]] = [(1, 0), (0, -1), (-1, 0), (0, 1)]  # R, U, L, D
    dir_index = 0
    step_length = 1
    steps_taken_at_length = 0

    for n in range(1, limit + 1):
        if primes[n]:
            if 0 <= y < size and 0 <= x < size:
                grid[y, x] = 1
        # Move to next cell in spiral unless done
        if n == limit:
            break
        dx, dy = directions[dir_index]
        x += dx
        y += dy
        step_length -= 0  # (kept for clarity; no change here)
        steps_taken_at_length += 1
        # After completing 'current step length' in a direction, rotate direction
        if steps_taken_at_length == step_length:
            dir_index = (dir_index + 1) % 4
            steps_taken_at_length = 0
            # Every two direction changes increase the segment length
            if dir_index % 2 == 0:
                step_length += 1
    return grid


# ------------------------------ Configuration ------------------------------ #


@dataclass(slots=True)
class SpiralConfig:
    size: int = 101
    cmap: str = "binary"
    figsize: float = 10.0
    save: Optional[str] = None
    json_out: bool = False
    show: bool = True

    def validate(self) -> None:
        if self.size < 1:
            raise ValueError("size must be positive")
        if self.figsize <= 0:
            raise ValueError("figsize must be > 0")


# ------------------------------ Plotting Helper ------------------------------ #


def plot_ulam(grid: np.ndarray, cfg: SpiralConfig) -> None:
    if not HAVE_MPL:
        raise RuntimeError("matplotlib not available; cannot plot (install matplotlib)")
    import matplotlib.pyplot as plt  # local import for clarity

    fig, ax = plt.subplots(figsize=(cfg.figsize, cfg.figsize))
    ax.imshow(grid, cmap=cfg.cmap, interpolation="nearest")
    ax.set_title(f"Ulam Spiral ({cfg.size}x{cfg.size})")
    ax.axis("off")
    if cfg.save:
        fig.savefig(cfg.save, dpi=150, bbox_inches="tight")
    if cfg.show:
        plt.show()
    else:  # Close to free memory in batch contexts
        plt.close(fig)


# ------------------------------ CLI / Main ------------------------------ #


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Generate an Ulam spiral image and/or JSON metadata."
    )
    p.add_argument(
        "-s",
        "--size",
        type=int,
        default=101,
        help="Edge length of spiral (odd recommended, default 101)",
    )
    p.add_argument(
        "--cmap", default="binary", help="Matplotlib colormap (default: binary)"
    )
    p.add_argument(
        "--figsize", type=float, default=10.0, help="Figure size (inches, default 10)"
    )
    p.add_argument(
        "--save",
        metavar="PATH",
        help="Save the generated image to PATH (PNG recommended)",
    )
    p.add_argument(
        "--json", action="store_true", help="Output JSON summary metadata to stdout"
    )
    p.add_argument(
        "--no-show",
        action="store_true",
        help="Do not display the window (useful with --save / --json)",
    )
    return p


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)
    cfg = SpiralConfig(
        size=args.size,
        cmap=args.cmap,
        figsize=args.figsize,
        save=args.save,
        json_out=args.json,
        show=not args.no_show,
    )

    try:
        cfg.validate()
    except ValueError as e:
        parser.error(str(e))

    grid = generate_ulam_spiral(cfg.size)
    prime_count = int(grid.sum())
    limit = cfg.size * cfg.size
    density = prime_count / max(1, limit)

    if cfg.json_out:
        summary = {
            "size": cfg.size,
            "limit": limit,
            "prime_count": prime_count,
            "prime_density": density,
            "cmap": cfg.cmap,
            "saved": bool(cfg.save),
        }
        print(json.dumps(summary, indent=2))

    # Only attempt plotting if user wants visualization
    if cfg.show or cfg.save:
        try:
            plot_ulam(grid, cfg)
        except RuntimeError as e:
            # If plotting requested but MPL not installed, surface clear message
            print(f"Plot skipped: {e}", file=sys.stderr)
            return 1
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry
    raise SystemExit(main())
