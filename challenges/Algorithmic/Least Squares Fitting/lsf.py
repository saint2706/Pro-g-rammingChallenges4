"""Least Squares Linear Regression (modernized)
================================================

This module provides ordinary least squares (OLS) linear regression y = m x + b.

Enhancements:
 - Dataclass config for synthetic data generation.
 - CLI: control points, noise, seed, explicit data via --x/--y, JSON output, headless mode.
 - Optional plotting (skips gracefully if matplotlib is unavailable).
 - Clear separation of computation from presentation.
 - Robust input validation and helpful error messages.

Examples:
  python lsf.py --points 50 --noise 8 --seed 42 --save plot.png
  python lsf.py --json --no-plot --seed 7
  python lsf.py --x 0 1 2 3 --y 1.1 2.9 4.2 6.0 --json --include-points
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from typing import Iterable, Optional, Tuple

import numpy as np

try:  # Optional dependency
    import matplotlib.pyplot as plt  # type: ignore

    _HAVE_PLT = True
except Exception:  # pragma: no cover
    plt = None  # type: ignore
    _HAVE_PLT = False


def least_squares_fit(
    x_coords: np.ndarray, y_coords: np.ndarray
) -> Tuple[float, float]:
    """Return slope (m) and intercept (b) via least squares.

    Raises ValueError for empty, size-mismatched, or vertical-line inputs.
    """
    if x_coords.size != y_coords.size:
        raise ValueError("Input arrays must be of the same size.")
    if x_coords.size == 0:
        raise ValueError("Input arrays must not be empty.")

    n = x_coords.size
    sum_x = float(np.sum(x_coords))
    sum_y = float(np.sum(y_coords))
    sum_xy = float(np.sum(x_coords * y_coords))
    sum_x_sq = float(np.sum(x_coords**2))

    numerator = n * sum_xy - sum_x * sum_y
    denominator = n * sum_x_sq - sum_x**2
    if np.isclose(denominator, 0.0):
        raise ValueError(
            "Cannot compute regression for a vertical set of points (denominator is zero)."
        )
    m = numerator / denominator
    b = (sum_y - m * sum_x) / n
    return m, b


def plot_regression(
    x: np.ndarray,
    y: np.ndarray,
    m: float,
    b: float,
    save_path: Optional[str] = None,
    show: bool = True,
) -> None:
    if not _HAVE_PLT or plt is None:  # type: ignore[truthy-bool]
        print("[warn] matplotlib not available; skipping plot", file=sys.stderr)
        return
    plt.figure(figsize=(10, 6))  # type: ignore[attr-defined]
    plt.scatter(x, y, color="tab:blue", s=35, zorder=5, label="Data Points")  # type: ignore[attr-defined]
    x_min, x_max = float(np.min(x)), float(np.max(x))
    span = x_max - x_min if x_max > x_min else 1.0
    line_x = np.array([x_min - 0.05 * span, x_max + 0.05 * span])
    line_y = m * line_x + b
    plt.plot(
        line_x,
        line_y,
        color="tab:red",
        linewidth=2,
        label=f"Regression Line y={m:.2f}x+{b:.2f}",
    )  # type: ignore[attr-defined]
    for i in range(x.size):
        label = "Error (Residuals)" if i == 0 else ""
        plt.plot(
            [x[i], x[i]],
            [y[i], m * x[i] + b],
            color="green",
            linestyle="--",
            linewidth=1,
            label=label,
        )  # type: ignore[attr-defined]
    plt.title("Least Squares Linear Regression", fontsize=15)  # type: ignore[attr-defined]
    plt.xlabel("X Values")  # type: ignore[attr-defined]
    plt.ylabel("Y Values")  # type: ignore[attr-defined]
    plt.grid(True, linestyle="--", linewidth=0.5)  # type: ignore[attr-defined]
    plt.legend(fontsize=9)  # type: ignore[attr-defined]
    plt.tight_layout()  # type: ignore[attr-defined]
    if save_path:
        try:
            plt.savefig(save_path, dpi=200)  # type: ignore[attr-defined]
            print(f"[info] Plot saved -> {save_path}")
        except Exception as e:  # pragma: no cover
            print(f"[error] Could not save plot: {e}", file=sys.stderr)
    if show:
        plt.show()  # type: ignore[attr-defined]
    else:
        plt.close()  # type: ignore[attr-defined]


@dataclass
class DemoConfig:
    points: int = 20
    noise: float = 10.0
    seed: Optional[int] = None
    save: Optional[str] = None
    show_plot: bool = True
    json: bool = False


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Least squares linear regression demo & utility."
    )
    p.add_argument(
        "--points",
        type=int,
        default=20,
        help="Synthetic points (ignored if --x provided)",
    )
    p.add_argument("--noise", type=float, default=10.0, help="Std dev Gaussian noise")
    p.add_argument("--seed", type=int, help="Random seed")
    p.add_argument("--save", type=str, help="Save plot to path")
    p.add_argument("--no-plot", action="store_true", help="Disable plot display")
    p.add_argument("--json", action="store_true", help="Emit JSON output")
    p.add_argument(
        "--x", type=float, nargs="*", help="Explicit x values (requires --y)"
    )
    p.add_argument(
        "--y", type=float, nargs="*", help="Explicit y values (requires --x)"
    )
    p.add_argument(
        "--include-points",
        action="store_true",
        help="Include raw points in JSON output",
    )
    return p


def parse_args(argv: Optional[Iterable[str]] = None):
    return build_arg_parser().parse_args(list(argv) if argv is not None else None)


def generate_demo_data(cfg: DemoConfig):
    true_m, true_b = 0.75, 10.0
    rng = np.random.default_rng(cfg.seed)
    x = np.linspace(0, 100, cfg.points)
    noise = rng.normal(0, cfg.noise, cfg.points)
    y = true_m * x + true_b + noise
    return x, y, true_m, true_b


def run_cli(argv: Optional[Iterable[str]] = None) -> int:
    args = parse_args(argv)
    if (args.x is None) ^ (args.y is None):
        print("[error] Must supply both --x and --y or neither.", file=sys.stderr)
        return 2
    if args.x is not None and len(args.x) != len(args.y):  # type: ignore[arg-type]
        print("[error] --x and --y lengths differ.", file=sys.stderr)
        return 2
    if args.x is None and args.points < 2:
        print("[error] Need at least 2 points.", file=sys.stderr)
        return 2

    cfg = DemoConfig(
        points=args.points,
        noise=args.noise,
        seed=args.seed,
        save=args.save,
        show_plot=not args.no_plot,
        json=args.json,
    )
    if args.x is not None:
        x = np.array(args.x, dtype=float)
        y = np.array(args.y, dtype=float)
        true_m = true_b = float("nan")
    else:
        x, y, true_m, true_b = generate_demo_data(cfg)

    try:
        m, b = least_squares_fit(x, y)
    except ValueError as e:
        print(f"[error] {e}", file=sys.stderr)
        return 3

    if args.json:
        payload = {"slope": m, "intercept": b}
        if not np.isnan(true_m):
            payload["true_slope"] = true_m
            payload["true_intercept"] = true_b
        if args.include_points:
            payload["x"] = x.tolist()
            payload["y"] = y.tolist()
        print(json.dumps(payload, indent=2))
    else:
        if not np.isnan(true_m):
            print(f"True line: y = {true_m:.3f}x + {true_b:.3f}")
        print(f"Fitted line: y = {m:.3f}x + {b:.3f}")

    if cfg.show_plot or cfg.save:
        plot_regression(x, y, m, b, save_path=cfg.save, show=cfg.show_plot)
    return 0


def main():  # pragma: no cover
    raise SystemExit(run_cli())


if __name__ == "__main__":  # pragma: no cover
    main()
