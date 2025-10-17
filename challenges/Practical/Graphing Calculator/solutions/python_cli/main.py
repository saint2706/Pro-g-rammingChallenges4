"""Command line graph plotter for mathematical expressions."""
from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

import matplotlib.pyplot as plt
import numpy as np
import sympy as sp


@dataclass
class PlotConfig:
    expression: str
    xmin: float
    xmax: float
    points: int
    outfile: Path

    @classmethod
    def from_args(cls, argv: Iterable[str] | None = None) -> "PlotConfig":
        parser = argparse.ArgumentParser(description=__doc__)
        parser.add_argument("expression", help="SymPy-compatible expression in terms of x")
        parser.add_argument("--xmin", type=float, default=-10.0, help="Minimum x value (default: -10)")
        parser.add_argument("--xmax", type=float, default=10.0, help="Maximum x value (default: 10)")
        parser.add_argument(
            "--points", type=int, default=500, help="Number of sampling points across the range"
        )
        parser.add_argument(
            "--outfile",
            type=Path,
            default=Path("plot.png"),
            help="Output PNG file for the rendered graph",
        )
        args = parser.parse_args(argv)
        if args.xmax <= args.xmin:
            parser.error("--xmax must be greater than --xmin")
        if args.points < 2:
            parser.error("--points must be at least 2")
        return cls(
            expression=args.expression,
            xmin=args.xmin,
            xmax=args.xmax,
            points=args.points,
            outfile=args.outfile,
        )


def generate_plot(config: PlotConfig) -> None:
    x = sp.symbols("x")
    try:
        sympy_expr = sp.sympify(config.expression)
    except sp.SympifyError as exc:  # pragma: no cover - CLI validation
        raise SystemExit(f"Invalid expression: {exc}") from exc

    lambdified = sp.lambdify(x, sympy_expr, modules=["numpy"])
    xs = np.linspace(config.xmin, config.xmax, config.points)
    ys = lambdified(xs)

    fig, ax = plt.subplots()
    ax.plot(xs, ys, label=config.expression)
    ax.axhline(0, color="black", linewidth=0.5)
    ax.axvline(0, color="black", linewidth=0.5)
    ax.set_xlabel("x")
    ax.set_ylabel("f(x)")
    ax.set_title("Graphing Calculator")
    ax.grid(True, linestyle="--", linewidth=0.5)
    ax.legend()
    fig.tight_layout()
    fig.savefig(config.outfile)
    plt.close(fig)


if __name__ == "__main__":
    generate_plot(PlotConfig.from_args())
