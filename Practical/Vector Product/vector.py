"""Vector operations demo: dot product, 2D scalar cross, and 3D vector cross.

Modernizations:
- Added type hints and clear docstrings.
- Provides reusable API: dot(), cross(), plot_vectors().
- CLI interface: supply vectors via command line, choose 2D or 3D, toggle plotting.
- Validation & helpful error messages.
- Removes unused explicit Axes3D import (modern Matplotlib handles 3D via projection keyword).
- Example usage preserved when run without CLI args (defaults shown in __main__).

Cross Product Semantics:
- For 2D inputs, returns the scalar z-component of the 3D cross (treating vectors as (x,y,0)).
- For 3D inputs, returns the 3-component vector.

Plotting:
- 2D: arrows on Cartesian plane.
- 3D: arrows from origin using quiver.

"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, List, Sequence, Tuple, Union
import math
import argparse
import sys
import matplotlib.pyplot as plt

Number = Union[int, float]
Vector2 = Tuple[Number, Number]
Vector3 = Tuple[Number, Number, Number]
Vector = Union[Vector2, Vector3]

# ----------------------------- Core Operations ----------------------------- #


def dot(v1: Sequence[Number], v2: Sequence[Number]) -> Number:
    """Return the dot (inner) product of two equal-length vectors."""
    if len(v1) != len(v2):
        raise ValueError("Vectors must be same dimension for dot product")
    return sum(a * b for a, b in zip(v1, v2))


def cross(v1: Sequence[Number], v2: Sequence[Number]) -> Union[Number, Vector3]:
    """Return the cross product.

    - If both are length 2 -> returns scalar (z-component of 3D cross)
    - If both are length 3 -> returns 3-tuple vector
    """
    if len(v1) != len(v2):
        raise ValueError("Vectors must be same dimension for cross product")
    if len(v1) == 2:
        return v1[0] * v2[1] - v1[1] * v2[0]
    if len(v1) == 3:
        return (
            v1[1] * v2[2] - v1[2] * v2[1],
            v1[2] * v2[0] - v1[0] * v2[2],
            v1[0] * v2[1] - v1[1] * v2[0],
        )
    raise ValueError("Only 2D or 3D vectors are supported")


def magnitude(v: Sequence[Number]) -> float:
    """Return Euclidean norm."""
    return math.sqrt(sum((x * x) for x in v))


# ----------------------------- Plotting Utilities ----------------------------- #


def plot_vectors(
    v1: Sequence[Number],
    v2: Sequence[Number],
    cross_val: Union[Number, Sequence[Number]],
    *,
    show: bool = True,
) -> plt.Figure:
    """Plot vectors v1, v2 and (optionally) their cross product.

    For 2D: cross_val is scalar (if provided) -> direction (perp) is not drawn (kept simple).
    For 3D: cross_val must be a 3D vector and is drawn.
    """
    dim = len(v1)
    if dim not in (2, 3):
        raise ValueError("Only 2D/3D vectors supported for plotting")

    if dim == 2:
        fig, ax = plt.subplots()
        ax.quiver(
            0,
            0,
            v1[0],
            v1[1],
            angles="xy",
            scale_units="xy",
            scale=1,
            color="r",
            label="v1",
        )
        ax.quiver(
            0,
            0,
            v2[0],
            v2[1],
            angles="xy",
            scale_units="xy",
            scale=1,
            color="b",
            label="v2",
        )
        # Optionally one could visualize orthogonal direction; omitted for clarity.
        limit = max(10, *(abs(c) for c in (*v1, *v2)))
        ax.set_xlim(-limit, limit)
        ax.set_ylim(-limit, limit)
        ax.set_xlabel("X")
        ax.set_ylabel("Y")
        ax.axhline(0, color="k", linewidth=0.8)
        ax.axvline(0, color="k", linewidth=0.8)
        ax.grid(alpha=0.3)
        ax.legend()
        ax.set_title("2D Vectors")
        fig.tight_layout()
        if show:
            plt.show()
        return fig

    # 3D
    fig = plt.figure()
    ax = fig.add_subplot(111, projection="3d")
    ax.quiver(0, 0, 0, v1[0], v1[1], v1[2], color="r", label="v1")
    ax.quiver(0, 0, 0, v2[0], v2[1], v2[2], color="b", label="v2")
    if isinstance(cross_val, (tuple, list)) and len(cross_val) == 3:
        ax.quiver(
            0,
            0,
            0,
            cross_val[0],
            cross_val[1],
            cross_val[2],
            color="g",
            label="v1 x v2",
        )
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.set_zlabel("Z")
    lim = max(10, *(abs(c) for c in (*v1, *v2)))
    ax.set_xlim(-lim, lim)
    ax.set_ylim(-lim, lim)
    ax.set_zlim(-lim, lim)
    ax.legend()
    plt.title("3D Vectors")
    plt.tight_layout()
    if show:
        plt.show()
    return fig


# ----------------------------- CLI Interface ----------------------------- #


def parse_vector(text: str, dim: int) -> Sequence[Number]:
    parts = text.replace(",", " ").split()
    if len(parts) != dim:
        raise argparse.ArgumentTypeError(f"Expected {dim} components, got {len(parts)}")
    try:
        return tuple(float(p) for p in parts)
    except ValueError as e:
        raise argparse.ArgumentTypeError(f"Invalid numeric component: {e}") from e


def prepare_vector(value: Union[str, Sequence[Number]], dim: int) -> Tuple[float, ...]:
    """Parse *value* into a numeric vector of length *dim*.

    The CLI uses :func:`parse_vector` which raises ``ArgumentTypeError``; this helper wraps
    that behaviour so GUI/front-end callers can share the logic and receive ``ValueError``
    instead.
    """

    if isinstance(value, str):
        try:
            parsed = parse_vector(value, dim)
        except argparse.ArgumentTypeError as exc:  # pragma: no cover - thin wrapper
            raise ValueError(str(exc)) from exc
        return tuple(float(c) for c in parsed)

    if isinstance(value, Sequence):
        if len(value) != dim:
            raise ValueError(f"Expected {dim} components, got {len(value)}")
        try:
            return tuple(float(component) for component in value)
        except (TypeError, ValueError) as exc:
            raise ValueError(f"Invalid numeric component: {exc}") from exc

    raise TypeError("Value must be a string or a numeric sequence")


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Vector dot and cross product demo")
    p.add_argument(
        "--dim", type=int, choices=[2, 3], default=3, help="Vector dimension (2 or 3)"
    )
    p.add_argument(
        "-v1",
        "--vector1",
        required=False,
        help="Vector 1 components separated by space/comma",
    )
    p.add_argument(
        "-v2",
        "--vector2",
        required=False,
        help="Vector 2 components separated by space/comma",
    )
    p.add_argument("--no-plot", action="store_true", help="Disable plotting")
    return p


def run_cli(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    # Provide defaults if none given
    dim = args.dim
    default_v1 = (3, 4) if dim == 2 else (3, 4, 5)
    default_v2 = (1, -2) if dim == 2 else (1, -2, 3)

    v1 = parse_vector(args.vector1, dim) if args.vector1 else default_v1
    v2 = parse_vector(args.vector2, dim) if args.vector2 else default_v2

    d = dot(v1, v2)
    c = cross(v1, v2)

    print(f"v1 = {v1}")
    print(f"v2 = {v2}")
    print(f"Dot(v1,v2) = {d}")
    if dim == 2:
        print(f"Cross_z (scalar) = {c}")
    else:
        print(f"Cross(v1,v2) = {c}")
    print(f"|v1| = {magnitude(v1):.4g}")
    print(f"|v2| = {magnitude(v2):.4g}")

    if not args.no_plot:
        try:
            plot_vectors(v1, v2, c)
        except Exception as e:
            print(f"Plotting failed: {e}", file=sys.stderr)
            return 2
    return 0


# ----------------------------- Module Entry Point ----------------------------- #

if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(run_cli())
