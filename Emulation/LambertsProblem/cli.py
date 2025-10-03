"""Command line utilities for Lambert's orbital boundary value problem."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Iterable, List

import numpy as np

from .lambert import (
    LambertNoConvergenceError,
    minimum_time_of_flight,
    solve_lambert_universal,
)


EARTH_MU = 398600.4418  # km^3 / s^2


def parse_vector(values: Iterable[str]) -> np.ndarray:
    data = [float(v) for v in values]
    if len(data) != 3:
        raise argparse.ArgumentTypeError("Position vectors must have three components")
    return np.array(data, dtype=float)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Solve Lambert's problem using the universal variable method. "
            "Provide start and end position vectors (km) along with a transfer "
            "time (seconds) to obtain the boundary velocities."
        )
    )
    parser.add_argument(
        "--mu",
        type=float,
        default=EARTH_MU,
        help="Gravitational parameter μ in km^3/s^2 (default: Earth's value).",
    )
    parser.add_argument(
        "--long-way",
        action="store_true",
        help="Use the long-way solution (transfer angle > 180 degrees).",
    )
    parser.add_argument(
        "--plot",
        action="store_true",
        help="Plot the numerically integrated two-body trajectory (requires matplotlib).",
    )
    parser.add_argument(
        "--samples",
        type=int,
        default=200,
        help="Number of integration samples when plotting (default: 200).",
    )
    parser.add_argument(
        "--json",
        type=Path,
        help="Optional path to dump the Lambert solution as JSON.",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    solve_parser = subparsers.add_parser(
        "solve",
        help="Solve for the transfer velocities given a time-of-flight.",
    )
    solve_parser.add_argument("r1", nargs=3, metavar="R1", help="Start position vector components (km).")
    solve_parser.add_argument("r2", nargs=3, metavar="R2", help="End position vector components (km).")
    solve_parser.add_argument(
        "time_of_flight",
        type=float,
        help="Transfer time in seconds.",
    )

    info_parser = subparsers.add_parser(
        "analyze",
        help="Report minimum transfer time without solving for velocities.",
    )
    info_parser.add_argument("r1", nargs=3, metavar="R1", help="Start position vector components (km).")
    info_parser.add_argument("r2", nargs=3, metavar="R2", help="End position vector components (km).")

    return parser


def solve_command(args: argparse.Namespace) -> int:
    r1 = parse_vector(args.r1)
    r2 = parse_vector(args.r2)
    try:
        result = solve_lambert_universal(
            r1,
            r2,
            time_of_flight=float(args.time_of_flight),
            mu=args.mu,
            long_way=args.long_way,
        )
    except (ValueError, LambertNoConvergenceError) as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1

    print("Lambert solution (universal variable method)")
    print(f"  r1 = {r1}")
    print(f"  r2 = {r2}")
    print(f"  μ  = {args.mu:.6f} km^3/s^2")
    print(f"  Transfer time = {float(args.time_of_flight):.6f} s")
    print(f"  Iterations: {result.iterations}")
    print(f"  Universal variable z = {result.z:.8f}")
    print(f"  v1 = {result.v1} km/s")
    print(f"  v2 = {result.v2} km/s")

    if args.json:
        payload = {
            "v1": result.v1.tolist(),
            "v2": result.v2.tolist(),
            "z": result.z,
            "iterations": result.iterations,
            "long_way": bool(args.long_way),
            "time_of_flight": float(args.time_of_flight),
            "mu": args.mu,
        }
        args.json.write_text(json.dumps(payload, indent=2))
        print(f"Solution written to {args.json}")

    if args.plot:
        _plot_transfer(r1, result.v1, args.mu, float(args.time_of_flight), args.samples)

    return 0


def analyze_command(args: argparse.Namespace) -> int:
    r1 = parse_vector(args.r1)
    r2 = parse_vector(args.r2)
    try:
        tof = minimum_time_of_flight(r1, r2, mu=args.mu)
    except ValueError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1

    print("Parabolic (minimum) transfer time")
    print(f"  r1 = {r1}")
    print(f"  r2 = {r2}")
    print(f"  μ  = {args.mu:.6f} km^3/s^2")
    print(f"  Minimum time-of-flight ≈ {tof:.6f} s")
    return 0


def _plot_transfer(
    r0: np.ndarray,
    v0: np.ndarray,
    mu: float,
    time_of_flight: float,
    samples: int,
) -> None:
    try:
        import matplotlib.pyplot as plt
        from mpl_toolkits.mplot3d import Axes3D  # noqa: F401 - side effect import
    except ImportError as exc:  # pragma: no cover - optional dependency
        raise SystemExit(
            "matplotlib is required for plotting. Install the 'visual' extra or "
            "matplotlib manually."
        ) from exc

    try:
        from scipy.integrate import solve_ivp
    except ImportError as exc:  # pragma: no cover - scipy already required
        raise SystemExit("scipy is required for propagation but is missing.") from exc

    def dynamics(_t: float, state: np.ndarray) -> np.ndarray:
        r = state[:3]
        v = state[3:]
        r_norm = np.linalg.norm(r)
        acc = -mu * r / r_norm**3
        return np.hstack((v, acc))

    state0 = np.hstack((r0, v0))
    sol = solve_ivp(
        dynamics,
        (0.0, time_of_flight),
        state0,
        t_eval=np.linspace(0.0, time_of_flight, samples),
        rtol=1e-9,
        atol=1e-9,
        vectorized=False,
    )

    if not sol.success:  # pragma: no cover - defensive
        raise SystemExit(f"Propagation failed: {sol.message}")

    fig = plt.figure()
    ax = fig.add_subplot(111, projection="3d")
    ax.plot(sol.y[0], sol.y[1], sol.y[2], label="Transfer arc")
    ax.scatter([r0[0]], [r0[1]], [r0[2]], color="green", label="Departure")
    ax.scatter([sol.y[0, -1]], [sol.y[1, -1]], [sol.y[2, -1]], color="red", label="Arrival")
    ax.set_xlabel("x (km)")
    ax.set_ylabel("y (km)")
    ax.set_zlabel("z (km)")
    ax.legend()
    ax.set_title("Lambert transfer trajectory")
    plt.show()


def main(argv: List[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    if args.command == "solve":
        return solve_command(args)
    if args.command == "analyze":
        return analyze_command(args)
    parser.error("No command provided")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
