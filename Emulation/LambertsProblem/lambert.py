"""Lambert's problem solvers using the universal variable formulation.

The implementation follows the approach described by
- Bate, Mueller, and White, *Fundamentals of Astrodynamics* (Dover, 1971)
- Vallado, *Fundamentals of Astrodynamics and Applications*, 5th ed.

Only the zero-revolution solution (short- and long-way transfers) is
implemented, which is sufficient for most practical rendezvous and
interplanetary transfer design exercises.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isclose
from typing import Callable, Optional

import numpy as np

try:
    from scipy.optimize import newton
except ImportError as exc:  # pragma: no cover - scipy is part of repo extras
    raise ImportError(
        "scipy is required for the Lambert solver. Install the 'algorithmic' "
        "extra or add scipy to your environment."
    ) from exc


@dataclass(slots=True)
class LambertResult:
    """Container for Lambert solutions."""

    v1: np.ndarray
    v2: np.ndarray
    z: float
    iterations: int
    converged: bool
    A: float
    r1_mag: float
    r2_mag: float

    def time_of_flight(self, mu: float) -> float:
        """Return the actual time-of-flight implied by the solution."""

        if mu <= 0:
            raise ValueError("mu must be positive")
        return _time_of_flight(self.z, self.A, self.r1_mag, self.r2_mag, mu)


class LambertNoConvergenceError(RuntimeError):
    """Raised when the universal variable iteration fails to converge."""


def solve_lambert_universal(
    r1: np.ndarray,
    r2: np.ndarray,
    time_of_flight: float,
    mu: float,
    long_way: bool = False,
    max_iterations: int = 50,
    tol: float = 1e-8,
    bracket: Optional[tuple[float, float]] = None,
) -> LambertResult:
    """Solve Lambert's problem using the universal variable formulation.

    Parameters
    ----------
    r1, r2:
        Position vectors (km) at the start and end of the transfer arc.
    time_of_flight:
        Transfer time (seconds).
    mu:
        Gravitational parameter of the central body (km^3/s^2).
    long_way:
        Whether to take the long-way solution (Δν > π). Defaults to the short-way.
    max_iterations:
        Maximum number of Newton iterations for the universal variable root find.
    tol:
        Absolute tolerance on the time-of-flight residual (seconds).
    bracket:
        Optional tuple providing a lower and upper bound for the universal
        variable `z`. If omitted, the solver starts from ``z=0`` and lets the
        Newton iteration wander as needed.

    Returns
    -------
    LambertResult
        Velocities at the start and end of the transfer along with diagnostic
        information.
    """

    r1 = np.asarray(r1, dtype=float)
    r2 = np.asarray(r2, dtype=float)

    if r1.shape != (3,) or r2.shape != (3,):
        raise ValueError("r1 and r2 must be 3D vectors")
    if time_of_flight <= 0:
        raise ValueError("time_of_flight must be positive")
    if mu <= 0:
        raise ValueError("mu must be positive")

    r1_mag = np.linalg.norm(r1)
    r2_mag = np.linalg.norm(r2)
    if isclose(r1_mag, 0.0) or isclose(r2_mag, 0.0):
        raise ValueError("Position vectors must have non-zero magnitude")

    cos_dnu = float(np.dot(r1, r2) / (r1_mag * r2_mag))
    cos_dnu = np.clip(cos_dnu, -1.0, 1.0)
    sin_dnu = np.linalg.norm(np.cross(r1, r2)) / (r1_mag * r2_mag)
    if long_way:
        sin_dnu = -sin_dnu

    if isclose(sin_dnu, 0.0, abs_tol=1e-12):
        raise ValueError(
            "Cannot solve Lambert's problem for co-linear vectors without "
            "additional constraints."
        )

    A = sin_dnu * np.sqrt(r1_mag * r2_mag / (1 - cos_dnu))
    if A == 0:
        raise ValueError("Geometric configuration leads to A = 0; no solution.")

    tof_function = _build_time_of_flight_residual(A, r1_mag, r2_mag, mu, time_of_flight)

    iterations = 0

    def _callback(z: float) -> None:
        nonlocal iterations
        iterations += 1

    z0 = 0.0 if bracket is None else 0.5 * (bracket[0] + bracket[1])

    try:
        if bracket is None:
            z = _newton_with_callback(tof_function, z0, _callback, tol, max_iterations)
        else:
            z = _bracketed_newton(
                tof_function, bracket, z0, _callback, tol, max_iterations
            )
    except RuntimeError as exc:
        raise LambertNoConvergenceError(str(exc)) from exc

    # Reconstruct y, f, g, gdot for the velocity calculation.
    Cz = _stumpff_C(z)
    Sz = _stumpff_S(z)
    y = r1_mag + r2_mag + A * ((z * Sz - 1.0) / np.sqrt(Cz))
    if y <= 0:
        raise LambertNoConvergenceError("Intermediate y parameter became non-positive.")

    f = 1.0 - y / r1_mag
    g = A * np.sqrt(y / mu)
    gdot = 1.0 - y / r2_mag

    if isclose(g, 0.0, abs_tol=1e-12):
        raise LambertNoConvergenceError("g function evaluated to zero; degenerate case.")

    v1 = (r2 - f * r1) / g
    v2 = (gdot * r2 - r1) / g

    return LambertResult(
        v1=v1,
        v2=v2,
        z=z,
        iterations=iterations,
        converged=True,
        A=A,
        r1_mag=r1_mag,
        r2_mag=r2_mag,
    )


def minimum_time_of_flight(r1: np.ndarray, r2: np.ndarray, mu: float) -> float:
    """Return the minimum (parabolic) time-of-flight between two position vectors."""

    r1 = np.asarray(r1, dtype=float)
    r2 = np.asarray(r2, dtype=float)

    if mu <= 0:
        raise ValueError("mu must be positive")

    r1_mag = np.linalg.norm(r1)
    r2_mag = np.linalg.norm(r2)
    if isclose(r1_mag, 0.0) or isclose(r2_mag, 0.0):
        raise ValueError("Position vectors must have non-zero magnitude")

    cos_dnu = float(np.dot(r1, r2) / (r1_mag * r2_mag))
    cos_dnu = np.clip(cos_dnu, -1.0, 1.0)
    sin_dnu = np.linalg.norm(np.cross(r1, r2)) / (r1_mag * r2_mag)
    if isclose(sin_dnu, 0.0, abs_tol=1e-12):
        raise ValueError("Vectors are co-linear; parabolic time is undefined.")

    A = sin_dnu * np.sqrt(r1_mag * r2_mag / (1 - cos_dnu))
    return _time_of_flight(0.0, A, r1_mag, r2_mag, mu)


def _build_time_of_flight_residual(
    A: float, r1_mag: float, r2_mag: float, mu: float, target_tof: float
) -> Callable[[float], float]:
    """Create a residual function of the universal variable z."""

    def residual(z: float) -> float:
        tof = _time_of_flight(z, A, r1_mag, r2_mag, mu)
        return tof - target_tof

    return residual


def _time_of_flight(z: float, A: float, r1: float, r2: float, mu: float) -> float:
    """Time-of-flight helper compatible with the universal variable method."""

    Cz = _stumpff_C(z)
    Sz = _stumpff_S(z)

    if Cz <= 0:
        raise LambertNoConvergenceError("C(z) became non-positive; invalid geometry.")

    y = r1 + r2 + A * ((z * Sz - 1.0) / np.sqrt(Cz))
    if y < 0:
        raise LambertNoConvergenceError("Intermediate y parameter became negative.")

    return (((y / Cz) ** 1.5) * Sz + A * np.sqrt(y)) / np.sqrt(mu)


def _stumpff_C(z: float) -> float:
    if z > 1e-8:
        sqrt_z = np.sqrt(z)
        return (1.0 - np.cos(sqrt_z)) / z
    if z < -1e-8:
        sqrt_minus_z = np.sqrt(-z)
        return (np.cosh(sqrt_minus_z) - 1.0) / (-z)
    # Use series expansion around z = 0
    return 0.5 - z / 24.0 + z**2 / 720.0


def _stumpff_S(z: float) -> float:
    if z > 1e-8:
        sqrt_z = np.sqrt(z)
        return (sqrt_z - np.sin(sqrt_z)) / (sqrt_z**3)
    if z < -1e-8:
        sqrt_minus_z = np.sqrt(-z)
        return (np.sinh(sqrt_minus_z) - sqrt_minus_z) / (sqrt_minus_z**3)
    # Series expansion around z = 0
    return 1.0 / 6.0 - z / 120.0 + z**2 / 5040.0


def _newton_with_callback(
    func: Callable[[float], float],
    x0: float,
    callback: Callable[[float], None],
    tol: float,
    max_iterations: int,
) -> float:
    """Run Newton's method with derivative supplied numerically."""

    def fprime(x: float) -> float:
        h = 1e-5 * max(1.0, abs(x))
        return (func(x + h) - func(x - h)) / (2.0 * h)

    def wrapped(x: float) -> float:
        callback(x)
        return func(x)

    return newton(wrapped, x0=x0, fprime=fprime, tol=tol, maxiter=max_iterations)


def _bracketed_newton(
    func: Callable[[float], float],
    bracket: tuple[float, float],
    x0: float,
    callback: Callable[[float], None],
    tol: float,
    max_iterations: int,
) -> float:
    """Fallback for Newton iterations with an explicit bracket."""

    a, b = bracket
    if a >= b:
        raise ValueError("Invalid bracket; lower bound must be < upper bound")

    fa = func(a)
    fb = func(b)
    if fa * fb > 0:
        raise ValueError("Time-of-flight residual does not change sign in the bracket")

    x = x0
    for _ in range(max_iterations):
        callback(x)
        fx = func(x)
        if abs(fx) < tol:
            return x
        dfdx = (func(x + 1e-6) - func(x - 1e-6)) / 2e-6
        if dfdx == 0:
            # Bisection fallback
            x = 0.5 * (a + b)
        else:
            x_new = x - fx / dfdx
            if not (a < x_new < b):
                x_new = 0.5 * (a + b)
            if fx * fa < 0:
                b, fb = x, fx
            else:
                a, fa = x, fx
            x = x_new
    raise RuntimeError("Bracketed Newton method failed to converge")


__all__ = [
    "LambertResult",
    "LambertNoConvergenceError",
    "solve_lambert_universal",
    "minimum_time_of_flight",
]
