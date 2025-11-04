"""Lambert's problem solvers using the universal variable formulation.

This module provides functions to solve Lambert's problem, which is the task of
finding the orbit that connects two position vectors in a given amount of time.
The implementation uses the universal variable formulation, which is robust
for elliptical, parabolic, and hyperbolic orbits.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isclose
from typing import Callable, Optional

import numpy as np

try:
    from scipy.optimize import newton
except ImportError as exc:
    raise ImportError(
        "scipy is required for the Lambert solver. Install the 'algorithmic' "
        "extra or add scipy to your environment."
    ) from exc


@dataclass(slots=True)
class LambertResult:
    """Container for the results of a Lambert's problem solution.

    Attributes:
        v1: The initial velocity vector.
        v2: The final velocity vector.
        z: The converged value of the universal variable.
        iterations: The number of iterations required for convergence.
        converged: A boolean indicating if the solver converged.
        A: A geometric parameter of the transfer.
        r1_mag: The magnitude of the initial position vector.
        r2_mag: The magnitude of the final position vector.
    """

    v1: np.ndarray
    v2: np.ndarray
    z: float
    iterations: int
    converged: bool
    A: float
    r1_mag: float
    r2_mag: float

    def time_of_flight(self, mu: float) -> float:
        """Calculates the time of flight for the solved trajectory.

        Args:
            mu: The gravitational parameter of the central body.

        Returns:
            The time of flight in seconds.
        """
        if mu <= 0:
            raise ValueError("mu must be positive.")
        return _time_of_flight(self.z, self.A, self.r1_mag, self.r2_mag, mu)


class LambertNoConvergenceError(RuntimeError):
    """Raised when the Lambert solver fails to converge."""


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
    """Solves Lambert's problem using the universal variable formulation.

    Args:
        r1: The initial position vector.
        r2: The final position vector.
        time_of_flight: The time of flight for the transfer.
        mu: The gravitational parameter of the central body.
        long_way: Whether to solve for the long-way (Δν > 180°) transfer.
        max_iterations: The maximum number of iterations for the root-finding algorithm.
        tol: The tolerance for the time-of-flight convergence.
        bracket: An optional tuple providing a search bracket for the universal variable z.

    Returns:
        A LambertResult object with the solution.
    """
    r1, r2 = np.asarray(r1, dtype=float), np.asarray(r2, dtype=float)
    if r1.shape != (3,) or r2.shape != (3,):
        raise ValueError("Position vectors must be 3D.")
    if time_of_flight <= 0 or mu <= 0:
        raise ValueError("Time of flight and mu must be positive.")

    r1_mag, r2_mag = np.linalg.norm(r1), np.linalg.norm(r2)
    cos_dnu = np.clip(np.dot(r1, r2) / (r1_mag * r2_mag), -1.0, 1.0)
    A = np.sqrt(r1_mag * r2_mag * (1 + cos_dnu))
    if not long_way:
        A = -A

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

    y = _y(z, A, r1_mag, r2_mag)
    f = 1 - y / r1_mag
    g = A * np.sqrt(y / mu)
    g_dot = 1 - y / r2_mag

    v1 = (r2 - f * r1) / g
    v2 = (g_dot * r2 - r1) / g

    return LambertResult(v1, v2, z, iterations, True, A, r1_mag, r2_mag)


def minimum_time_of_flight(r1: np.ndarray, r2: np.ndarray, mu: float) -> float:
    """Calculates the minimum (parabolic) time of flight between two points.

    Args:
        r1: The initial position vector.
        r2: The final position vector.
        mu: The gravitational parameter of the central body.

    Returns:
        The minimum time of flight.
    """
    r1_mag, r2_mag = np.linalg.norm(r1), np.linalg.norm(r2)
    cos_dnu = np.dot(r1, r2) / (r1_mag * r2_mag)
    A = np.sqrt(r1_mag * r2_mag * (1 + cos_dnu))
    return _time_of_flight(0.0, A, r1_mag, r2_mag, mu)


def _build_time_of_flight_residual(
    A: float, r1_mag: float, r2_mag: float, mu: float, target_tof: float
) -> Callable[[float], float]:
    """Creates a function that returns the residual of the time-of-flight equation."""

    def residual(z: float) -> float:
        return _time_of_flight(z, A, r1_mag, r2_mag, mu) - target_tof

    return residual


def _time_of_flight(z: float, A: float, r1: float, r2: float, mu: float) -> float:
    """Calculates the time of flight for a given universal variable z."""
    y = _y(z, A, r1, r2)
    Cz = _stumpff_C(z)
    Sz = _stumpff_S(z)
    return (y**1.5 * Sz + A * np.sqrt(y)) / np.sqrt(mu)


def _y(z: float, A: float, r1: float, r2: float) -> float:
    """An intermediate variable in the universal variable formulation."""
    Cz = _stumpff_C(z)
    return r1 + r2 + A * (_stumpff_S(z) * z - 1) / np.sqrt(Cz)


def _stumpff_C(z: float) -> float:
    """The Stumpff C function, used in the universal variable formulation."""
    if z > 1e-6:
        return (1 - np.cos(np.sqrt(z))) / z
    elif z < -1e-6:
        return (np.cosh(np.sqrt(-z)) - 1) / (-z)
    return 0.5 - z / 24


def _stumpff_S(z: float) -> float:
    """The Stumpff S function, used in the universal variable formulation."""
    if z > 1e-6:
        sqrt_z = np.sqrt(z)
        return (sqrt_z - np.sin(sqrt_z)) / (sqrt_z**3)
    elif z < -1e-6:
        sqrt_mz = np.sqrt(-z)
        return (np.sinh(sqrt_mz) - sqrt_mz) / (sqrt_mz**3)
    return 1 / 6 - z / 120


def _newton_with_callback(
    func: Callable[[float], float],
    x0: float,
    callback: Callable[[float], None],
    tol: float,
    max_iterations: int,
) -> float:
    """A wrapper for scipy.optimize.newton that includes a callback."""

    def wrapped(x: float) -> float:
        callback(x)
        return func(x)

    return newton(wrapped, x0=x0, tol=tol, maxiter=max_iterations)


def _bracketed_newton(
    func: Callable[[float], float],
    bracket: tuple[float, float],
    x0: float,
    callback: Callable[[float], None],
    tol: float,
    max_iterations: int,
) -> float:
    """A Newton's method solver with a fallback to bisection if it goes out of bounds."""
    a, b = bracket
    if a >= b:
        raise ValueError("Invalid bracket.")
    fa, fb = func(a), func(b)
    if fa * fb > 0:
        raise ValueError("The function does not change sign over the bracket.")

    x = x0
    for _ in range(max_iterations):
        callback(x)
        fx = func(x)
        if abs(fx) < tol:
            return x
        dfdx = (func(x + 1e-6) - func(x - 1e-6)) / 2e-6
        if dfdx == 0:
            x = (a + b) / 2
        else:
            x_new = x - fx / dfdx
            if not a < x_new < b:
                x_new = (a + b) / 2
            x = x_new
        if fa * fx < 0:
            b = x
        else:
            a = x
    raise RuntimeError("Bracketed Newton method failed to converge.")


__all__ = [
    "LambertResult",
    "LambertNoConvergenceError",
    "solve_lambert_universal",
    "minimum_time_of_flight",
]
