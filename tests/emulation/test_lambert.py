"""Regression tests for the Lambert solver."""

from __future__ import annotations

# ruff: noqa: E402

import pytest

pytest.importorskip("scipy")

import numpy as np

from Emulation.LambertsProblem import minimum_time_of_flight, solve_lambert_universal


@pytest.fixture()
def vallado_case() -> dict[str, np.ndarray | float]:
    """Return the benchmark problem from Vallado (Example 5-4)."""

    r1 = np.array([5000.0, 10000.0, 2100.0])
    r2 = np.array([-14600.0, 2500.0, 7000.0])
    dt = 3600.0
    mu = 398600.4418
    v1_expected = np.array([-5.992495, 1.925367, 3.245638])
    v2_expected = np.array([-3.312458, -4.196619, -0.385289])
    tmin_expected = 2761.371855
    return {
        "r1": r1,
        "r2": r2,
        "dt": dt,
        "mu": mu,
        "v1_expected": v1_expected,
        "v2_expected": v2_expected,
        "tmin_expected": tmin_expected,
    }


def test_universal_variable_matches_vallado(
    vallado_case: dict[str, np.ndarray | float],
) -> None:
    result = solve_lambert_universal(
        vallado_case["r1"],
        vallado_case["r2"],
        vallado_case["dt"],
        vallado_case["mu"],
    )

    np.testing.assert_allclose(
        result.v1, vallado_case["v1_expected"], rtol=0, atol=5e-4
    )
    np.testing.assert_allclose(
        result.v2, vallado_case["v2_expected"], rtol=0, atol=5e-4
    )
    np.testing.assert_allclose(
        result.time_of_flight(vallado_case["mu"]), vallado_case["dt"], atol=1e-6
    )


def test_minimum_time_of_flight_matches_reference(
    vallado_case: dict[str, np.ndarray | float],
) -> None:
    tof = minimum_time_of_flight(
        vallado_case["r1"], vallado_case["r2"], vallado_case["mu"]
    )
    assert tof == pytest.approx(vallado_case["tmin_expected"], abs=5e-6)


def test_invalid_geometry_raises() -> None:
    r1 = np.array([1.0, 0.0, 0.0])
    r2 = np.array([2.0, 0.0, 0.0])
    mu = 398600.4418

    with pytest.raises(ValueError):
        solve_lambert_universal(r1, r2, 1000.0, mu)

    with pytest.raises(ValueError):
        minimum_time_of_flight(r1, r2, mu)
