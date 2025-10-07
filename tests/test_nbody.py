"""Tests for the N-body simulator physics core."""

from __future__ import annotations

# ruff: noqa: E402

import math
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))

from pro_g_rammingchallenges4.nbody import Body, NBodySimulation, radius_from_mass


def test_gravitational_force_pair() -> None:
    sim = NBodySimulation(gravitational_constant=2.0, softening=0.0)
    body_a = Body(mass=5.0, position=[0.0, 0.0], velocity=[0.0, 0.0])
    body_b = Body(mass=10.0, position=[3.0, 0.0], velocity=[0.0, 0.0])
    fx, fy = sim.gravitational_force(body_a, body_b)
    expected = 2.0 * body_a.mass * body_b.mass / (3.0**2)
    assert math.isclose(fx, expected, rel_tol=1e-6)
    assert math.isclose(fy, 0.0, abs_tol=1e-12)
    fx_b, _ = sim.gravitational_force(body_b, body_a)
    assert math.isclose(fx_b, -expected, rel_tol=1e-6)


def test_collision_merge_conserves_mass_and_momentum() -> None:
    sim = NBodySimulation(gravitational_constant=0.0, softening=0.0)
    body_a = Body(mass=4.0, position=[0.0, 0.0], velocity=[1.0, 0.0], color=(255, 0, 0))
    body_b = Body(
        mass=6.0, position=[0.0, 0.0], velocity=[-1.0, 0.0], color=(0, 0, 255)
    )
    sim.bodies.extend([body_a, body_b])
    sim.step(0.0)
    assert len(sim.bodies) == 1
    merged = sim.bodies[0]
    assert math.isclose(merged.mass, 10.0)
    expected_velocity = (
        body_a.velocity[0] * body_a.mass + body_b.velocity[0] * body_b.mass
    ) / 10.0
    assert math.isclose(merged.velocity[0], expected_velocity)
    assert math.isclose(merged.velocity[1], 0.0)
    assert math.isclose(merged.radius, radius_from_mass(merged.mass))


def test_radius_from_mass_monotonic() -> None:
    r1 = radius_from_mass(1.0)
    r8 = radius_from_mass(8.0)
    assert r8 == pytest.approx(r1 * 8.0 ** (1 / 3))
    assert r8 > r1
