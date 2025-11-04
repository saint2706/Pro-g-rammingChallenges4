"""Numerical checks for the double pendulum integrator."""

from math import radians

from challenges.Emulation.double_pendulum import (
    DoublePendulumConfig,
    DoublePendulumSimulator,
    compute_total_energy,
)


def test_energy_conservation_small_angles() -> None:
    cfg = DoublePendulumConfig()
    simulator = DoublePendulumSimulator(cfg)
    initial_state = (radians(10.0), 0.0, radians(15.0), 0.0)
    result = simulator.simulate(5.0, 0.005, initial_state)

    initial_energy = compute_total_energy(
        cfg,
        result.theta1[0],
        result.omega1[0],
        result.theta2[0],
        result.omega2[0],
    )

    max_delta = 0.0
    for th1, om1, th2, om2 in zip(
        result.theta1, result.omega1, result.theta2, result.omega2
    ):
        energy = compute_total_energy(cfg, th1, om1, th2, om2)
        max_delta = max(max_delta, abs(energy - initial_energy))

    assert max_delta < 1e-2
