"""Utility policies for quick Gymnasium regression tests."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol

import numpy as np


class StatelessPolicy(Protocol):
    """Protocol for simple policies that map observations to discrete actions."""

    def __call__(self, observation: np.ndarray) -> int:
        """Return the action to take for the provided observation."""


@dataclass(frozen=True)
class CartPoleHeuristic:
    """A lightweight controller for CartPole-v1 using angle feedback."""

    pole_angle_threshold: float = 0.0
    pole_velocity_scale: float = 0.5

    def __call__(self, observation: np.ndarray) -> int:
        """Tilt the cart toward the pole's lean direction to keep it upright."""

        # CartPole observation: [cart position, cart velocity, pole angle, pole angular velocity]
        _, _, theta, theta_dot = observation
        lean_measure = theta + self.pole_velocity_scale * theta_dot
        return 1 if lean_measure >= self.pole_angle_threshold else 0


def cartpole_balance_policy() -> StatelessPolicy:
    """Return a deterministic heuristic that keeps the CartPole upright."""

    return CartPoleHeuristic()
