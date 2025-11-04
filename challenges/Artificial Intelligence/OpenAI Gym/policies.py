"""Utility policies for quick Gymnasium regression tests."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Protocol

import numpy as np


class StatelessPolicy(Protocol):
    """Protocol for simple policies that map observations to discrete actions."""

    def __call__(self, observation: np.ndarray) -> int:
        """Return the action to take for the provided observation.

        Args:
            observation: The environment observation.

        Returns:
            The action to take.
        """


@dataclass(frozen=True)
class CartPoleHeuristic:
    """A lightweight controller for CartPole-v1 using angle feedback.

    This heuristic policy determines the direction to push the cart based on the
    pole's angle and angular velocity. It's a simple, deterministic policy
    that can be used for testing and baseline comparisons.

    Attributes:
        pole_angle_threshold: The threshold for the pole's lean measure.
        pole_velocity_scale: A scaling factor for the pole's angular velocity.
    """

    pole_angle_threshold: float = 0.0
    pole_velocity_scale: float = 0.5

    def __call__(self, observation: np.ndarray) -> int:
        """Tilt the cart toward the pole's lean direction to keep it upright.

        Args:
            observation: The environment observation, which is expected to be a
                NumPy array containing [cart position, cart velocity,
                pole angle, pole angular velocity].

        Returns:
            The action to take (0 for push left, 1 for push right).
        """

        # CartPole observation: [cart position, cart velocity, pole angle, pole angular velocity]
        _, _, theta, theta_dot = observation
        lean_measure = theta + self.pole_velocity_scale * theta_dot
        return 1 if lean_measure >= self.pole_angle_threshold else 0


def cartpole_balance_policy() -> StatelessPolicy:
    """Return a deterministic heuristic that keeps the CartPole upright."""

    return CartPoleHeuristic()
