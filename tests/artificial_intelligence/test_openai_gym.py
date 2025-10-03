"""Regression tests for the OpenAI Gym reinforcement-learning project."""

from __future__ import annotations

from pathlib import Path
from statistics import mean
from typing import Callable, Sequence
from runpy import run_path

import gymnasium as gym
import pytest
from stable_baselines3.common.monitor import Monitor

PROJECT_ROOT = Path("Artificial Intelligence/OpenAI Gym")
POLICY_PATH = PROJECT_ROOT / "policies.py"


@pytest.mark.skipif(
    not POLICY_PATH.exists(),
    reason="Heuristic policy helper is missing",
)
def test_cartpole_heuristic_policy_has_high_reward() -> None:
    """The bundled heuristic should keep the CartPole upright for long episodes."""

    module_globals = run_path(str(POLICY_PATH))
    policy_factory: Callable[[], Callable[[Sequence[float]], int]] = module_globals[
        "cartpole_balance_policy"
    ]
    policy = policy_factory()

    seed = 123
    env = gym.make("CartPole-v1")
    env = Monitor(env)
    rewards = []

    for episode in range(5):
        observation, _ = env.reset(seed=seed + episode)
        terminated = truncated = False
        total_reward = 0.0

        while not (terminated or truncated):
            action = policy(observation)
            observation, reward, terminated, truncated, _ = env.step(action)
            total_reward += reward

        rewards.append(total_reward)

    env.close()

    assert mean(rewards) >= 150
