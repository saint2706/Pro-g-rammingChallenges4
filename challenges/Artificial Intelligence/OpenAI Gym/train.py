"""Train or evaluate a reinforcement learning agent on Gymnasium environments.

This script uses Stable Baselines3's DQN implementation as a
practical, batteries-included baseline that new learners can extend.
It supports basic checkpointing, reproducible seeding, and a
train/eval command-line interface for experimentation.
"""

from __future__ import annotations

import argparse
import random
from datetime import datetime
from pathlib import Path
from typing import Callable, Dict

import gymnasium as gym
import numpy as np
import torch
from stable_baselines3 import DQN
from stable_baselines3.common.callbacks import (
    CallbackList,
    CheckpointCallback,
    EvalCallback,
)
from stable_baselines3.common.evaluation import evaluate_policy
from stable_baselines3.common.vec_env import DummyVecEnv


PRESETS: Dict[str, Dict[str, object]] = {
    "cartpole": {
        "env_id": "CartPole-v1",
        "total_timesteps": 100_000,
        "learning_rate": 5e-4,
        "buffer_size": 50_000,
        "batch_size": 64,
        "gamma": 0.99,
    },
    "lunarlander": {
        "env_id": "LunarLander-v2",
        "total_timesteps": 300_000,
        "learning_rate": 3e-4,
        "buffer_size": 200_000,
        "batch_size": 128,
        "gamma": 0.995,
    },
}


def make_env(env_id: str, seed: int) -> Callable[[], gym.Env]:
    """Construct a Gymnasium environment factory with deterministic seeding.

    Args:
        env_id: The ID of the Gymnasium environment.
        seed: The random seed for the environment.

    Returns:
        A function that creates and returns a Gymnasium environment.
    """

    def _init() -> gym.Env:
        env = gym.make(env_id)
        env.reset(seed=seed)
        env.action_space.seed(seed)
        env.observation_space.seed(seed)
        return env

    return _init


def set_global_seeds(seed: int) -> None:
    """Set seeds for Python, NumPy, and PyTorch RNGs.

    Args:
        seed: The random seed to use.
    """

    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)


def build_parser() -> argparse.ArgumentParser:
    """Builds and configures the argument parser for the script.

    Returns:
        An ArgumentParser object.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--mode",
        choices=("train", "eval"),
        default="train",
        help="Whether to train a new agent or evaluate a saved checkpoint.",
    )
    parser.add_argument(
        "--env-id",
        default="CartPole-v1",
        help="Gymnasium environment ID to target.",
    )
    parser.add_argument(
        "--total-timesteps",
        type=int,
        default=80_000,
        help="Total environment steps to train for.",
    )
    parser.add_argument(
        "--eval-episodes",
        type=int,
        default=10,
        help="Number of episodes to average over during evaluation.",
    )
    parser.add_argument(
        "--eval-frequency",
        type=int,
        default=10_000,
        help="Number of steps between evaluation runs during training.",
    )
    parser.add_argument(
        "--checkpoint-dir",
        type=Path,
        default=Path("checkpoints"),
        help="Directory to store checkpoint files.",
    )
    parser.add_argument(
        "--model-path",
        type=Path,
        default=Path("checkpoints") / "dqn_model",
        help="Path to store or load the final policy.",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=7,
        help="Random seed for reproducibility.",
    )
    parser.add_argument(
        "--learning-rate",
        type=float,
        default=1e-3,
        help="Learning rate for DQN training.",
    )
    parser.add_argument(
        "--buffer-size",
        type=int,
        default=50_000,
        help="Replay buffer size.",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=64,
        help="Batch size for training updates.",
    )
    parser.add_argument(
        "--gamma",
        type=float,
        default=0.99,
        help="Discount factor.",
    )
    parser.add_argument(
        "--exploration-final-eps",
        type=float,
        default=0.02,
        help="Final value of the epsilon-greedy exploration schedule.",
    )
    parser.add_argument(
        "--exploration-fraction",
        type=float,
        default=0.2,
        help="Fraction of training steps used for exploration annealing.",
    )
    parser.add_argument(
        "--preset",
        choices=tuple(PRESETS.keys()),
        help="Optional configuration preset to apply before running.",
    )
    return parser


def build_model(env: DummyVecEnv, args: argparse.Namespace) -> DQN:
    """Instantiate a DQN model configured for classic control tasks.

    Args:
        env: The vectorized environment to train on.
        args: The command-line arguments.

    Returns:
        A DQN model.
    """

    return DQN(
        "MlpPolicy",
        env,
        learning_rate=args.learning_rate,
        buffer_size=args.buffer_size,
        batch_size=args.batch_size,
        gamma=args.gamma,
        exploration_final_eps=args.exploration_final_eps,
        exploration_fraction=args.exploration_fraction,
        verbose=0,
        seed=args.seed,
        policy_kwargs={"net_arch": [128, 128]},
    )


def train(args: argparse.Namespace) -> None:
    """Train a DQN agent with periodic checkpointing and evaluation.

    Args:
        args: The command-line arguments.
    """

    set_global_seeds(args.seed)

    # Create the training and evaluation environments
    env = DummyVecEnv([make_env(args.env_id, args.seed)])
    eval_env = DummyVecEnv([make_env(args.env_id, args.seed + 1)])

    # Create directories for checkpoints and the final model
    args.checkpoint_dir.mkdir(parents=True, exist_ok=True)
    args.model_path.parent.mkdir(parents=True, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")

    model = build_model(env, args)

    # Set up callbacks for checkpointing and evaluation
    checkpoint_callback = CheckpointCallback(
        save_freq=max(args.eval_frequency, 1),
        save_path=str(args.checkpoint_dir),
        name_prefix=f"dqn_{args.env_id}_{timestamp}",
        save_replay_buffer=True,
    )
    eval_callback = EvalCallback(
        eval_env,
        best_model_save_path=str(args.checkpoint_dir / "best"),
        log_path=str(args.checkpoint_dir / "logs"),
        eval_freq=max(args.eval_frequency // env.num_envs, 1),
        n_eval_episodes=max(args.eval_episodes // 2, 1),
        deterministic=True,
    )

    # Train the model
    callbacks = CallbackList([checkpoint_callback, eval_callback])
    model.learn(total_timesteps=args.total_timesteps, callback=callbacks)

    # Save the final model
    model.save(str(args.model_path))
    print(f"Model saved to {args.model_path}")

    # Evaluate the final model
    mean_reward, std_reward = evaluate_policy(
        model,
        eval_env,
        n_eval_episodes=args.eval_episodes,
        deterministic=True,
    )
    print(
        f"Evaluation after training — mean reward: {mean_reward:.2f} ± {std_reward:.2f}"
    )


def evaluate(args: argparse.Namespace) -> None:
    """Evaluate a saved policy on the target environment.

    Args:
        args: The command-line arguments.
    """

    set_global_seeds(args.seed)

    env = DummyVecEnv([make_env(args.env_id, args.seed)])

    # Load the trained model
    model = DQN.load(str(args.model_path), env=env)
    # Evaluate the model
    mean_reward, std_reward = evaluate_policy(
        model,
        env,
        n_eval_episodes=args.eval_episodes,
        deterministic=True,
    )
    print(f"Evaluation — mean reward: {mean_reward:.2f} ± {std_reward:.2f}")


def main() -> None:
    """Main entry point for the script."""
    parser = build_parser()
    args = parser.parse_args()

    # Apply a preset configuration if specified
    if getattr(args, "preset", None):
        for key, value in PRESETS[args.preset].items():
            setattr(args, key, value)

    if args.mode == "train":
        train(args)
    else:
        evaluate(args)


if __name__ == "__main__":
    main()
