"""Command-line interface for running the roguelike AI agent."""

from __future__ import annotations

import argparse
import logging
import random
from typing import Sequence

from .environment import RoguelikeEnvironment, create_sample_environment
from .mcts import ActionEvaluation, MCTSAgent


def _format_action_stats(stats: Sequence[ActionEvaluation]) -> str:
    if not stats:
        return "<no actions explored>"
    rows = ["Action       Visits  Mean Reward  Total Reward"]
    for entry in stats:
        rows.append(
            f"{entry.action.name:<11} {entry.visits:>6}  {entry.mean_reward:>11.2f}  {entry.total_reward:>12.2f}"
        )
    return "\n".join(rows)


def _log_environment(logger: logging.Logger, env: RoguelikeEnvironment) -> None:
    summary = env.summary()
    logger.info(
        "Turn %(turn)d | HP=%(player_hp)d | hostiles=%(hostiles)d | hostile HP=%(hostile_hp)d",
        summary,
    )


def run_episode(
    *,
    turns: int,
    iterations: int,
    rollout_depth: int,
    exploration: float,
    seed: int | None,
    visualise: bool,
) -> None:
    if seed is not None:
        random.seed(seed)

    env = create_sample_environment(seed=seed)
    agent = MCTSAgent(
        iterations=iterations,
        rollout_depth=rollout_depth,
        exploration_weight=exploration,
        rng=random.Random(seed),
    )
    logger = logging.getLogger("ai_roguelike")

    logger.info("Starting automated run for %d turns", turns)
    if visualise:
        print(env.to_ascii())
        print()

    for turn in range(1, turns + 1):
        if env.is_terminal:
            logger.info("Episode finished early at turn %d", turn - 1)
            break
        action, stats = agent.choose_action(env)
        logger.info("Chosen action: %s", action.name)
        logger.debug("\n%s", _format_action_stats(stats))
        reward, done = env.apply_action(action)
        logger.info("Reward %.2f", reward)
        _log_environment(logger, env)
        if visualise:
            print(env.to_ascii())
            print()
        if done:
            logger.info("Terminal state reached at turn %d", turn)
            break
    else:
        logger.info("Reached turn limit without a terminal state")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Headless roguelike runner powered by a Monte Carlo agent.",
    )
    parser.add_argument(
        "--turns", type=int, default=20, help="Maximum number of turns to simulate"
    )
    parser.add_argument(
        "--iterations", type=int, default=96, help="MCTS iterations to run each turn"
    )
    parser.add_argument(
        "--rollout-depth", type=int, default=4, help="Depth of each random rollout"
    )
    parser.add_argument(
        "--exploration", type=float, default=1.1, help="Exploration constant for UCT"
    )
    parser.add_argument(
        "--seed", type=int, default=None, help="Seed for deterministic runs"
    )
    parser.add_argument(
        "--log-file",
        default=None,
        help="Optional path to write detailed logs (defaults to stderr only)",
    )
    parser.add_argument(
        "--visualise",
        action="store_true",
        help="Print an ASCII view of the dungeon after each decision",
    )
    parser.add_argument(
        "--log-level",
        default="INFO",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Verbosity for console logging",
    )
    return parser


def configure_logging(level: str, *, log_file: str | None) -> None:
    logging_kwargs: dict = {
        "level": getattr(logging, level.upper(), logging.INFO),
        "format": "%(asctime)s | %(name)s | %(levelname)s | %(message)s",
    }
    handlers = []
    if log_file:
        handlers.append(logging.FileHandler(log_file, mode="w", encoding="utf-8"))
    handlers.append(logging.StreamHandler())
    logging_kwargs["handlers"] = handlers
    logging.basicConfig(**logging_kwargs)


def main(argv: Sequence[str] | None = None) -> None:
    parser = build_parser()
    args = parser.parse_args(argv)
    configure_logging(args.log_level, log_file=args.log_file)
    run_episode(
        turns=args.turns,
        iterations=args.iterations,
        rollout_depth=args.rollout_depth,
        exploration=args.exploration,
        seed=args.seed,
        visualise=args.visualise,
    )


if __name__ == "__main__":
    main()
