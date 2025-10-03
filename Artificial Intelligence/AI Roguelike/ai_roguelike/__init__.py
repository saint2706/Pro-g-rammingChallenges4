"""AI agent utilities for the Roguelike challenge.

The module exposes helpers to create headless game environments and
plan turns using Monte Carlo Tree Search.  The implementation leans on
``Games.Roguelike`` for the underlying game rules so that automated
agents can run the exact same content as the interactive client.
"""

from .environment import (
    Action,
    RoguelikeEnvironment,
    create_environment,
    create_sample_environment,
    render_ascii,
)
from .mcts import ActionEvaluation, MCTSAgent

__all__ = [
    "Action",
    "ActionEvaluation",
    "MCTSAgent",
    "RoguelikeEnvironment",
    "create_environment",
    "create_sample_environment",
    "render_ascii",
]
