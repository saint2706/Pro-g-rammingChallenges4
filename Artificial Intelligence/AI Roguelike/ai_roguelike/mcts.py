"""Monte Carlo Tree Search planner for the roguelike environment."""

from __future__ import annotations

import math
import random
from dataclasses import dataclass
from typing import List, Optional, Sequence, Tuple

from .environment import Action, RoguelikeEnvironment


@dataclass
class ActionEvaluation:
    """Aggregated statistics for an action explored by MCTS."""

    action: Action
    visits: int
    total_reward: float

    @property
    def mean_reward(self) -> float:
        return self.total_reward / self.visits if self.visits else 0.0


class _TreeNode:
    __slots__ = (
        "env",
        "parent",
        "action",
        "children",
        "untried_actions",
        "visits",
        "total_reward",
        "terminal",
    )

    def __init__(
        self,
        env: RoguelikeEnvironment,
        *,
        parent: Optional["_TreeNode"] = None,
        action: Optional[Action] = None,
    ) -> None:
        self.env = env
        self.parent = parent
        self.action = action
        self.children: List[_TreeNode] = []
        self.untried_actions: List[Action] = list(env.available_actions())
        self.visits = 0
        self.total_reward = 0.0
        self.terminal = env.is_terminal

    def is_fully_expanded(self) -> bool:
        return not self.untried_actions

    def best_child(self, exploration_weight: float) -> "_TreeNode":
        if not self.children:
            raise RuntimeError("best_child called on a leaf node with no children")
        log_parent = math.log(self.visits) if self.visits > 0 else 0.0
        best_score = float("-inf")
        best = self.children[0]
        for child in self.children:
            if child.visits == 0:
                return child
            exploit = child.total_reward / child.visits
            explore = math.sqrt(log_parent / child.visits) if child.visits else 0.0
            score = exploit + exploration_weight * explore
            if score > best_score:
                best_score = score
                best = child
        return best

    def expand(self, rng: random.Random) -> "_TreeNode":
        if not self.untried_actions:
            raise RuntimeError("expand called on fully expanded node")
        index = rng.randrange(len(self.untried_actions)) if len(self.untried_actions) > 1 else 0
        action = self.untried_actions.pop(index)
        child_env = self.env.clone()
        child_env.step(action)
        child = _TreeNode(child_env, parent=self, action=action)
        self.children.append(child)
        return child

    def backpropagate(self, reward: float) -> None:
        node: Optional[_TreeNode] = self
        while node is not None:
            node.visits += 1
            node.total_reward += reward
            node = node.parent


class MCTSAgent:
    """Simple MCTS controller for the roguelike."""

    def __init__(
        self,
        *,
        iterations: int = 64,
        rollout_depth: int = 4,
        exploration_weight: float = 1.4,
        rng: Optional[random.Random] = None,
    ) -> None:
        self.iterations = iterations
        self.rollout_depth = rollout_depth
        self.exploration_weight = exploration_weight
        self.rng = rng or random.Random()

    def choose_action(
        self, env: RoguelikeEnvironment
    ) -> Tuple[Action, Sequence[ActionEvaluation]]:
        """Return the next action and statistics about explored actions."""

        if env.is_terminal:
            raise RuntimeError("Environment is terminal; no actions available")

        root = _TreeNode(env.clone())
        for _ in range(self.iterations):
            node = root
            # Selection
            while node.is_fully_expanded() and node.children and not node.terminal:
                node = node.best_child(self.exploration_weight)
            # Expansion
            if not node.terminal and node.untried_actions:
                node = node.expand(self.rng)
            # Simulation
            reward = self._rollout(node)
            # Backpropagation
            node.backpropagate(reward)

        if not root.children:
            raise RuntimeError("Planner failed to explore any actions")

        best_child = max(root.children, key=lambda child: child.visits)
        evaluations = tuple(
            ActionEvaluation(action=child.action, visits=child.visits, total_reward=child.total_reward)
            for child in sorted(root.children, key=lambda c: c.visits, reverse=True)
        )
        return best_child.action, evaluations

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------
    def _rollout(self, node: _TreeNode) -> float:
        if node.terminal:
            return 0.0
        rollout_env = node.env.clone()
        total_reward = 0.0
        for _ in range(self.rollout_depth):
            actions = rollout_env.available_actions()
            if not actions:
                break
            action = self.rng.choice(actions)
            reward, done = rollout_env.step(action)
            total_reward += reward
            if done:
                break
        return total_reward


__all__ = ["ActionEvaluation", "MCTSAgent"]
