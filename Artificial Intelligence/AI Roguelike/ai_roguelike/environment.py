"""Headless environment helpers for the Roguelike challenge.

The environment exposes a light-weight wrapper around ``Games.Roguelike``'s
``Engine`` so that automated agents can plan in the same world model used by
the interactive client.  It provides deterministic seeding, a compact action
space, heuristics for evaluating states, and convenience helpers for
rendering an ASCII representation of the dungeon for logging purposes.
"""

from __future__ import annotations

import copy
import random
from dataclasses import dataclass
from typing import List, Optional, Sequence, Tuple

import numpy as np

from Games.Roguelike.config import GameConfig
from Games.Roguelike.entity import Actor
from Games.Roguelike.main import new_game

# The roguelike is fundamentally tile based so a Moore neighbourhood (including
# diagonals) plus a wait action offers a compact-yet-expressive action space for
# planning algorithms like MCTS.


@dataclass(frozen=True)
class Action:
    """High-level intent for the agent.

    ``dx`` and ``dy`` map onto the player's attempted movement vector.
    ``name`` is used purely for logging/visualisation and keeps CLI output
    readable without additional lookups.
    """

    name: str
    dx: int
    dy: int


ALL_ACTIONS: Tuple[Action, ...] = (
    Action("north", 0, -1),
    Action("south", 0, 1),
    Action("west", -1, 0),
    Action("east", 1, 0),
    Action("north_west", -1, -1),
    Action("north_east", 1, -1),
    Action("south_west", -1, 1),
    Action("south_east", 1, 1),
    Action("wait", 0, 0),
)


class RoguelikeEnvironment:
    """Mutable game wrapper tailored for automated agents."""

    def __init__(self, engine, *, turn: int = 0) -> None:
        self.engine = engine
        self.turn = turn
        self._terminal_cache: Optional[bool] = None

    # ------------------------------------------------------------------
    # Creation helpers
    # ------------------------------------------------------------------
    @classmethod
    def from_config(
        cls, config: GameConfig, *, seed: Optional[int] = None
    ) -> "RoguelikeEnvironment":
        """Create a fresh environment using ``config``.

        ``Games.Roguelike`` relies on Python's global PRNG during dungeon
        generation.  For deterministic tests/experiments ``seed`` can be
        supplied to seed both :mod:`random` and :mod:`numpy.random`.
        """

        if seed is not None:
            random.seed(seed)
            np.random.seed(seed % (2**32))
        engine = new_game(config)
        return cls(engine)

    @classmethod
    def sample(
        cls,
        *,
        seed: Optional[int] = None,
        map_width: int = 40,
        map_height: int = 30,
        max_rooms: int = 8,
        max_monsters_per_room: int = 2,
        max_items_per_room: int = 1,
    ) -> "RoguelikeEnvironment":
        """Return a compact dungeon suitable for automated smoke-tests."""

        config = GameConfig(
            map_width=map_width,
            map_height=map_height,
            max_rooms=max_rooms,
            max_monsters_per_room=max_monsters_per_room,
            max_items_per_room=max_items_per_room,
            screen_width=map_width,
            screen_height=map_height + 7,
        )
        return cls.from_config(config, seed=seed)

    # ------------------------------------------------------------------
    # Basic environment protocol
    # ------------------------------------------------------------------
    def clone(self) -> "RoguelikeEnvironment":
        """Deep copy the environment so planners can explore hypotheticals."""

        return RoguelikeEnvironment(copy.deepcopy(self.engine), turn=self.turn)

    def available_actions(self) -> Sequence[Action]:
        """Return the actions that are legal in the current state."""

        if self.is_terminal:
            return ()

        actions: List[Action] = []
        player = self.engine.player
        game_map = self.engine.game_map
        for action in ALL_ACTIONS:
            if action.dx == 0 and action.dy == 0:
                actions.append(action)
                continue
            dest_x = player.x + action.dx
            dest_y = player.y + action.dy
            if not game_map.in_bounds(dest_x, dest_y):
                continue
            tile = game_map.tiles[dest_x, dest_y]
            if not tile["walkable"]:
                # Wall or obstacle – the only time we allow stepping into it is
                # if an enemy blocks the way so we can attack.
                target = game_map.get_blocking_entity_at_location(dest_x, dest_y)
                if isinstance(target, Actor) and target is not player and target.is_alive:
                    actions.append(action)
                continue
            blocker = game_map.get_blocking_entity_at_location(dest_x, dest_y)
            if blocker and blocker is not player:
                # Enemy standing on a floor tile: still a legal attack action.
                if isinstance(blocker, Actor) and blocker.is_alive:
                    actions.append(action)
                continue
            actions.append(action)
        return tuple(actions)

    def step(self, action: Action) -> Tuple[float, bool]:
        """Apply ``action`` in-place.

        Returns a reward signal derived from the heuristic evaluation change and
        a boolean indicating whether the game reached a terminal state.
        """

        if self.is_terminal:
            return 0.0, True

        before = self.evaluate()
        self._apply_action(action)
        after = self.evaluate()
        self.turn += 1
        self._terminal_cache = None
        reward = after - before
        return reward, self.is_terminal

    def apply_action(self, action: Action) -> Tuple[float, bool]:
        """Alias for :meth:`step` used by higher-level orchestration."""

        return self.step(action)

    # ------------------------------------------------------------------
    # Introspection helpers
    # ------------------------------------------------------------------
    @property
    def is_terminal(self) -> bool:
        if self._terminal_cache is None:
            player_alive = getattr(self.engine.player, "is_alive", False)
            hostile_count, _ = self._hostile_stats()
            self._terminal_cache = not player_alive or hostile_count == 0
        return self._terminal_cache

    def evaluate(self) -> float:
        """Heuristic valuation of the current state.

        The score rewards the player's health, exploration progress, and the
        elimination of hostiles.  It is intentionally heuristic – the goal is
        to give the planner a smooth landscape to optimise over, not to be a
        perfect value function.
        """

        player = self.engine.player
        if not player.is_alive:
            return -1_000.0
        player_hp = float(player.fighter.hp if player.fighter else 0)
        hostile_count, hostile_hp = self._hostile_stats()
        explored_ratio = float(np.count_nonzero(self.engine.game_map.explored))
        explored_ratio /= max(1, self.engine.game_map.explored.size)
        # Weight terms: HP is critical, hostiles penalise, exploration encourages
        # moving through the dungeon instead of waiting in place.
        return (player_hp * 2.0) - (hostile_hp * 0.75) - (hostile_count * 1.5) + (
            explored_ratio * 10.0
        )

    def summary(self) -> dict:
        """Return lightweight telemetry for logging or dashboards."""

        hostile_count, hostile_hp = self._hostile_stats()
        player = self.engine.player
        return {
            "turn": self.turn,
            "player_hp": player.fighter.hp if player.fighter else 0,
            "hostiles": hostile_count,
            "hostile_hp": hostile_hp,
        }

    # ------------------------------------------------------------------
    # Rendering helpers
    # ------------------------------------------------------------------
    def to_ascii(self, *, reveal: bool = False) -> str:
        """Render the dungeon as ASCII for logging/debugging."""

        return render_ascii(self.engine, reveal=reveal)

    # ------------------------------------------------------------------
    # Internal utilities
    # ------------------------------------------------------------------
    def _apply_action(self, action: Action) -> None:
        self.engine.handle_player_movement(action.dx, action.dy)

    def _hostile_stats(self) -> Tuple[int, int]:
        count = 0
        total_hp = 0
        for entity in self.engine.game_map.entities:
            if isinstance(entity, Actor) and entity is not self.engine.player and entity.is_alive:
                count += 1
                total_hp += int(entity.fighter.hp if entity.fighter else 0)
        return count, total_hp


def create_environment(
    config: Optional[GameConfig] = None, *, seed: Optional[int] = None
) -> RoguelikeEnvironment:
    """Factory helper mirroring :meth:`RoguelikeEnvironment.from_config`."""

    config = config or GameConfig()
    return RoguelikeEnvironment.from_config(config, seed=seed)


def create_sample_environment(*, seed: Optional[int] = None) -> RoguelikeEnvironment:
    """Return a deterministic small dungeon used by the CLI and tests."""

    return RoguelikeEnvironment.sample(seed=seed)


def render_ascii(engine, *, reveal: bool = False) -> str:
    """Return a text-mode representation of ``engine``'s dungeon state."""

    game_map = engine.game_map
    width, height = game_map.tiles.shape
    visible = game_map.visible
    explored = game_map.explored
    chars: List[List[str]] = []
    for y in range(height):
        row: List[str] = []
        for x in range(width):
            if reveal or visible[x, y] or explored[x, y]:
                glyph = game_map.tiles[x, y]["light"]["ch"]
                row.append(chr(int(glyph)))
            else:
                row.append(" ")
        chars.append(row)

    # Overlay entities
    for entity in game_map.entities:
        if reveal or visible[entity.x, entity.y]:
            chars[entity.y][entity.x] = entity.char
    # Ensure the player is visible even if dead (glyph already mutated by engine)
    player = engine.player
    chars[player.y][player.x] = player.char

    return "\n".join("".join(row) for row in chars)


__all__ = [
    "Action",
    "ALL_ACTIONS",
    "RoguelikeEnvironment",
    "create_environment",
    "create_sample_environment",
    "render_ascii",
]
