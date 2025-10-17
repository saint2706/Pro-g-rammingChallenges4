"""Simple monster AI routines."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional, TYPE_CHECKING

import tcod

if TYPE_CHECKING:  # pragma: no cover - hints only
    from ..engine import Engine
    from ..entity import Actor


@dataclass
class BaseAI:
    parent: Optional["Actor"] = field(default=None, repr=False)

    def perform(self, engine: "Engine") -> None:  # pragma: no cover - interface
        raise NotImplementedError


@dataclass
class HostileEnemy(BaseAI):
    """A simple chase-and-attack behaviour."""

    def perform(self, engine: "Engine") -> None:
        actor = self.parent
        if not actor or not actor.fighter or not actor.is_alive:
            return

        player = engine.player
        dx = player.x - actor.x
        dy = player.y - actor.y
        distance = max(abs(dx), abs(dy))

        if engine.game_map.visible[actor.x, actor.y]:
            if distance <= 1:
                message = actor.fighter.attack(player, engine)
                engine.message_log.add(message)
                return
            self.move_towards(player.x, player.y, engine)

    def move_towards(self, target_x: int, target_y: int, engine: "Engine") -> None:
        actor = self.parent
        if not actor:
            return

        graph = tcod.path.SimpleGraph(
            cost=engine.game_map.tiles["walkable"].astype(int) + 1,
            cardinal=2,
            diagonal=3,
        )
        pathfinder = tcod.path.Pathfinder(graph)
        pathfinder.add_root((actor.x, actor.y))
        path = pathfinder.path_to((target_x, target_y))[1:].tolist()

        if path:
            dest_x, dest_y = path[0]
            if not engine.game_map.get_blocking_entity_at_location(dest_x, dest_y):
                actor.place(dest_x, dest_y)
