"""Map and dungeon-generation helpers."""

from __future__ import annotations

import random
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Tuple, TYPE_CHECKING

import numpy as np
import tcod

from .config import GameConfig
from .tiles import build_tiles, tile_dt

if TYPE_CHECKING:  # pragma: no cover - hints only
    from .entity import Actor, Entity


@dataclass
class Rect:
    x1: int
    y1: int
    width: int
    height: int

    @property
    def x2(self) -> int:
        return self.x1 + self.width

    @property
    def y2(self) -> int:
        return self.y1 + self.height

    @property
    def centre(self) -> Tuple[int, int]:
        return (self.x1 + self.width // 2, self.y1 + self.height // 2)

    def intersects(self, other: "Rect") -> bool:
        return not (
            self.x2 <= other.x1
            or self.x1 >= other.x2
            or self.y2 <= other.y1
            or self.y1 >= other.y2
        )


class GameMap:
    def __init__(self, config: GameConfig) -> None:
        self.config = config
        floor, wall = build_tiles(config)
        self.floor = floor
        self.wall = wall
        self.tiles = np.full((config.map_width, config.map_height), wall, dtype=tile_dt)
        self.visible = np.zeros((config.map_width, config.map_height), dtype=bool)
        self.explored = np.zeros((config.map_width, config.map_height), dtype=bool)
        self.entities: List["Entity"] = []

    @property
    def actors(self) -> Iterable["Actor"]:
        return (entity for entity in self.entities if getattr(entity, "fighter", None))

    def add_entity(self, entity: "Entity") -> None:
        entity.parent = self
        if entity not in self.entities:
            self.entities.append(entity)

    def remove_entity(self, entity: "Entity") -> None:
        if entity in self.entities:
            self.entities.remove(entity)
        entity.parent = None

    def remove_actor_ai(self, actor: "Actor") -> None:
        actor.ai = None

    def in_bounds(self, x: int, y: int) -> bool:
        return 0 <= x < self.tiles.shape[0] and 0 <= y < self.tiles.shape[1]

    def is_blocked(self, x: int, y: int) -> bool:
        if not self.in_bounds(x, y):
            return True
        if not self.tiles[x, y]["walkable"]:
            return True
        return bool(self.get_blocking_entity_at_location(x, y))

    def get_blocking_entity_at_location(
        self, dest_x: int, dest_y: int
    ) -> Optional["Entity"]:
        for entity in self.entities:
            if entity.blocks_movement and entity.x == dest_x and entity.y == dest_y:
                return entity
        return None

    def compute_fov(self, x: int, y: int, radius: int) -> None:
        self.visible[:] = tcod.map.compute_fov(
            transparency=self.tiles["transparent"],
            pov=(x, y),
            radius=radius,
            algorithm=tcod.FOV_RESTRICTIVE,
        )
        self.explored |= self.visible

    def carve_room(self, room: Rect) -> None:
        self.tiles[room.x1 : room.x2, room.y1 : room.y2] = self.floor

    def carve_h_tunnel(self, x1: int, x2: int, y: int) -> None:
        for x in range(min(x1, x2), max(x1, x2) + 1):
            self.tiles[x, y] = self.floor

    def carve_v_tunnel(self, y1: int, y2: int, x: int) -> None:
        for y in range(min(y1, y2), max(y1, y2) + 1):
            self.tiles[x, y] = self.floor

    def generate_dungeon(
        self,
        player: "Actor",
        *,
        spawn_monsters,
        spawn_items,
    ) -> None:
        rooms: List[Rect] = []
        for _ in range(self.config.max_rooms):
            w = random.randint(self.config.room_min_size, self.config.room_max_size)
            h = random.randint(self.config.room_min_size, self.config.room_max_size)
            x = random.randint(0, self.config.map_width - w - 1)
            y = random.randint(0, self.config.map_height - h - 1)

            new_room = Rect(x, y, w, h)

            if any(new_room.intersects(other_room) for other_room in rooms):
                continue

            self.carve_room(new_room)

            if rooms:
                prev_centre_x, prev_centre_y = rooms[-1].centre
                new_centre_x, new_centre_y = new_room.centre
                if random.random() < 0.5:
                    self.carve_h_tunnel(prev_centre_x, new_centre_x, prev_centre_y)
                    self.carve_v_tunnel(prev_centre_y, new_centre_y, new_centre_x)
                else:
                    self.carve_v_tunnel(prev_centre_y, new_centre_y, prev_centre_x)
                    self.carve_h_tunnel(prev_centre_x, new_centre_x, new_centre_y)
            else:
                player.place(*new_room.centre, self)

            spawn_monsters(self, new_room)
            spawn_items(self, new_room)

            rooms.append(new_room)

    def to_dict(self) -> Dict[str, object]:
        return {
            "tiles": self.tiles.tolist(),
            "visible": self.visible.tolist(),
            "explored": self.explored.tolist(),
            "entities": [self.serialise_entity(e) for e in self.entities],
        }

    def serialise_entity(self, entity: "Entity") -> Dict[str, object]:
        data = {
            "class": entity.__class__.__name__,
            "x": entity.x,
            "y": entity.y,
            "char": entity.char,
            "colour": entity.colour,
            "name": entity.name,
            "blocks": entity.blocks_movement,
            "description": entity.description,
        }
        if getattr(entity, "fighter", None):
            fighter = entity.fighter
            data["fighter"] = {
                "hp": fighter.hp,
                "max_hp": fighter.max_hp,
                "defense": fighter.defense,
                "power": fighter.power,
            }
        if getattr(entity, "inventory", None):
            data["inventory"] = {
                "capacity": entity.inventory.capacity,
                "items": [
                    self.serialise_entity(item) for item in entity.inventory.items
                ],
            }
        if getattr(entity, "consumable", None):
            consumable = entity.consumable
            serialisable = {
                key: value
                for key, value in consumable.__dict__.items()
                if key != "parent"
            }
            serialisable["type"] = consumable.__class__.__name__
            data["consumable"] = serialisable
        if getattr(entity, "ai", None):
            data["ai"] = entity.ai.__class__.__name__
        return data
