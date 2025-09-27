"""World map, player, and NPC handling."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List, Tuple

import pygame

from .constants import TILE_SIZE

Color = Tuple[int, int, int]


@dataclass
class TileType:
    tile_id: str
    color: Color
    walkable: bool
    encounter_rate: float = 0.0
    encounters: List[str] = field(default_factory=list)


class TileMap:
    def __init__(self, grid: List[List[str]], tile_types: Dict[str, TileType]):
        self.grid = grid
        self.tile_types = tile_types
        self.width = len(grid[0])
        self.height = len(grid)

    def draw(self, surface: pygame.Surface) -> None:
        for y, row in enumerate(self.grid):
            for x, tile_id in enumerate(row):
                tile = self.tile_types[tile_id]
                rect = pygame.Rect(x * TILE_SIZE, y * TILE_SIZE, TILE_SIZE, TILE_SIZE)
                surface.fill(tile.color, rect)

    def is_walkable(self, x: int, y: int) -> bool:
        if x < 0 or y < 0 or y >= self.height or x >= self.width:
            return False
        tile_id = self.grid[y][x]
        return self.tile_types[tile_id].walkable

    def get_tile(self, x: int, y: int) -> TileType | None:
        if x < 0 or y < 0 or y >= self.height or x >= self.width:
            return None
        return self.tile_types[self.grid[y][x]]


@dataclass
class NPC:
    npc_id: str
    name: str
    position: Tuple[int, int]
    dialogue_id: str
    quest_id: str | None = None

    def rect(self) -> pygame.Rect:
        x, y = self.position
        return pygame.Rect(x * TILE_SIZE, y * TILE_SIZE, TILE_SIZE, TILE_SIZE)


class Player:
    def __init__(self, position: Tuple[int, int]):
        self.x, self.y = position
        self.color: Color = (255, 255, 255)

    def move(self, dx: int, dy: int, tile_map: TileMap) -> bool:
        new_x = self.x + dx
        new_y = self.y + dy
        if tile_map.is_walkable(new_x, new_y):
            self.x = new_x
            self.y = new_y
            return True
        return False

    @property
    def rect(self) -> pygame.Rect:
        return pygame.Rect(self.x * TILE_SIZE, self.y * TILE_SIZE, TILE_SIZE, TILE_SIZE)

    def draw(self, surface: pygame.Surface) -> None:
        surface.fill(self.color, self.rect)
