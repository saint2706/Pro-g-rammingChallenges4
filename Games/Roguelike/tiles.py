"""Tile helper routines used by the roguelike."""
from __future__ import annotations

from typing import Tuple

import numpy as np

from .config import GameConfig, get_tileset_char


graphic_dt = np.dtype([("ch", np.int32), ("fg", "3B"), ("bg", "3B")])
tile_dt = np.dtype(
    [
        ("walkable", np.bool_),
        ("transparent", np.bool_),
        ("dark", graphic_dt),
        ("light", graphic_dt),
    ]
)


def new_tile(
    *,
    walkable: bool,
    transparent: bool,
    dark: Tuple[int, Tuple[int, int, int], Tuple[int, int, int]],
    light: Tuple[int, Tuple[int, int, int], Tuple[int, int, int]],
) -> np.ndarray:
    """Helper to initialise individual tile definitions."""

    return np.array((walkable, transparent, dark, light), dtype=tile_dt)


def build_tiles(config: GameConfig) -> Tuple[np.ndarray, np.ndarray]:
    """Return `(floor, wall)` tiles derived from the configured tileset."""

    floor_char, floor_fg, floor_bg = get_tileset_char(config.tile_set, "floor")
    wall_char, wall_fg, wall_bg = get_tileset_char(config.tile_set, "wall")

    floor = new_tile(
        walkable=True,
        transparent=True,
        dark=(floor_char, tuple(int(c * 0.4) for c in floor_fg), floor_bg),
        light=(floor_char, floor_fg, floor_bg),
    )
    wall = new_tile(
        walkable=False,
        transparent=False,
        dark=(wall_char, tuple(int(c * 0.3) for c in wall_fg), wall_bg),
        light=(wall_char, wall_fg, wall_bg),
    )
    return floor, wall
