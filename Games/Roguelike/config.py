"""Game configuration and tileset definitions.

This module exposes configuration objects for the roguelike.  The ``GameConfig``
dataclass centralises tunable parameters like map size, field-of-view radius,
and which tile palette to use.  ``TILESETS`` describes the glyph/colour pairing
for tiles so modders can swap out palettes without rewriting rendering logic.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Tuple

Colour = Tuple[int, int, int]


@dataclass(slots=True)
class GameConfig:
    """Mutable configuration for a single game session."""

    screen_width: int = 80
    screen_height: int = 50
    map_width: int = 80
    map_height: int = 43
    room_min_size: int = 6
    room_max_size: int = 15
    max_rooms: int = 32
    max_monsters_per_room: int = 3
    max_items_per_room: int = 2
    fov_radius: int = 8
    torch_light_radius: int = 8
    tile_set: str = "classic"
    save_path: str = "savegame.json"


TILESETS: Dict[str, Dict[str, Tuple[int, Colour, Colour]]] = {
    "classic": {
        "floor": (ord("."), (180, 180, 180), (20, 20, 20)),
        "wall": (ord("#"), (255, 255, 255), (0, 0, 0)),
        "player": (ord("@"), (255, 255, 255), (0, 0, 0)),
        "monster": (ord("M"), (255, 50, 50), (0, 0, 0)),
        "item": (ord("!"), (50, 200, 255), (0, 0, 0)),
    },
    "high_contrast": {
        "floor": (ord("·"), (220, 220, 220), (0, 0, 0)),
        "wall": (ord("█"), (255, 255, 0), (0, 0, 0)),
        "player": (ord("@"), (0, 255, 127), (0, 0, 0)),
        "monster": (ord("G"), (255, 69, 0), (0, 0, 0)),
        "item": (ord("∞"), (30, 144, 255), (0, 0, 0)),
    },
}


def get_tileset_char(name: str, key: str) -> Tuple[int, Colour, Colour]:
    """Return the glyph and colours for ``key`` from ``name``'s tile set."""

    try:
        tileset = TILESETS[name]
    except KeyError as exc:  # pragma: no cover - defensive guard for config edits
        raise KeyError(f"Unknown tileset '{name}'. Available: {', '.join(TILESETS)}") from exc

    try:
        return tileset[key]
    except KeyError as exc:  # pragma: no cover - defensive guard for config edits
        raise KeyError(f"Tileset '{name}' has no definition for '{key}'.") from exc
