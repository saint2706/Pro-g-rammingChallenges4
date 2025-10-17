"""Headless smoke tests for the Wolfenstein raycasting engine."""

from __future__ import annotations

# ruff: noqa: E402

import json
import os
from pathlib import Path

import pytest

pygame = pytest.importorskip("pygame")

from challenges.Games.WolfensteinClone.game import GameConfig, MAP_DIR, WolfensteinGame

ASSET_DIR = Path("challenges/Games/WolfensteinClone/assets")
REQUIRED_TEXTURE = ASSET_DIR / "wall_stone.png"

if not REQUIRED_TEXTURE.exists():
    pytest.skip(
        "Wolfenstein textures are missing (requires Git LFS assets).",
        allow_module_level=True,
    )


def _ensure_dummy_video() -> None:
    # SDL needs the dummy driver in CI to avoid opening a window.
    os.environ.setdefault("SDL_VIDEODRIVER", "dummy")


def test_game_runs_for_a_few_frames() -> None:
    _ensure_dummy_video()
    config = GameConfig(map_path=MAP_DIR / "default_map.json")
    game = WolfensteinGame(config, headless=True)
    game.run(max_frames=5)

    # After the run the display should be uninitialised to keep pytest happy.
    assert not pygame.display.get_init()


def test_custom_map_loading(tmp_path: Path) -> None:
    _ensure_dummy_video()
    custom_map = {
        "name": "Test Arena",
        "player_start": {"x": 1.5, "y": 1.5, "angle": 0.0},
        "enemies": [{"x": 2.5, "y": 2.5}],
        "layout": [
            [1, 1, 1, 1],
            [1, 0, 0, 1],
            [1, 0, 0, 1],
            [1, 1, 1, 1],
        ],
    }
    map_path = tmp_path / "arena.json"
    map_path.write_text(json.dumps(custom_map), encoding="utf-8")

    config = GameConfig(map_path=map_path)
    game = WolfensteinGame(config, headless=True)
    assert game.map.name == "Test Arena"
    assert len(game.sprites) == 1
    game.run(max_frames=1)
    assert not pygame.display.get_init()
