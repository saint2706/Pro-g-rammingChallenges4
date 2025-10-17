"""Entry point for the Roguelike challenge."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Optional

import tcod
import tcod.console
import tcod.context
import tcod.event

from .config import GameConfig
from .content import item_spawner, monster_spawner
from .engine import create_engine
from .entity import Actor
from .game_map import GameMap
from .storage import load_game
from .systems.combat import Fighter
from .systems.inventory import Inventory


def new_game(config: GameConfig) -> "Engine":
    player = Actor(
        x=0,
        y=0,
        char="@",
        colour=(255, 255, 255),
        name="Player",
        fighter=Fighter(hp=30, defense=2, power=5),
        inventory=Inventory(capacity=20),
        ai=None,
    )
    game_map = GameMap(config)
    game_map.generate_dungeon(
        player,
        spawn_monsters=monster_spawner(config),
        spawn_items=item_spawner(config),
    )
    engine = create_engine(config, player=player, game_map=game_map)
    engine.message_log.add("You arrive at the mysterious ruins…")
    return engine


def load_tileset() -> Optional[tcod.tileset.Tileset]:
    candidates = [
        Path("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"),
        Path("/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"),
    ]
    for path in candidates:
        if path.exists():
            return tcod.tileset.load_truetype_font(str(path), 16, 16)
    return None


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Roguelike dungeon crawler built with tcod."
    )
    parser.add_argument(
        "--load", dest="load", default=None, help="Load a saved game file."
    )
    parser.add_argument(
        "--tileset", dest="tileset", default=None, help="Path to a custom .ttf tileset."
    )
    args = parser.parse_args()

    if args.tileset:
        tileset = tcod.tileset.load_truetype_font(args.tileset, 16, 16)
    else:
        tileset = load_tileset()

    if args.load:
        save_path = Path(args.load)
        if not save_path.exists():
            raise SystemExit(f"Save file {save_path} not found.")
        engine = load_game(str(save_path))
        engine.config.save_path = str(save_path)
    else:
        config = GameConfig()
        engine = new_game(config)

    config = engine.config

    tileset_kwargs = {"tileset": tileset} if tileset else {}

    with tcod.context.new(
        columns=config.screen_width,
        rows=config.screen_height,
        title="Roguelike Challenge",
        **tileset_kwargs,
    ) as context:
        console = tcod.console.Console(
            config.screen_width, config.screen_height, order="F"
        )

        while True:
            engine.render(console, context)
            try:
                for event in tcod.event.wait():
                    context.convert_event(event)
                    engine.event_handler.dispatch(event)
            except SystemExit:
                engine.save()
                raise


if __name__ == "__main__":
    main()
