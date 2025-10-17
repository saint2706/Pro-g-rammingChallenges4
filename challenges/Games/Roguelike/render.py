"""Rendering helpers for the roguelike."""

from __future__ import annotations

from typing import Iterable

import tcod.console

from .entity import Entity, RenderOrder
from .message_log import MessageLog


def render_all(console: tcod.console.Console, engine: "Engine") -> None:
    game_map = engine.game_map
    config = engine.config

    console.clear()

    for x in range(game_map.tiles.shape[0]):
        for y in range(game_map.tiles.shape[1]):
            visible = game_map.visible[x, y]
            explored = game_map.explored[x, y]
            if visible:
                tile = game_map.tiles[x, y]["light"]
            elif explored:
                tile = game_map.tiles[x, y]["dark"]
            else:
                continue
            console.tiles_rgb[x, y] = tile

    entities_in_render_order = sorted(
        game_map.entities, key=lambda e: e.render_order.value
    )
    for entity in entities_in_render_order:
        draw_entity(console, entity, game_map.visible)

    render_ui(console, engine)


def draw_entity(console: tcod.console.Console, entity: Entity, fov_map) -> None:
    if fov_map[entity.x, entity.y]:
        console.print(entity.x, entity.y, entity.char, fg=entity.colour)


def render_ui(console: tcod.console.Console, engine: "Engine") -> None:
    config = engine.config
    player = engine.player
    log = engine.message_log

    y_offset = config.map_height + 1
    hp_text = f"HP: {player.fighter.hp}/{player.fighter.max_hp}"
    console.print(1, y_offset, hp_text, fg=(255, 255, 255))

    draw_message_log(
        console, log, start_y=y_offset + 2, height=config.screen_height - (y_offset + 2)
    )

    draw_inventory(console, player.inventory, x=config.map_width - 25, y=y_offset)


def draw_message_log(
    console: tcod.console.Console, log: MessageLog, *, start_y: int, height: int
) -> None:
    messages = log.render()[-height:]
    for i, message in enumerate(messages):
        console.print(1, start_y + i, message.text, fg=message.colour)


def draw_inventory(console: tcod.console.Console, inventory, *, x: int, y: int) -> None:
    console.print(x, y, "Inventory:", fg=(200, 200, 200))
    for i, item in enumerate(inventory.items, start=1):
        console.print(x, y + i, f"{i}. {item.name}", fg=(180, 180, 180))
