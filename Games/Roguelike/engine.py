"""Core game engine for the roguelike."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Optional

import tcod

from .config import GameConfig, get_tileset_char
from .entity import Actor, Item
from .game_map import GameMap
from .input_handlers import EventHandler
from .message_log import MessageLog
from .render import render_all


@dataclass
class Engine:
    config: GameConfig
    player: Actor
    game_map: GameMap
    message_log: MessageLog

    def __post_init__(self) -> None:
        self.event_handler = EventHandler(self)
        self.game_map.add_entity(self.player)
        self.update_player_glyph()
        self.update_fov()

    def update_player_glyph(self) -> None:
        char_code, colour, _ = get_tileset_char(self.config.tile_set, "player")
        self.player.char = chr(char_code)
        self.player.colour = colour

    def update_fov(self) -> None:
        self.game_map.compute_fov(self.player.x, self.player.y, self.config.fov_radius)

    def handle_player_movement(self, dx: int, dy: int) -> None:
        dest_x = self.player.x + dx
        dest_y = self.player.y + dy
        if dx == 0 and dy == 0:
            self.message_log.add("You wait for a moment.")
            self.handle_enemy_turns()
            return
        if self.game_map.is_blocked(dest_x, dest_y):
            target = self.game_map.get_blocking_entity_at_location(dest_x, dest_y)
            if isinstance(target, Actor) and target.is_alive:
                message = self.player.fighter.attack(target, self)
                self.message_log.add(message)
        else:
            self.player.move(dx, dy)
        self.end_player_turn()

    def end_player_turn(self) -> None:
        self.update_fov()
        self.handle_enemy_turns()

    def handle_enemy_turns(self) -> None:
        for entity in list(self.game_map.entities):
            if isinstance(entity, Actor) and entity is not self.player and entity.ai:
                entity.ai.perform(self)
        self.update_fov()

    def closest_hostile(self, source: Actor, *, maximum_range: int) -> Optional[Actor]:
        closest_distance = maximum_range + 1
        closest_enemy: Optional[Actor] = None
        for entity in self.game_map.entities:
            if isinstance(entity, Actor) and entity is not source and entity.is_alive:
                distance = max(abs(entity.x - source.x), abs(entity.y - source.y))
                if distance <= maximum_range and distance < closest_distance:
                    closest_enemy = entity
                    closest_distance = distance
        return closest_enemy

    def handle_pickup(self) -> None:
        items = [
            entity
            for entity in self.game_map.entities
            if isinstance(entity, Item) and entity.x == self.player.x and entity.y == self.player.y
        ]
        if not items:
            self.message_log.add("There is nothing here to pick up.")
            return
        inventory = self.player.inventory
        took_turn = False
        for item in items:
            success, message = inventory.add(item)
            if success:
                self.game_map.remove_entity(item)
                took_turn = True
            self.message_log.add(message)
        if took_turn:
            self.end_player_turn()

    def handle_drop(self) -> None:
        inventory = self.player.inventory
        if not inventory.items:
            self.message_log.add("You have nothing to drop.")
            return
        item = inventory.items[-1]
        message = inventory.drop(item, self)
        self.message_log.add(message)
        self.end_player_turn()

    def handle_use(self) -> None:
        inventory = self.player.inventory
        if not inventory.items:
            self.message_log.add("You have nothing to use.")
            return
        item = inventory.items[0]
        message = inventory.use(item, self)
        self.message_log.add(message)
        if item not in inventory.items:
            self.game_map.remove_entity(item)
        self.end_player_turn()

    def describe_inventory(self) -> str:
        inventory = self.player.inventory
        if not inventory.items:
            return "Inventory is empty."
        return "Inventory: " + ", ".join(item.name for item in inventory.items)

    def render(self, console: tcod.console.Console, context: tcod.context.Context) -> None:
        render_all(console, self)
        context.present(console)

    def save(self, path: Optional[str] = None) -> None:
        from . import storage

        storage.save_game(self, path or self.config.save_path)

    def load_entities(self, entities: Iterable[Item | Actor]) -> None:
        for entity in entities:
            self.game_map.add_entity(entity)


def create_engine(config: GameConfig, *, player: Actor, game_map: GameMap) -> Engine:
    message_log = MessageLog()
    return Engine(config=config, player=player, game_map=game_map, message_log=message_log)
