"""Content definitions for the roguelike."""

from __future__ import annotations

import random
from typing import Callable, Dict

from .config import GameConfig, get_tileset_char
from .entity import Actor, HealingConsumable, Item, LightningConsumable
from .systems.ai import HostileEnemy
from .systems.combat import Fighter
from .systems.inventory import Inventory


MonsterFactory = Callable[[int, int], Actor]
ItemFactory = Callable[[int, int], Item]


def build_monsters(config: GameConfig) -> Dict[str, MonsterFactory]:
    monster_char, _, _ = get_tileset_char(config.tile_set, "monster")
    return {
        "orc": lambda x, y: Actor(
            x=x,
            y=y,
            char=chr(monster_char),
            colour=(63, 127, 63),
            name="orc",
            fighter=Fighter(hp=10, defense=0, power=3),
            inventory=Inventory(capacity=0),
            ai=HostileEnemy(),
        ),
        "troll": lambda x, y: Actor(
            x=x,
            y=y,
            char=chr(monster_char),
            colour=(0, 127, 0),
            name="troll",
            fighter=Fighter(hp=16, defense=1, power=4),
            inventory=Inventory(capacity=0),
            ai=HostileEnemy(),
        ),
    }


def build_items(config: GameConfig) -> Dict[str, ItemFactory]:
    item_char, item_colour, _ = get_tileset_char(config.tile_set, "item")
    return {
        "healing_potion": lambda x, y: Item(
            x=x,
            y=y,
            char=chr(item_char),
            colour=item_colour,
            name="Healing Potion",
            description="Restores 4 HP",
            consumable=HealingConsumable(amount=4),
        ),
        "lightning_scroll": lambda x, y: Item(
            x=x,
            y=y,
            char=chr(item_char),
            colour=(255, 255, 0),
            name="Lightning Scroll",
            description="Deals 20 damage to the nearest enemy",
            consumable=LightningConsumable(damage=20, maximum_range=6),
        ),
    }


def monster_spawner(config: GameConfig):
    monsters = build_monsters(config)

    def spawn(game_map, room) -> None:
        number_of_monsters = random.randint(0, config.max_monsters_per_room)
        for _ in range(number_of_monsters):
            x = random.randint(room.x1 + 1, room.x2 - 2)
            y = random.randint(room.y1 + 1, room.y2 - 2)
            if not game_map.get_blocking_entity_at_location(x, y):
                monster = random.choices(
                    population=[monsters["orc"], monsters["troll"]],
                    weights=[0.8, 0.2],
                    k=1,
                )[0](x, y)
                game_map.add_entity(monster)

    return spawn


def item_spawner(config: GameConfig):
    items = build_items(config)

    def spawn(game_map, room) -> None:
        number_of_items = random.randint(0, config.max_items_per_room)
        for _ in range(number_of_items):
            x = random.randint(room.x1 + 1, room.x2 - 2)
            y = random.randint(room.y1 + 1, room.y2 - 2)
            if not game_map.get_blocking_entity_at_location(x, y):
                item_factory = random.choices(
                    population=[items["healing_potion"], items["lightning_scroll"]],
                    weights=[0.7, 0.3],
                    k=1,
                )[0]
                item = item_factory(x, y)
                game_map.add_entity(item)

    return spawn
