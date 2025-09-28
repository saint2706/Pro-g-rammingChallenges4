"""Save/load helpers for the roguelike."""

from __future__ import annotations

import json
from dataclasses import asdict
from pathlib import Path
from typing import Dict, TYPE_CHECKING

import numpy as np

from .config import GameConfig
from .entity import Actor, HealingConsumable, Item, LightningConsumable
from .game_map import GameMap
from .systems.ai import HostileEnemy
from .systems.combat import Fighter
from .systems.inventory import Inventory
from .tiles import tile_dt

if TYPE_CHECKING:  # pragma: no cover - hints only
    from .engine import Engine


CONSUMABLE_TYPES = {
    "HealingConsumable": HealingConsumable,
    "LightningConsumable": LightningConsumable,
}


def save_game(engine: "Engine", path: str) -> None:
    data = {
        "config": asdict(engine.config),
        "map": {
            "tiles": engine.game_map.tiles.tolist(),
            "visible": engine.game_map.visible.tolist(),
            "explored": engine.game_map.explored.tolist(),
        },
        "player": engine.game_map.serialise_entity(engine.player),
        "entities": [
            engine.game_map.serialise_entity(e)
            for e in engine.game_map.entities
            if e is not engine.player
        ],
        "messages": [message.__dict__ for message in engine.message_log.render()],
    }
    Path(path).write_text(json.dumps(data))


def load_game(path: str) -> "Engine":
    from .engine import create_engine

    payload = json.loads(Path(path).read_text())
    config = GameConfig(**payload["config"])
    game_map = GameMap(config)
    map_payload = payload["map"]
    game_map.tiles = np.array(map_payload["tiles"], dtype=tile_dt)
    game_map.visible = np.array(map_payload["visible"], dtype=bool)
    game_map.explored = np.array(map_payload["explored"], dtype=bool)

    player = build_entity(payload["player"])
    assert isinstance(player, Actor)

    for entity_payload in payload["entities"]:
        entity = build_entity(entity_payload)
        game_map.add_entity(entity)

    engine = create_engine(config, player=player, game_map=game_map)

    for message_payload in payload.get("messages", []):
        engine.message_log.add(
            message_payload["text"], tuple(message_payload["colour"])
        )

    return engine


def build_entity(data: Dict[str, object]):
    entity_class = data["class"]
    base_kwargs = {
        "x": data["x"],
        "y": data["y"],
        "char": data["char"],
        "colour": tuple(data["colour"]),
        "name": data["name"],
        "blocks_movement": data["blocks"],
        "description": data.get("description", ""),
    }
    if entity_class == "Actor":
        fighter_payload = data.get("fighter") or {}
        fighter = Fighter(
            hp=fighter_payload.get("hp", 1),
            defense=fighter_payload.get("defense", 0),
            power=fighter_payload.get("power", 1),
        )
        fighter.max_hp = fighter_payload.get("max_hp", fighter.hp)
        inventory_payload = data.get("inventory") or {"capacity": 0, "items": []}
        inventory = Inventory(capacity=inventory_payload.get("capacity", 0))
        actor = Actor(**base_kwargs, fighter=fighter, inventory=inventory, ai=None)
        ai_name = data.get("ai")
        if ai_name == "HostileEnemy":
            actor.ai = HostileEnemy()
            actor.ai.parent = actor
        fighter.parent = actor
        inventory.parent = actor
        for item_payload in inventory_payload.get("items", []):
            item = build_entity(item_payload)
            if isinstance(item, Item):
                inventory.items.append(item)
                item.parent = None
        return actor
    if entity_class == "Item":
        consumable_payload = data.get("consumable")
        consumable = None
        if consumable_payload:
            consumable_type = consumable_payload.get("type")
            cls = CONSUMABLE_TYPES.get(consumable_type)
            if cls:
                kwargs = {
                    k: v for k, v in consumable_payload.items() if k not in {"type"}
                }
                consumable = cls(**kwargs)
        item = Item(**base_kwargs, consumable=consumable)
        if consumable:
            consumable.parent = item
        return item
    raise ValueError(f"Unknown entity class {entity_class}")
