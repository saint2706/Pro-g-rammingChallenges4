"""Entity, stats, and inventory systems."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, List


@dataclass
class Stats:
    max_hp: int
    attack: int
    defense: int
    speed: int

    hp: int = field(init=False)

    def __post_init__(self) -> None:
        self.hp = self.max_hp

    def is_alive(self) -> bool:
        return self.hp > 0

    def take_damage(self, amount: int) -> int:
        damage = max(1, amount)
        self.hp = max(0, self.hp - damage)
        return damage

    def heal(self, amount: int) -> None:
        self.hp = min(self.max_hp, self.hp + amount)


@dataclass
class Item:
    name: str
    description: str
    heal_amount: int = 0
    attack_boost: int = 0
    defense_boost: int = 0


@dataclass
class Inventory:
    items: Dict[str, int] = field(default_factory=dict)

    def add(self, item_id: str, quantity: int = 1) -> None:
        self.items[item_id] = self.items.get(item_id, 0) + quantity

    def remove(self, item_id: str, quantity: int = 1) -> bool:
        if self.items.get(item_id, 0) < quantity:
            return False
        self.items[item_id] -= quantity
        if self.items[item_id] <= 0:
            del self.items[item_id]
        return True

    def list_items(self) -> List[str]:
        return [item_id for item_id, count in self.items.items() for _ in range(count)]


@dataclass
class Entity:
    entity_id: str
    name: str
    stats: Stats
    inventory: Inventory = field(default_factory=Inventory)
    experience: int = 0
    level: int = 1

    def to_dict(self) -> Dict[str, int | str | dict]:
        return {
            "entity_id": self.entity_id,
            "name": self.name,
            "stats": {
                "max_hp": self.stats.max_hp,
                "attack": self.stats.attack,
                "defense": self.stats.defense,
                "speed": self.stats.speed,
                "hp": self.stats.hp,
            },
            "inventory": self.inventory.items,
            "experience": self.experience,
            "level": self.level,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, object]) -> "Entity":
        stats_data = data["stats"]
        stats = Stats(
            max_hp=int(stats_data["max_hp"]),
            attack=int(stats_data["attack"]),
            defense=int(stats_data["defense"]),
            speed=int(stats_data["speed"]),
        )
        stats.hp = int(stats_data.get("hp", stats.max_hp))
        inventory = Inventory({k: int(v) for k, v in data.get("inventory", {}).items()})
        entity = cls(
            entity_id=str(data["entity_id"]),
            name=str(data["name"]),
            stats=stats,
            inventory=inventory,
            experience=int(data.get("experience", 0)),
            level=int(data.get("level", 1)),
        )
        return entity
