"""Entity and component definitions for the roguelike."""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Optional, Tuple, TYPE_CHECKING

if TYPE_CHECKING:  # pragma: no cover - type checking only
    from .engine import Engine
    from .game_map import GameMap
    from .systems.ai import BaseAI
    from .systems.combat import Fighter
    from .systems.inventory import Inventory


class RenderOrder(Enum):
    CORPSE = auto()
    ITEM = auto()
    ACTOR = auto()


@dataclass
class Entity:
    """A generic object that can appear on the map."""

    x: int
    y: int
    char: str
    colour: Tuple[int, int, int]
    name: str
    blocks_movement: bool = False
    render_order: RenderOrder = RenderOrder.CORPSE
    description: str = ""
    parent: Optional[GameMap] = field(default=None, repr=False)

    def place(self, x: int, y: int, game_map: Optional["GameMap"] = None) -> None:
        if game_map:
            if self.parent is not None:
                self.parent.entities.remove(self)
            self.parent = game_map
            game_map.add_entity(self)
        self.x = x
        self.y = y

    def move(self, dx: int, dy: int) -> None:
        self.x += dx
        self.y += dy

    def move_towards(self, target_x: int, target_y: int, game_map: "GameMap") -> None:
        dx = target_x - self.x
        dy = target_y - self.y
        distance = max(1, (dx**2 + dy**2) ** 0.5)
        dx = int(round(dx / distance))
        dy = int(round(dy / distance))
        if not game_map.is_blocked(self.x + dx, self.y + dy):
            self.move(dx, dy)


@dataclass
class Actor(Entity):
    fighter: "Fighter" = field(default=None, repr=False)
    inventory: "Inventory" = field(default=None, repr=False)
    ai: Optional["BaseAI"] = field(default=None, repr=False)

    def __post_init__(self) -> None:
        self.blocks_movement = True
        self.render_order = RenderOrder.ACTOR
        if self.fighter:
            self.fighter.parent = self
        if self.inventory:
            self.inventory.parent = self
        if self.ai:
            self.ai.parent = self

    @property
    def is_alive(self) -> bool:
        return bool(self.fighter and self.fighter.hp > 0)


@dataclass
class Item(Entity):
    consumable: Optional["Consumable"] = field(default=None, repr=False)

    def __post_init__(self) -> None:
        self.render_order = RenderOrder.ITEM
        if self.consumable:
            self.consumable.parent = self


class Consumable:
    parent: Item

    def activate(
        self, consumer: Actor, engine: "Engine"
    ) -> str:  # pragma: no cover - ABC
        raise NotImplementedError


@dataclass
class HealingConsumable(Consumable):
    amount: int

    def activate(self, consumer: Actor, engine: "Engine") -> str:
        if not consumer.fighter:
            return f"{consumer.name} cannot use items."
        consumer.fighter.heal(self.amount)
        consumer.inventory.remove(self.parent)
        return f"{consumer.name} feels better!"


@dataclass
class LightningConsumable(Consumable):
    damage: int
    maximum_range: int

    def activate(self, consumer: Actor, engine: "Engine") -> str:
        target = engine.closest_hostile(consumer, maximum_range=self.maximum_range)
        if not target:
            return "No enemy is close enough to strike."
        results = target.fighter.take_damage(self.damage)
        consumer.inventory.remove(self.parent)
        return f"Lightning strikes {target.name} for {results['damage']} damage!"


ConsumableType = Consumable
