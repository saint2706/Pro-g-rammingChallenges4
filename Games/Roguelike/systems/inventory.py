"""Inventory management for actors."""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Optional, TYPE_CHECKING

if TYPE_CHECKING:  # pragma: no cover - hints only
    from ..entity import Actor, Item
    from ..engine import Engine


@dataclass
class Inventory:
    capacity: int
    items: List["Item"] = field(default_factory=list)
    parent: Optional["Actor"] = field(default=None, repr=False)

    def add(self, item: "Item") -> tuple[bool, str]:
        if len(self.items) >= self.capacity:
            return False, f"{self.parent.name}'s inventory is full."
        self.items.append(item)
        item.parent = None
        return True, f"Picked up {item.name}."

    def remove(self, item: "Item") -> None:
        self.items.remove(item)

    def drop(self, item: "Item", engine: "Engine") -> str:
        self.remove(item)
        item.place(self.parent.x, self.parent.y, engine.game_map)
        return f"Dropped {item.name}."

    def use(self, item: "Item", engine: "Engine") -> str:
        if item.consumable:
            return item.consumable.activate(self.parent, engine)
        return f"{item.name} cannot be used."
