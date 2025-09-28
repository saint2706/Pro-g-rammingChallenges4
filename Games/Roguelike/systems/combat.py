"""Combat routines for actors."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Optional, TYPE_CHECKING

if TYPE_CHECKING:  # pragma: no cover - hints only
    from ..engine import Engine
    from ..entity import Actor


@dataclass
class Fighter:
    hp: int
    defense: int
    power: int
    parent: Optional["Actor"] = field(default=None, repr=False)

    @property
    def max_hp(self) -> int:
        return self._max_hp

    @max_hp.setter
    def max_hp(self, value: int) -> None:
        self._max_hp = value

    def __post_init__(self) -> None:
        self._max_hp = self.hp

    def heal(self, amount: int) -> int:
        if not self.parent:
            return 0
        new_hp = min(self.hp + amount, self.max_hp)
        healed = new_hp - self.hp
        self.hp = new_hp
        return healed

    def take_damage(self, amount: int) -> Dict[str, int]:
        self.hp -= amount
        result = {"damage": amount}
        if self.hp <= 0 and self.parent:
            self.die()
        return result

    def attack(self, target: "Actor", engine: "Engine") -> str:
        damage = max(0, self.power - target.fighter.defense)
        if damage > 0:
            results = target.fighter.take_damage(damage)
            return f"{self.parent.name.capitalize()} attacks {target.name} for {results['damage']} hit points."
        return (
            f"{self.parent.name.capitalize()} attacks {target.name} but does no damage!"
        )

    def die(self) -> None:
        if not self.parent:
            return
        self.parent.char = "%"
        self.parent.colour = (191, 0, 0)
        self.parent.blocks_movement = False
        self.parent.render_order = self.parent.render_order.__class__.CORPSE
        if self.parent.parent:
            self.parent.parent.remove_actor_ai(self.parent)
