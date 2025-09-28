"""Simulation logic for the Monster Raising challenge."""

from __future__ import annotations

from collections import deque
from dataclasses import dataclass, field
from pathlib import Path
import json
import random
from typing import Deque, Dict, List, Optional, Tuple

from monster_data import (
    Egg,
    Monster,
    MonsterSpecies,
    STAT_NAMES,
    blend_offspring_stats,
    create_monster,
)


@dataclass
class GameState:
    """Encapsulates the ranch state, actions, and persistence helpers."""

    species_catalog: Dict[str, MonsterSpecies]
    day: int = 1
    gold: int = 120
    reputation: int = 0
    inventory: Dict[str, int] = field(
        default_factory=lambda: {"basic_feed": 8, "deluxe_feed": 2}
    )
    monsters: List[Monster] = field(default_factory=list)
    eggs: List[Egg] = field(default_factory=list)
    event_log: Deque[str] = field(default_factory=lambda: deque(maxlen=120))

    def log_event(self, message: str) -> None:
        self.event_log.append(message)

    # ------------------------------------------------------------------
    # Core actions
    # ------------------------------------------------------------------
    def advance_day(self) -> None:
        self.day += 1
        self.log_event(f"--- Dawn of Day {self.day} ---")
        for monster in self.monsters:
            events = monster.advance_day()
            for event in events:
                self.log_event(event)
            # Bonus payout if discipline high
            if monster.discipline > 80:
                self.gold += 3
        self._process_eggs()
        self._maybe_random_event()

    def feed_monster(self, monster: Monster, quality: str = "basic") -> str:
        item_key = "basic_feed" if quality == "basic" else "deluxe_feed"
        if self.inventory.get(item_key, 0) <= 0:
            raise RuntimeError("No feed of that type left in inventory.")
        self.inventory[item_key] -= 1
        result = monster.feed("basic" if quality == "basic" else "deluxe")
        self.log_event(result)
        return result

    def rest_monster(self, monster: Monster) -> str:
        result = monster.rest()
        self.log_event(result)
        return result

    def train_monster(self, monster: Monster, focus: str) -> str:
        if focus not in STAT_NAMES:
            raise ValueError(f"Unknown focus '{focus}'.")
        reward = monster.train(focus)
        self.gold += 6
        self.reputation += 1
        self.log_event(reward)
        return reward

    def breed_monsters(self, parent_a: Monster, parent_b: Monster) -> Egg:
        if not parent_a.is_adult or not parent_b.is_adult:
            raise RuntimeError("Both parents must be adults.")
        if parent_a is parent_b:
            raise RuntimeError("Select two different monsters for breeding.")
        potential = 0.95 + random.random() * 0.2
        dominant_species = (
            parent_a.species if parent_a.level >= parent_b.level else parent_b.species
        )
        stats = blend_offspring_stats(parent_a, parent_b, potential)
        nickname_hint = (
            f"{dominant_species.name[:3]}-{parent_a.name[0]}{parent_b.name[0]}"
        )
        egg = Egg(
            species=dominant_species.name,
            days_until_hatch=random.randint(2, 4),
            potential=potential,
            nickname_hint=nickname_hint,
            parent_names=[parent_a.name, parent_b.name],
            stat_blueprint=stats,
        )
        self.eggs.append(egg)
        self.log_event(
            f"Breeding lab scheduled a {egg.species} egg (hatches in {egg.days_until_hatch} days)."
        )
        self.reputation += 2
        return egg

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    def select_best_partner(self, source: Monster) -> Optional[Monster]:
        candidates = [m for m in self.monsters if m.is_adult and m is not source]
        if not candidates:
            return None
        return max(candidates, key=lambda m: sum(m.stats.values()))

    def _process_eggs(self) -> None:
        hatched: List[Tuple[Egg, Monster]] = []
        for egg in self.eggs:
            egg.days_until_hatch -= 1
            if egg.days_until_hatch <= 0:
                species = self.species_catalog.get(egg.species)
                if not species:
                    self.log_event(
                        f"Egg could not hatch: unknown species '{egg.species}'."
                    )
                    continue
                stats = egg.stat_blueprint or species.base_stats
                name = egg.nickname_hint or f"New {species.name}"
                monster = Monster(
                    name=name, species=species, stats=stats, stage="hatchling"
                )
                hatched.append((egg, monster))
        for egg, monster in hatched:
            self.eggs.remove(egg)
            self.monsters.append(monster)
            parent_info = " & ".join(egg.parent_names or [])
            self.log_event(
                f"Egg hatched into {monster.species.name} {monster.name}! (Parents: {parent_info})"
            )

    def _maybe_random_event(self) -> None:
        roll = random.random()
        if roll < 0.12:
            self.gold += 10
            self.log_event("Visitors tipped 10 gold after a stable tour.")
        elif roll > 0.9 and self.gold > 30:
            loss = random.randint(5, 15)
            self.gold -= loss
            self.log_event(f"Equipment repairs cost {loss} gold.")

    # ------------------------------------------------------------------
    # Persistence
    # ------------------------------------------------------------------
    def to_dict(self) -> Dict[str, object]:
        return {
            "day": self.day,
            "gold": self.gold,
            "reputation": self.reputation,
            "inventory": dict(self.inventory),
            "monsters": [monster.to_dict() for monster in self.monsters],
            "eggs": [egg.to_dict() for egg in self.eggs],
            "event_log": list(self.event_log),
            "metadata": {"version": 1},
        }

    @classmethod
    def from_dict(
        cls, species_catalog: Dict[str, MonsterSpecies], data: Dict[str, object]
    ) -> "GameState":
        state = cls(
            species_catalog=species_catalog,
            day=int(data.get("day", 1)),
            gold=int(data.get("gold", 0)),
            reputation=int(data.get("reputation", 0)),
            inventory={
                str(key): int(value)
                for key, value in (data.get("inventory") or {}).items()
            },
        )
        state.monsters = [
            Monster.from_dict(species_catalog, entry)
            for entry in data.get("monsters", [])
        ]
        state.eggs = [Egg.from_dict(entry) for entry in data.get("eggs", [])]
        for event in data.get("event_log", [])[-120:]:
            state.event_log.append(str(event))
        return state

    def save(self, path: Path) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("w", encoding="utf-8") as handle:
            json.dump(self.to_dict(), handle, indent=2)

    @classmethod
    def load(
        cls, species_catalog: Dict[str, MonsterSpecies], path: Path
    ) -> "GameState":
        with path.open("r", encoding="utf-8") as handle:
            data = json.load(handle)
        return cls.from_dict(species_catalog, data)


# ----------------------------------------------------------------------
# Convenience factories
# ----------------------------------------------------------------------


def default_state(species_catalog: Dict[str, MonsterSpecies]) -> GameState:
    state = GameState(species_catalog=species_catalog)
    if not state.monsters:
        for species in species_catalog.values():
            if len(state.monsters) >= 2:
                break
            state.monsters.append(create_monster(species))
    state.log_event("Stable opened for business.")
    return state


def quickstart_state(species_catalog: Dict[str, MonsterSpecies]) -> GameState:
    state = GameState(species_catalog=species_catalog, day=1, gold=140, reputation=1)
    sprigbit = create_monster(species_catalog["Sprigbit"], nickname="Mossy")
    embercub = create_monster(species_catalog["Embercub"], nickname="Flare")
    sprigbit.age_days = 14
    embercub.age_days = 16
    sprigbit.stage = "adult"
    embercub.stage = "adult"
    state.monsters.extend([sprigbit, embercub])
    state.eggs.append(
        Egg(
            species="Sprigbit",
            days_until_hatch=1,
            potential=1.05,
            nickname_hint="Spr-MF",
            parent_names=[sprigbit.name, embercub.name],
            stat_blueprint=blend_offspring_stats(sprigbit, embercub, 1.05),
        )
    )
    state.log_event("Quick-start stable loaded: Mossy and Flare await your orders.")
    state.log_event("Egg expected to hatch tomorrow.")
    return state


def load_or_create(path: Path, species_catalog: Dict[str, MonsterSpecies]) -> GameState:
    if path.exists():
        return GameState.load(species_catalog, path)
    state = default_state(species_catalog)
    state.save(path)
    return state
