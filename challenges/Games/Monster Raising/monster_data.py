"""Data models and helpers for the Monster Raising challenge.

This module exposes dataclasses for monster species and
individual monsters plus convenience functions for loading
species definitions from JSON.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
import json
import random
from typing import Any, Dict, Iterable, List, Optional

STAT_NAMES: List[str] = [
    "strength",
    "intelligence",
    "endurance",
    "agility",
    "charisma",
]


@dataclass(slots=True)
class MonsterSpecies:
    """Immutable species definition used when instantiating monsters."""

    name: str
    element: str
    temperament: str
    base_stats: Dict[str, int]
    growth_rates: Dict[str, float]

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "MonsterSpecies":
        missing = {
            key
            for key in ("name", "element", "temperament", "base_stats", "growth_rates")
            if key not in data
        }
        if missing:
            raise ValueError(
                f"Species definition missing keys: {', '.join(sorted(missing))}"
            )
        base_stats = {stat: int(data["base_stats"].get(stat, 0)) for stat in STAT_NAMES}
        growth_rates = {
            stat: float(data["growth_rates"].get(stat, 1.0)) for stat in STAT_NAMES
        }
        return cls(
            name=str(data["name"]),
            element=str(data["element"]),
            temperament=str(data["temperament"]),
            base_stats=base_stats,
            growth_rates=growth_rates,
        )

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "element": self.element,
            "temperament": self.temperament,
            "base_stats": dict(self.base_stats),
            "growth_rates": dict(self.growth_rates),
        }


@dataclass
class Monster:
    """Runtime representation of a single monster in the ranch."""

    name: str
    species: MonsterSpecies
    age_days: int = 0
    stage: str = "hatchling"
    stats: Dict[str, float] = field(
        default_factory=lambda: {stat: 0.0 for stat in STAT_NAMES}
    )
    energy: float = 80.0
    hunger: float = 20.0
    happiness: float = 70.0
    discipline: float = 50.0
    training_focus: Optional[str] = None
    training_days_remaining: int = 0
    last_event: str = "Born at the ranch."

    def __post_init__(self) -> None:
        for stat in STAT_NAMES:
            self.stats.setdefault(stat, float(self.species.base_stats.get(stat, 0)))
        self.stats = {stat: float(value) for stat, value in self.stats.items()}
        self.stage = self._determine_stage()
        self.energy = max(0.0, min(100.0, float(self.energy)))
        self.hunger = max(0.0, min(100.0, float(self.hunger)))
        self.happiness = max(0.0, min(100.0, float(self.happiness)))
        self.discipline = max(0.0, min(100.0, float(self.discipline)))

    # ------------------------------------------------------------------
    # Derived helpers
    # ------------------------------------------------------------------
    @property
    def level(self) -> int:
        avg_stat = sum(self.stats.values()) / len(STAT_NAMES)
        return max(1, int(avg_stat // 10) + 1)

    @property
    def is_adult(self) -> bool:
        return self.stage == "adult"

    def mood(self) -> str:
        if self.happiness > 75 and self.hunger < 40:
            return "content"
        if self.hunger > 70:
            return "famished"
        if self.energy < 30:
            return "tired"
        if self.happiness < 40:
            return "irritable"
        return "neutral"

    # ------------------------------------------------------------------
    # Core state transitions
    # ------------------------------------------------------------------
    def advance_day(self) -> List[str]:
        """Update needs, growth, and training for a single in-game day."""

        events: List[str] = []
        self.age_days += 1
        self.stage = self._determine_stage()

        # Needs drift
        self.hunger = min(100.0, self.hunger + 12.0)
        self.energy = min(100.0, self.energy + 6.0)
        self.happiness = max(
            0.0, min(100.0, self.happiness - (3.0 if self.hunger > 70 else 0.5))
        )
        self.discipline = max(0.0, min(100.0, self.discipline - 0.5))

        # Passive stat growth tied to temperament
        growth_factor = 0.8 + (self.happiness / 200.0)
        for stat in STAT_NAMES:
            base_growth = self.species.growth_rates.get(stat, 1.0)
            training_bonus = 0.0
            if self.training_focus == stat and self.training_days_remaining > 0:
                training_bonus = 1.5
            increment = (base_growth * growth_factor) + training_bonus
            self.stats[stat] += increment
        if self.training_days_remaining > 0:
            self.training_days_remaining -= 1
            if self.training_days_remaining == 0:
                events.append(f"{self.name}'s {self.training_focus} regimen concluded.")
                self.training_focus = None

        # Random temperament event
        random_roll = random.random()
        if random_roll < 0.08:
            self.happiness = min(100.0, self.happiness + 6.0)
            events.append(f"{self.name} enjoyed a relaxing walk (+happiness).")
        elif random_roll > 0.94:
            self.discipline = max(0.0, self.discipline - 8.0)
            events.append(f"{self.name} got into mischief (discipline -8).")

        self.last_event = events[-1] if events else f"{self.name} had a calm day."
        return events

    def feed(self, quality: str = "basic") -> str:
        """Feed the monster, adjusting hunger and mood."""

        hunger_reduction = 25.0 if quality == "basic" else 40.0
        happiness_boost = 4.0 if quality == "basic" else 8.0
        self.hunger = max(0.0, self.hunger - hunger_reduction)
        self.happiness = min(100.0, self.happiness + happiness_boost)
        self.energy = min(100.0, self.energy + 3.0)
        message = f"{self.name} munched on {'premium' if quality != 'basic' else 'basic'} feed."
        self.last_event = message
        return message

    def rest(self) -> str:
        self.energy = min(100.0, self.energy + 18.0)
        self.hunger = min(100.0, self.hunger + 6.0)
        self.happiness = min(100.0, self.happiness + 5.0)
        message = f"{self.name} took a restorative nap."
        self.last_event = message
        return message

    def train(self, focus: str) -> str:
        if focus not in STAT_NAMES:
            raise ValueError(
                f"Invalid focus '{focus}'. Expected one of {', '.join(STAT_NAMES)}."
            )
        if self.energy < 20 or self.hunger > 80:
            raise RuntimeError("Monster is too tired or hungry to train.")
        immediate_gain = 3.0
        self.stats[focus] += immediate_gain
        self.energy = max(0.0, self.energy - 18.0)
        self.hunger = min(100.0, self.hunger + 10.0)
        self.happiness = max(0.0, self.happiness - 2.0)
        self.discipline = min(100.0, self.discipline + 4.0)
        self.training_focus = focus
        self.training_days_remaining = 2
        message = f"{self.name} trained {focus} (+{immediate_gain:.1f})."
        self.last_event = message
        return message

    def to_dict(self) -> Dict[str, Any]:
        return {
            "name": self.name,
            "species": self.species.name,
            "age_days": self.age_days,
            "stage": self.stage,
            "stats": {stat: round(value, 2) for stat, value in self.stats.items()},
            "energy": round(self.energy, 2),
            "hunger": round(self.hunger, 2),
            "happiness": round(self.happiness, 2),
            "discipline": round(self.discipline, 2),
            "training_focus": self.training_focus,
            "training_days_remaining": self.training_days_remaining,
            "last_event": self.last_event,
        }

    @classmethod
    def from_dict(
        cls, species_catalog: Dict[str, MonsterSpecies], data: Dict[str, Any]
    ) -> "Monster":
        species_name = data.get("species")
        if species_name not in species_catalog:
            raise KeyError(f"Unknown species '{species_name}' in save file.")
        return cls(
            name=str(data.get("name", "Unnamed")),
            species=species_catalog[species_name],
            age_days=int(data.get("age_days", 0)),
            stage=str(data.get("stage", "hatchling")),
            stats={
                stat: float(
                    data.get("stats", {}).get(
                        stat, species_catalog[species_name].base_stats.get(stat, 0)
                    )
                )
                for stat in STAT_NAMES
            },
            energy=float(data.get("energy", 80.0)),
            hunger=float(data.get("hunger", 20.0)),
            happiness=float(data.get("happiness", 70.0)),
            discipline=float(data.get("discipline", 50.0)),
            training_focus=data.get("training_focus"),
            training_days_remaining=int(data.get("training_days_remaining", 0)),
            last_event=str(data.get("last_event", "Loaded from save.")),
        )

    def _determine_stage(self) -> str:
        if self.age_days < 3:
            return "hatchling"
        if self.age_days < 12:
            return "teen"
        return "adult"


@dataclass(slots=True)
class Egg:
    """A scheduled offspring waiting to hatch."""

    species: str
    days_until_hatch: int
    potential: float
    nickname_hint: Optional[str] = None
    parent_names: Optional[List[str]] = None
    stat_blueprint: Optional[Dict[str, float]] = None

    def to_dict(self) -> Dict[str, Any]:
        return {
            "species": self.species,
            "days_until_hatch": self.days_until_hatch,
            "potential": round(self.potential, 3),
            "nickname_hint": self.nickname_hint,
            "parents": list(self.parent_names) if self.parent_names else None,
            "stat_blueprint": (
                dict(self.stat_blueprint) if self.stat_blueprint else None
            ),
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Egg":
        return cls(
            species=str(data.get("species")),
            days_until_hatch=int(data.get("days_until_hatch", 1)),
            potential=float(data.get("potential", 1.0)),
            nickname_hint=data.get("nickname_hint"),
            parent_names=list(data.get("parents", []) or []),
            stat_blueprint={
                stat: float(value)
                for stat, value in (data.get("stat_blueprint") or {}).items()
            }
            or None,
        )


DEFAULT_SPECIES_DATA: List[Dict[str, Any]] = [
    {
        "name": "Sprigbit",
        "element": "Nature",
        "temperament": "cheerful",
        "base_stats": {
            "strength": 18,
            "intelligence": 16,
            "endurance": 20,
            "agility": 22,
            "charisma": 24,
        },
        "growth_rates": {
            "strength": 1.1,
            "intelligence": 1.2,
            "endurance": 1.3,
            "agility": 1.4,
            "charisma": 1.2,
        },
    },
    {
        "name": "Embercub",
        "element": "Fire",
        "temperament": "brash",
        "base_stats": {
            "strength": 24,
            "intelligence": 14,
            "endurance": 22,
            "agility": 20,
            "charisma": 18,
        },
        "growth_rates": {
            "strength": 1.5,
            "intelligence": 1.0,
            "endurance": 1.2,
            "agility": 1.3,
            "charisma": 1.1,
        },
    },
    {
        "name": "Starlet",
        "element": "Light",
        "temperament": "focused",
        "base_stats": {
            "strength": 16,
            "intelligence": 26,
            "endurance": 18,
            "agility": 20,
            "charisma": 28,
        },
        "growth_rates": {
            "strength": 1.0,
            "intelligence": 1.6,
            "endurance": 1.1,
            "agility": 1.2,
            "charisma": 1.5,
        },
    },
    {
        "name": "Tidewhirl",
        "element": "Water",
        "temperament": "calm",
        "base_stats": {
            "strength": 20,
            "intelligence": 22,
            "endurance": 24,
            "agility": 18,
            "charisma": 19,
        },
        "growth_rates": {
            "strength": 1.2,
            "intelligence": 1.3,
            "endurance": 1.5,
            "agility": 1.0,
            "charisma": 1.1,
        },
    },
]


def load_species_catalog(data_path: Optional[Path] = None) -> Dict[str, MonsterSpecies]:
    """Load species definitions from JSON, falling back to defaults."""

    data: Iterable[Dict[str, Any]]
    if data_path and data_path.exists():
        with data_path.open("r", encoding="utf-8") as handle:
            raw = json.load(handle)
            if not isinstance(raw, list):
                raise TypeError("Species file must contain a list of species records.")
            data = raw
    else:
        data = DEFAULT_SPECIES_DATA

    catalog: Dict[str, MonsterSpecies] = {}
    for entry in data:
        species = MonsterSpecies.from_dict(entry)
        catalog[species.name] = species
    return catalog


def generate_monster_name(species: MonsterSpecies) -> str:
    syllables = [
        species.name[:3].lower(),
        random.choice(["ri", "na", "lo", "mi", "ta", "ze"]),
        random.choice(["n", "ra", "lo", "mi", "tu"]),
    ]
    name = "".join(syllables)
    return name.capitalize()


def create_monster(species: MonsterSpecies, nickname: Optional[str] = None) -> Monster:
    """Instantiate a monster with slight stat variance."""

    stats = {
        stat: float(value + random.uniform(-2.0, 2.0))
        for stat, value in species.base_stats.items()
    }
    name = nickname or generate_monster_name(species)
    return Monster(name=name, species=species, stats=stats)


def blend_offspring_stats(
    parent_a: Monster, parent_b: Monster, potential: float
) -> Dict[str, float]:
    stats: Dict[str, float] = {}
    for stat in STAT_NAMES:
        average = (parent_a.stats[stat] + parent_b.stats[stat]) / 2.0
        variance = random.uniform(-3.0, 3.0)
        stats[stat] = max(5.0, average * potential + variance)
    return stats
