# Monster Raising / Breeding Simulator

A cozy management sim inspired by classic monster ranchers. Raise a stable of creatures, train them toward specialised builds, and discover hybrid bloodlines through the breeding lab. This build focuses on quick iteration for the `/g/` challenge: it uses pygame for lightweight UI panels, is modular so the simulation can be scripted, and persists progress to JSON saves that you can inspect or version control.

---

## Gameplay Overview

* **Stable management:** Monitor hunger, energy, happiness, and discipline. Daily actions (feeding, training, resting) trade resources for stat growth.
* **Stat growth:** Each species ships base stats and growth rates. Training focuses grant immediate bonuses while daily ticks provide passive gains that scale with temperament and mood.
* **Breeding lab:** Pair two adults to produce an egg. Offspring inherit averaged stats plus a dash of species variance, with elemental combos logged in the save file.
* **Time-based events:** Advancing the day ages monsters, processes eggs, applies scheduled training, and can trigger random mood swings if needs are ignored.
* **Player economy:** Spend gold on feed, earn it from successful training streaks, and invest in infrastructure upgrades (represented by stable reputation).

---

## Controls & UI

Launch with:

```bash
python "Games/Monster Raising/main.py"
```

| Panel | Location | Purpose |
|-------|----------|---------|
| Stable roster | Left column | Click to select a monster. The list shows nickname, species, and current mood indicator. |
| Monster detail | Right column | Displays core stats, age, temperament, and latest activity log. |
| Event log | Upper centre | Shows day summaries, breeding outcomes, and random events. Scroll with mouse wheel. |
| Action bar | Bottom row | Buttons for **Feed**, **Train**, **Rest**, **Breed**, **Next Day**, **Save**, **Load**, and toggling the quick-start roster. |

**Interactions**

* **Left click** buttons to trigger actions for the selected monster. Disabled buttons grey out if the action is invalid (e.g., not enough feed or fewer than two adults for breeding).
* **Mouse wheel** over the event log to review the last 50 events.
* **Keyboard shortcuts:** `F` (feed using the default quality), `D` (deluxe feed), `T` (train), `R` (rest), `B` (breed with best match), `N` (advance to the next day), `S` (save), `L` (load), `Q` (load quick-start), `P` (toggle the default feed quality).
* The window is resizable; panels adapt to maintain layout at common 16:9 and 4:3 sizes.

---

## Save Files & Quick Start

Saves are human-readable JSON documents stored under `Games/Monster Raising/saves/`.

```json
{
  "day": 12,
  "gold": 140,
  "inventory": {"basic_feed": 8, "deluxe_feed": 2},
  "reputation": 1,
  "monsters": [
    {
      "name": "Lumen",
      "species": "Starlet",
      "age_days": 28,
      "stage": "adult",
      "stats": {"strength": 34, "intelligence": 48, "endurance": 29, "agility": 40, "charisma": 55},
      "energy": 72,
      "hunger": 22,
      "happiness": 81,
      "discipline": 68,
      "training_focus": "intelligence"
    }
  ],
  "eggs": [
    {"species": "Starlet", "days_until_hatch": 2, "potential": 1.08}
  ],
  "event_log": ["Starlet Lumen completed a focus session (+3 intelligence)."],
  "metadata": {"version": 1}
}
```

* `stats` keys are always the same set: strength, intelligence, endurance, agility, charisma.
* `potential` scores are multipliers applied during hatching to scale base stat inheritance.
* Eggs move into the `monsters` array automatically when `days_until_hatch` reaches zero.

### Quick-start Scenario

A seeded profile lives at `saves/quickstart.json`. It gives you two adults (Sprigbit and Embercub) and one egg ready to hatch on Day 2 so you can test breeding, daily upkeep, and hatching flows without grinding from scratch. Use the **Load** button (or press `L`) right after launching to jump into this scenario.

---

## Data Files

`data/species.json` defines four starter species with elemental affinities, growth curves, and temperament tags. Edit or extend this file to rebalance the stable. The simulator hot-reloads it whenever you re-launch `main.py`.

For scripting or automated tests, import the modules directly:

```python
from monster_data import load_species_catalog
from simulation import GameState, quickstart_state
```

You can then manipulate the state, run headless day simulations, or write regression tests against the breeding logic without touching pygame.

---

## Development Notes

* **Dependencies:** pygame 2.1+, Python 3.10+. All required packages are covered by the repository-wide `.[games]` extra.
* **Linting:** Modules are type annotated and designed for `ruff`/`mypy` friendliness. Run `python -m compileall "Games/Monster Raising"` as a quick syntax check.
* **Extensibility:** The simulation module exposes clean hooks for new event types. Extend `GameState.advance_day` and the `Action` helpers to bolt on tournaments, quests, or network play without refactoring the UI shell.

Happy ranching!
