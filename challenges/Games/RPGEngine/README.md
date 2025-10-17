# RPG Engine Prototype

A compact RPG sandbox built with **Python + pygame** that demonstrates reusable engine systems: overworld exploration on a data-driven tile map, branching dialogue with quest hooks, an inventory-backed stat model, and menu-driven combat scenes. Content lives entirely in JSON/YAML files so writers can expand the demo without touching code.

![Overview](../programming%20challenges.png)

## Quick Start

```bash
python -m pip install -e .[games]  # installs pygame + helpers
python challenges/Games/RPGEngine/main.py
```

Controls:

- **Arrow keys / WASD** – move across the overworld
- **Space** – talk to NPCs, advance dialogue, confirm battle prompts
- **S** – quick-save to `challenges/Games/RPGEngine/saves/savegame.json`
- **L** – load the last save
- **Esc** – quit

> pygame opens a desktop window; make sure your environment supports simple SDL windows.

## Playable Loop

1. Spawn in Riverfell village and speak to **Elder Rowan** to accept the *Welcome to Riverfell* quest.
2. Venture north into the forest to trigger a random battle with a **River Slime**. Combat uses a Final Fantasy–style menu with Attack / Item / Flee.
3. Win to update the quest log, then return to the elder for thanks and head to **Tilda** the merchant to collect a potion reward.
4. Save/load at any point to persist hero stats, inventory, and quest progress.

The quest demonstrates the core state machines: dialogue nodes mark objectives complete, battles can unlock follow-up nodes, and conversations can distribute inventory rewards.

## Engine Architecture

| System | Module | Notes |
|--------|--------|-------|
| Constants & paths | `engine/constants.py` | Screen, tile size, FPS, default data/save directories |
| Data loading | `engine/data_loader.py` | JSON/YAML loader with friendly error messages |
| Entities & inventory | `engine/entity.py` | `Stats`, `Inventory`, and serialisable `Entity` objects used for both heroes and enemies |
| World map | `engine/world.py` | Tile definitions, walkability, NPC placement, and simple player movement |
| Dialogue & quests | `engine/dialogue.py` | Dialogue graph nodes, quest objective tracking, and reward hooks |
| Battle scene | `engine/battle.py` | Menu UI, turn resolution, and combat logging |
| Save system | `engine/save_system.py` | Persists player + world state to JSON |
| Game loop | `engine/game.py` | Coordinates overworld, dialogue, and battle states |

### Data-first content

All gameplay content ships inside `challenges/Games/RPGEngine/data/`:

- `world.json` – tile palette, encounter rates, encounter tables, and the overworld grid.
- `npcs.json` – NPC ids, names, spawn coordinates, dialogue references, and optional quest ids.
- `dialogue.yaml` – dialogue trees with per-node objective updates, battle hooks, and optional item grants.
- `quests.json` – quest titles, descriptions, objective copy, and reward metadata.
- `items.json` – inventory definitions used by both NPC rewards and battle menus.
- `player.json` – starting stats and inventory for the hero.
- `enemies.json` – stat blocks for random encounters and scripted battles.

Saving uses `challenges/Games/RPGEngine/saves/savegame.json`; delete the file to start fresh.

### Extensibility Tips

- **Add a new tile type** by updating `world.json` with a palette entry and swapping ids in the `map` grid. Encounter tables support multiple enemy ids per tile type.
- **Add NPCs** by placing new entries in `npcs.json` and referencing dialogue ids. Dialogue nodes can reward items (`give_items`) or mark quest objectives via `complete_objectives` and `reward_objectives`.
- **Script quests** by editing `quests.json` and wiring objectives into dialogue nodes. The engine treats quest updates as idempotent booleans, so repeated conversations stay consistent.
- **Design battles** by adding stat blocks in `enemies.json`. Dialogue nodes with a `battle` field trigger a `BattleScene`; random encounters pull from each tile type’s `encounters` list.
- **Persist extra world state** by extending the `world_state` dictionary in `engine/game.py::save` and reading it back inside `try_resume_save`.

### File format summary

```yaml
# dialogue.yaml
elder_intro:
  start:
    speaker: Elder Rowan
    text: "Hero, we have awaited your arrival."
    complete_objectives:
      welcome_hero: [0]
    next: warning
```

```json
// world.json
{
  "tiles": {
    "forest": {
      "color": [20, 90, 40],
      "walkable": true,
      "encounter_rate": 0.15,
      "encounters": ["slime", "wolf"]
    }
  },
  "map": [["forest", "grass", "grass"], ...]
}
```

## Extending Further

- Drop in new quests that chain multiple battles or reference alternative rewards (e.g., give equipment via `items.json`).
- Replace the rendering layer with sprites/tilesheets—the engine already batches draw calls per tile, so swapping `surface.fill` with `blit` sprites is straightforward.
- Expand combat: add skills by extending `BattleScene.actions`, or expose skill definitions via data files.
- Swap pygame for Godot or another engine while keeping the data contracts identical; the clean separation between systems and data makes porting easier.

Happy hacking!
