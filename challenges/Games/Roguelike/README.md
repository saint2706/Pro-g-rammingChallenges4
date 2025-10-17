# Roguelike Challenge (Python + tcod)

This implementation tackles challenge #121 with a focused roguelike playable in the terminal using [`tcod`](https://python-tcod.readthedocs.io/en/latest/). The build follows a lightweight ECS-inspired structure and exposes modding hooks for tilesets, dungeon content, and save files.

## Features

- **Procedural dungeon generation** using a rectangular room algorithm with corridors.
- **Field of view (FOV)** computed via `tcod.map.compute_fov`, with exploration fog.
- **Turn-based combat** with melee attacks, lightning scrolls, and monster retaliation.
- **Inventory system** for picking up, dropping, and consuming items.
- **Hostile monster AI** that hunts the player using `tcod.path` pathfinding when in sight.
- **Save/Load support** that automatically writes `savegame.json` on exit and can be resumed with `--load`.
- **Tileset configuration** so you can swap glyph palettes without touching the renderer.

## Controls

| Key | Action |
| --- | ------ |
| Arrow keys / Numpad | Move or bump-attack in 8 directions |
| `.` | Wait a turn |
| `g` | Pick up items on the current tile |
| `d` | Drop the last item in your inventory |
| `u` | Use the first item in your inventory |
| `i` | Print inventory summary to the log |
| `Esc` | Quit (auto-saves) |

## Running the Game

```bash
# From the repository root
python -m pip install -e .[games]
python -m pip install tcod
python -m challenges.Games.Roguelike.main
```

The launcher auto-selects a system monospace font (`DejaVuSansMono` or `Liberation Mono`). Provide a custom tileset with `--tileset path/to/font.ttf`. To resume a saved run, point `--load` at the generated JSON file:

```bash
python -m challenges.Games.Roguelike.main --load savegame.json
```

## Architecture Overview

- `config.py` — shared `GameConfig` dataclass plus glyph/colour palettes (`TILESETS`).
- `tiles.py` — `numpy` structured array helpers for wall/floor tiles and lighting variants.
- `entity.py` — entity classes (actors, items, consumables) and rendering metadata.
- `systems/` — `combat.py`, `inventory.py`, and `ai.py` systems managing ECS behaviour.
- `game_map.py` — dungeon generation, FOV data, and entity serialisation.
- `engine.py` — turn loop, action handlers, and message log dispatch.
- `render.py` — tcod console drawing for map, UI, inventory, and message log.
- `content.py` — sample monsters (orcs, trolls) and items (healing potions, lightning scrolls).
- `storage.py` — JSON save/load implementation (tiles, entities, and log history).
- `main.py` — CLI entry point with `--load` and `--tileset` flags.

## Modding Hooks

- **Tilesets:** Add palettes to `TILESETS` in `config.py` and set `GameConfig.tile_set` to swap glyphs/colours.
- **Content:** Extend `build_monsters` / `build_items` in `content.py` with new factories and adjust spawn weights.
- **Map parameters:** Tweak `GameConfig` defaults (map size, room counts, spawn caps, FOV radius) for harder or larger runs.
- **Saves:** `storage.py` serialises to JSON, making it easy to inspect or generate saved games programmatically.

## Save Files

Games auto-save on `Esc` (or any exit) to `savegame.json` alongside the script by default. Use `--load` to resume or override `GameConfig.save_path` if you want separate slots.

Enjoy exploring, modding, and extending the dungeon!
