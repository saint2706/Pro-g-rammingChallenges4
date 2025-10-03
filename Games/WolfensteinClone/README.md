# Wolfenstein Clone (Challenge #115)

A Python/pygame homage to *Wolfenstein 3D* featuring a classic 2.5D raycasting renderer, sprite guards, collision detection, and
an always-on minimap. The engine is intentionally data-driven: maps, spawn points, and control bindings can be tweaked without
changing the source so you can rapidly prototype new mazes.

![Gameplay preview](../Assets/wolfenstein_clone_preview.png)

## Features

- **Textured raycasting walls** – 60° field of view rendering with per-column texture sampling and simple light falloff for depth.
- **Sprite enemies** – billboarded guards that sort by depth so they feel anchored inside the world.
- **Player physics** – forward/back strafing, rotation, sprint toggle, and solid wall collision against the tile map.
- **Minimap overlay** – top-left fogged minimap showing walls, player heading, and enemy positions in real time.
- **Configurable maps and controls** – load any JSON map via `--map` and adjust the WASD/arrow bindings through `ControlScheme`.

## Running the Game

```bash
python -m Games.WolfensteinClone.game
```

Optional arguments:

```bash
python -m Games.WolfensteinClone.game --map Games/WolfensteinClone/maps/default_map.json --width 1280 --height 720 --fov 1.2
```

### Controls

| Action | Default | Notes |
| ------ | ------- | ----- |
| Move forward/back | `W` / `S` | Classic Wolfenstein stepping. |
| Strafe left/right | `A` / `D` | Lateral movement. |
| Turn left/right | `←` / `→` | Rotate view. |
| Sprint | `Left Shift` | Increases move speed by 80%. |
| Exit | `Esc` | Closes the window. |

To customise bindings, instantiate `WolfensteinGame` with a different `ControlScheme` or modify the dataclass defaults inside
`game.py`.

## Map Format

Maps are stored as JSON files (see [`maps/default_map.json`](maps/default_map.json)). Keys:

- `name`: Human readable label.
- `player_start`: `{ "x": float, "y": float, "angle": float }` – spawn position in tile coordinates and facing angle (radians).
- `enemies`: List of guard spawn objects with `x`/`y` positions.
- `layout`: 2D grid of integers where `0` = empty space and other numbers map to texture IDs configured in `wall_textures`.

You can create additional JSON files and pass them to `--map` to explore new layouts.

## Assets

All bundled textures are CC0 originals authored for this repository:

- `Assets/wall_stone.png` – grey masonry tiles with subtle vertical ridges.
- `Assets/wall_brick.png` – muted red bricks with mortar columns.
- `Assets/enemy_guard.png` – stylised guard sprite with transparent background.

Feel free to replace them with your own creations; just keep the filenames or update the mapping in `GameConfig`.

## Development Notes

- The renderer runs at 60 FPS by default and supports dummy display drivers, making it friendly for CI smoke tests.
- `WolfensteinGame.run(max_frames=N)` is exposed to facilitate automated testing or scripted captures.

Enjoy storming the procedural castle!
