# Scorched Earth Clone (Challenge 116)

A pygame-based homage to the classic artillery game. Tanks take turns lobbing
projectiles over destructible terrain while gusty winds, weapon selection, and
power/angle controls determine the outcome. Human pilots can square off against
AI gunners, tweak physics parameters via configuration menus, and persist custom
weapon presets to JSON for future matches.

![Gameplay mockup](../programming%20challenges.png)

## Features

- **Destructible terrain** generated from a sinusoidal height map and carved by
  explosions.
- **Wind and gravity** influence each projectile each frame for satisfying
  artillery arcs.
- **Multiple weapon types** with adjustable blast radius, damage, projectile
  speed, and splash falloff.
- **Turn-based human & AI** combat supporting configurable player counts.
- **Scoreboard** tallies wins and total damage per combatant after each match.
- **Configuration menus** for match rules (rounds, wind range, player mix) and
  weapon preset editing with JSON save/load (`weapon_presets.json`).

## Requirements

```bash
python -m pip install -e .[games]
```

The `games` extra installs `pygame` and dependencies required by other projects
in this directory.

## Running the Game

```bash
python "Games/ScorchedEarth/scorched_earth.py"
```

Set `SDL_VIDEODRIVER=dummy` when launching on headless systems for a smoketest:

```bash
SDL_VIDEODRIVER=dummy python "Games/ScorchedEarth/scorched_earth.py"
```

## Controls

| Action | Keys |
| ------ | ---- |
| Navigate menus | Arrow keys, `Enter` to confirm, `Esc` to cancel |
| Aim cannon | Left/Right arrow keys |
| Adjust power | Up/Down arrow keys |
| Cycle weapon | `Q` / `E` |
| Fire | Space |
| Save weapon presets | `S` in the weapon menu |
| Reset weapon presets | `R` in the weapon menu |

AI turns resolve automatically once their aiming simulation completes.

## Physics & Gameplay Tunables

- **Gravity:** `80 px/s²` (edit `GRAVITY` in `scorched_earth.py`).
- **Wind range:** Adjustable in the match configuration menu, defaults to
  ±25 px/s² and re-rolls every turn.
- **Tank health:** 100 HP. Damage scales with distance from explosion using a
  configurable splash falloff per weapon.
- **Projectile speed:** Derived from weapon presets and current firing power.
- **Terrain granularity:** `TERRAIN_RESOLUTION` controls heightmap fidelity.

## Custom Weapon Presets

Weapon definitions live in `weapon_presets.json`. Launch the "Weapon Presets"
menu from the title screen to tweak radius, damage, speed, and falloff for each
weapon. Press `S` to save your adjustments or `R` to restore the defaults.

Sample JSON structure:

```json
[
  {
    "name": "Grenade",
    "explosion_radius": 45.0,
    "base_damage": 35.0,
    "projectile_speed": 85.0,
    "splash_falloff": 0.6
  }
]
```

## Assets & Credits

This implementation uses pygame primitives for tanks, projectiles, and terrain,
so no external art or audio assets are required. The included repository banner
`screenshot` is reused in this README as a placeholder preview.

## Troubleshooting

- **No window appears / SDL errors:** Ensure SDL2 is installed on your system.
  Linux users may need `sudo apt install libsdl2-dev libsdl2-ttf-2.0-0`.
- **Preset file missing:** The game recreates defaults if `weapon_presets.json`
  is deleted. Saving from the weapon menu writes the current presets.
- **AI shots seem off:** Narrow the wind range or reduce gravity for easier
  targeting.

Enjoy the artillery duels!
