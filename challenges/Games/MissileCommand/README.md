# Missile Command (Challenge #129)

A modernised take on Atari's Missile Command built with Python and pygame.
Defend six cities and three interceptor bases from incoming enemy missiles,
managing limited ammunition and chaining explosions to rack up points. The
game features dynamic difficulty scaling, a level-based score multiplier, and
an optional two-player cooperative cursor for hectic defence sessions.

## Features

- **Arcade missile defence** with cities, missile bases, interceptor rockets,
  and radial explosions that damage multiple incoming threats.
- **Progressive levels** that spawn faster and more numerous waves while
  increasing the score multiplier and base ammunition.
- **Resource management**: each base has limited missiles and short cooldowns,
  rewarding careful shot selection.
- **Procedural sound effects** generated at runtime (no asset downloads).
- **Optional co-op mode**: Player Two guides a reticle with the keyboard and
  fires from the central base while Player One keeps mouse control.
- **Difficulty tuning** via the `DifficultySettings` dataclass for quick
  gameplay experiments.

## Requirements

- Python 3.10+
- `pygame`
- `numpy`

Install the repo's bundled extras from the repository root:

```bash
python -m pip install -e .[games]
```

## Running the Game

```bash
cd challenges/Games/MissileCommand
python missile_command.py          # Solo defence using the mouse
python missile_command.py --two-player  # Start with the cooperative cursor active
```

## Controls

| Action | Player | Control |
| ------ | ------ | ------- |
| Fire interceptor | Player One | Left mouse button (fires from the closest armed base) |
| Aim | Player One | Mouse cursor |
| Toggle 2P mode | Both | `F2` |
| Fire interceptor | Player Two | `Space` (launches from the central base) |
| Aim | Player Two | `WASD` or arrow keys |
| Restart after defeat | Both | `R` |
| Quit | Both | `Esc` or window close |

Radial explosions destroy any missile that enters the blast radius. Surviving
cities award a bonus at the end of each wave, and the score multiplier scales
with the current level.

## Difficulty Scaling

The `DifficultySettings` dataclass at the top of `missile_command.py` controls
all progression parameters: wave size, enemy speed, spawn cadence, explosion
lifetimes, base ammunition, and scoring bonuses. Tweak these values to create
practice-friendly or punishing scenarios without modifying the rest of the
code.

## Two-Player Cooperative Mode

Activate co-op with `F2` (or launch with `--two-player`). Player Two controls a
highlighted targeting cursor using the keyboard and fires from the central
missile base. Coordinated shots can blanket the sky with overlapping explosions,
covering multiple city lanes at once.

Have fun defending the last bastions of civilisation!
