# Pac-Man (Challenge #109)

A Python + pygame homage to the 1980 arcade classic. The maze, pellets, and spawn points are defined in JSON tile maps so you can edit or create new layouts without touching code. Each ghost mirrors the original behavioural archetypes—Chaser, Ambusher, Flanker, and Shy—using weighted targeting logic and alternating scatter/chase timing.

## Requirements

- Python 3.10+
- `pygame` (install via the repo extras: `python -m pip install -e .[games]`)

All art and audio assets in `Assets/` are original procedural renders generated specifically for this project. Feel free to reuse them under the MIT license bundled with the repository.

## Running the Game

From the repository root (or `challenges/Games/` directory):

```bash
python "challenges/Games/Pacman/main.py"
```

Use the arrow keys to steer Pac-Man. Press `Esc` or close the window to quit.

## Controls & Gameplay

| Action | Key |
| ------ | --- |
| Move up | ↑ or `W` (arrow keys take priority) |
| Move down | ↓ |
| Move left | ← |
| Move right | → |
| Exit | `Esc` |

- Eat small pellets for **10 points** and power pellets for **50 points**.
- Power pellets trigger **frightened mode** for six seconds; ghosts turn blue and can be eaten for **200, 400, 800, 1600** points per chain.
- Clear every pellet to advance to the next level. Speeds increase slightly each round through `config.json` modifiers.
- Losing all three lives restarts the run from level one.

## Ghost Behaviours

Ghosts cycle between **scatter** (retreat to their corner) and **chase** (target Pac-Man) following the classic arcade timing table. Each ghost updates its target tile according to its personality:

- **Blinky (red)** – Targets Pac-Man's current tile in chase mode, relentlessly pursuing him.
- **Pinky (pink)** – Aims four tiles ahead of Pac-Man's facing direction, attempting an ambush.
- **Inky (cyan)** – Computes a point two tiles ahead of Pac-Man, mirrors it around Blinky, and aims for that flanking tile.
- **Clyde (orange)** – If more than eight tiles away he chases Pac-Man; otherwise he retreats to his scatter corner.

When frightened, ghosts slow down and wander away from Pac-Man. If eaten they speed back to the ghost house, revert to chase, and rejoin the cycle.

## Editing Levels & Speed Tuning

- Maze layout, pellets, spawn tiles, and scatter targets live in `maps/level1.json`. Add new maps following the same schema to extend the campaign.
- `config.json` controls base speeds, frightened duration, level-specific multipliers, and the scatter/chase timer sequence.

The engine automatically loads maps alphabetically (`level1.json`, `level2.json`, ...). Create new layouts, tweak speeds, and swap assets to customise the experience.
