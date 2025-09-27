# Breakout (Challenge #131)

A feature-rich Breakout implementation built with **Python + pygame**. The game
loads brick formations, physics tuning, and power-up drops from JSON layout
files so difficulty ramps up across multiple stages. Responsive paddle control,
audio cues, particle bursts, and optional multiball power-ups keep each volley
lively while you chase the high score.

![Breakout gameplay preview](../programming%20challenges.png)

## Gameplay

- **Move**: Left/Right arrow keys or `A`/`D`
- **Launch ball**: `Space`
- **Quit**: `Esc`

### Mechanics

- Three handcrafted levels ship in `levels/`, each with unique brick palettes
  and ball/paddle speed profiles.
- Bricks may require multiple hits and can drop power-ups:
  - `enlarge`: temporarily widens the paddle.
  - `life`: awards an extra life.
  - `multiball`: clones every active ball with a new trajectory.
- Particle trails and procedurally generated chime effects reinforce collisions
  and pick-ups.
- Lives carry between stages; clear every brick to advance to the next level.

## Requirements

The repo exposes a `games` extra that installs pygame and helper libraries:

```bash
python -m pip install -e .[games]
```

If you prefer manual installs, `pygame>=2.6` is the only runtime dependency.

## Run

From the repository root (or `Games/` directory):

```bash
python Games/Breakout/breakout.py
```

## Level authoring

Layouts are plain ASCII text interpreted via the legend map. Use the
`level_editor.py` helper to convert text files into JSON manifests:

```bash
python Games/Breakout/level_editor.py layout.txt Games/Breakout/levels/custom.json \
    --name "Custom Stage" --ball-speed 360 --paddle-speed 440 --brick-size 64 24
```

Provide a JSON snippet to `--legend` to override the default symbol mapping or
add new bricks with unique hit counts and power-ups.

## Assets & Audio

All sounds are generated at runtime, avoiding external licensing concerns.
Particles are rendered procedurally with pygame surfaces, so no image assets are
required.

## Status

✅ Challenge #131 (Breakout) completed.
