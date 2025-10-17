# Old School Multi-Effect Demo

A modern take on the "old school" demoscene vibe. `retro_demo.py` blends a plasma tunnel, parallax text scroller, and chromatic aberration zoomer while a synthesized chiptune soundtrack keeps everything in sync.

## Highlights

- **Timeline-driven show** – deterministic segments with configurable resolution, frame rate, and BPM.
- **Audio/visual sync** – procedurally generated stereo track stays locked to the beat clock that drives visual pulses.
- **Multiple effects** – plasma tunnel intro, marquee scroller with star field, and a depth-based zoomer with scanlines.
- **Capture-ready** – optional GIF / frame dumping with headless mode for reproducible screenshots.

> Note: Captured GIFs/screenshots are generated locally via the CLI flags below and are not tracked in git.

## Quick start

```bash
pip install pygame imageio numpy
python "retro_demo.py"
```

Controls:

- `Esc` / `Q` – exit early
- `Space` – pause / resume timeline (audio is paused too)
- `O` – toggle the overlay (timecode + active segment)

## Configuration options

`retro_demo.py --help` shows all switches. The most useful flags:

| Option | Purpose |
| ------ | ------- |
| `--width` / `--height` | Resolution of the window / captures. |
| `--fps` | Frame cap (visuals + GIF capture). |
| `--bpm` | Change beat clock + synthesized track tempo. |
| `--no-audio` | Disable mixer playback (useful for capture rigs). |
| `--capture-gif path` | Save an animated GIF (combined with `--headless`). |
| `--capture-frames dir` | Dump PNG frames on a stride (`--capture-stride`). |
| `--stop-after seconds` | Terminate early when recording a short clip. |

Example capture commands:

```bash
# 960x540 clip, 60 fps visuals, capture every 4th frame (~15 fps GIF)
python "retro_demo.py" --headless --no-audio \
  --capture-gif demo.gif --capture-stride 4 --stop-after 12

# High-res screenshot series
python "retro_demo.py" --headless --no-audio \
  --capture-frames captures --capture-stride 6 --stop-after 8
```

## Timeline

| Segment | Duration | Notes |
| ------- | -------- | ----- |
| Plasma Tunnel | 12s | Sinusoidal depth cue + beat-synced zoom pulses. |
| Scroller | 9s | Star field, marquee text, beat flash overlay. |
| Zoomer | 10s | Chromatic aberration on beats, scanlines for CRT feel. |

Total runtime is ~31 seconds; the mixer generates enough audio to cover an entire loop. Use `--stop-after` for shorter exports.

## Updating the challenge status

Once you are happy with the output, update `stats.json` (see below) so the repository log reflects the latest configuration, runtime, and asset names.
