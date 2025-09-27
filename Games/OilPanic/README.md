# Oil Panic (Game & Watch Tribute)

Challenge **#124** recreates Nintendo's dual-screen Game & Watch classic in Python using `pygame`. The game features the
signature split-level play: catch dripping oil on the top screen, then dash downstairs to pour it safely into the tower while
dodging the patrolling policeman.

## Features

- Three fully-tunable difficulty levels (Easy, Normal, Hard) that adjust drip cadence, fall speed, and policeman patrol rate.
- Timer-driven scoring: the faster you empty the bucket, the higher the bonus per pour.
- Persistent high scores per difficulty level saved to `high_scores.json`.
- Responsive keyboard controls with smooth bucket movement on both screens.
- Hand-drawn sprites and palettes inspired by the original LCD handheld.
- Audio cues for catches, spills, floor changes, and successful deposits.
- `config.json` toggle for window scaling (1x–5x) and the default difficulty.

## Requirements

Install the repository with the `games` extra to grab `pygame` and its dependencies:

```bash
python -m pip install -e .[games]
```

## Running the Game

From the repository root:

```bash
python Games/OilPanic/oil_panic.py
```

Set the `SDL_VIDEODRIVER=dummy` environment variable if you only need a headless smoke test.

## Controls

| Key | Action |
| --- | ------ |
| Left / Right | Move the bucket between the three catch lanes |
| Down | Move to the lower screen to start pouring |
| Up | Return to the upper screen |
| Space (bottom screen) | Pour a single unit of oil into the tower |
| 1 / 2 / 3 | Switch to Easy / Normal / Hard and restart |
| Enter | Restart after a game over |
| Esc | Quit |

## Gameplay Tips

- The bucket holds three drops. A fourth drop spills immediately, costing a life.
- Pouring while the policeman walks through the red danger strip causes a spill and costs a life.
- Score bonuses factor in the remaining time: empty the bucket quickly to maximise points.
- High scores persist automatically—delete `high_scores.json` to reset the records.

## Configuration

`config.json` ships with sane defaults:

```json
{
  "window_scale": 2,
  "difficulty": "Normal"
}
```

- `window_scale` multiplies the base 256×352 surface to accommodate larger displays.
- `difficulty` sets the default mode at launch. You can still switch in-game with the number keys.

## Assets and Audio

The game uses lightweight programmatic sprites and procedurally-generated tones (no external media files). All graphics and
sounds were created for this project and fall under the repository's MIT licence.
