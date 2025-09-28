# Simon Memory Game

- **Challenge:** #132 — Simon
- **Language:** Python (pygame)

## Overview
Colour-and-sound-based Simon clone built with pygame. Features menu, increasing difficulty, score tracking, and audio/visual feedback using bundled assets.

## Dependencies
- `pip install -e .[games,audio]` (ensures `pygame` plus audio helpers).

## Run
```bash
python simon.py
```
Controls:
- Click the coloured panels or press arrow keys to repeat the pattern.
- Press `Esc` to quit.

## Assets
- Audio cues: `Assets/Audio/*.wav`
- Button art: `Assets/Images/*.png`
Keep the `Assets/` folder beside `simon.py`.

## Notes
- On Linux, install SDL mixer packages if audio fails to initialise.
- Screen size and colour palette are configurable in the constants at the top of the script.
