# Tetris (Pygame)

A modernised take on the `/g/` Tetris challenge with the classic seven tetrominoes, bag
randomiser, gravity, soft/hard drop scoring, and a configurable ghost piece.

![Gameplay preview](assets/tetris_gameplay.png)

> Add your own screenshot or GIF to `Games/Tetris/assets/tetris_gameplay.png` to replace this
> placeholder reference.

## Features

- Seven-piece bag randomiser with five-piece preview queue
- Configurable Delayed Auto Shift (DAS) and Auto Repeat Rate (ARR)
- Soft drop (per-cell scoring) and space-bar hard drop (double scoring)
- Level-based gravity acceleration and Tetris line clear rewards
- Optional ghost piece overlay (`G` toggles at runtime)
- Modular architecture: `board.py` (rules), `input_handler.py` (controls), `renderer.py` (drawing),
  `settings.py` (tweakable options), and `main.py` (entry point)

## Controls

| Action | Key(s) |
| ------ | ------ |
| Move left / right | Arrow keys |
| Soft drop | Down arrow |
| Hard drop | Space |
| Rotate counter-clockwise | Z |
| Rotate clockwise | X or Up arrow |
| Toggle ghost piece | G |
| Restart | R |
| Quit | Esc or window close |

## Configuration

Edit [`settings.py`](settings.py) to tailor the playfield:

- `board_width` / `board_height`: change the board size (classic is 10×20)
- `das` / `arr`: adjust movement responsiveness in milliseconds
- `soft_drop_rate`: set how many cells per second soft drop should attempt
- `gravity_seconds`: baseline gravity interval (scales with level)
- `ghost_piece`: default state for the ghost overlay
- `next_queue_length`: number of pieces shown in the preview column

## Running the Game

Install the shared game dependencies from the repository root:

```bash
python -m pip install -e .[games]
```

Then launch Tetris from `Games/`:

```bash
python -m Tetris.main
```

You can also run it via the script path:

```bash
python "Tetris/main.py"
```

## Troubleshooting

| Issue | Fix |
| ----- | --- |
| Window opens and closes immediately | Run from a terminal and ensure pygame is installed via `pip install -e .[games]`. |
| Inputs feel unresponsive | Lower the `das`/`arr` values in `settings.py` or check that the game window has focus. |
| Ghost piece is distracting | Press `G` to toggle it off or set `ghost_piece = False` in `settings.py`. |
| Preview queue clips off-screen | Increase `preview_columns` or decrease `block_size`. |

## Roadmap

- Optional hold queue
- Sound effects and music toggle
- High score persistence
