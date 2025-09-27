# Minesweeper

- **Challenge:** #117 — Minesweeper
- **Language:** Python (tkinter)

## Overview
Graphical Minesweeper clone with adjustable board size and mine density. Uses tkinter buttons for cells and message boxes for win/loss states.

## Dependencies
- Python standard library + `tkinter` (ships with most desktop Python builds).
- Optional: `pip install -e .[games]` to align with repo-wide tooling.

## Run
```bash
python mine.py --rows 10 --cols 10 --mines 15
```
Arguments:
- `--rows` / `--cols`: board dimensions (defaults: 10×10).
- `--mines`: number of mines (default: 10).

## Notes
- On Linux ensure `python3-tk` is installed if tkinter is missing.
- The GUI auto-resizes based on the board; adjust the constants at the top for custom styling.
