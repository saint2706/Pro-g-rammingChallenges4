# Sudoku Generator & Solver

- **Challenge:** #119 — Sudoku
- **Language:** Python (tkinter + numpy)

## Overview
Tkinter GUI that generates Sudoku puzzles, validates input, and solves boards using backtracking. Includes difficulty presets and automatic board highlighting.

## Dependencies
- `pip install -e .[games]` (provides `numpy` and `pygame`; pygame is unused but bundled via the games extra).
- Ensure tkinter is available (install `python3-tk` on Linux if required).

## Run
```bash
python sudoku.py
```
Controls:
- Click a cell and type digits 1–9.
- Use the buttons to generate puzzles, solve, or clear entries.

## Notes
- Board logic lives in `SudokuLogic`; you can import it for CLI experiments or testing.
- Difficulty weights can be tuned in the `SudokuGame` class.
