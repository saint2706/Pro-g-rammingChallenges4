# Knight's Tour Solver

- **Challenge:** #104 — Knight's Tour
- **Language:** Python

## Overview
Command-line backtracking solver that searches for a full knight's tour on an `n × n` board. Prints the move sequence and board layout directly to stdout.

## Dependencies
No third-party packages. Python 3.10+ is sufficient.

## Run
```bash
python knight.py --size 8 --start 0 0
```
Arguments:
- `--size`: board dimension (default 8).
- `--start`: starting coordinates as `row col` (default `0 0`).

## Notes
- The solver uses Warnsdorff ordering for speed; adjust heuristics in `next_moves` if you want to experiment.
- For very large boards expect longer runtimes.
