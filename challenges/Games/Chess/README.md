# Chess

A feature-complete chess implementation written in Python with modular board
logic, PGN support, an optional minimax AI, and both Pygame and CLI
interfaces.

## Features

- Full legal move generation including castling, en passant, promotions, and
  check/checkmate detection.
- Pure-Python board module that can be reused or tested independently.
- PGN import/export helpers for sharing games.
- Optional minimax AI opponent with a material-based heuristic.
- Two front-ends: a headless CLI and a graphical Pygame board.
- Automated pytest suite covering tricky move rules.

## Installation

Install the repository in editable mode with the `games` extra to pull in
Pygame:

```bash
python -m pip install -e .[games]
```

For development (tests, linting) you may also want the `developer` extra:

```bash
python -m pip install -e .[games,developer]
```

## Running the CLI

```
python -m challenges.Games.Chess.cli [--ai white|black|both] [--depth N] [--pgn game.pgn]
```

> Tip: If you `cd challenges/Games` first, run `python -m Chess.cli` instead of prefixing
> with `challenges.Games.`

Commands available while playing:

- Enter moves in SAN (e.g. `Nf3`) or UCI (`g1f3`).
- Type `moves` to see all legal moves.
- `save output.pgn` exports the move history.
- `load input.pgn` replaces the current position with moves from the given PGN.
- `quit` ends the session.

## Running the Pygame UI

```
python -m challenges.Games.Chess.pygame_ui [ai]
```

> From inside `challenges/Games/`, launch with `python -m Chess.pygame_ui`.

Click on a piece to see its legal moves and click a highlighted square to move.
Pass `ai` as `w`, `b`, or `wb` to let the AI play a side.

## Tests

Run the automated tests for chess with:

```
pytest challenges/Games/Chess/tests
```

This exercises key move legality scenarios (castling, en passant, check
handling) to ensure the engine stays correct.
