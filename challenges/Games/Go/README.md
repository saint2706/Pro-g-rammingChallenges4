# Go (Weiqi/Baduk)

Challenge **#126** from the `/g/` programming list implemented with Python 3 and PySide6. The desktop client supports 9x9, 13x13,
 and 19x19 boards with full capture, ko, suicide, scoring, undo, SGF export, and an optional practice AI.

## Features

- Interactive PySide6 UI with mouse placement, undo, pass, and SGF save.
- Rule enforcement for captures, suicide prevention, and the (simple) super-ko rule.
- Territory and area scoring summaries available at any point in the game.
- Unlimited undo/history that rewinds the game state (board, captures, and move list).
- Optional "Enable AI for White" toggle that makes random legal moves for solo practice.
- SGF export compatible with editors such as Sabaki and online viewers.

## Requirements

Install the repository with the `games` extra to pull in PySide6 and the existing pygame/matplotlib dependencies:

```bash
python -m pip install -e .[games]
```

Running the game requires a Python 3.10+ interpreter capable of launching Qt applications. On Linux you may need the `qtwayland5`
 package (or analogous) to ensure hardware acceleration is available.

## Usage

```bash
python challenges/Games/Go/go.py
```

### Controls

- **Left-click**: place a stone for the current player.
- **Pass**: skip a turn without changing the board. Two consecutive passes end the game in traditional play.
- **Undo**: revert the last move (includes moves made by the optional AI).
- **Score**: view both territory and area scoring summaries, including captured stones.
- **Save SGF**: export the current move list to Smart Game Format.
- **Enable AI for White**: let a random legal-move generator respond for White after each human move or pass.

### Rules Summary

- Stones are captured when their connected group has no liberties (orthogonally adjacent empty points).
- Suicide is disallowed: you may not play a move that leaves your own group without liberties unless it captures opponent stones.
- Ko (simple super-ko) prevents the board from returning to any previous state immediately after a capture.
- The scoring dialog reports both **territory** (captured stones plus surrounded empty points) and **area** (captured stones plus
  surrounded empty points plus stones on the board). Komi can be set when exporting SGF (default 6.5).

### SGF Interoperability

The exported SGF uses FF[4] with GM[1] and encodes each move as `;B[xy]` / `;W[xy]`. Passes are saved with empty coordinates
(`;B[]`). Files import cleanly into Sabaki, OGS review tools, or any FF[4]-compatible editor.

## Testing

Unit tests validate capture removal, suicide detection, ko enforcement, scoring, and undo behaviour. Run them with:

```bash
python -m pytest challenges/Games/Go/tests
```

## Screenshots

Launch the application and take screenshots using the system screenshot tool of your choice once the Qt window is running. (No
pre-rendered assets are required for gameplay.)
