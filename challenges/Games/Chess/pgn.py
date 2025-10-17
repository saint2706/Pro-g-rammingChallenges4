"""PGN import and export helpers."""

from __future__ import annotations

import re
from typing import List, Sequence

from .board import Board
from .move import Move

MOVE_TEXT_RE = re.compile(r"(\d+\.+)|([\w+#=O-]+)")


DEFAULT_TAGS = {
    "Event": "Casual Game",
    "Site": "Local",
    "Date": "????.??.??",
    "Round": "-",
    "White": "Human",
    "Black": "Human",
    "Result": "*",
}


def export_pgn(board: Board, moves: Sequence[Move], result: str = "*") -> str:
    """Return a PGN string representing the game."""

    tags = dict(DEFAULT_TAGS)
    tags["Result"] = result
    header = [f'[{key} "{value}"]' for key, value in tags.items()]

    move_text_parts: List[str] = []
    state = board
    for index, move in enumerate(moves):
        if index % 2 == 0:
            move_text_parts.append(f"{index // 2 + 1}.")
        move_text_parts.append(state.san(move))
        state = state.make_move(move)
    move_text = " ".join(move_text_parts)
    if result != "*":
        move_text = f"{move_text} {result}".strip()

    return "\n".join(header + ["", move_text])


def import_pgn(text: str, board: Board | None = None) -> List[Move]:
    """Parse a PGN string and return the move list."""

    board = board or Board.starting_position()
    moves: List[Move] = []
    for line in text.strip().splitlines():
        line = line.strip()
        if not line or line.startswith("["):
            continue
        for token in MOVE_TEXT_RE.findall(line):
            word = next(filter(None, token))
            if word.endswith("."):
                continue
            if word in {"1-0", "0-1", "1/2-1/2", "*"}:
                break
            move = board.parse_san(word)
            moves.append(move)
            board = board.make_move(move)
    return moves
