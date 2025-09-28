"""Command line interface for the chess game."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Iterable, List, Optional

from . import Board
from .ai import choose_move
from .move import Move
from .pgn import export_pgn, import_pgn

PIECE_SYMBOLS = {
    "P": "♙",
    "N": "♘",
    "B": "♗",
    "R": "♖",
    "Q": "♕",
    "K": "♔",
    "p": "♟",
    "n": "♞",
    "b": "♝",
    "r": "♜",
    "q": "♛",
    "k": "♚",
}


def render_board(board: Board, use_unicode: bool = True) -> str:
    """Return an ASCII or unicode representation of the board."""

    lines: List[str] = []
    for r in range(8):
        row = [str(8 - r)]
        for c in range(8):
            piece = board.board[r][c]
            if piece is None:
                row.append("." if not use_unicode else "·")
            elif use_unicode:
                row.append(PIECE_SYMBOLS[piece])
            else:
                row.append(piece)
        lines.append(" ".join(row))
    lines.append("  a b c d e f g h")
    return "\n".join(lines)


def parse_move(board: Board, raw: str) -> Move:
    raw = raw.strip()
    if not raw:
        raise ValueError("Empty move")
    legal = board.legal_moves()
    try:
        tentative = Move.from_uci(raw)
        for move in legal:
            if move.start == tentative.start and move.end == tentative.end:
                if (move.promotion or "") == (tentative.promotion or ""):
                    return move
        raise ValueError("UCI move does not match a legal move")
    except Exception:
        return board.parse_san(raw)


def play_cli(
    initial_board: Optional[Board] = None,
    ai: Optional[str] = None,
    depth: int = 2,
    use_unicode: bool = True,
    history: Optional[List[Move]] = None,
) -> None:
    board = initial_board or Board.starting_position()
    history = list(history or [])

    print(render_board(board, use_unicode=use_unicode))
    while True:
        legal = board.legal_moves()
        if not legal:
            if board.is_in_check(board.turn):
                winner = "Black" if board.turn == "w" else "White"
                print(f"Checkmate! {winner} wins.")
            else:
                print("Stalemate.")
            break

        if board.turn == "w":
            turn_name = "White"
        else:
            turn_name = "Black"
        ai_turn = ai and board.turn in ai

        if ai_turn:
            move = choose_move(board, depth)
            print(f"AI plays {board.san(move)} ({move.to_uci()})")
        else:
            prompt = f"{turn_name} to move (enter SAN or UCI, 'moves', 'save <file>', 'load <file>', 'quit'): "
            raw = input(prompt).strip()
            if raw.lower() in {"quit", "exit"}:
                print("Game aborted.")
                break
            if raw.lower() == "moves":
                san_moves = [board.san(m) for m in legal]
                print("Available moves:", ", ".join(sorted(san_moves)))
                continue
            if raw.lower().startswith("save "):
                path = Path(raw.split(maxsplit=1)[1])
                pgn = export_pgn(Board.starting_position(), history, "*")
                path.write_text(pgn, encoding="utf-8")
                print(f"Saved PGN to {path}")
                continue
            if raw.lower().startswith("load "):
                path = Path(raw.split(maxsplit=1)[1])
                moves = import_pgn(path.read_text(encoding="utf-8"))
                board = Board.starting_position()
                history = []
                for mv in moves:
                    board = board.make_move(mv)
                    history.append(mv)
                print("Loaded PGN.")
                print(render_board(board, use_unicode=use_unicode))
                continue
            try:
                move = parse_move(board, raw)
            except ValueError as exc:  # pragma: no cover - defensive
                print(f"Invalid move: {exc}")
                continue
            if move not in legal:
                print("Move is not legal from this position.")
                continue
        board = board.make_move(move)
        history.append(move)
        print(render_board(board, use_unicode=use_unicode))


def main(argv: Optional[Iterable[str]] = None) -> None:
    parser = argparse.ArgumentParser(description="Play chess from the command line.")
    parser.add_argument(
        "--ai", choices=["white", "black", "both"], help="Enable an AI for a side."
    )
    parser.add_argument("--depth", type=int, default=2, help="Search depth for the AI.")
    parser.add_argument(
        "--pgn", type=Path, help="Load moves from a PGN file before starting."
    )
    parser.add_argument(
        "--ascii", action="store_true", help="Render the board using ASCII characters."
    )
    args = parser.parse_args(list(argv) if argv is not None else None)

    board = Board.starting_position()
    history: List[Move] = []
    if args.pgn:
        text = args.pgn.read_text(encoding="utf-8")
        moves = import_pgn(text, board)
        for mv in moves:
            board = board.make_move(mv)
            history.append(mv)
        print(f"Loaded {len(history)} moves from {args.pgn}.")

    ai_setting = None
    if args.ai:
        if args.ai == "both":
            ai_setting = "wb"
        elif args.ai == "white":
            ai_setting = "w"
        else:
            ai_setting = "b"

    play_cli(
        board,
        ai_setting,
        depth=args.depth,
        use_unicode=not args.ascii,
        history=history,
    )


if __name__ == "__main__":  # pragma: no cover
    main()
