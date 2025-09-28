import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from Games.Chess.board import Board


def test_starting_position_has_twenty_moves():
    board = Board.starting_position()
    moves = board.legal_moves()
    assert len(moves) == 20


def test_simple_opening_sequence():
    board = Board.starting_position()
    move = next(m for m in board.legal_moves() if m.to_uci() == "e2e4")
    board = board.make_move(move)
    assert board.turn == "b"
    assert board.piece_at(Board.parse_square("e4")) == "P"

    reply = next(m for m in board.legal_moves() if m.to_uci() == "e7e5")
    board = board.make_move(reply)

    knight_move = next(m for m in board.legal_moves() if m.to_uci() == "g1f3")
    board = board.make_move(knight_move)
    assert board.piece_at(Board.parse_square("f3")) == "N"


def test_castling_detection():
    board = Board.from_fen("r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")
    legal = {m.to_uci(): m for m in board.legal_moves()}
    assert "e1g1" in legal
    assert legal["e1g1"].is_castling
    assert "e1c1" in legal


def test_en_passant_capture():
    board = Board.from_fen("4k3/8/8/3pP3/8/8/8/4K3 w - d6 0 1")
    move = next(m for m in board.legal_moves() if m.to_uci() == "e5d6")
    assert move.is_en_passant
    board = board.make_move(move)
    assert board.piece_at(Board.parse_square("d6")) == "P"
    assert board.piece_at(Board.parse_square("d5")) is None


def test_check_detection():
    board = Board.from_fen("r3k2r/8/8/4Q3/8/8/8/R3K2R b KQkq - 0 1")
    assert board.is_in_check("b")
    legal = [m.to_uci() for m in board.legal_moves()]
    assert "e8d7" in legal
