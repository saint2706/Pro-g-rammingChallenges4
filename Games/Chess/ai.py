"""Simple minimax AI opponent for chess."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, List, Optional

from .board import BLACK, WHITE, Board
from .move import Move


@dataclass
class SearchResult:
    move: Optional[Move]
    score: int
    nodes: int = 0


def evaluate(board: Board) -> int:
    """Material-only evaluation from white's perspective."""

    return board.material_balance()


def minimax(
    board: Board,
    depth: int,
    maximizing: bool,
    alpha: int = -10_000_000,
    beta: int = 10_000_000,
    eval_fn: Callable[[Board], int] = evaluate,
) -> SearchResult:
    """Perform a minimax search with alpha-beta pruning."""

    legal_moves = board.legal_moves()
    if depth == 0 or not legal_moves:
        base_score = eval_fn(board)
        if not legal_moves:
            if board.is_in_check(board.turn):
                # Mate is bad for side to move
                base_score = -10_000 if maximizing else 10_000
            else:
                base_score = 0
        return SearchResult(move=None, score=base_score, nodes=1)

    best_move: Optional[Move] = None
    nodes = 0
    if maximizing:
        value = -10_000_000
        for move in legal_moves:
            child = board.make_move(move)
            result = minimax(child, depth - 1, False, alpha, beta, eval_fn)
            nodes += result.nodes
            if result.score > value:
                value = result.score
                best_move = move
            alpha = max(alpha, value)
            if alpha >= beta:
                break
        return SearchResult(move=best_move, score=value, nodes=nodes)
    value = 10_000_000
    for move in legal_moves:
        child = board.make_move(move)
        result = minimax(child, depth - 1, True, alpha, beta, eval_fn)
        nodes += result.nodes
        if result.score < value:
            value = result.score
            best_move = move
        beta = min(beta, value)
        if beta <= alpha:
            break
    return SearchResult(move=best_move, score=value, nodes=nodes)


def choose_move(
    board: Board, depth: int = 2, eval_fn: Callable[[Board], int] = evaluate
) -> Move:
    """Choose a move for the side to move on ``board``."""

    maximizing = board.turn == WHITE
    result = minimax(board, depth, maximizing, eval_fn=eval_fn)
    if result.move is None:
        raise ValueError("No legal moves available")
    return result.move
