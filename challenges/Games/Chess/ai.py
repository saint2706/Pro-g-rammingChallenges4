"""Simple minimax AI opponent for chess."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Dict, List, Optional, Tuple

from .board import BLACK, WHITE, Board
from .move import Move


@dataclass
class SearchResult:
    move: Optional[Move]
    score: int
    nodes: int = 0


@dataclass
class TTEntry:
    depth: int
    score: int
    move: Optional[Move]


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
    tt: Optional[Dict[Tuple[Board, bool], TTEntry]] = None,
) -> SearchResult:
    """Perform a minimax search with alpha-beta pruning."""

    key = (board, maximizing)
    if tt is not None:
        entry = tt.get(key)
        if entry and entry.depth >= depth:
            return SearchResult(move=entry.move, score=entry.score, nodes=1)

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
    nodes = 1
    if maximizing:
        value = -10_000_000
        for move in legal_moves:
            child = board.make_move(move)
            result = minimax(child, depth - 1, False, alpha, beta, eval_fn, tt)
            nodes += result.nodes
            if result.score > value:
                value = result.score
                best_move = move
            alpha = max(alpha, value)
            if alpha >= beta:
                break
        result = SearchResult(move=best_move, score=value, nodes=nodes)
        if tt is not None:
            tt[key] = TTEntry(depth=depth, score=value, move=best_move)
        return result
    value = 10_000_000
    for move in legal_moves:
        child = board.make_move(move)
        result = minimax(child, depth - 1, True, alpha, beta, eval_fn, tt)
        nodes += result.nodes
        if result.score < value:
            value = result.score
            best_move = move
        beta = min(beta, value)
        if beta <= alpha:
            break
    result = SearchResult(move=best_move, score=value, nodes=nodes)
    if tt is not None:
        tt[key] = TTEntry(depth=depth, score=value, move=best_move)
    return result


def choose_move(
    board: Board, depth: int = 2, eval_fn: Callable[[Board], int] = evaluate
) -> Move:
    """Choose a move for the side to move on ``board``."""

    maximizing = board.turn == WHITE
    tt: Dict[Tuple[Board, bool], TTEntry] = {}
    best_move: Optional[Move] = None
    for current_depth in range(1, depth + 1):
        result = minimax(
            board,
            current_depth,
            maximizing,
            eval_fn=eval_fn,
            tt=tt,
        )
        if result.move is not None:
            best_move = result.move
    if best_move is None:
        raise ValueError("No legal moves available")
    return best_move
