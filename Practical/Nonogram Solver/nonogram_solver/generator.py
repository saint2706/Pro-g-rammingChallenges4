"""Puzzle generation helpers for Nonograms."""
from __future__ import annotations

import random
from typing import Optional

from .puzzle import Board, NonogramPuzzle, compute_clues
from .solver import NonogramSolver


def _random_solution(width: int, height: int, density: float, rng: random.Random) -> Board:
    board: Board = []
    for _ in range(height):
        row = [1 if rng.random() < density else 0 for _ in range(width)]
        board.append(row)
    if all(cell == 0 for row in board for cell in row):
        # ensure at least one filled cell so the puzzle is not trivial
        r = rng.randrange(height)
        c = rng.randrange(width)
        board[r][c] = 1
    return board


def generate_puzzle(
    width: int,
    height: int,
    density: float = 0.45,
    max_attempts: int = 200,
    rng: Optional[random.Random] = None,
) -> NonogramPuzzle:
    """Generate a solvable and unique Nonogram puzzle."""

    if width <= 0 or height <= 0:
        raise ValueError("width and height must be positive")
    if not (0 < density < 1):
        raise ValueError("density must be between 0 and 1")
    rng = rng or random.Random()

    for attempt in range(1, max_attempts + 1):
        board = _random_solution(width, height, density, rng)
        row_clues, col_clues = compute_clues(board)
        puzzle = NonogramPuzzle(row_clues, col_clues, name=f"Generated {width}x{height}", solution=board)
        solver = NonogramSolver(puzzle)
        solutions = solver.solve(max_solutions=2)
        if not solutions:
            continue
        if len(solutions) == 1:
            puzzle.solution = solutions[0]
            return puzzle
    raise RuntimeError("Failed to generate a unique puzzle within the attempt limit")
