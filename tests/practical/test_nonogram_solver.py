import json
import random
import sys
from pathlib import Path

import pytest

PROJECT_ROOT = Path(__file__).resolve().parents[2]
MODULE_ROOT = PROJECT_ROOT / "Practical" / "Nonogram Solver"
if str(MODULE_ROOT) not in sys.path:
    sys.path.insert(0, str(MODULE_ROOT))

from nonogram_solver import (  # noqa: E402
    EMPTY,
    FILLED,
    NonogramPuzzle,
    NonogramSolver,
    generate_puzzle,
    puzzle_to_json,
    puzzle_from_json,
    render_board_to_image,
)


ROWS = [[3], [1, 2], [2], [1], [1, 1]]
COLS = [[1, 2], [1], [3], [2], [1, 1]]
SOLUTION = [
    [EMPTY, EMPTY, FILLED, FILLED, FILLED],
    [FILLED, EMPTY, FILLED, FILLED, EMPTY],
    [EMPTY, FILLED, FILLED, EMPTY, EMPTY],
    [FILLED, EMPTY, EMPTY, EMPTY, EMPTY],
    [FILLED, EMPTY, EMPTY, EMPTY, FILLED],
]


def test_solver_finds_known_solution():
    puzzle = NonogramPuzzle(ROWS, COLS)
    solver = NonogramSolver(puzzle)
    solved = solver.solve(max_solutions=1)
    assert solved == [SOLUTION]


def test_puzzle_json_roundtrip():
    puzzle = NonogramPuzzle(ROWS, COLS, name="Sample", solution=SOLUTION, notes="demo")
    dumped = puzzle_to_json(puzzle, board=SOLUTION)
    payload = json.loads(dumped)
    assert payload["name"] == "Sample"
    restored = puzzle_from_json(dumped)
    assert restored.row_clues == puzzle.row_clues
    assert restored.column_clues == puzzle.column_clues


def test_generator_produces_unique_solvable_puzzle():
    rng = random.Random(42)
    puzzle = generate_puzzle(5, 5, density=0.5, rng=rng, max_attempts=50)
    solver = NonogramSolver(puzzle)
    solved = solver.solve(max_solutions=2)
    assert solved
    assert len(solved) == 1
    assert solved[0] == puzzle.solution


def test_render_board_dimensions():
    puzzle = NonogramPuzzle(ROWS, COLS, solution=SOLUTION)
    image = render_board_to_image(puzzle, board=SOLUTION, cell_size=20)
    width, height = image.size
    assert width > 0 and height > 0
    # Ensure the rendered grid matches expected geometry (5 cells + clues)
    assert width >= 20 * (5 + max(len(clue) or 1 for clue in ROWS))
    assert height >= 20 * (5 + max(len(clue) or 1 for clue in COLS))


def test_iter_solutions_stops_at_limit():
    puzzle = NonogramPuzzle(ROWS, COLS)
    solver = NonogramSolver(puzzle)
    iterator = solver.iter_solutions(max_solutions=1)
    first = next(iterator)
    assert first == SOLUTION
    with pytest.raises(StopIteration):
        next(iterator)
