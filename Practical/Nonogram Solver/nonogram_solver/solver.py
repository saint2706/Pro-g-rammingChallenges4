"""Constraint-propagation Nonogram solver."""
from __future__ import annotations

from dataclasses import dataclass
from functools import lru_cache
from typing import Iterator, List, Optional, Sequence, Tuple

from .puzzle import Board, NonogramPuzzle, FILLED, EMPTY, UNKNOWN


Line = Tuple[int, ...]
ClueTuple = Tuple[int, ...]


@lru_cache(maxsize=None)
def _generate_line_patterns(length: int, clues: ClueTuple, constraint: Line) -> Tuple[Line, ...]:
    """Generate all line permutations compatible with the constraint."""

    def fits(line: Sequence[int]) -> bool:
        return all(c == UNKNOWN or c == v for c, v in zip(constraint, line))

    def recurse(index: int, clue_index: int, line: List[int]) -> Iterator[Line]:
        if clue_index == len(clues):
            for pos in range(index, length):
                if constraint[pos] not in (UNKNOWN, EMPTY):
                    return
                line[pos] = EMPTY
            yield tuple(line)
            return

        clue = clues[clue_index]
        remaining = sum(clues[clue_index + 1 :]) + (len(clues) - clue_index - 1)
        max_start = length - remaining - clue
        for start in range(index, max_start + 1):
            # Fill empties before the block
            valid = True
            for pos in range(index, start):
                if constraint[pos] not in (UNKNOWN, EMPTY):
                    valid = False
                    break
                line[pos] = EMPTY
            if not valid:
                continue

            block_end = start + clue
            if block_end > length:
                break
            for pos in range(start, block_end):
                if constraint[pos] not in (UNKNOWN, FILLED):
                    valid = False
                    break
                line[pos] = FILLED
            if not valid:
                continue

            next_index = block_end
            if block_end < length:
                if clue_index < len(clues) - 1:
                    if constraint[block_end] not in (UNKNOWN, EMPTY):
                        valid = False
                    else:
                        line[block_end] = EMPTY
                    next_index = block_end + 1
                else:
                    if constraint[block_end] not in (UNKNOWN, EMPTY):
                        valid = False
                    else:
                        line[block_end] = EMPTY
                        next_index = block_end + 1
            if not valid:
                continue

            yield from recurse(next_index, clue_index + 1, line)

            # Reset mutated cells for next iteration
            for pos in range(start, min(block_end + 1, length)):
                line[pos] = UNKNOWN
            for pos in range(index, start):
                line[pos] = UNKNOWN

    return tuple(pattern for pattern in recurse(0, 0, [UNKNOWN] * length) if fits(pattern))


def _merge_patterns(patterns: Sequence[Line]) -> Line:
    merged = []
    for cells in zip(*patterns):
        first = cells[0]
        if all(cell == first for cell in cells):
            merged.append(first)
        else:
            merged.append(UNKNOWN)
    return tuple(merged)


def _line_constraint(line: Sequence[int]) -> Line:
    return tuple(line)


@dataclass
class SolverState:
    board: Board
    row_possibilities: List[Tuple[Line, ...]]
    col_possibilities: List[Tuple[Line, ...]]


class NonogramSolver:
    """Solve Nonogram puzzles using propagation + heuristic search."""

    def __init__(self, puzzle: NonogramPuzzle):
        self.puzzle = puzzle

    def solve(self, max_solutions: int = 1) -> List[Board]:
        return list(self.iter_solutions(max_solutions=max_solutions))

    def iter_solutions(self, max_solutions: Optional[int] = None) -> Iterator[Board]:
        initial_state = SolverState(
            board=self.puzzle.empty_board(),
            row_possibilities=[tuple() for _ in range(self.puzzle.height)],
            col_possibilities=[tuple() for _ in range(self.puzzle.width)],
        )

        yielded = 0

        def search(state: SolverState) -> Iterator[Board]:
            nonlocal yielded
            if max_solutions is not None and yielded >= max_solutions:
                return
            try:
                board, row_poss, col_poss = self._propagate(state)
            except ValueError:
                return

            if all(cell != UNKNOWN for row in board for cell in row):
                yielded += 1
                yield [list(row) for row in board]
                return

            target = self._select_line(row_poss, col_poss)
            if target is None:
                return
            axis, index, patterns = target

            for pattern in patterns:
                if max_solutions is not None and yielded >= max_solutions:
                    break
                new_board = [list(row) for row in board]
                if axis == "row":
                    new_board[index] = list(pattern)
                else:
                    for r, value in enumerate(pattern):
                        new_board[r][index] = value
                new_state = SolverState(new_board, row_poss.copy(), col_poss.copy())
                yield from search(new_state)

        yield from search(initial_state)

    def _propagate(self, state: SolverState) -> Tuple[Board, List[Tuple[Line, ...]], List[Tuple[Line, ...]]]:
        height = self.puzzle.height
        width = self.puzzle.width
        board = [list(row) for row in state.board]
        row_poss = [tuple(p) for p in state.row_possibilities]
        col_poss = [tuple(p) for p in state.col_possibilities]

        changed = True
        while changed:
            changed = False

            for r in range(height):
                constraint = _line_constraint(board[r])
                possibilities = _generate_line_patterns(width, tuple(self.puzzle.row_clues[r]), constraint)
                if not possibilities:
                    raise ValueError("No valid row configuration")
                row_poss[r] = possibilities
                merged = _merge_patterns(possibilities)
                for c, value in enumerate(merged):
                    if value != UNKNOWN and board[r][c] == UNKNOWN:
                        board[r][c] = value
                        changed = True

            for c in range(width):
                constraint = _line_constraint([board[r][c] for r in range(height)])
                possibilities = _generate_line_patterns(height, tuple(self.puzzle.column_clues[c]), constraint)
                if not possibilities:
                    raise ValueError("No valid column configuration")
                col_poss[c] = possibilities
                merged = _merge_patterns(possibilities)
                for r, value in enumerate(merged):
                    if value != UNKNOWN and board[r][c] == UNKNOWN:
                        board[r][c] = value
                        changed = True

        return board, row_poss, col_poss

    def _select_line(
        self,
        row_poss: Sequence[Tuple[Line, ...]],
        col_poss: Sequence[Tuple[Line, ...]],
    ) -> Optional[Tuple[str, int, Tuple[Line, ...]]]:
        candidate: Optional[Tuple[str, int, Tuple[Line, ...]]] = None
        best_score = float("inf")

        for idx, possibilities in enumerate(row_poss):
            if len(possibilities) <= 1:
                continue
            if len(possibilities) < best_score:
                candidate = ("row", idx, possibilities)
                best_score = len(possibilities)

        for idx, possibilities in enumerate(col_poss):
            if len(possibilities) <= 1:
                continue
            if len(possibilities) < best_score:
                candidate = ("col", idx, possibilities)
                best_score = len(possibilities)

        return candidate


def solve_puzzle(puzzle: NonogramPuzzle, max_solutions: int = 1) -> List[Board]:
    solver = NonogramSolver(puzzle)
    return solver.solve(max_solutions=max_solutions)
