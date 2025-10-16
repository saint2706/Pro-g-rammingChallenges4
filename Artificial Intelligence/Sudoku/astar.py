# Modern, clean A* Sudoku solver with optional backtracking comparison
import argparse
import numpy as np
import heapq
from typing import List, Optional, Tuple


class Sudoku:
    def __init__(self, board: List[List[int]]):
        if not all(len(row) == 9 for row in board) or len(board) != 9:
            raise ValueError("Input board must be a 9x9 grid.")
        self.board = np.array(board)
        self.original_board = self.board.copy()

    def astar_solve(self) -> bool:
        """
        Solve the Sudoku puzzle using the A* search algorithm.
        Returns True if solved, False otherwise. Modifies the board in-place.
        """

        def heuristic(board: np.ndarray) -> tuple[int, int]:
            empties = np.argwhere(board == 0)
            if empties.size == 0:
                return (0, 0)
            candidate_score = sum(
                self._candidate_count(board, r, c) for r, c in map(tuple, empties)
            )
            return (int(empties.shape[0]), int(candidate_score))

        g = 0
        h_empty, h_candidates = heuristic(self.board)
        start = (g + h_empty, h_empty, h_candidates, g, 0, self.board.copy())
        heap: list[tuple[int, int, int, int, int, np.ndarray]] = [start]
        visited: set[bytes] = set()
        counter = 1

        while heap:
            _, _, _, g, _, board = heapq.heappop(heap)
            board_bytes = board.tobytes()
            if board_bytes in visited:
                continue
            visited.add(board_bytes)
            empty = np.argwhere(board == 0)
            if empty.size == 0:
                self.board = board
                return True
            candidates_info = [
                (self._candidate_count(board, r, c), r, c)
                for r, c in map(tuple, empty)
            ]
            candidates_info.sort()
            count, r, c = candidates_info[0]
            if count == 0:
                continue

            for num in self._candidates(board, r, c):
                new_board = board.copy()
                new_board[r, c] = num
                h_empty, h_candidates = heuristic(new_board)
                heapq.heappush(
                    heap,
                    (
                        (g + 1) + h_empty,
                        h_empty,
                        h_candidates,
                        g + 1,
                        counter,
                        new_board,
                    ),
                )
                counter += 1
        return False

    def solve(self) -> bool:
        """
        Solves the Sudoku puzzle using recursive backtracking.
        This method modifies the board in-place.
        Returns:
            True if a solution is found, False otherwise.
        """
        empty_pos = self.find_empty_cell()
        if not empty_pos:
            return True  # No empty cells left, puzzle is solved

        row, col = empty_pos

        for num in range(1, 10):
            if self.is_valid_move(num, (row, col)):
                self.board[row, col] = num

                # Recursively try to solve the rest of the board
                if self.solve():
                    return True

                # If the recursive call fails, backtrack and try the next number
                self.board[row, col] = 0

        return False

    @staticmethod
    def _candidates(board: np.ndarray, row: int, col: int) -> List[int]:
        if board[row, col] != 0:
            return []
        row_vals = board[row, :]
        col_vals = board[:, col]
        box_r = (row // 3) * 3
        box_c = (col // 3) * 3
        box_vals = board[box_r : box_r + 3, box_c : box_c + 3].ravel()
        used = set(np.concatenate((row_vals, col_vals, box_vals)))
        used.discard(0)
        return [num for num in range(1, 10) if num not in used]

    @classmethod
    def _candidate_count(cls, board: np.ndarray, row: int, col: int) -> int:
        return len(cls._candidates(board, row, col))

    def is_valid_move(self, num: int, pos: Tuple[int, int]) -> bool:
        return num in self._candidates(self.board, *pos)

    def find_empty_cell(self) -> Optional[Tuple[int, int]]:
        """
        Finds the first empty cell (with value 0) in the Sudoku board.
        Returns:
            A tuple (row, col) if an empty cell is found, or None if the board is full.
        """
        empty = np.argwhere(self.board == 0)
        if empty.size == 0:
            return None
        return tuple(empty[0])

    def display(self):
        for i in range(9):
            if i % 3 == 0 and i != 0:
                print("- - - - - - - - - - -")
            for j in range(9):
                if j % 3 == 0 and j != 0:
                    print("| ", end="")
                print(self.board[i, j], end=" ")
            print()


def main():
    parser = argparse.ArgumentParser(
        description="Sudoku Solver using A* search (main) with optional backtracking comparison. "
        "Input file should contain 9 lines of 9 digits (0 for empty)."
    )
    parser.add_argument(
        "--file",
        type=str,
        help="Path to input file containing Sudoku puzzle",
        required=True,
    )
    parser.add_argument(
        "--compare", action="store_true", help="Compare A* with recursive backtracking"
    )
    args = parser.parse_args()

    with open(args.file, "r") as f:
        lines = [line.strip() for line in f if line.strip()]
    board = [[int(ch) if ch.isdigit() else 0 for ch in line] for line in lines]
    sudoku = Sudoku(board)
    print("Input Puzzle:")
    sudoku.display()

    print("\nSolving with A* search...")
    solved = sudoku.astar_solve()
    if solved:
        print("Solved Puzzle (A*):")
        sudoku.display()
    else:
        print("A* could not solve the puzzle.")

    if args.compare:
        print("\nSolving with recursive backtracking...")
        sudoku_bt = Sudoku(board)
        solved_bt = sudoku_bt.solve()
        if solved_bt:
            print("Solved Puzzle (Backtracking):")
            sudoku_bt.display()
        else:
            print("Backtracking could not solve the puzzle.")
