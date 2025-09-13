# NOTE: This file was originally named 'astar.py' and contained an A* search
# implementation for solving Sudoku, which was inefficient and ill-suited for
# the problem. The algorithm has been replaced with a much more appropriate
# and efficient recursive backtracking solver. The filename is kept for
# consistency with the original repository structure.

import numpy as np
import sys
from typing import List, Tuple, Optional

class Sudoku:
    """
    A Sudoku solver that uses a recursive backtracking algorithm.
    """
    def __init__(self, board: List[List[int]]):
        # Validate board dimensions
        if not all(len(row) == 9 for row in board) or len(board) != 9:
            raise ValueError("Input board must be a 9x9 grid.")
        self.board = np.array(board)
        self.original_board = self.board.copy()

    def find_empty_cell(self) -> Optional[Tuple[int, int]]:
        """Finds the next empty cell (represented by 0) in the board."""
        for r in range(9):
            for c in range(9):
                if self.board[r, c] == 0:
                    return (r, c)
        return None

    def is_valid_move(self, num: int, pos: Tuple[int, int]) -> bool:
        """Checks if placing a number in a given position is valid."""
        row, col = pos
        # Check row
        if num in self.board[row, :]:
            return False
        # Check column
        if num in self.board[:, col]:
            return False
        # Check 3x3 box
        box_x, box_y = col // 3, row // 3
        if num in self.board[box_y*3 : box_y*3 + 3, box_x*3 : box_x*3 + 3]:
            return False
        return True

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

    def __str__(self) -> str:
        """Returns a string representation of the board."""
        s = ""
        for i in range(9):
            if i % 3 == 0 and i != 0:
                s += "- - - - - - - - - - - \n"
            for j in range(9):
                if j % 3 == 0 and j != 0:
                    s += "| "
                s += str(self.board[i, j]) + " "
            s += "\n"
        return s

def main():
    """
    Main function to initialize and solve a Sudoku puzzle.
    """
    # 0 represents empty cells
    default_puzzle = [
        [5, 3, 0, 0, 7, 0, 0, 0, 0],
        [6, 0, 0, 1, 9, 5, 0, 0, 0],
        [0, 9, 8, 0, 0, 0, 0, 6, 0],
        [8, 0, 0, 0, 6, 0, 0, 0, 3],
        [4, 0, 0, 8, 0, 3, 0, 0, 1],
        [7, 0, 0, 0, 2, 0, 0, 0, 6],
        [0, 6, 0, 0, 0, 0, 2, 8, 0],
        [0, 0, 0, 4, 1, 9, 0, 0, 5],
        [0, 0, 0, 0, 8, 0, 0, 7, 9]
    ]

    print("--- Sudoku Solver using Backtracking ---")
    solver = Sudoku(default_puzzle)

    print("Original Puzzle:")
    print(solver)

    if solver.solve():
        print("Sudoku Puzzle Solved:")
        print(solver)
    else:
        print("No solution found for the puzzle.")

if __name__ == "__main__":
    main()
