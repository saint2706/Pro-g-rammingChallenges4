"""
Sudoku Game (Tkinter + Numpy Implementation)
-------------------------------------------
Modern, well-documented, and beginner-friendly implementation of a Sudoku puzzle generator and solver with a Tkinter GUI.
Features:
- Modular class-based design
- Clear comments and docstrings
- Optimized for readability and maintainability
"""

import tkinter as tk
from tkinter import messagebox
import random
import numpy as np
from typing import Optional, Tuple, List


class SudokuLogic:
    """
    Handles the internal logic of generating and solving Sudoku puzzles.
    Provides methods for board validation, solving, and puzzle generation.
    """

    def __init__(self) -> None:
        self.board: np.ndarray = np.zeros((9, 9), dtype=int)
        self.solution_count: int = 0

    def get_board(self) -> np.ndarray:
        """
        Returns the current Sudoku board as a numpy array.
        """
        return self.board

    def set_board(self, new_board: List[List[int]]) -> None:
        """
        Sets the current Sudoku board from a list of lists.
        """
        self.board = np.array(new_board, dtype=int)

    def find_empty_cell(self) -> Optional[Tuple[int, int]]:
        """
        Finds the first empty cell (0) in the board.
        Returns a tuple (row, col) or None if the board is full.
        """
        for r in range(9):
            for c in range(9):
                if self.board[r, c] == 0:
                    return (r, c)
        return None

    def is_valid_move(self, num: int, pos: Tuple[int, int]) -> bool:
        """
        Checks if placing a number in a given position is valid.
        Ensures no conflicts in row, column, or 3x3 box.
        """
        r, c = pos
        if num in self.board[r, :] or num in self.board[:, c]:
            return False
        box_r, box_c = r // 3 * 3, c // 3 * 3
        if num in self.board[box_r : box_r + 3, box_c : box_c + 3]:
            return False
        return True

    def solve(self) -> bool:
        """
        Solves the current board using recursive backtracking.
        Returns True if solved, False otherwise.
        """
        empty = self.find_empty_cell()
        if not empty:
            return True  # Solved

        r, c = empty
        for num in range(1, 10):
            if self.is_valid_move(num, (r, c)):
                self.board[r, c] = num
                if self.solve():
                    return True
                self.board[r, c] = 0  # Backtrack
        return False

    def count_solutions(self) -> None:
        """
        Counts the number of solutions for the current board.
        Stops early if more than one solution is found (optimization).
        """
        empty = self.find_empty_cell()
        if not empty:
            self.solution_count += 1
            return

        r, c = empty
        for num in range(1, 10):
            if self.is_valid_move(num, (r, c)):
                self.board[r, c] = num
                self.count_solutions()
                self.board[r, c] = 0  # Backtrack to find all solutions
                if self.solution_count > 1:
                    return

    def generate_puzzle(self, difficulty: float = 0.5) -> None:
        """
        Generates a new puzzle with a guaranteed unique solution.
        Difficulty is a float between 0 (easiest) and 1 (hardest).
        """
        # 1. Create a fully solved board
        self.board = np.zeros((9, 9), dtype=int)
        self.solve()

        # 2. Poke holes in the board until a unique solution remains
        while True:
            puzzle = self.board.copy()
            # Randomly remove numbers based on difficulty
            for r in range(9):
                for c in range(9):
                    if random.random() < difficulty:
                        puzzle[r, c] = 0

            # 3. Check if the puzzle has a unique solution
            self.board = puzzle.copy()
            self.solution_count = 0
            self.count_solutions()

            if self.solution_count == 1:
                self.board = puzzle
                return  # Found a valid puzzle


class SudokuGUI:
    """
    Handles the Tkinter GUI for the Sudoku game.
    Manages the grid, buttons, and user interaction.
    """

    def __init__(self, master: tk.Tk) -> None:
        self.master = master
        self.master.title("Sudoku")
        self.logic = SudokuLogic()

        self.cells: List[List[tk.Entry]] = []
        self.create_widgets()
        self.draw_grid_lines()

    def create_widgets(self) -> None:
        """
        Creates the main GUI components (grid, buttons, status bar).
        """
        frame = tk.Frame(self.master)
        frame.pack()

        for r in range(9):
            row_entries = []
            for c in range(9):
                entry = tk.Entry(
                    frame, width=3, font=("Arial", 18, "bold"), justify="center"
                )
                entry.grid(row=r, column=c, padx=1, pady=1)
                row_entries.append(entry)
            self.cells.append(row_entries)

        button_frame = tk.Frame(self.master)
        button_frame.pack(pady=10)

        tk.Button(button_frame, text="Solve", command=self.solve_puzzle).pack(
            side=tk.LEFT, padx=5
        )
        tk.Button(
            button_frame, text="Generate Puzzle", command=self.generate_new_puzzle
        ).pack(side=tk.LEFT, padx=5)
        tk.Button(button_frame, text="Clear", command=self.clear_board).pack(
            side=tk.LEFT, padx=5
        )

        self.status_bar = tk.Label(
            self.master, text="Welcome to Sudoku!", bd=1, relief=tk.SUNKEN, anchor=tk.W
        )
        self.status_bar.pack(side=tk.BOTTOM, fill=tk.X)

    def draw_grid_lines(self) -> None:
        """
        Draws the thicker lines for the 3x3 subgrids.
        Note: This is a workaround using frames; a canvas would be more flexible.
        """
        for i in range(10):
            width = 3 if i % 3 == 0 else 1
            self.master.children["!frame"].grid_rowconfigure(i, weight=1)
            self.master.children["!frame"].grid_columnconfigure(i, weight=1)
        for i in range(3, 10, 3):
            tk.Frame(self.master.children["!frame"], height=2, bg="black").grid(
                row=i, columnspan=9, sticky="ew"
            )
            tk.Frame(self.master.children["!frame"], width=2, bg="black").grid(
                column=i, rowspan=9, sticky="ns"
            )

    def update_gui_from_board(self) -> None:
        """
        Updates the Tkinter entry widgets with the values from the logic board.
        """
        board = self.logic.get_board()
        for r in range(9):
            for c in range(9):
                self.cells[r][c].delete(0, tk.END)
                if board[r, c] != 0:
                    self.cells[r][c].insert(0, str(board[r, c]))

    def read_board_from_gui(self) -> None:
        """
        Reads the values from the GUI and updates the logic board.
        """
        new_board = np.zeros((9, 9), dtype=int)
        for r in range(9):
            for c in range(9):
                val = self.cells[r][c].get()
                if val.isdigit() and 1 <= int(val) <= 9:
                    new_board[r, c] = int(val)
        self.logic.set_board(new_board.tolist())

    def solve_puzzle(self) -> None:
        """
        Attempts to solve the current puzzle and updates the GUI.
        """
        self.read_board_from_gui()
        self.status_bar.config(text="Solving...")
        if self.logic.solve():
            self.update_gui_from_board()
            self.status_bar.config(text="Puzzle Solved!")
        else:
            self.status_bar.config(text="No solution exists for this puzzle.")
            messagebox.showerror("Solver", "No solution found.")

    def generate_new_puzzle(self) -> None:
        """
        Generates a new puzzle and updates the GUI.
        """
        self.status_bar.config(text="Generating new puzzle...")
        self.logic.generate_puzzle()
        self.update_gui_from_board()
        self.status_bar.config(text="New puzzle generated. Good luck!")

    def clear_board(self) -> None:
        """
        Clears the board and updates the GUI.
        """
        self.logic.set_board(np.zeros((9, 9), dtype=int).tolist())
        self.update_gui_from_board()
        self.status_bar.config(text="Board cleared.")


if __name__ == "__main__":
    root = tk.Tk()
    app = SudokuGUI(root)
    root.mainloop()
