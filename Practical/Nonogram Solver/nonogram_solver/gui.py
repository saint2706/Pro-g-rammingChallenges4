"""Tkinter based Nonogram player and visualiser."""
from __future__ import annotations

import datetime as _dt
from pathlib import Path
from typing import Optional
import tkinter as tk
from tkinter import filedialog, messagebox

from .generator import generate_puzzle
from .puzzle import FILLED, EMPTY, UNKNOWN, NonogramPuzzle, puzzle_to_json, render_board_to_image
from .solver import NonogramSolver


class NonogramApp(tk.Tk):
    def __init__(self, puzzle: Optional[NonogramPuzzle] = None):
        super().__init__()
        self.title("Nonogram Solver")
        self.puzzle = puzzle or generate_puzzle(10, 10)
        self.board = self.puzzle.empty_board()
        self.cell_size = 32
        self.padding = 16
        self.status_var = tk.StringVar(value="Ready")
        self._create_widgets()
        self._bind_events()
        self._redraw()

    # UI construction -------------------------------------------------
    def _create_widgets(self) -> None:
        toolbar = tk.Frame(self)
        toolbar.pack(side=tk.TOP, fill=tk.X)

        tk.Button(toolbar, text="New Puzzle", command=self.new_puzzle).pack(side=tk.LEFT, padx=4, pady=4)
        tk.Button(toolbar, text="Hint", command=self.give_hint).pack(side=tk.LEFT, padx=4)
        tk.Button(toolbar, text="Solve", command=self.solve_current).pack(side=tk.LEFT, padx=4)
        tk.Button(toolbar, text="Export PNG", command=self.export_png).pack(side=tk.LEFT, padx=4)
        tk.Button(toolbar, text="Export JSON", command=self.export_json).pack(side=tk.LEFT, padx=4)

        self.canvas = tk.Canvas(self, background="white")
        self.canvas.pack(expand=True, fill=tk.BOTH)

        status = tk.Label(self, textvariable=self.status_var, anchor="w")
        status.pack(side=tk.BOTTOM, fill=tk.X)

    def _bind_events(self) -> None:
        self.canvas.bind("<Button-1>", self._on_left_click)
        self.canvas.bind("<Button-3>", self._on_right_click)
        self.bind("n", lambda _: self.new_puzzle())
        self.bind("N", lambda _: self.new_puzzle())
        self.bind("s", lambda _: self.solve_current())
        self.bind("S", lambda _: self.solve_current())
        self.bind("h", lambda _: self.give_hint())
        self.bind("H", lambda _: self.give_hint())
        self.bind("<Control-e>", lambda _: self.export_png())
        self.bind("<Control-E>", lambda _: self.export_png())
        self.bind("<Control-j>", lambda _: self.export_json())
        self.bind("<Control-J>", lambda _: self.export_json())

    # Puzzle management -----------------------------------------------
    def new_puzzle(self) -> None:
        self.puzzle = generate_puzzle(self.puzzle.width, self.puzzle.height)
        self.board = self.puzzle.empty_board()
        self.status_var.set("New puzzle generated")
        self._redraw()

    def solve_current(self) -> None:
        solver = NonogramSolver(self.puzzle)
        solutions = solver.solve(max_solutions=1)
        if solutions:
            self.board = solutions[0]
            self.status_var.set("Puzzle solved")
            self._redraw()
        else:
            messagebox.showinfo("Solver", "No solution found for this puzzle.")

    def give_hint(self) -> None:
        solver = NonogramSolver(self.puzzle)
        solutions = solver.solve(max_solutions=1)
        if not solutions:
            self.status_var.set("No solution available")
            return
        solution = solutions[0]
        for r in range(self.puzzle.height):
            for c in range(self.puzzle.width):
                if self.board[r][c] == UNKNOWN and solution[r][c] != UNKNOWN:
                    self.board[r][c] = solution[r][c]
                    self.status_var.set(f"Hint applied at row {r+1}, column {c+1}")
                    self._redraw()
                    return
        self.status_var.set("Board already matches the solution")

    # Events ----------------------------------------------------------
    def _on_left_click(self, event) -> None:
        cell = self._cell_from_event(event)
        if cell is None:
            return
        r, c = cell
        current = self.board[r][c]
        if current == UNKNOWN:
            self.board[r][c] = FILLED
        elif current == FILLED:
            self.board[r][c] = EMPTY
        else:
            self.board[r][c] = UNKNOWN
        self._redraw()
        self._update_status()

    def _on_right_click(self, event) -> None:
        cell = self._cell_from_event(event)
        if cell is None:
            return
        r, c = cell
        self.board[r][c] = EMPTY if self.board[r][c] != EMPTY else UNKNOWN
        self._redraw()
        self._update_status()

    # Drawing ---------------------------------------------------------
    def _redraw(self) -> None:
        self.canvas.delete("all")
        width = self.puzzle.width
        height = self.puzzle.height
        max_row = max((len(clue) or 1) for clue in self.puzzle.row_clues)
        max_col = max((len(clue) or 1) for clue in self.puzzle.column_clues)

        origin_x = self.padding + max_row * self.cell_size
        origin_y = self.padding + max_col * self.cell_size
        canvas_width = origin_x + width * self.cell_size + self.padding
        canvas_height = origin_y + height * self.cell_size + self.padding
        self.canvas.config(width=canvas_width, height=canvas_height)

        # Draw clues
        for r, clues in enumerate(self.puzzle.row_clues):
            display = clues or [0]
            x = origin_x - self.cell_size // 2
            y = origin_y + r * self.cell_size + self.cell_size // 2
            for clue in reversed(display):
                self.canvas.create_text(x, y, text=str(clue), font=("TkDefaultFont", 12))
                x -= self.cell_size

        for c, clues in enumerate(self.puzzle.column_clues):
            display = clues or [0]
            x = origin_x + c * self.cell_size + self.cell_size // 2
            y = origin_y - self.cell_size // 2
            for clue in reversed(display):
                self.canvas.create_text(x, y, text=str(clue), font=("TkDefaultFont", 12))
                y -= self.cell_size

        # Draw grid and cells
        for r in range(height):
            for c in range(width):
                x0 = origin_x + c * self.cell_size
                y0 = origin_y + r * self.cell_size
                x1 = x0 + self.cell_size
                y1 = y0 + self.cell_size
                self.canvas.create_rectangle(x0, y0, x1, y1, outline="black", width=1)
                value = self.board[r][c]
                if value == FILLED:
                    self.canvas.create_rectangle(x0 + 2, y0 + 2, x1 - 2, y1 - 2, fill="black")
                elif value == EMPTY:
                    self.canvas.create_line(x0 + 3, y0 + 3, x1 - 3, y1 - 3, fill="gray", width=2)
                    self.canvas.create_line(x0 + 3, y1 - 3, x1 - 3, y0 + 3, fill="gray", width=2)

        self._origin = (origin_x, origin_y)

    def _cell_from_event(self, event) -> Optional[tuple[int, int]]:
        if not hasattr(self, "_origin"):
            return None
        origin_x, origin_y = self._origin
        col = (event.x - origin_x) // self.cell_size
        row = (event.y - origin_y) // self.cell_size
        if 0 <= row < self.puzzle.height and 0 <= col < self.puzzle.width:
            return int(row), int(col)
        return None

    # Exports ---------------------------------------------------------
    def export_json(self) -> None:
        path = filedialog.asksaveasfilename(
            defaultextension=".json",
            filetypes=[["JSON", "*.json"]],
            initialfile=self._default_filename("nonogram", "json"),
        )
        if not path:
            return
        data = puzzle_to_json(self.puzzle, board=self.board)
        Path(path).write_text(data, encoding="utf-8")
        self.status_var.set(f"Saved JSON to {path}")

    def export_png(self) -> None:
        path = filedialog.asksaveasfilename(
            defaultextension=".png",
            filetypes=[["PNG", "*.png"]],
            initialfile=self._default_filename("nonogram", "png"),
        )
        if not path:
            return
        image = render_board_to_image(self.puzzle, board=self.board)
        image.save(path)
        self.status_var.set(f"Saved image to {path}")

    # Helpers ---------------------------------------------------------
    def _default_filename(self, prefix: str, suffix: str) -> str:
        stamp = _dt.datetime.now().strftime("%Y%m%d-%H%M%S")
        return f"{prefix}-{stamp}.{suffix}"

    def _update_status(self) -> None:
        if self.puzzle.solution and self.board == self.puzzle.solution:
            self.status_var.set("Completed! 🎉")
        else:
            self.status_var.set("Editing board")


def launch_app() -> None:
    app = NonogramApp()
    app.mainloop()


__all__ = ["NonogramApp", "launch_app"]
