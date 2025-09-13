import tkinter as tk
from tkinter import messagebox
import random
import argparse
from typing import Set, Tuple, List

class MinesweeperLogic:
    """Handles the internal game logic for Minesweeper."""
    def __init__(self, rows: int, cols: int, num_mines: int):
        self.rows = rows
        self.cols = cols
        self.num_mines = num_mines
        self.reset()

    def reset(self):
        """Resets the game state for a new game."""
        self.board = [[' ' for _ in range(self.cols)] for _ in range(self.rows)]
        self.mines = self._generate_mines()
        self.revealed: Set[Tuple[int, int]] = set()
        self.flags: Set[Tuple[int, int]] = set()
        self.game_over = False
        self.win = False

    def _generate_mines(self) -> Set[Tuple[int, int]]:
        """Generates a set of random mine locations."""
        mines = set()
        while len(mines) < self.num_mines:
            row = random.randint(0, self.rows - 1)
            col = random.randint(0, self.cols - 1)
            mines.add((row, col))
        return mines

    def count_adjacent_mines(self, row: int, col: int) -> int:
        """Counts mines in the 8 cells surrounding a given cell."""
        count = 0
        for dr in [-1, 0, 1]:
            for dc in [-1, 0, 1]:
                if dr == 0 and dc == 0:
                    continue
                r, c = row + dr, col + dc
                if 0 <= r < self.rows and 0 <= c < self.cols and (r, c) in self.mines:
                    count += 1
        return count

    def reveal(self, row: int, col: int):
        """Reveals a cell and triggers a flood fill if it's empty."""
        if (row, col) in self.revealed or (row, col) in self.flags:
            return

        self.revealed.add((row, col))

        if (row, col) in self.mines:
            self.game_over = True
            return

        count = self.count_adjacent_mines(row, col)
        self.board[row][col] = str(count)

        if count == 0:
            # Flood fill for empty cells
            for dr in [-1, 0, 1]:
                for dc in [-1, 0, 1]:
                    r, c = row + dr, col + dc
                    if 0 <= r < self.rows and 0 <= c < self.cols:
                        self.reveal(r, c)

        self.check_win_condition()

    def toggle_flag(self, row: int, col: int):
        """Toggles a flag on or off for a given cell."""
        if (row, col) not in self.revealed:
            if (row, col) in self.flags:
                self.flags.remove((row, col))
            else:
                self.flags.add((row, col))

    def check_win_condition(self):
        """Checks if all non-mine cells have been revealed."""
        if len(self.revealed) == self.rows * self.cols - self.num_mines:
            self.win = True
            self.game_over = True


class MinesweeperGUI:
    """Handles the Tkinter GUI for the Minesweeper game."""
    def __init__(self, master: tk.Tk, logic: MinesweeperLogic):
        self.master = master
        self.logic = logic

        self.number_colors = {
            '1': '#0000FF', '2': '#008200', '3': '#FF0000',
            '4': '#000084', '5': '#840000', '6': '#008284',
            '7': '#840084', '8': '#000000'
        }

        self.buttons: List[List[tk.Button]] = []
        self.create_widgets()
        self.update_view()

    def create_widgets(self):
        """Creates the grid of buttons."""
        for r in range(self.logic.rows):
            row_buttons = []
            for c in range(self.logic.cols):
                button = tk.Button(self.master, width=2, height=1, font=('Helvetica', '10', 'bold'))
                button.bind('<Button-1>', lambda e, r=r, c=c: self.on_left_click(r, c))
                button.bind('<Button-3>', lambda e, r=r, c=c: self.on_right_click(r, c))
                button.grid(row=r, column=c)
                row_buttons.append(button)
            self.buttons.append(row_buttons)

    def on_left_click(self, row: int, col: int):
        if self.logic.game_over: return
        self.logic.reveal(row, col)
        self.update_view()
        if self.logic.game_over:
            self.show_game_over_popup()

    def on_right_click(self, row: int, col: int):
        if self.logic.game_over: return
        self.logic.toggle_flag(row, col)
        self.update_view()

    def update_view(self):
        """Updates the GUI to reflect the current game state."""
        for r in range(self.logic.rows):
            for c in range(self.logic.cols):
                button = self.buttons[r][c]
                if (r, c) in self.logic.flags:
                    button.config(text='P', state='disabled', relief=tk.FLAT, bg='orange')
                elif (r, c) in self.logic.revealed:
                    if (r, c) in self.logic.mines:
                        button.config(text='*', bg='red')
                    else:
                        count_str = self.logic.board[r][c]
                        color = self.number_colors.get(count_str, 'black')
                        button.config(text=count_str if count_str != '0' else '', relief=tk.SUNKEN, fg=color, bg='#ddd')
                        button.config(state='disabled')
                else:
                    button.config(text='', state='normal', relief=tk.RAISED, bg='SystemButtonFace')

    def show_game_over_popup(self):
        """Shows a popup message for game over or win."""
        message = "You Win! Congratulations!" if self.logic.win else "Game Over! You hit a mine."
        if messagebox.askyesno("Game Over", f"{message}\nPlay Again?"):
            self.logic.reset()
            self.update_view()
        else:
            self.master.quit()

def main():
    parser = argparse.ArgumentParser(description="Minesweeper Game")
    parser.add_argument("--rows", type=int, default=10, help="Number of rows.")
    parser.add_argument("--cols", type=int, default=10, help="Number of columns.")
    parser.add_argument("--mines", type=int, default=15, help="Number of mines.")
    args = parser.parse_args()

    root = tk.Tk()
    root.title("Minesweeper")

    logic = MinesweeperLogic(args.rows, args.cols, args.mines)
    game_gui = MinesweeperGUI(root, logic)

    root.mainloop()

if __name__ == "__main__":
    main()
