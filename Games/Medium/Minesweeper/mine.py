import tkinter as tk
import random

class MinesweeperGUI:
    def __init__(self, master, rows, cols, num_mines):
        self.master = master
        self.rows = rows
        self.cols = cols
        self.num_mines = num_mines
        self.create_widgets()
        self.create_board()
        self.generate_mines()

    def create_widgets(self):
        self.buttons = []
        for r in range(self.rows):
            row_buttons = []
            for c in range(self.cols):
                button = tk.Button(self.master, width=2, height=1, command=lambda r=r, c=c: self.on_click(r, c))
                button.grid(row=r, column=c)
                row_buttons.append(button)
            self.buttons.append(row_buttons)

    def create_board(self):
        self.board = [[' ' for _ in range(self.cols)] for _ in range(self.rows)]

    def generate_mines(self):
        self.mines = set()
        while len(self.mines) < self.num_mines:
            row = random.randint(0, self.rows - 1)
            col = random.randint(0, self.cols - 1)
            if (row, col) not in self.mines:
                self.mines.add((row, col))

    def count_adjacent_mines(self, row, col):
        count = 0
        for dr in [-1, 0, 1]:
            for dc in [-1, 0, 1]:
                if (dr, dc) != (0, 0):
                    r, c = row + dr, col + dc
                    if 0 <= r < self.rows and 0 <= c < self.cols and (r, c) in self.mines:
                        count += 1
        return count

    def reveal(self, row, col):
        if (row, col) in self.mines:
            return False
        else:
            count = self.count_adjacent_mines(row, col)
            self.board[row][col] = str(count)
            return True

    def on_click(self, row, col):
        if (row, col) in self.mines:
            self.buttons[row][col].config(text='*')
            self.game_over_popup()
        else:
            count = self.count_adjacent_mines(row, col)
            bg_color = self.get_button_color(count)
            self.buttons[row][col].config(text=str(count), state='disabled', bg=bg_color)

    def get_button_color(self, count):
        if count == 0:
            return 'green'
        elif count <= 9:
            intensity = int(count / 9 * 255)
            return f'#{255-intensity:02x}ff{255-intensity:02x}'
        else:
            return 'red'

    def game_over_popup(self):
        popup = tk.Toplevel(self.master)
        popup.title("Game Over")
        label = tk.Label(popup, text="Game Over! Play Again?")
        label.pack()
        play_again_button = tk.Button(popup, text="Play Again", command=lambda: [popup.destroy(), self.play_again()])
        play_again_button.pack(side="left")
        quit_button = tk.Button(popup, text="Quit", command=self.master.quit)
        quit_button.pack(side="right")

    def play_again(self):
        for row_buttons in self.buttons:
            for button in row_buttons:
                button.config(text='', state='active', bg='SystemButtonFace')
        self.create_board()
        self.generate_mines()

# Example usage:
if __name__ == "__main__":
    rows = 8
    cols = 8
    num_mines = 10
    root = tk.Tk()
    root.title("Minesweeper")
    game = MinesweeperGUI(root, rows, cols, num_mines)
    root.mainloop()
