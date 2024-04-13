import tkinter as tk
import random

class SudokuGUI:
    def __init__(self, master):
        self.master = master
        self.master.title("Sudoku Solver")

        self.board = [[tk.StringVar() for _ in range(9)] for _ in range(9)]

        self.create_widgets()

    def create_widgets(self):
        for i in range(9):
            for j in range(9):
                entry = tk.Entry(self.master, textvariable=self.board[i][j], width=3, font=('Arial', 16))
                entry.grid(row=i, column=j)
        
        solve_button = tk.Button(self.master, text="Solve", command=self.solve)
        solve_button.grid(row=9, columnspan=9)

        generate_button = tk.Button(self.master, text="Generate Puzzle", command=self.generate_puzzle)
        generate_button.grid(row=10, columnspan=9)

    def solve(self):
        sudoku_board = [[0 for _ in range(9)] for _ in range(9)]

        for i in range(9):
            for j in range(9):
                value = self.board[i][j].get()
                if value.isdigit():
                    sudoku_board[i][j] = int(value)
                else:
                    sudoku_board[i][j] = 0
        
        if self.solve_sudoku(sudoku_board):
            for i in range(9):
                for j in range(9):
                    self.board[i][j].set(str(sudoku_board[i][j]))
        else:
            print("No solution exists for the given Sudoku puzzle.")

    def solve_sudoku(self, board):
        empty_cell = self.find_empty_cell(board)
        if not empty_cell:
            return True

        row, col = empty_cell
        for num in range(1, 10):
            if self.is_valid_move(board, row, col, num):
                board[row][col] = num
                if self.solve_sudoku(board):
                    return True
                board[row][col] = 0
        
        return False

    def find_empty_cell(self, board):
        for i in range(9):
            for j in range(9):
                if board[i][j] == 0:
                    return (i, j)
        return None

    def is_valid_move(self, board, row, col, num):
        return not self.used_in_row(board, row, num) and \
               not self.used_in_col(board, col, num) and \
               not self.used_in_box(board, row - row % 3, col - col % 3, num)

    def used_in_row(self, board, row, num):
        return num in board[row]

    def used_in_col(self, board, col, num):
        return num in [board[i][col] for i in range(9)]

    def used_in_box(self, board, start_row, start_col, num):
        for i in range(3):
            for j in range(3):
                if board[i + start_row][j + start_col] == num:
                    return True
        return False

    def generate_puzzle(self):
        self.clear_board()
        sudoku_board = self.generate_valid_board()
        for i in range(9):
            for j in range(9):
                if random.random() < 0.5:  # Randomly remove some numbers to make it a puzzle
                    self.board[i][j].set(str(sudoku_board[i][j]))
                else:
                    self.board[i][j].set('')

    def generate_valid_board(self):
        base = 3
        side = base * base

        def pattern(r, c):
            return (base * (r % base) + r // base + c) % side

        def shuffle(s):
            return random.sample(s, len(s))

        r_base = range(base)
        rows = [g * base + r for g in shuffle(r_base) for r in shuffle(r_base)]
        cols = [g * base + c for g in shuffle(r_base) for c in shuffle(r_base)]
        nums = shuffle(range(1, base * base + 1))

        # Produce board using randomized baseline pattern
        board = [[nums[pattern(r, c)] for c in cols] for r in rows]

        return board

    def clear_board(self):
        for i in range(9):
            for j in range(9):
                self.board[i][j].set('')

if __name__ == "__main__":
    root = tk.Tk()
    sudoku_gui = SudokuGUI(root)
    root.mainloop()
