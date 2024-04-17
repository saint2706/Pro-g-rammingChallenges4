import heapq
import numpy as np

# Sample Sudoku puzzle
# 0 represents empty cells
sudoku_puzzle = [
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


def is_valid_move(board, row, col, num):
    # Check if the number is not already used in the row
    if num in board[row]:
        return False

    # Check if the number is not already used in the column
    if num in [board[i][col] for i in range(9)]:
        return False

    # Check if the number is not already used in the 3x3 subgrid
    start_row, start_col = 3 * (row // 3), 3 * (col // 3)
    for i in range(3):
        for j in range(3):
            if board[start_row + i][start_col + j] == num:
                return False

    return True


def A_star_search(board):
    priority_queue = []
    heapq.heappush(priority_queue, (0, board))

    while priority_queue:
        _, current_board = heapq.heappop(priority_queue)

        if is_board_solved(current_board):
            return current_board

        row, col = next_empty_cell(current_board)

        for num in range(1, 10):
            if is_valid_move(current_board, row, col, num):
                new_board = [row.copy() for row in current_board]
                new_board[row][col] = num

                # Calculate priority (heuristic function)
                priority = calculate_priority(new_board)
                heapq.heappush(priority_queue, (priority, new_board))

    return None


def is_board_solved(board):
    for row in board:
        if 0 in row:
            return False
    return True


def next_empty_cell(board):
    for i in range(9):
        for j in range(9):
            if board[i][j] == 0:
                return i, j
    return None, None


def calculate_priority(board):
    # A simple heuristic function: count the number of empty cells
    return sum(row.count(0) for row in board)


def print_board(board):
    for row in board:
        print(row)


solved_board = A_star_search(sudoku_puzzle)

if solved_board:
    print("Sudoku Puzzle Solved:")
    print_board(solved_board)
else:
    print("No solution found.")
