# Sudoku Solver

This project provides a Sudoku solver that can solve any 9x9 Sudoku puzzle using two different algorithms: A* search and backtracking. The solver is implemented in Python using NumPy for efficient array operations.

## Features

- **A* Search Algorithm**: A more advanced search algorithm that uses a heuristic to guide its search. The heuristic is based on the number of empty cells and the number of possible candidates for each empty cell.
- **Backtracking Algorithm**: A classic recursive algorithm for solving Sudoku puzzles.
- **Command-Line Interface**: A simple CLI for solving puzzles from a file and comparing the two algorithms.

## How to Use

To solve a Sudoku puzzle, create a text file with 9 lines of 9 digits, where 0 represents an empty cell. For example:

```
530070000
600195000
098000060
800060003
400803001
700020006
060000280
000419005
000080079
```

Then, run the script with the following command:

```bash
python -m "challenges/Artificial Intelligence/Sudoku/astar" --file path/to/your/puzzle.txt
```

### Options

- `--file`: The path to the input file containing the Sudoku puzzle (required).
- `--compare`: A flag to compare the A* algorithm with the backtracking algorithm.

## Code Structure

- **`astar.py`**: The main file, containing the entire implementation of the Sudoku solver.
  - **`Sudoku`**: A class that represents a Sudoku puzzle and contains the solving logic.
    - **`astar_solve()`**: Solves the puzzle using the A* search algorithm.
    - **`solve()`**: Solves the puzzle using the backtracking algorithm.
    - **`_candidates()`**: A helper function to find the possible candidates for a given cell.
    - **`is_valid_move()`**: Checks if a move is valid.
    - **`find_empty_cell()`**: Finds the next empty cell on the board.
    - **`display()`**: Prints the board to the console.
  - **`main()`**: The main function that parses command-line arguments, loads a puzzle, and solves it.
