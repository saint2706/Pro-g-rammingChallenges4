# Connect 4 with AI

This project is an implementation of the classic game Connect 4, featuring a playable GUI and an AI opponent powered by the minimax algorithm with alpha-beta pruning.

## Features

- **Playable GUI**: A simple and intuitive graphical user interface built with Pygame.
- **AI Opponent**: A challenging AI that uses the minimax algorithm with alpha-beta pruning to make decisions.
- **Configurable Difficulty**: The AI's difficulty can be adjusted by changing the search depth of the minimax algorithm.

## How to Play

1. **Run the game**:
   ```bash
   python -m challenges.Artificial Intelligence.Connect4.ui
   ```
2. **Make a move**: Click on the column where you want to drop your piece.
3. **Winning**: The first player to get four of their pieces in a row (horizontally, vertically, or diagonally) wins the game.

## Code Structure

- **`c4.py`**: This module contains the core game logic.
  - **`Board`**: A class that represents the game board and handles game rules, such as dropping pieces, checking for valid moves, and detecting winning conditions.
  - **`AIPlayer`**: A class that encapsulates the AI's logic. It uses the minimax algorithm with alpha-beta pruning to find the best move.
- **`ui.py`**: This module contains the user interface logic, built with Pygame.
  - **`Connect4Game`**: A class that manages the game loop, rendering, and user input.

## Minimax with Alpha-Beta Pruning

The AI's decision-making is based on the minimax algorithm, a recursive algorithm used in decision-making and game theory. It aims to minimize the possible loss for a worst-case (maximum loss) scenario.

Alpha-beta pruning is an optimization technique for the minimax algorithm that reduces the number of nodes evaluated in the search tree. It stops evaluating a move when it finds a move that is better than the one it is currently evaluating.
