"""
Connect 4 with AI (Minimax + Alpha-Beta Pruning)
------------------------------------------------
Educational, beginner-friendly implementation of Connect 4 with a simple GUI and an AI opponent.

This module contains the core game logic for Connect 4, including the board representation,
game rules, and the AI player, which uses the minimax algorithm with alpha-beta pruning.

Features:
- Playable with mouse (Pygame)
- AI uses minimax with alpha-beta pruning
- Modular, well-commented code for learning
"""

import math
import random
import sys
from typing import List, Optional, Tuple

import numpy as np

sys.modules.setdefault("c4", sys.modules[__name__])

# --- Constants ---
BLUE = (20, 50, 200)
BLACK = (0, 0, 0)
RED = (210, 40, 40)
YELLOW = (255, 200, 0)

ROW_COUNT = 6
COLUMN_COUNT = 7
WINDOW_LENGTH = 4

EMPTY = 0
PLAYER_PIECE = 1
AI_PIECE = 2


class Board:
    """
    Manages the Connect 4 board state and logic.

    The board is represented as a 2D NumPy array where:
    - 0 represents an empty slot
    - 1 represents a player's piece
    - 2 represents an AI's piece
    """

    def __init__(self):
        """Initializes the board."""
        self.board = np.zeros((ROW_COUNT, COLUMN_COUNT), dtype=int)

    def drop_piece(self, row: int, col: int, piece: int) -> None:
        """Place a piece in the board at (row, col).

        Args:
            row: The row to place the piece in.
            col: The column to place the piece in.
            piece: The piece to be placed (PLAYER_PIECE or AI_PIECE).
        """
        self.board[row][col] = piece

    def apply_move(self, col: int, piece: int) -> Optional[int]:
        """Apply a move in ``col`` for ``piece`` returning the played row or ``None``.

        Args:
            col: The column to apply the move in.
            piece: The piece to be placed.

        Returns:
            The row the piece was placed in, or None if the column is full.
        """

        row = self.get_next_open_row(col)
        if row is None:
            return None
        self.board[row][col] = piece
        return row

    def is_valid_location(self, col: int) -> bool:
        """Check if a move can be made in the given column.

        Args:
            col: The column to check.

        Returns:
            True if the column is not full, False otherwise.
        """
        return self.board[ROW_COUNT - 1][col] == 0

    def get_next_open_row(self, col: int) -> Optional[int]:
        """Return the next available row in the given column, or None if full.

        Args:
            col: The column to check.

        Returns:
            The index of the next open row, or None if the column is full.
        """
        for r in range(ROW_COUNT):
            if self.board[r][col] == 0:
                return r
        return None

    def undo_move(self, col: int) -> None:
        """Undo the most recent move in ``col`` if present.

        Args:
            col: The column to undo the move in.
        """

        for r in range(ROW_COUNT - 1, -1, -1):
            if self.board[r][col] != 0:
                self.board[r][col] = 0
                break

    def winning_move(self, piece: int) -> bool:
        """Check if the given piece has a winning position.

        Args:
            piece: The piece to check for a win.

        Returns:
            True if the piece has a winning move, False otherwise.
        """
        # Horizontal
        for c in range(COLUMN_COUNT - 3):
            for r in range(ROW_COUNT):
                if all(self.board[r][c + i] == piece for i in range(WINDOW_LENGTH)):
                    return True
        # Vertical
        for c in range(COLUMN_COUNT):
            for r in range(ROW_COUNT - 3):
                if all(self.board[r + i][c] == piece for i in range(WINDOW_LENGTH)):
                    return True
        # Positive diagonal
        for c in range(COLUMN_COUNT - 3):
            for r in range(ROW_COUNT - 3):
                if all(self.board[r + i][c + i] == piece for i in range(WINDOW_LENGTH)):
                    return True
        # Negative diagonal
        for c in range(COLUMN_COUNT - 3):
            for r in range(3, ROW_COUNT):
                if all(self.board[r - i][c + i] == piece for i in range(WINDOW_LENGTH)):
                    return True
        return False

    def get_valid_locations(self) -> List[int]:
        """Return a list of columns where a move is possible."""
        return [col for col in range(COLUMN_COUNT) if self.is_valid_location(col)]

    def is_terminal_node(self) -> bool:
        """Return True if the game is over (win or tie)."""
        return (
            self.winning_move(PLAYER_PIECE)
            or self.winning_move(AI_PIECE)
            or len(self.get_valid_locations()) == 0
        )


class AIPlayer:
    """
    Encapsulates the AI logic using Minimax with Alpha-Beta Pruning.
    Difficulty controls search depth (higher = smarter, slower).
    """

    def __init__(self, ai_piece: int, player_piece: int, difficulty: int = 5):
        """Initializes the AI player.

        Args:
            ai_piece: The piece representing the AI.
            player_piece: The piece representing the human player.
            difficulty: The search depth for the minimax algorithm.
        """
        self.ai_piece = ai_piece
        self.player_piece = player_piece
        self.difficulty = difficulty

    def evaluate_window(self, window: List[int], piece: int) -> int:
        """Score a window of 4 cells for the given piece.

        Args:
            window: A list of 4 cells.
            piece: The piece to evaluate the window for.

        Returns:
            The score for the window.
        """
        score = 0
        opp_piece = self.player_piece if piece == self.ai_piece else self.ai_piece

        if window.count(piece) == 4:
            score += 100
        elif window.count(piece) == 3 and window.count(EMPTY) == 1:
            score += 5
        elif window.count(piece) == 2 and window.count(EMPTY) == 2:
            score += 2

        if window.count(opp_piece) == 3 and window.count(EMPTY) == 1:
            score -= 4

        return score

    def score_position(self, board: np.ndarray, piece: int) -> int:
        """Heuristic score for the board for the given piece.

        Args:
            board: The game board.
            piece: The piece to score the board for.

        Returns:
            The heuristic score for the board.
        """
        score = 0
        # Center column preference
        center_array = list(board[:, COLUMN_COUNT // 2])
        score += center_array.count(piece) * 3

        # Horizontal score
        for r in range(ROW_COUNT):
            row_array = list(board[r, :])
            for c in range(COLUMN_COUNT - 3):
                window = row_array[c : c + WINDOW_LENGTH]
                score += self.evaluate_window(window, piece)

        # Vertical score
        for c in range(COLUMN_COUNT):
            col_array = list(board[:, c])
            for r in range(ROW_COUNT - 3):
                window = col_array[r : r + WINDOW_LENGTH]
                score += self.evaluate_window(window, piece)

        # Diagonal scores
        for r in range(ROW_COUNT - 3):
            for c in range(COLUMN_COUNT - 3):
                window = [board[r + i][c + i] for i in range(WINDOW_LENGTH)]
                score += self.evaluate_window(window, piece)
                window = [board[r + 3 - i][c + i] for i in range(WINDOW_LENGTH)]
                score += self.evaluate_window(window, piece)

        return score

    def minimax(
        self,
        board_obj: Board,
        depth: int,
        alpha: float,
        beta: float,
        maximizing_player: bool,
    ) -> Tuple[Optional[int], float]:
        """
        Minimax with alpha-beta pruning.

        Args:
            board_obj: The game board object.
            depth: The current depth of the search tree.
            alpha: The alpha value for alpha-beta pruning.
            beta: The beta value for alpha-beta pruning.
            maximizing_player: True if the current player is the maximizing player,
                False otherwise.

        Returns:
            A tuple containing the best column to move to and the score of that move.
        """
        if depth == 0 or board_obj.is_terminal_node():
            if board_obj.is_terminal_node():
                if board_obj.winning_move(self.ai_piece):
                    return (None, math.inf)
                elif board_obj.winning_move(self.player_piece):
                    return (None, -math.inf)
                else:
                    return (None, 0)  # Tie
            else:  # Depth is zero
                return (None, self.score_position(board_obj.board, self.ai_piece))

        valid_locations = board_obj.get_valid_locations()

        # Prefer random among best moves for variety
        best_col = random.choice(valid_locations)

        if maximizing_player:
            value = -math.inf
            for col in valid_locations:
                row = board_obj.apply_move(col, self.ai_piece)
                if row is None:
                    continue
                new_score = self.minimax(board_obj, depth - 1, alpha, beta, False)[1]
                board_obj.undo_move(col)
                if new_score > value:
                    value = new_score
                    best_col = col
                alpha = max(alpha, value)
                if alpha >= beta:
                    break
            return best_col, value
        else:
            value = math.inf
            for col in valid_locations:
                row = board_obj.apply_move(col, self.player_piece)
                if row is None:
                    continue
                new_score = self.minimax(board_obj, depth - 1, alpha, beta, True)[1]
                board_obj.undo_move(col)
                if new_score < value:
                    value = new_score
                    best_col = col
                beta = min(beta, value)
                if alpha >= beta:
                    break
            return best_col, value

    def get_best_move(self, board: Board) -> int:
        """Return the best column for the AI to play.

        Args:
            board: The game board.

        Returns:
            The best column to move to.
        """
        col = self.minimax(board, self.difficulty, -math.inf, math.inf, True)[0]
        if col is None:
            valid = board.get_valid_locations()
            return valid[0] if valid else 0
        return col


if __name__ == "__main__":
    from ui import main as run_game

    run_game()
