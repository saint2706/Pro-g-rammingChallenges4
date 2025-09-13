"""
Connect 4 with AI (Minimax + Alpha-Beta Pruning)
------------------------------------------------
Educational, beginner-friendly implementation of Connect 4 with a simple GUI and an AI opponent.
Features:
- Playable with mouse (Pygame)
- AI uses minimax with alpha-beta pruning
- Modular, well-commented code for learning
"""

import numpy as np
import random
import pygame
import sys
import math
from typing import Tuple, List, Optional

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
    Board is a 2D numpy array: 0=empty, 1=player, 2=AI.
    """

    def __init__(self):
        self.board = np.zeros((ROW_COUNT, COLUMN_COUNT), dtype=int)

    def drop_piece(self, row: int, col: int, piece: int) -> None:
        """Place a piece in the board at (row, col)."""
        self.board[row][col] = piece

    def is_valid_location(self, col: int) -> bool:
        """Check if a move can be made in the given column."""
        return self.board[ROW_COUNT - 1][col] == 0

    def get_next_open_row(self, col: int) -> Optional[int]:
        """Return the next available row in the given column, or None if full."""
        for r in range(ROW_COUNT):
            if self.board[r][col] == 0:
                return r
        return None

    def winning_move(self, piece: int) -> bool:
        """Check if the given piece has a winning position."""
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
        self.ai_piece = ai_piece
        self.player_piece = player_piece
        self.difficulty = difficulty

    def evaluate_window(self, window: List[int], piece: int) -> int:
        """Score a window of 4 cells for the given piece."""
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
        """Heuristic score for the board for the given piece."""
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
        Returns (best_col, score).
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
                temp_board = Board()
                temp_board.board = board_obj.board.copy()
                row = temp_board.get_next_open_row(col)
                if row is not None:
                    temp_board.drop_piece(row, col, self.ai_piece)
                new_score = self.minimax(temp_board, depth - 1, alpha, beta, False)[1]
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
                temp_board = Board()
                temp_board.board = board_obj.board.copy()
                row = temp_board.get_next_open_row(col)
                if row is not None:
                    temp_board.drop_piece(row, col, self.player_piece)
                new_score = self.minimax(temp_board, depth - 1, alpha, beta, True)[1]
                if new_score < value:
                    value = new_score
                    best_col = col
                beta = min(beta, value)
                if alpha >= beta:
                    break
            return best_col, value

    def get_best_move(self, board: Board) -> int:
        """Return the best column for the AI to play. Fallback to first valid col if None."""
        col = self.minimax(board, self.difficulty, -math.inf, math.inf, True)[0]
        if col is None:
            valid = board.get_valid_locations()
            return valid[0] if valid else 0
        return col


class Connect4Game:
    """
    Manages the main game loop, graphics, and player interactions.
    Handles drawing, input, and switching turns.
    """

    def __init__(self, screen_size: int = 100):
        pygame.init()
        self.square_size = screen_size
        self.width = COLUMN_COUNT * self.square_size
        self.height = (ROW_COUNT + 1) * self.square_size
        self.radius = int(self.square_size / 2 - 5)

        self.screen = pygame.display.set_mode((self.width, self.height))
        pygame.display.set_caption("Connect 4 - AI Challenge")
        self.font = pygame.font.SysFont("monospace", 75)

        self.board = Board()
        self.ai_player = AIPlayer(AI_PIECE, PLAYER_PIECE)
        self.game_over = False
        self.turn = random.randint(PLAYER_PIECE, AI_PIECE) - 1  # PLAYER=0, AI=1

    def draw_board(self) -> None:
        """Draw the Connect 4 board and pieces."""
        for c in range(COLUMN_COUNT):
            for r in range(ROW_COUNT):
                pygame.draw.rect(
                    self.screen,
                    BLUE,
                    (
                        c * self.square_size,
                        r * self.square_size + self.square_size,
                        self.square_size,
                        self.square_size,
                    ),
                )
                pygame.draw.circle(
                    self.screen,
                    BLACK,
                    (
                        int(c * self.square_size + self.square_size / 2),
                        int(
                            r * self.square_size
                            + self.square_size
                            + self.square_size / 2
                        ),
                    ),
                    self.radius,
                )

        for c in range(COLUMN_COUNT):
            for r in range(ROW_COUNT):
                if self.board.board[r][c] == PLAYER_PIECE:
                    pygame.draw.circle(
                        self.screen,
                        RED,
                        (
                            int(c * self.square_size + self.square_size / 2),
                            self.height
                            - int(r * self.square_size + self.square_size / 2),
                        ),
                        self.radius,
                    )
                elif self.board.board[r][c] == AI_PIECE:
                    pygame.draw.circle(
                        self.screen,
                        YELLOW,
                        (
                            int(c * self.square_size + self.square_size / 2),
                            self.height
                            - int(r * self.square_size + self.square_size / 2),
                        ),
                        self.radius,
                    )
        pygame.display.update()

    def handle_player_move(self, posx: int) -> None:
        """Handle a player's move based on mouse x position."""
        col = posx // self.square_size
        if self.board.is_valid_location(col):
            row = self.board.get_next_open_row(col)
            if row is not None:
                self.board.drop_piece(row, col, PLAYER_PIECE)
                if self.board.winning_move(PLAYER_PIECE):
                    self.end_game("You win!!")
                self.switch_turn()

    def handle_ai_move(self) -> None:
        """Handle the AI's move using minimax."""
        col = self.ai_player.get_best_move(self.board)
        if col is not None and self.board.is_valid_location(col):
            row = self.board.get_next_open_row(col)
            if row is not None:
                self.board.drop_piece(row, col, AI_PIECE)
                if self.board.winning_move(AI_PIECE):
                    self.end_game("AI wins!!")
                self.switch_turn()

    def switch_turn(self) -> None:
        """Switch to the other player's turn."""
        self.turn = (self.turn + 1) % 2

    def end_game(self, message: str) -> None:
        """Display the end game message and set game_over flag."""
        color = RED if "You" in message else YELLOW
        label = self.font.render(message, True, color)
        self.screen.blit(label, (40, 10))
        self.game_over = True

    def run(self) -> None:
        """Main game loop: handles events, drawing, and turn logic."""
        while not self.game_over:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    sys.exit()

                if event.type == pygame.MOUSEMOTION:
                    pygame.draw.rect(
                        self.screen, BLACK, (0, 0, self.width, self.square_size)
                    )
                    if self.turn == 0:  # Player's turn
                        pygame.draw.circle(
                            self.screen,
                            RED,
                            (event.pos[0], int(self.square_size / 2)),
                            self.radius,
                        )

                if event.type == pygame.MOUSEBUTTONDOWN and self.turn == 0:
                    self.handle_player_move(event.pos[0])

            # AI move outside event loop for responsiveness
            if self.turn == 1 and not self.game_over:
                self.handle_ai_move()

            self.draw_board()

            if self.board.is_terminal_node() and not self.game_over:
                self.end_game("It's a Tie!")

            if self.game_over:
                pygame.time.wait(3000)


if __name__ == "__main__":
    # Entry point: launch the game
    game = Connect4Game()
    game.run()
