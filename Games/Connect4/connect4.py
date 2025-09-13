import numpy as np
import pygame
import sys
import math
from typing import Tuple, List, Optional

# --- Constants ---
# Colors
BLUE = (20, 50, 200)
BLACK = (0, 0, 0)
RED = (210, 40, 40)
YELLOW = (255, 200, 0)

# Board dimensions
ROW_COUNT = 6
COLUMN_COUNT = 7
WINDOW_LENGTH = 4

# Player pieces
PLAYER_1_PIECE = 1

"""
Connect 4 Game (Two Player, Pygame)
Modern, well-documented, and beginner-friendly implementation.
"""

import numpy as np
import pygame
import sys
from typing import Tuple, List, Optional

# --- Constants ---
BLUE = (20, 50, 200)  # Board color
BLACK = (0, 0, 0)  # Background and empty slot color
RED = (210, 40, 40)  # Player 1 color
YELLOW = (255, 200, 0)  # Player 2 color

ROW_COUNT = 6
COLUMN_COUNT = 7
WINDOW_LENGTH = 4
PLAYER_1_PIECE = 1
PLAYER_2_PIECE = 2


class Board:
    """
    Manages the Connect 4 board state and logic.
    Board is a 6x7 numpy array. 0 = empty, 1 = Player 1, 2 = Player 2.
    """

    def __init__(self) -> None:
        self.board: np.ndarray = np.zeros((ROW_COUNT, COLUMN_COUNT), dtype=int)

    def drop_piece(self, row: int, col: int, piece: int) -> None:
        """Place a piece in the board at the given row and column."""
        self.board[row][col] = piece

    def is_valid_location(self, col: int) -> bool:
        """Return True if the top cell in the column is empty (valid move)."""
        return self.board[ROW_COUNT - 1][col] == 0

    def get_next_open_row(self, col: int) -> Optional[int]:
        """Return the lowest empty row in the given column, or None if full."""
        for r in range(ROW_COUNT):
            if self.board[r][col] == 0:
                return r
        return None

    def winning_move(self, piece: int) -> bool:
        """Check if the given piece has a winning position on the board."""
        # Horizontal check
        for c in range(COLUMN_COUNT - 3):
            for r in range(ROW_COUNT):
                if all(self.board[r][c + i] == piece for i in range(WINDOW_LENGTH)):
                    return True
        # Vertical check
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

    def is_full(self) -> bool:
        """Return True if the board is full (no empty slots in top row)."""
        return 0 not in self.board[ROW_COUNT - 1]


class Connect4Game:
    """
    Manages the main game loop, graphics, and player interactions for a two-player game.
    Uses Pygame for rendering and input.
    """

    def __init__(self, square_size: int = 100) -> None:
        pygame.init()
        self.square_size = square_size
        self.width = COLUMN_COUNT * self.square_size
        self.height = (ROW_COUNT + 1) * self.square_size
        self.radius = int(self.square_size / 2 - 5)

        self.screen = pygame.display.set_mode((self.width, self.height))
        pygame.display.set_caption("Connect 4 - Two Player")
        self.font = pygame.font.SysFont("monospace", 75)

        self.board = Board()
        self.game_over = False
        self.turn = 0  # 0 for Player 1, 1 for Player 2

    def draw_board(self) -> None:
        """Draw the Connect 4 board and all pieces."""
        self.screen.fill(BLACK)
        # Draw the board grid and empty slots
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
        # Draw the pieces
        for c in range(COLUMN_COUNT):
            for r in range(ROW_COUNT):
                if self.board.board[r][c] == PLAYER_1_PIECE:
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
                elif self.board.board[r][c] == PLAYER_2_PIECE:
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
        # Draw the current player's piece at the top
        if not self.game_over:
            color = RED if self.turn == 0 else YELLOW
            posx = pygame.mouse.get_pos()[0]
            pygame.draw.circle(
                self.screen, color, (posx, int(self.square_size / 2)), self.radius
            )
        pygame.display.update()

    def handle_mouse_click(self, posx: int) -> None:
        """Handle a mouse click event, placing a piece if valid and checking for win/tie."""
        if self.game_over:
            return
        col = posx // self.square_size
        if self.board.is_valid_location(col):
            row = self.board.get_next_open_row(col)
            if row is not None:
                piece = PLAYER_1_PIECE if self.turn == 0 else PLAYER_2_PIECE
                self.board.drop_piece(row, col, piece)
                if self.board.winning_move(piece):
                    player_num = 1 if self.turn == 0 else 2
                    color = RED if player_num == 1 else YELLOW
                    self.end_game(f"Player {player_num} wins!!", color)
                elif self.board.is_full():
                    self.end_game("It's a Tie!", BLUE)
                self.turn = (self.turn + 1) % 2

    def end_game(self, message: str, color: Tuple[int, int, int]) -> None:
        """Display the end game message and set game_over flag."""
        label = self.font.render(message, True, color)
        self.screen.blit(label, (40, 10))
        self.game_over = True

    def run(self) -> None:
        """Main game loop: handle events, update display, and manage game state."""
        while True:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    sys.exit()
                if event.type == pygame.MOUSEBUTTONDOWN:
                    self.handle_mouse_click(event.pos[0])
            self.draw_board()
            if self.game_over:
                pygame.time.wait(3000)
                sys.exit()  # For simplicity, exit after game over


if __name__ == "__main__":
    game = Connect4Game()
    game.run()
