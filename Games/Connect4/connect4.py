"""Pygame implementation of the Connect 4 board game."""

from dataclasses import dataclass
from typing import Deque, Iterable, List, Optional, Tuple

import numpy as np
import pygame
import sys
from collections import deque


@dataclass(frozen=True)
class Connect4Config:
    """Configuration container so constants stay in one place."""

    rows: int = 6
    columns: int = 7
    window_length: int = 4
    board_colour: Tuple[int, int, int] = (20, 50, 200)
    background_colour: Tuple[int, int, int] = (0, 0, 0)
    player_colours: Tuple[Tuple[int, int, int], Tuple[int, int, int]] = (
        (210, 40, 40),
        (255, 200, 0),
    )
    message_colour: Tuple[int, int, int] = (255, 255, 255)

    @property
    def player_1_piece(self) -> int:
        return 1

    @property
    def player_2_piece(self) -> int:
        return 2


class InputController:
    """Normalises pygame events into queued intents."""

    def __init__(self) -> None:
        self._clicks: Deque[int] = deque()
        self._quit_requested = False

    def pump(self, events: Iterable[pygame.event.Event]) -> None:
        for event in events:
            if event.type == pygame.QUIT:
                self._quit_requested = True
            elif event.type == pygame.MOUSEBUTTONDOWN:
                self._clicks.append(event.pos[0])

    def pop_click(self) -> Optional[int]:
        if self._clicks:
            return self._clicks.popleft()
        return None

    @property
    def quit_requested(self) -> bool:
        return self._quit_requested


class Board:
    """
    Manages the Connect 4 board state and logic.
    Board is a 6x7 numpy array. 0 = empty, 1 = Player 1, 2 = Player 2.
    """

    def __init__(self, config: Connect4Config) -> None:
        self.config = config
        self.board: np.ndarray = np.zeros(
            (config.rows, config.columns), dtype=int
        )

    def drop_piece(self, row: int, col: int, piece: int) -> None:
        """Place a piece in the board at the given row and column."""
        self.board[row][col] = piece

    def is_valid_location(self, col: int) -> bool:
        """Return True if the top cell in the column is empty (valid move)."""
        return self.board[self.config.rows - 1][col] == 0

    def get_next_open_row(self, col: int) -> Optional[int]:
        """Return the lowest empty row in the given column, or None if full."""
        for r in range(self.config.rows):
            if self.board[r][col] == 0:
                return r
        return None

    def winning_move(self, piece: int) -> bool:
        """Check if the given piece has a winning position on the board."""
        # Horizontal check
        for c in range(self.config.columns - 3):
            for r in range(self.config.rows):
                if all(
                    self.board[r][c + i] == piece
                    for i in range(self.config.window_length)
                ):
                    return True
        # Vertical check
        for c in range(self.config.columns):
            for r in range(self.config.rows - 3):
                if all(
                    self.board[r + i][c] == piece
                    for i in range(self.config.window_length)
                ):
                    return True
        # Positive diagonal
        for c in range(self.config.columns - 3):
            for r in range(self.config.rows - 3):
                if all(
                    self.board[r + i][c + i] == piece
                    for i in range(self.config.window_length)
                ):
                    return True
        # Negative diagonal
        for c in range(self.config.columns - 3):
            for r in range(3, self.config.rows):
                if all(
                    self.board[r - i][c + i] == piece
                    for i in range(self.config.window_length)
                ):
                    return True
        return False

    def is_full(self) -> bool:
        """Return True if the board is full (no empty slots in top row)."""
        return 0 not in self.board[self.config.rows - 1]


class Connect4Game:
    """
    Manages the main game loop, graphics, and player interactions for a two-player game.
    Uses Pygame for rendering and input.
    """

    def __init__(
        self,
        square_size: int = 100,
        *,
        config: Connect4Config | None = None,
        controller: Optional[InputController] = None,
    ) -> None:
        pygame.init()
        self.square_size = square_size
        self.config = config or Connect4Config()
        self.width = self.config.columns * self.square_size
        self.height = (self.config.rows + 1) * self.square_size
        self.radius = int(self.square_size / 2 - 5)

        self.screen = pygame.display.set_mode((self.width, self.height))
        pygame.display.set_caption("Connect 4 - Two Player")
        self.font = pygame.font.SysFont("monospace", 75)

        self.board = Board(self.config)
        self.game_over = False
        self.turn = 0  # 0 for Player 1, 1 for Player 2
        self.controller = controller or InputController()

    def draw_board(self) -> None:
        """Draw the Connect 4 board and all pieces."""
        self.screen.fill(self.config.background_colour)
        # Draw the board grid and empty slots
        for c in range(self.config.columns):
            for r in range(self.config.rows):
                pygame.draw.rect(
                    self.screen,
                    self.config.board_colour,
                    (
                        c * self.square_size,
                        r * self.square_size + self.square_size,
                        self.square_size,
                        self.square_size,
                    ),
                )
                pygame.draw.circle(
                    self.screen,
                    self.config.background_colour,
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
        for c in range(self.config.columns):
            for r in range(self.config.rows):
                if self.board.board[r][c] == self.config.player_1_piece:
                    pygame.draw.circle(
                        self.screen,
                        self.config.player_colours[0],
                        (
                            int(c * self.square_size + self.square_size / 2),
                            self.height
                            - int(r * self.square_size + self.square_size / 2),
                        ),
                        self.radius,
                    )
                elif self.board.board[r][c] == self.config.player_2_piece:
                    pygame.draw.circle(
                        self.screen,
                        self.config.player_colours[1],
                        (
                            int(c * self.square_size + self.square_size / 2),
                            self.height
                            - int(r * self.square_size + self.square_size / 2),
                        ),
                        self.radius,
                    )
        # Draw the current player's piece at the top
        if not self.game_over:
            color = self.config.player_colours[self.turn]
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
                piece = (
                    self.config.player_1_piece
                    if self.turn == 0
                    else self.config.player_2_piece
                )
                self.board.drop_piece(row, col, piece)
                if self.board.winning_move(piece):
                    player_num = 1 if self.turn == 0 else 2
                    color = self.config.player_colours[player_num - 1]
                    self.end_game(f"Player {player_num} wins!!", color)
                elif self.board.is_full():
                    self.end_game("It's a Tie!", self.config.board_colour)
                self.turn = (self.turn + 1) % 2

    def end_game(self, message: str, color: Tuple[int, int, int]) -> None:
        """Display the end game message and set game_over flag."""
        label = self.font.render(message, True, color)
        self.screen.blit(label, (40, 10))
        self.game_over = True

    def run(self) -> None:
        """Main game loop: handle events, update display, and manage game state."""
        while True:
            self.controller.pump(pygame.event.get())
            if self.controller.quit_requested:
                sys.exit()
            posx = self.controller.pop_click()
            if posx is not None:
                self.handle_mouse_click(posx)
            self.draw_board()
            if self.game_over:
                pygame.time.wait(3000)
                sys.exit()  # For simplicity, exit after game over


if __name__ == "__main__":
    game = Connect4Game()
    game.run()
