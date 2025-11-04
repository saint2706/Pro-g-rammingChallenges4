"""Pygame user interface for the Connect 4 minimax challenge."""

from __future__ import annotations

import random
import sys
from dataclasses import dataclass

import pygame

from c4 import (
    AIPlayer,
    Board,
    AI_PIECE,
    PLAYER_PIECE,
    COLUMN_COUNT,
    ROW_COUNT,
    BLUE,
    BLACK,
    RED,
    YELLOW,
)


@dataclass
class Connect4UIConfig:
    square_size: int = 100
    window_caption: str = "Connect 4 - AI Challenge"


class Connect4Game:
    """Handle rendering, input, and turn management for Connect 4."""

    def __init__(self, config: Connect4UIConfig | None = None):
        pygame.init()
        self.config = config or Connect4UIConfig()
        self.square_size = self.config.square_size
        self.width = COLUMN_COUNT * self.square_size
        self.height = (ROW_COUNT + 1) * self.square_size
        self.radius = int(self.square_size / 2 - 5)

        self.screen = pygame.display.set_mode((self.width, self.height))
        pygame.display.set_caption(self.config.window_caption)
        self.font = pygame.font.SysFont("monospace", 75)

        self.board = Board()
        self.ai_player = AIPlayer(AI_PIECE, PLAYER_PIECE)
        self.game_over = False
        self.turn = random.randint(PLAYER_PIECE, AI_PIECE) - 1  # PLAYER=0, AI=1

    def draw_board(self) -> None:
        """Draw the Connect 4 grid and pieces."""

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
                            + self.square_size / 2
                            + self.square_size
                        ),
                    ),
                    self.radius,
                )

        for c in range(COLUMN_COUNT):
            for r in range(ROW_COUNT):
                piece = self.board.board[r][c]
                if piece == PLAYER_PIECE:
                    color = RED
                elif piece == AI_PIECE:
                    color = YELLOW
                else:
                    continue
                pygame.draw.circle(
                    self.screen,
                    color,
                    (
                        int(c * self.square_size + self.square_size / 2),
                        self.height - int(r * self.square_size + self.square_size / 2),
                    ),
                    self.radius,
                )
        pygame.display.update()

    def handle_player_move(self, posx: int) -> None:
        """Handle the human player's move triggered by mouse input."""

        col = posx // self.square_size
        if self.board.is_valid_location(col):
            if self.board.apply_move(col, PLAYER_PIECE) is not None:
                if self.board.winning_move(PLAYER_PIECE):
                    self.end_game("You win!!")
                self.switch_turn()

    def handle_ai_move(self) -> None:
        """Handle the AI player's move."""

        col = self.ai_player.get_best_move(self.board)
        if col is not None and self.board.is_valid_location(col):
            if self.board.apply_move(col, AI_PIECE) is not None:
                if self.board.winning_move(AI_PIECE):
                    self.end_game("AI wins!!")
                self.switch_turn()

    def switch_turn(self) -> None:
        self.turn = (self.turn + 1) % 2

    def end_game(self, message: str) -> None:
        color = RED if "You" in message else YELLOW
        label = self.font.render(message, True, color)
        self.screen.blit(label, (40, 10))
        self.game_over = True

    def run(self) -> None:
        while not self.game_over:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    pygame.quit()
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

            if self.turn == 1 and not self.game_over:
                self.handle_ai_move()

            self.draw_board()

            if self.board.is_terminal_node() and not self.game_over:
                self.end_game("It's a Tie!")

            if self.game_over:
                pygame.time.wait(3000)


def main() -> None:
    game = Connect4Game()
    game.run()


if __name__ == "__main__":
    main()
