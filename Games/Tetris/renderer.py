"""Rendering helpers for the Pygame Tetris implementation."""

from __future__ import annotations

from typing import Iterable, Tuple

import pygame

from .board import PIECES, TetrisBoard
from .settings import TetrisSettings

Color = Tuple[int, int, int]
GRID_COLOR: Color = (40, 40, 50)
BACKGROUND_COLOR: Color = (18, 18, 24)
TEXT_COLOR: Color = (240, 240, 240)
GHOST_ALPHA: int = 80


class Renderer:
    """Owns the pygame surface and draws board + HUD."""

    def __init__(self, board: TetrisBoard, settings: TetrisSettings) -> None:
        self.board = board
        self.settings = settings
        width, height = settings.playfield_pixel_size
        total_width = width + settings.sidebar_pixel_width
        self.surface = pygame.display.set_mode((total_width, height))
        pygame.display.set_caption("Tetris")
        self.font = pygame.font.SysFont("Roboto", 18)
        self.small_font = pygame.font.SysFont("Roboto", 14)
        self.large_font = pygame.font.SysFont("Roboto", 36, bold=True)

    # --- Drawing ---------------------------------------------------------
    def draw(self) -> None:
        self.surface.fill(BACKGROUND_COLOR)
        self._draw_locked_cells()
        if self.board.active and self.settings.ghost_piece:
            self._draw_ghost(self.board.ghost_cells())
        self._draw_active_piece()
        self._draw_grid()
        self._draw_sidebar()
        if self.board.game_over:
            self._draw_game_over()
        pygame.display.flip()

    def _draw_grid(self) -> None:
        width, height = self.settings.playfield_pixel_size
        block = self.settings.block_size
        for x in range(self.board.width + 1):
            pygame.draw.line(
                self.surface,
                GRID_COLOR,
                (x * block, 0),
                (x * block, height),
                1,
            )
        for y in range(self.board.height + 1):
            pygame.draw.line(
                self.surface,
                GRID_COLOR,
                (0, y * block),
                (width, y * block),
                1,
            )

    def _draw_locked_cells(self) -> None:
        for y, row in enumerate(self.board.grid):
            for x, cell in enumerate(row):
                if cell is not None:
                    self._draw_block((x, y), cell)

    def _draw_active_piece(self) -> None:
        if not self.board.active:
            return
        for cell in self.board.get_cells():
            self._draw_block(cell, self.board.active.color)

    def _draw_ghost(self, cells: Iterable[Tuple[int, int]]) -> None:
        ghost_surface = pygame.Surface(
            self.settings.playfield_pixel_size, pygame.SRCALPHA
        )
        color = (
            (*self.board.active.color, GHOST_ALPHA)
            if self.board.active
            else (255, 255, 255, GHOST_ALPHA)
        )
        block = self.settings.block_size
        for x, y in cells:
            rect = pygame.Rect(x * block, y * block, block, block)
            pygame.draw.rect(ghost_surface, color, rect)
        self.surface.blit(ghost_surface, (0, 0))

    def _draw_sidebar(self) -> None:
        width, _ = self.settings.playfield_pixel_size
        sidebar_x = width + 20
        self._draw_text(f"Score: {self.board.score}", (sidebar_x, 20))
        self._draw_text(f"Lines: {self.board.lines_cleared}", (sidebar_x, 50))
        self._draw_text(f"Level: {self.board.level}", (sidebar_x, 80))
        self._draw_text("Next", (sidebar_x, 130))
        self._draw_queue((sidebar_x, 160))
        controls = [
            "Left/Right: Move",
            "Down: Soft drop",
            "Space: Hard drop",
            "Z/X or Up: Rotate",
            "R: Restart",
            "G: Toggle ghost",
            "Esc: Quit",
        ]
        self._draw_text("Controls", (sidebar_x, 320))
        for idx, line in enumerate(controls):
            self._draw_text(line, (sidebar_x, 350 + idx * 24), font=self.small_font)

    def _draw_queue(self, origin: Tuple[int, int]) -> None:
        block = self.settings.block_size // 2
        origin_x, origin_y = origin
        for idx, name in enumerate(
            self.board.peek_queue()[: self.settings.next_queue_length]
        ):
            shape = PIECES[name]["rotations"][0]
            color = PIECES[name]["color"]
            offset_x = 20
            offset_y = idx * 60 + 10
            for x, y in shape:
                rect = pygame.Rect(
                    origin_x + offset_x + x * block,
                    origin_y + offset_y + y * block,
                    block,
                    block,
                )
                pygame.draw.rect(self.surface, color, rect)
                pygame.draw.rect(self.surface, GRID_COLOR, rect, 1)

    def _draw_text(
        self, text: str, position: Tuple[int, int], font: pygame.font.Font | None = None
    ) -> None:
        font = font or self.font
        surface = font.render(text, True, TEXT_COLOR)
        self.surface.blit(surface, position)

    def _draw_block(self, cell: Tuple[int, int], color: Color) -> None:
        block = self.settings.block_size
        rect = pygame.Rect(cell[0] * block, cell[1] * block, block, block)
        pygame.draw.rect(self.surface, color, rect)
        pygame.draw.rect(self.surface, GRID_COLOR, rect, 1)

    def _draw_game_over(self) -> None:
        width, height = self.settings.playfield_pixel_size
        overlay = pygame.Surface((width, height), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 160))
        self.surface.blit(overlay, (0, 0))
        text = self.large_font.render("Game Over", True, TEXT_COLOR)
        prompt = self.font.render("Press R to restart", True, TEXT_COLOR)
        rect = text.get_rect(center=(width // 2, height // 2 - 20))
        prect = prompt.get_rect(center=(width // 2, height // 2 + 20))
        self.surface.blit(text, rect)
        self.surface.blit(prompt, prect)
