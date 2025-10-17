"""Keyboard event handling for Tetris (DAS/ARR + gameplay controls)."""

from __future__ import annotations

from dataclasses import dataclass
import pygame

from .board import TetrisBoard
from .settings import TetrisSettings


@dataclass
class _DirectionState:
    pressed: bool = False
    das_timer: float = 0.0
    arr_timer: float = 0.0


class InputHandler:
    """Convert pygame events into board actions with DAS/ARR semantics."""

    def __init__(self, settings: TetrisSettings) -> None:
        self.settings = settings
        self.left = _DirectionState()
        self.right = _DirectionState()
        self.down_pressed = False
        self.soft_drop_buffer = 0.0

    def process_event(self, event: pygame.event.Event, board: TetrisBoard) -> bool:
        """Return False to exit the game loop."""
        if event.type == pygame.QUIT:
            return False
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_ESCAPE:
                return False
            if event.key == pygame.K_LEFT:
                self._handle_direction(self.left, True)
                board.move(-1)
            elif event.key == pygame.K_RIGHT:
                self._handle_direction(self.right, True)
                board.move(1)
            elif event.key == pygame.K_DOWN:
                self.down_pressed = True
            elif event.key == pygame.K_SPACE:
                board.hard_drop()
            elif event.key == pygame.K_z:
                board.rotate(-1)
            elif event.key in (pygame.K_x, pygame.K_UP):
                board.rotate(1)
            elif event.key == pygame.K_r:
                board.reset()
            elif event.key == pygame.K_g:
                self.settings.ghost_piece = not self.settings.ghost_piece
        elif event.type == pygame.KEYUP:
            if event.key == pygame.K_LEFT:
                self._handle_direction(self.left, False)
            elif event.key == pygame.K_RIGHT:
                self._handle_direction(self.right, False)
            elif event.key == pygame.K_DOWN:
                self.down_pressed = False
                self.soft_drop_buffer = 0.0
        return True

    def update(self, dt: float, board: TetrisBoard) -> None:
        """Handle DAS/ARR repeats and held soft drops."""
        self._update_direction(dt, board)
        if self.down_pressed and not board.game_over:
            self.soft_drop_buffer += dt * self.settings.soft_drop_rate
            while self.soft_drop_buffer >= 1.0:
                locked = board.soft_drop()
                self.soft_drop_buffer -= 1.0
                if locked:
                    self.soft_drop_buffer = 0.0
                    break

    # --- Helpers ---------------------------------------------------------
    def _handle_direction(self, state: _DirectionState, pressed: bool) -> None:
        state.pressed = pressed
        state.das_timer = 0.0
        state.arr_timer = 0.0

    def _update_direction(self, dt: float, board: TetrisBoard) -> None:
        dt_ms = dt * 1000
        if self.left.pressed and not self.right.pressed:
            self._apply_shift(self.left, -1, dt_ms, board)
        elif self.right.pressed and not self.left.pressed:
            self._apply_shift(self.right, 1, dt_ms, board)

    def _apply_shift(
        self, state: _DirectionState, direction: int, dt_ms: float, board: TetrisBoard
    ) -> None:
        state.das_timer += dt_ms
        if state.das_timer < self.settings.das:
            return
        state.arr_timer += dt_ms
        if state.arr_timer >= self.settings.arr:
            board.move(direction)
            state.arr_timer = 0.0
