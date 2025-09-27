"""Pygame entry point wiring Tetris modules together."""
from __future__ import annotations

import pygame

from .board import TetrisBoard
from .input_handler import InputHandler
from .renderer import Renderer
from .settings import TetrisSettings


def run() -> None:
    pygame.init()
    settings = TetrisSettings()
    board = TetrisBoard(settings)
    renderer = Renderer(board, settings)
    input_handler = InputHandler(settings)
    clock = pygame.time.Clock()
    fall_timer = 0.0
    running = True

    while running:
        dt = clock.tick(60) / 1000.0
        fall_timer += dt
        for event in pygame.event.get():
            running = input_handler.process_event(event, board)
            if not running:
                break
        if not running:
            break

        if not board.game_over and fall_timer >= board.gravity_interval():
            board.tick_gravity()
            fall_timer = 0.0

        input_handler.update(dt, board)
        renderer.draw()

    pygame.quit()


if __name__ == "__main__":
    run()
