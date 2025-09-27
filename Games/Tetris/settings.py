"""Configuration for the Pygame Tetris implementation."""
from dataclasses import dataclass


@dataclass
class TetrisSettings:
    """Configuration values that can be tweaked without touching game logic."""

    board_width: int = 10
    board_height: int = 20
    block_size: int = 30
    preview_columns: int = 6
    das: int = 160  # Delayed auto shift in milliseconds.
    arr: int = 40   # Auto repeat rate in milliseconds.
    soft_drop_rate: float = 20.0  # Cells per second while soft dropping.
    gravity_seconds: float = 1.0  # Base gravity interval at level 1.
    ghost_piece: bool = True
    next_queue_length: int = 5

    @property
    def playfield_pixel_size(self) -> tuple[int, int]:
        return self.board_width * self.block_size, self.board_height * self.block_size

    @property
    def sidebar_pixel_width(self) -> int:
        return self.preview_columns * self.block_size
