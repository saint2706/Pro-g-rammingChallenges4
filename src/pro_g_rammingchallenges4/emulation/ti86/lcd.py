"""Simplified LCD panel for the TI-86."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List

LCD_WIDTH = 128
LCD_HEIGHT = 64


@dataclass
class LCD:
    """Monochrome LCD buffer.

    The physical device organises pixels as 8 vertical banks.  For the emulator we
    expose a linear frame buffer and convenience helpers for unit testing and
    future visualisation back-ends.
    """

    width: int = LCD_WIDTH
    height: int = LCD_HEIGHT
    buffer: List[int] = field(default_factory=lambda: [0] * (LCD_WIDTH * LCD_HEIGHT))

    def clear(self, value: int = 0) -> None:
        value = 1 if value else 0
        for idx in range(len(self.buffer)):
            self.buffer[idx] = value

    def write_pixel(self, x: int, y: int, value: int) -> None:
        if 0 <= x < self.width and 0 <= y < self.height:
            self.buffer[y * self.width + x] = 1 if value else 0

    def blit_bitmap(self, x: int, y: int, bitmap: Iterable[int], stride: int) -> None:
        for row, row_data in enumerate(bitmap):
            for col in range(stride):
                pixel = (row_data >> col) & 0x01
                self.write_pixel(x + col, y + row, pixel)

    def get_pixels(self) -> List[int]:
        return list(self.buffer)
