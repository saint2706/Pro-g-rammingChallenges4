"""Simplified LCD panel for the TI-86 emulator.

This module provides a basic representation of the TI-86's monochrome LCD screen.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List

LCD_WIDTH = 128
LCD_HEIGHT = 64


@dataclass
class LCD:
    """Represents the monochrome LCD buffer.

    While the physical hardware has a more complex, banked memory layout, this
    class provides a simplified linear frame buffer. This is easier to work
    with for visualization and testing.

    Attributes:
        width: The width of the LCD in pixels.
        height: The height of the LCD in pixels.
        buffer: A list of integers representing the pixel data (1 for on, 0 for off).
    """

    width: int = LCD_WIDTH
    height: int = LCD_HEIGHT
    buffer: List[int] = field(
        default_factory=lambda: [0] * (LCD_WIDTH * LCD_HEIGHT)
    )

    def clear(self, value: int = 0) -> None:
        """Clears the LCD buffer to a specified value.

        Args:
            value: The value to clear the buffer with (0 or 1).
        """
        value = 1 if value else 0
        for i in range(len(self.buffer)):
            self.buffer[i] = value

    def write_pixel(self, x: int, y: int, value: int) -> None:
        """Writes a single pixel to the LCD buffer.

        Args:
            x: The x-coordinate of the pixel.
            y: The y-coordinate of the pixel.
            value: The value of the pixel (0 or 1).
        """
        if 0 <= x < self.width and 0 <= y < self.height:
            self.buffer[y * self.width + x] = 1 if value else 0

    def blit_bitmap(self, x: int, y: int, bitmap: Iterable[int], stride: int) -> None:
        """Draws a bitmap onto the LCD buffer.

        Args:
            x: The x-coordinate to draw the bitmap at.
            y: The y-coordinate to draw the bitmap at.
            bitmap: An iterable of integers representing the bitmap data.
            stride: The width of the bitmap in pixels.
        """
        for row_index, row_data in enumerate(bitmap):
            for col_index in range(stride):
                if (row_data >> col_index) & 1:
                    self.write_pixel(x + col_index, y + row_index, 1)

    def get_pixels(self) -> List[int]:
        """Returns a copy of the pixel buffer.

        Returns:
            A list of integers representing the pixel data.
        """
        return list(self.buffer)
