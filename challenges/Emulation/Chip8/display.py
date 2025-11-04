"""Display handling for CHIP-8.

This module provides the display buffer and rendering backends for the Chip-8
emulator. It includes a base DisplayBuffer class for managing the pixel state,
as well as Pygame and Curses implementations for rendering.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, List, Tuple


@dataclass
class DisplayBuffer:
    """Stores the CHIP-8 display buffer and provides drawing helpers.

    This class manages the 64x32 monochrome display of the Chip-8. It provides
    methods for clearing the screen, drawing sprites, and accessing the pixel data.

    Attributes:
        width: The width of the display.
        height: The height of the display.
    """

    width: int = 64
    height: int = 32

    def __post_init__(self) -> None:
        """Initializes the display buffer."""
        self._buffer: List[List[int]] = [
            [0 for _ in range(self.width)] for _ in range(self.height)
        ]
        self._dirty = True  # Flag to track if the buffer needs to be redrawn.

    def clear(self) -> None:
        """Clears the display buffer."""
        for row in self._buffer:
            for index in range(len(row)):
                row[index] = 0
        self._dirty = True

    def draw_sprite(self, x: int, y: int, sprite: Iterable[int]) -> bool:
        """Draws a sprite on the display buffer.

        The sprite is XORed onto the existing pixels.

        Args:
            x: The x-coordinate to draw the sprite at.
            y: The y-coordinate to draw the sprite at.
            sprite: An iterable of bytes representing the sprite data.

        Returns:
            True if a collision occurred (a pixel was flipped from 1 to 0),
            False otherwise.
        """
        collision = False
        for y_offset, byte in enumerate(sprite):
            row = (y + y_offset) % self.height
            for bit in range(8):
                pixel = (byte >> (7 - bit)) & 0x1
                if pixel == 0:
                    continue
                column = (x + bit) % self.width
                if self._buffer[row][column] == 1:
                    collision = True
                self._buffer[row][column] ^= pixel
        self._dirty = True
        return collision

    def pixels(self) -> Tuple[Tuple[int, ...], ...]:
        """Returns the current state of the display buffer.

        Returns:
            A tuple of tuples representing the pixel data.
        """
        return tuple(tuple(row) for row in self._buffer)

    def present(self) -> None:
        """Presents the display buffer to the screen.

        This method is implemented by the rendering backend subclasses.
        """
        self._dirty = False

    def close(self) -> None:
        """Cleans up any resources used by the display.

        This method is implemented by the rendering backend subclasses.
        """
        pass


class PygameDisplay(DisplayBuffer):
    """Renders the CHIP-8 display using Pygame."""

    def __init__(self, scale: int = 12) -> None:
        """Initializes the Pygame display.

        Args:
            scale: The scaling factor for each pixel.
        """
        self.scale = scale
        try:
            import pygame
        except ImportError as exc:
            raise RuntimeError(
                "pygame is required for the pygame display backend"
            ) from exc
        super().__init__()
        self._pygame = pygame
        pygame.init()
        self._surface = pygame.display.set_mode(
            (self.width * scale, self.height * scale)
        )
        pygame.display.set_caption("CHIP-8 Emulator")
        self._clock = pygame.time.Clock()

    def present(self) -> None:
        """Presents the display buffer to the Pygame window."""
        if not self._dirty:
            self._clock.tick(60)
            return
        for y, row in enumerate(self._buffer):
            for x, pixel in enumerate(row):
                color = (255, 255, 255) if pixel else (20, 20, 20)
                rect = (x * self.scale, y * self.scale, self.scale, self.scale)
                self._surface.fill(color, rect)
        self._pygame.display.flip()
        self._clock.tick(60)
        self._dirty = False

    def close(self) -> None:
        """Closes the Pygame window."""
        self._pygame.quit()


class CursesDisplay(DisplayBuffer):
    """Renders the CHIP-8 display using Curses."""

    def __init__(self) -> None:
        """Initializes the Curses display."""
        import curses

        self._curses = curses
        self._screen = curses.initscr()
        curses.noecho()
        curses.cbreak()
        curses.curs_set(0)
        self._screen.nodelay(True)
        super().__init__()

    def present(self) -> None:
        """Presents the display buffer to the Curses terminal."""
        if not self._dirty:
            return
        for y, row in enumerate(self._buffer):
            line = "".join("██" if pixel else "  " for pixel in row)
            try:
                self._screen.addstr(y, 0, line)
            except self._curses.error:
                # Ignore errors if the terminal is too small.
                pass
        self._screen.refresh()
        self._dirty = False

    def close(self) -> None:
        """Restores the terminal to its normal state."""
        curses = self._curses
        curses.nocbreak()
        curses.echo()
        curses.endwin()


def create_display(backend: str = "pygame", **kwargs) -> DisplayBuffer:
    """Factory function for creating display instances.

    Args:
        backend: The name of the backend to use ("pygame", "curses", or "headless").
        **kwargs: Additional arguments to pass to the display constructor.

    Returns:
        A DisplayBuffer instance for the specified backend.

    Raises:
        ValueError: If an unknown backend is specified.
    """
    backend = backend.lower()
    if backend == "pygame":
        return PygameDisplay(**kwargs)
    if backend == "curses":
        return CursesDisplay()
    if backend == "headless":
        return DisplayBuffer()
    raise ValueError(f"Unknown display backend: {backend}")
