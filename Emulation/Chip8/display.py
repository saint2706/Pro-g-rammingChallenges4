"""Display handling for CHIP-8."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, List, Tuple


@dataclass
class DisplayBuffer:
    """Stores the CHIP-8 display buffer and provides drawing helpers."""

    width: int = 64
    height: int = 32

    def __post_init__(self) -> None:
        self._buffer: List[List[int]] = [[0 for _ in range(self.width)] for _ in range(self.height)]
        self._dirty = True

    def clear(self) -> None:
        for row in self._buffer:
            for index in range(len(row)):
                row[index] = 0
        self._dirty = True

    def draw_sprite(self, x: int, y: int, sprite: Iterable[int]) -> bool:
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
        return tuple(tuple(row) for row in self._buffer)

    def present(self) -> None:  # pragma: no cover - implemented by subclasses
        self._dirty = False

    def close(self) -> None:  # pragma: no cover - for subclasses cleanup
        pass


class PygameDisplay(DisplayBuffer):
    """Render CHIP-8 output via pygame."""

    def __init__(self, scale: int = 12) -> None:
        self.scale = scale
        try:
            import pygame
        except ImportError as exc:  # pragma: no cover - optional dependency
            raise RuntimeError("pygame is required for the pygame display backend") from exc
        super().__init__()
        self._pygame = pygame
        pygame.init()
        self._surface = pygame.display.set_mode((self.width * scale, self.height * scale))
        pygame.display.set_caption("CHIP-8 Emulator")
        self._clock = pygame.time.Clock()

    def present(self) -> None:  # pragma: no cover - requires pygame
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

    def close(self) -> None:  # pragma: no cover - requires pygame
        self._pygame.quit()


class CursesDisplay(DisplayBuffer):
    """Render CHIP-8 output using curses."""

    def __init__(self) -> None:
        import curses

        self._curses = curses
        self._screen = curses.initscr()
        curses.noecho()
        curses.cbreak()
        curses.curs_set(0)
        self._screen.nodelay(True)
        super().__init__()

    def present(self) -> None:  # pragma: no cover - interactive
        if not self._dirty:
            return
        for y, row in enumerate(self._buffer):
            line = "".join("██" if pixel else "  " for pixel in row)
            try:
                self._screen.addstr(y, 0, line)
            except self._curses.error:
                # Terminal too small; ignore updates beyond boundaries.
                pass
        self._screen.refresh()
        self._dirty = False

    def close(self) -> None:  # pragma: no cover - interactive cleanup
        curses = self._curses
        curses.nocbreak()
        curses.echo()
        curses.endwin()


def create_display(backend: str = "pygame", **kwargs) -> DisplayBuffer:
    """Factory for display implementations."""

    backend = backend.lower()
    if backend == "pygame":
        return PygameDisplay(**kwargs)
    if backend == "curses":
        return CursesDisplay()
    if backend == "headless":
        return DisplayBuffer()
    raise ValueError(f"Unknown display backend: {backend}")
