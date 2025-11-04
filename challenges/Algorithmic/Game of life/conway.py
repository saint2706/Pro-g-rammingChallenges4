"""Conway's Game of Life (modernized)
====================================

Features added in this refactor:
 - Configuration via dataclasses and CLI flags (size, cell size, fps, density, theme, wrapping, max generations, headless).
 - Pattern library (glider, Gosper glider gun) with safe placement.
 - SciPy ``convolve2d`` acceleration when available; transparent NumPy fallback (no external dependency required).
 - Multiple color themes and runtime theme cycling.
 - Pausing, single-step advance, speed adjustment, grid clearing, pattern injection keys.
 - Optional gridlines & overlay (toggle keys).
 - Screenshot capture (PNG) to a chosen output directory.
 - Headless mode (no Pygame window) useful for benchmarking or automated runs.
 - Graceful exit & generation limiting.

Keyboard shortcuts:
 SPACE  : Pause/Resume
 N      : Advance one generation when paused
 R      : Reset random grid
 C      : Clear grid (all dead)
 G      : Place glider (center)
 P      : Place Gosper glider gun (upper-left offset)
 T      : Cycle theme
 +/-    : Increase / decrease target FPS
 S      : Save screenshot
 H      : Toggle gridlines
 O      : Toggle overlay text (status/fps/generation)
 Q / ESC: Quit

Example CLI usage:
 python conway.py --width 1000 --height 800 --cell-size 8 --fps 30 --density 0.15 --theme neon
 python conway.py --pattern gosper_glider_gun --no-wrap --max-generations 500 --headless
 python conway.py --headless --max-generations 2000 --fps 200

Performance notes:
 The SciPy backend (if installed) provides fast convolution. The NumPy fallback uses ``np.roll`` shifts
 which is competitive for moderate grid sizes. Avoid very small ``cell_size`` with very large windows if your
 machine struggles; instead reduce dimensions or FPS.
"""

from __future__ import annotations

import argparse
import time
from dataclasses import dataclass
from pathlib import Path
from textwrap import dedent
from typing import Dict, Iterable, List, Optional, Tuple

import numpy as np
from numpy.lib.stride_tricks import sliding_window_view

try:  # Optional acceleration
    from scipy.signal import convolve2d  # type: ignore

    _SCIPY_AVAILABLE = True
except Exception:  # pragma: no cover - environment dependent
    convolve2d = None  # type: ignore
    _SCIPY_AVAILABLE = False

import pygame

# -----------------------------
# Pattern Library
# -----------------------------


def _parse_pattern(art: str) -> np.ndarray:
    lines = [line for line in dedent(art).splitlines() if line.strip()]
    grid = []
    for line in lines:
        row = [1 if ch in {"#", "O", "o", "X"} else 0 for ch in line.strip()]
        grid.append(row)
    return np.array(grid, dtype=int)


PATTERNS: Dict[str, np.ndarray] = {
    "glider": _parse_pattern(
        """
        .#.
        ..#
        ###
        """
    ),
    "gosper_glider_gun": _parse_pattern(
        """
        ........................#...........
        ......................#.#...........
        ............##......##............##
        ...........#...#....##............##
        ##........#.....#...##..............
        ##........#...#.##....#.#...........
        ..........#.....#.......#...........
        ...........#...#....................
        ............##......................
        """
    ),
}


# -----------------------------
# Themes
# -----------------------------
@dataclass(frozen=True)
class Theme:
    name: str
    background: Tuple[int, int, int]
    cell: Tuple[int, int, int]
    grid: Tuple[int, int, int]
    overlay: Tuple[int, int, int]


THEMES: List[Theme] = [
    Theme("classic", (20, 20, 40), (255, 255, 0), (55, 55, 80), (220, 220, 220)),
    Theme("matrix", (5, 15, 5), (140, 255, 140), (25, 60, 25), (180, 255, 180)),
    Theme("neon", (15, 10, 30), (255, 0, 200), (60, 30, 90), (255, 170, 230)),
    Theme("ice", (10, 20, 35), (120, 200, 255), (40, 70, 100), (200, 230, 255)),
]

THEME_INDEX: Dict[str, int] = {t.name: i for i, t in enumerate(THEMES)}


# -----------------------------
# Configuration dataclass
# -----------------------------
@dataclass
class GameConfig:
    width: int = 800
    height: int = 600
    cell_size: int = 10
    fps: int = 15
    density: float = 0.2  # probability of a cell being alive on random reset
    wrap: bool = True
    pattern: Optional[str] = None
    theme: str = "classic"
    headless: bool = False
    max_generations: Optional[int] = None
    show_grid: bool = False
    show_overlay: bool = True
    screenshot_dir: Path = Path("screenshots")

    def validate(self) -> None:
        if self.cell_size <= 0:
            raise ValueError("cell_size must be > 0")
        if self.width <= 0 or self.height <= 0:
            raise ValueError("width/height must be > 0")
        if not (0.0 <= self.density <= 1.0):
            raise ValueError("density must be between 0 and 1")
        if self.fps <= 0:
            raise ValueError("fps must be > 0")
        if self.theme not in THEME_INDEX:
            raise ValueError(
                f"Unknown theme '{self.theme}' (choices: {', '.join(THEME_INDEX)})"
            )
        if self.pattern and self.pattern not in PATTERNS:
            raise ValueError(
                f"Unknown pattern '{self.pattern}' (choices: {', '.join(PATTERNS)})"
            )


class NeighborCalculator:
    """Strategy object for neighbor count supporting wrap or bounded edges.

    Uses SciPy convolution if available, else a NumPy sliding-window implementation.
    """

    _KERNEL = np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]], dtype=int)

    def __init__(self, shape: Tuple[int, int], wrap: bool):
        self.shape = shape
        self.wrap = wrap

    def counts(self, grid: np.ndarray) -> np.ndarray:
        if _SCIPY_AVAILABLE:
            boundary = "wrap" if self.wrap else "fill"
            return convolve2d(grid, self._KERNEL, mode="same", boundary=boundary)  # type: ignore[arg-type]
        pad_mode = "wrap" if self.wrap else "constant"
        padded = np.pad(grid, 1, mode=pad_mode)
        windows = sliding_window_view(padded, (3, 3))
        return np.tensordot(windows, self._KERNEL, axes=((2, 3), (0, 1)))


class GameOfLife:
    """Manages the Game of Life simulation, rendering, and user interaction.

    Attributes:
        config: An object containing the game's configuration settings.
        generation: The current generation number of the simulation.
        paused: A boolean indicating whether the simulation is paused.
        theme: The current color theme for the game display.
    """

    def __init__(self, config: GameConfig):
        self.config = config
        config.validate()

        self.rows = config.height // config.cell_size
        self.cols = config.width // config.cell_size
        self.shape = (self.rows, self.cols)
        self.generation = 0
        self.paused = False

        # Theme
        self.theme_index = THEME_INDEX[config.theme]
        self.theme = THEMES[self.theme_index]

        # Initialize pygame only if not headless
        if not config.headless:
            pygame.init()
            self.screen = pygame.display.set_mode((config.width, config.height))
            pygame.display.set_caption("Conway's Game of Life (initializing)")
            self.clock = pygame.time.Clock()
            self.font = pygame.font.SysFont("consolas", 14)
        else:  # Minimal placeholders for type consistency
            self.screen = None  # type: ignore
            self.clock = None  # type: ignore
            self.font = None  # type: ignore

        self.grid = self._create_grid(random=True)
        if config.pattern:
            self.place_pattern(config.pattern)

        self.neighbor_calc = NeighborCalculator(self.shape, config.wrap)

    # -----------------------------
    # Grid & pattern utilities
    # -----------------------------
    def _create_grid(self, random: bool = True) -> np.ndarray:
        if random:
            return (np.random.random(self.shape) < self.config.density).astype(int)
        return np.zeros(self.shape, dtype=int)

    def reset_random(self) -> None:
        self.grid = self._create_grid(random=True)
        self.generation = 0

    def clear(self) -> None:
        self.grid = self._create_grid(random=False)
        self.generation = 0

    def place_pattern(
        self, pattern_name: str, offset: Optional[Tuple[int, int]] = None
    ) -> None:
        pattern = PATTERNS.get(pattern_name)
        if pattern is None:
            print(f"Pattern '{pattern_name}' not found.")
            return
        if offset is None:
            # Center by default (approx)
            pr, pc = pattern.shape
            offset = ((self.rows - pr) // 2, (self.cols - pc) // 2)
        r0, c0 = offset
        pr, pc = pattern.shape
        if r0 < 0 or c0 < 0 or r0 + pr > self.rows or c0 + pc > self.cols:
            print("Pattern does not fit at requested offset; skipping placement.")
            return
        self.clear()
        self.grid[r0 : r0 + pr, c0 : c0 + pc] = pattern
        self.generation = 0

    # -----------------------------
    # Simulation update
    # -----------------------------
    def step(self) -> None:
        """Advances the simulation by one generation."""
        counts = self.neighbor_calc.counts(self.grid)
        born = (self.grid == 0) & (counts == 3)
        survive = (self.grid == 1) & ((counts == 2) | (counts == 3))
        self.grid[...] = 0
        self.grid[born | survive] = 1
        self.generation += 1

    def update(self) -> None:
        """Updates the simulation state if it is not paused."""
        if not self.paused:
            self.step()

    # -----------------------------
    # Rendering
    # -----------------------------
    def draw(self) -> None:
        """Draws the current state of the game grid to the screen."""
        if self.config.headless:
            return
        # Fill the background
        self.screen.fill(self.theme.background)  # type: ignore[attr-defined]

        # Get the coordinates of all live cells
        live_rows, live_cols = np.nonzero(self.grid)
        cs = self.config.cell_size

        # Draw a rectangle for each live cell
        for r, c in zip(live_rows.tolist(), live_cols.tolist()):
            pygame.draw.rect(self.screen, self.theme.cell, (c * cs, r * cs, cs, cs))  # type: ignore[arg-type]

        if self.config.show_grid and cs >= 4:
            self._draw_gridlines()
        if self.config.show_overlay:
            self._draw_overlay()

        # Update the full display Surface to the screen
        pygame.display.flip()

    def _draw_gridlines(self) -> None:
        if self.config.headless:
            return
        w, h, cs = self.config.width, self.config.height, self.config.cell_size
        for x in range(0, w, cs):
            pygame.draw.line(self.screen, self.theme.grid, (x, 0), (x, h))  # type: ignore[arg-type]
        for y in range(0, h, cs):
            pygame.draw.line(self.screen, self.theme.grid, (0, y), (w, y))  # type: ignore[arg-type]

    def _draw_overlay(self) -> None:
        if self.config.headless:
            return
        txt = f"Gen: {self.generation}  FPS: {int(self.clock.get_fps()) if self.clock else 0}  Cells: {self.rows}x{self.cols}  Theme: {self.theme.name}  {'WRAP' if self.config.wrap else 'NO-WRAP'}"  # type: ignore[union-attr]
        color = self.theme.overlay
        surf = self.font.render(txt, True, color)  # type: ignore[union-attr]
        self.screen.blit(surf, (8, 6))  # type: ignore[union-attr]

    # -----------------------------
    # Event handling
    # -----------------------------
    def handle_events(self) -> bool:
        if self.config.headless:
            return True
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            if event.type == pygame.KEYDOWN:
                k = event.key
                if k in (pygame.K_q, pygame.K_ESCAPE):
                    return False
                elif k == pygame.K_SPACE:
                    self.paused = not self.paused
                elif k == pygame.K_n:  # single step
                    if self.paused:
                        self.step()
                elif k == pygame.K_r:
                    self.reset_random()
                elif k == pygame.K_c:
                    self.clear()
                elif k == pygame.K_g:
                    self.place_pattern("glider")
                elif k == pygame.K_p:
                    self.place_pattern("gosper_glider_gun")
                elif k == pygame.K_t:
                    self._cycle_theme()
                elif k in (pygame.K_PLUS, pygame.K_EQUALS):  # increase FPS
                    self.config.fps = min(self.config.fps + 5, 600)
                elif k == pygame.K_MINUS:
                    self.config.fps = max(self.config.fps - 5, 1)
                elif k == pygame.K_s:
                    self._save_screenshot()
                elif k == pygame.K_h:
                    self.config.show_grid = not self.config.show_grid
                elif k == pygame.K_o:
                    self.config.show_overlay = not self.config.show_overlay
        return True

    def _cycle_theme(self) -> None:
        self.theme_index = (self.theme_index + 1) % len(THEMES)
        self.theme = THEMES[self.theme_index]

    def _save_screenshot(self) -> None:
        if self.config.headless:
            return
        self.config.screenshot_dir.mkdir(parents=True, exist_ok=True)
        ts = time.strftime("%Y%m%d-%H%M%S")
        path = self.config.screenshot_dir / f"gol_{ts}.png"
        pygame.image.save(self.screen, str(path))  # type: ignore[arg-type]
        print(f"Saved screenshot -> {path}")

    # -----------------------------
    # Main loop
    # -----------------------------
    def run(self) -> None:
        # Headless run (no window) -- just iterate
        if self.config.headless:
            max_gen = self.config.max_generations or 100
            for _ in range(max_gen):
                self.step()
            return

        running = True
        cap_base = "Conway's Game of Life"
        while running:
            running = self.handle_events()
            self.update()
            self.draw()
            status = "Paused" if self.paused else "Running"
            fps_val = int(self.clock.get_fps()) if self.clock else self.config.fps  # type: ignore[union-attr]
            pygame.display.set_caption(
                f"{cap_base} | {status} | FPS: {fps_val} | Gen: {self.generation}"
            )
            self.clock.tick(self.config.fps)  # type: ignore[union-attr]
            if (
                self.config.max_generations is not None
                and self.generation >= self.config.max_generations
            ):
                running = False

        pygame.quit()


# -----------------------------
# CLI
# -----------------------------
def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Conway's Game of Life simulation with themes & interactive controls."
    )
    p.add_argument("--width", type=int, default=800)
    p.add_argument("--height", type=int, default=600)
    p.add_argument("--cell-size", type=int, default=10)
    p.add_argument(
        "--fps",
        type=int,
        default=15,
        help="Target frames per second (interactive mode)",
    )
    p.add_argument(
        "--density",
        type=float,
        default=0.2,
        help="Initial random live cell probability (0-1)",
    )
    p.add_argument(
        "--pattern",
        choices=list(PATTERNS),
        help="Start with a known pattern (grid is cleared)",
    )
    p.add_argument("--theme", choices=list(THEME_INDEX), default="classic")
    p.add_argument(
        "--no-wrap",
        action="store_true",
        help="Disable toroidal wrapping; edges die off",
    )
    p.add_argument(
        "--max-generations",
        type=int,
        help="Stop after this many generations (interactive still renders)",
    )
    p.add_argument(
        "--headless",
        action="store_true",
        help="Run without opening a window (implies auto-stop)",
    )
    p.add_argument(
        "--show-grid",
        action="store_true",
        help="Draw gridlines (slower at tiny cell sizes)",
    )
    p.add_argument(
        "--no-overlay", action="store_true", help="Hide overlay text (FPS, generation)"
    )
    p.add_argument("--screenshot-dir", type=Path, default=Path("screenshots"))
    return p


def parse_args_to_config(argv: Optional[Iterable[str]] = None) -> GameConfig:
    args = build_arg_parser().parse_args(list(argv) if argv is not None else None)
    return GameConfig(
        width=args.width,
        height=args.height,
        cell_size=args.cell_size,
        fps=args.fps,
        density=args.density,
        wrap=not args.no_wrap,
        pattern=args.pattern,
        theme=args.theme,
        headless=args.headless,
        max_generations=args.max_generations,
        show_grid=args.show_grid,
        show_overlay=not args.no_overlay,
        screenshot_dir=args.screenshot_dir,
    )


def main(argv: Optional[Iterable[str]] = None) -> int:
    try:
        config = parse_args_to_config(argv)
        game = GameOfLife(config)
        game.run()
        return 0
    except Exception as exc:  # Broad catch to present friendly error
        print(f"[error] {exc}")
        return 1


if __name__ == "__main__":  # pragma: no cover - manual execution entry
    raise SystemExit(main())
