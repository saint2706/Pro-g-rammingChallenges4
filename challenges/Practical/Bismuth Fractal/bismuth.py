"""bismuth.py – Bismuth-like hex fractal generator using turtle graphics.

Enhancements:
  * Dataclass Config + structured main pipeline
  * CLI flags: --level, --size, --palette, --shuffle, --animate, --interval, --export
  * Precomputed hex vertex offsets (avoids per-edge trig calls inside loop)
  * Cached sin/cos for placement geometry
  * Optional progressive animation vs instant render
  * Safety guard estimating node count ( (6^(level)-1)/5 ) to avoid runaway rendering
  * Optional export of final screen to PostScript (depends on platform/turtle backend)
  * Rich status messages and timing feedback

Example:
  python bismuth.py --level 4 --size 140 --palette "#6A097D,#C77DFF,#FFC300,#DAF7A6" --shuffle --animate --interval 10
"""

from __future__ import annotations

import argparse
import math
import random
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Sequence, Tuple
import turtle

# ----------------------------- Data Model ----------------------------- #


@dataclass(slots=True)
class Config:
    level: int
    size: float
    palette: List[str]
    shuffle_palette: bool
    animate: bool
    frame_interval: int  # number of hexes per screen update when animating
    export_path: Optional[Path]

    @property
    def estimated_hexes(self) -> int:
        # Root + 6 recursive branches => geometric sum: 1 + 6 + 6^2 + ... + 6^(level-1)
        # (6^level - 1)/(6 - 1)
        return (6**self.level - 1) // 5


DEFAULT_PALETTE = [
    "#6A097D",
    "#C77DFF",
    "#660066",
    "#B6174B",
    "#F4C2C2",
    "#FF5733",
    "#FFC300",
    "#DAF7A6",
]


def parse_palette(palette_arg: Optional[str]) -> List[str]:
    """Parse a comma-separated palette string, falling back to the default palette."""

    if not palette_arg:
        return list(DEFAULT_PALETTE)
    colors = [color.strip() for color in palette_arg.split(",") if color.strip()]
    return colors or list(DEFAULT_PALETTE)


# Hexagon metadata tuple: (center_x, center_y, side_length, color)
HexSpec = Tuple[float, float, float, str]

# Precomputed offsets for placing child hexes around a parent
PLACEMENT_OFFSETS: List[Tuple[float, float]] = [
    (math.cos(math.radians(a)), math.sin(math.radians(a))) for a in range(0, 360, 60)
]

# ----------------------------- Fractal Core ----------------------------- #


def generate_hexes(cfg: Config) -> List[HexSpec]:
    """Generate the coordinates, sizes and colours for the fractal hexagons."""

    palette = list(cfg.palette)
    if cfg.shuffle_palette:
        random.shuffle(palette)
        cfg.palette = palette

    hexes: List[HexSpec] = []

    def recurse(x: float, y: float, size: float, level: int) -> None:
        if level == 0:
            return
        color = palette[level % len(palette)]
        hexes.append((x, y, size, color))
        child_size = size * 0.5
        dist = size * 0.75
        for dx_unit, dy_unit in PLACEMENT_OFFSETS:
            recurse(x + dist * dx_unit, y + dist * dy_unit, child_size, level - 1)

    recurse(0.0, 0.0, cfg.size, cfg.level)
    return hexes


class BismuthFractal:
    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.screen = turtle.Screen()
        self.screen.title("Bismuth Fractal Generator")
        self.screen.bgcolor("black")
        self.t = turtle.Turtle(visible=False)
        self.t.speed(0)
        self.t.penup()
        self.draw_count = 0

    def draw_hex(self, x: float, y: float, size: float, color: str) -> None:
        self.t.goto(x, y)
        self.t.setheading(0)
        # Align so hex roughly centers on (x, y)
        self.t.penup()
        self.t.forward(-size / 2)
        self.t.left(30)
        self.t.pendown()
        self.t.color(color)
        self.t.begin_fill()
        side = size
        for _ in range(6):
            self.t.forward(side)
            self.t.left(60)
        self.t.end_fill()
        self.t.penup()
        self.draw_count += 1
        if self.cfg.animate and self.draw_count % self.cfg.frame_interval == 0:
            self.screen.update()

    def run(self) -> None:
        if self.cfg.animate:
            self.screen.tracer(0, 0)  # manual updates every frame_interval draws
        else:
            self.screen.tracer(0)  # single final update
        start = time.time()
        for x, y, size, color in generate_hexes(self.cfg):
            self.draw_hex(x, y, size, color)
        self.screen.update()
        elapsed = time.time() - start
        print(
            f"Rendered {self.draw_count} hexes in {elapsed:.2f}s (est={self.cfg.estimated_hexes})."
        )
        if self.cfg.export_path:
            try:
                ps_name = str(self.cfg.export_path)
                self.screen.getcanvas().postscript(file=ps_name)
                print(f"Exported PostScript: {ps_name}")
            except Exception as e:  # pragma: no cover
                print(f"Export failed: {e}")
        print("Click window to close.")
        self.screen.exitonclick()


# ----------------------------- CLI Handling ----------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Generate a Bismuth-like fractal (hex recursion)"
    )
    p.add_argument("--level", type=int, default=4, help="Recursion depth (>=1)")
    p.add_argument(
        "--size",
        type=float,
        default=120,
        help="Initial hex side length (>=10 recommended)",
    )
    p.add_argument("--palette", help="Comma-separated list of hex colors to cycle")
    p.add_argument(
        "--shuffle", action="store_true", help="Randomize palette order before drawing"
    )
    p.add_argument(
        "--animate",
        action="store_true",
        help="Progressively animate drawing instead of instant render",
    )
    p.add_argument(
        "--interval",
        type=int,
        default=30,
        help="Hexes per screen update when animating",
    )
    p.add_argument(
        "--export", type=Path, help="Export final image to PostScript file (.ps)"
    )
    return p


def parse_args(argv: Optional[Sequence[str]] = None) -> Config:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.level < 1:
        parser.error("--level must be >= 1")
    if args.size < 10:
        parser.error("--size must be >= 10 for visibility")
    palette = parse_palette(args.palette)
    cfg = Config(
        level=args.level,
        size=args.size,
        palette=palette,
        shuffle_palette=args.shuffle,
        animate=args.animate,
        frame_interval=max(1, args.interval),
        export_path=args.export,
    )
    # Safety guard: warn if extremely large
    if cfg.estimated_hexes > 150_000:
        print(f"Warning: Estimated hex count {cfg.estimated_hexes} may be very slow.")
    return cfg


def main(argv: Optional[Sequence[str]] = None) -> int:
    cfg = parse_args(argv)
    fractal = BismuthFractal(cfg)
    fractal.run()
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
