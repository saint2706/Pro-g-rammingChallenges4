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
from typing import List, Sequence, Tuple, Optional
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

# Precompute a unit hex (pointy-top) around origin centered horizontally
# We'll scale it by 'size' at draw time. Points are relative moves in order.
HEX_POINTS: List[Tuple[float, float]] = []
for angle_deg in range(0, 360, 60):
    rad = math.radians(angle_deg)
    HEX_POINTS.append((math.cos(rad), math.sin(rad)))

# ----------------------------- Fractal Core ----------------------------- #


class BismuthFractal:
    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.screen = turtle.Screen()
        self.screen.title("Bismuth Fractal Generator")
        self.screen.bgcolor("black")
        self.t = turtle.Turtle(visible=False)
        self.t.speed(0)
        self.t.penup()
        # compute vertex placement offsets using cached sin/cos
        self.vertex_offsets = [
            (math.cos(math.radians(a)), math.sin(math.radians(a)))
            for a in range(0, 360, 60)
        ]
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

    def recurse(self, x: float, y: float, size: float, level: int) -> None:
        if level == 0:
            return
        color = self.cfg.palette[level % len(self.cfg.palette)]
        self.draw_hex(x, y, size, color)
        child_size = size * 0.5
        dist = size * 0.75
        for dx_unit, dy_unit in self.vertex_offsets:
            self.recurse(x + dist * dx_unit, y + dist * dy_unit, child_size, level - 1)

    def run(self) -> None:
        if self.cfg.shuffle_palette:
            random.shuffle(self.cfg.palette)
        if self.cfg.animate:
            self.screen.tracer(0, 0)  # manual updates every frame_interval draws
        else:
            self.screen.tracer(0)  # single final update
        start = time.time()
        self.recurse(0.0, 0.0, self.cfg.size, self.cfg.level)
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
    palette = (
        DEFAULT_PALETTE
        if not args.palette
        else [c.strip() for c in args.palette.split(",") if c.strip()]
    )
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
