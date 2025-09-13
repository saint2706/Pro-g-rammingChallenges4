"""ToH_visualizer.py - Matplotlib animation for Towers of Hanoi states.

Features added:
  * Dataclass config with validation
  * CLI options: --disks, --interval, --colormap, --save, --max-frames, --json, --no-show
  * Optional limiting of frames for large n previews
  * JSON metadata output (frame count, disks) for automation
  * Safe NumPy import handling with user guidance
  * Optional animation saving (MP4 or GIF) if writers available
  * Improved layout & adaptive disk width scaling
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence

import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.animation import FuncAnimation

try:  # Attempt numpy import early
    import numpy as np  # type: ignore
except Exception as e:  # pragma: no cover - environment dependent
    print(
        "Warning: numpy unavailable; colormap gradients may fail. Install with: pip install numpy",
        file=sys.stderr,
    )
    np = None  # type: ignore

try:
    from ToH import hanoi_state_generator
except ImportError:
    print("Error: 'ToH.py' not found in the same directory.", file=sys.stderr)
    sys.exit(1)


@dataclass(slots=True)
class VizConfig:
    disks: int = 4
    interval: int = 400
    colormap: str = "viridis"
    save: Optional[str] = None
    max_frames: Optional[int] = None
    json_output: bool = False
    no_show: bool = False

    def validate(self) -> None:
        if not (1 <= self.disks <= 12):
            raise ValueError("disks must be between 1 and 12")
        if self.interval <= 0:
            raise ValueError("interval must be positive")
        if self.max_frames is not None and self.max_frames < 1:
            raise ValueError("max-frames must be positive")


class HanoiVisualizer:
    def __init__(self, cfg: VizConfig):
        self.cfg = cfg
        self.peg_names = ["A", "B", "C"]
        self.peg_coords = {"A": 1, "B": 3, "C": 5}
        self.steps = list(hanoi_state_generator(cfg.disks, "A", "C", "B"))
        if cfg.max_frames is not None:
            self.steps = self.steps[: cfg.max_frames]
        self.fig, self.ax = plt.subplots(figsize=(10, 6))
        self._setup_plot()
        self.disk_colors = self._compute_colors()

    def _compute_colors(self):
        if np is None:
            return [
                "#%02x%02x%02x" % (50 + i * 10, 100, 180) for i in range(self.cfg.disks)
            ]
        cmap = plt.get_cmap(self.cfg.colormap)
        return cmap(np.linspace(0, 1, self.cfg.disks))

    def _setup_plot(self):
        self.ax.set_xlim(0, 6)
        self.ax.set_ylim(0, self.cfg.disks + 2)
        self.ax.set_aspect("equal")
        self.ax.axis("off")
        for name, x in self.peg_coords.items():
            self.ax.plot([x, x], [0, self.cfg.disks + 1], "black", lw=4)
            self.ax.text(x, self.cfg.disks + 1.1, name, ha="center", fontsize=14)

    def _disk_width(self, disk_num: int) -> float:
        return 0.4 * disk_num + 0.6

    def _draw_disk(self, disk_num: int, peg_name: str, position: int):
        width = self._disk_width(disk_num)
        height = 0.4
        x = self.peg_coords[peg_name] - width / 2
        y = position * height
        color = self.disk_colors[disk_num - 1]
        rect = patches.Rectangle(
            (x, y), width, height, facecolor=color, edgecolor="black"
        )
        self.ax.add_patch(rect)
        self.ax.text(
            self.peg_coords[peg_name],
            y + height / 2,
            str(disk_num),
            ha="center",
            va="center",
            color="white",
            fontsize=10,
        )

    def _update(self, frame_num: int):  # return artists for better blitting hint
        self.ax.clear()
        self._setup_plot()
        state = self.steps[frame_num]
        artists = []
        for peg_name in self.peg_names:
            disks_on_peg = state[peg_name]
            for i, disk_num in enumerate(disks_on_peg):
                self._draw_disk(disk_num, peg_name, i)
        title = self.ax.set_title(
            f"Hanoi | Disks: {self.cfg.disks} | Frame: {frame_num}/{len(self.steps)-1}",
            fontsize=14,
        )
        artists.append(title)
        return artists

    def animate(self):
        frames = len(self.steps)
        anim = FuncAnimation(
            self.fig,
            self._update,
            frames=frames,
            interval=self.cfg.interval,
            repeat=False,
        )
        if self.cfg.save:
            try:
                anim.save(self.cfg.save)
                print(f"Saved animation to {self.cfg.save}")
            except Exception as e:  # pragma: no cover (depends on writer availability)
                print(f"Warning: failed to save animation: {e}", file=sys.stderr)
        if not self.cfg.no_show and not self.cfg.json_output:
            plt.show()
        return frames


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Visualize Towers of Hanoi solution states")
    p.add_argument("--disks", type=int, default=4)
    p.add_argument("--interval", type=int, default=400, help="Frame interval in ms")
    p.add_argument("--colormap", default="viridis", help="Matplotlib colormap")
    p.add_argument("--save", help="Output animation filename (.mp4, .gif)")
    p.add_argument("--max-frames", type=int, help="Limit frames (preview)")
    p.add_argument("--json", action="store_true", help="Emit JSON metadata only")
    p.add_argument("--no-show", action="store_true", help="Do not display window")
    return p


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = VizConfig(
        disks=args.disks,
        interval=args.interval,
        colormap=args.colormap,
        save=args.save,
        max_frames=args.max_frames,
        json_output=args.json,
        no_show=args.no_show,
    )
    try:
        cfg.validate()
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    viz = HanoiVisualizer(cfg)
    frame_count = viz.animate()
    if cfg.json_output:
        print(json.dumps({"disks": cfg.disks, "frames": frame_count}, indent=2))
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
