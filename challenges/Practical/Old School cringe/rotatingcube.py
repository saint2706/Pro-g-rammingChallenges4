"""rotatingcube.py - Generate an animated GIF of a rotating wireframe cube.

Enhancements:
  * Added CubeConfig dataclass for reproducible configuration
  * CLI exposes frames, fps, size, edge color, vertex color, thickness, seed
  * Deterministic quaternion-based rotation with optional random wobble
  * Precomputes rotation matrices for performance
  * JSON summary export with timing and config (--json)
  * Separate build & save steps; improved error handling
  * Clear math docstrings: quaternion -> rotation matrix & perspective projection
  * Exit codes: 0 success, 1 runtime error, 2 argument error

Example:
  python rotatingcube.py -o cube.gif -f 180 --fps 30 --edge-color "#4ade80" --vertex-color "#fbbf24" --thickness 2
  python rotatingcube.py --seed 42 --json stats.json
"""

from __future__ import annotations

import argparse
import json
import math
import os
import random
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.animation import FuncAnimation

# --------------------------- Configuration --------------------------- #


@dataclass(slots=True)
class CubeConfig:
    frames: int = 120
    fps: int = 24
    width: float = 8.0
    height: float = 6.0
    edge_color: str = "#10b981"  # emerald
    vertex_color: str = "#2563eb"  # blue
    thickness: float = 2.0
    projection_distance: float = -5.0
    seed: int | None = None
    wobble: float = 0.0  # small random quaternion noise amplitude
    json_path: Path | None = None
    output: Path = Path("rotating_cube.gif")

    def validate(self) -> None:
        if self.frames < 1:
            raise ValueError("frames must be >= 1")
        if self.fps < 1 or self.fps > 120:
            raise ValueError("fps must be between 1 and 120")
        if self.thickness <= 0:
            raise ValueError("thickness must be > 0")
        if self.width <= 0 or self.height <= 0:
            raise ValueError("width/height must be > 0")


# --------------------------- Core Animator --------------------------- #


class CubeAnimator:
    """Generate frames of a rotating cube using quaternion-derived rotation matrices."""

    def __init__(self, cfg: CubeConfig):
        self.cfg = cfg
        self.fig, self.ax = plt.subplots(figsize=(cfg.width, cfg.height))
        self.vertices = np.array(
            [[x, y, z] for x in (-1, 1) for y in (-1, 1) for z in (-1, 1)], dtype=float
        )
        self.edges = [
            (i, j)
            for i in range(8)
            for j in range(i + 1, 8)
            if int(np.sum(np.abs(self.vertices[i] - self.vertices[j]))) == 2
        ]
        self._rotations = self._precompute_rotations()

    # ---------------- Rotation Math ---------------- #
    @staticmethod
    def _quaternion_to_matrix(q_vec: np.ndarray) -> np.ndarray:
        """Convert a quaternion vector (x,y,z with computed w) into a 3x3 rotation matrix.

        We treat the input 3-vector as the imaginary part (x,y,z) and derive w to keep unit norm.
        When ||v|| > 1 we normalize with a mirrored effect to produce an exaggerated rotation.
        """
        x, y, z = q_vec
        norm_sq = float(np.dot(q_vec, q_vec))
        if norm_sq < 1.0:
            w = math.sqrt(1.0 - norm_sq)
        else:  # beyond unit sphere: invert & damp for stylistic effect
            inv = 1.0 / norm_sq
            w = 0.0
            x, y, z = -x * inv, -y * inv, -z * inv
        xx, yy, zz = x * x, y * y, z * z
        xy, xz, yz = x * y, x * z, y * z
        wx, wy, wz = w * x, w * y, w * z
        return np.array(
            [
                [1 - 2 * (yy + zz), 2 * (xy - wz), 2 * (xz + wy)],
                [2 * (xy + wz), 1 - 2 * (xx + zz), 2 * (yz - wx)],
                [2 * (xz - wy), 2 * (yz + wx), 1 - 2 * (xx + yy)],
            ]
        )

    def _precompute_rotations(self) -> List[np.ndarray]:
        """Precompute rotation matrices for each frame for performance & determinism."""
        mats: List[np.ndarray] = []
        rng = random.Random(self.cfg.seed)
        for f in range(self.cfg.frames):
            angle = 2 * math.pi * (f / self.cfg.frames)
            base = 0.5 * np.sin(angle * np.array([1.0, 2.0, 3.0]))
            if self.cfg.wobble > 0:
                noise = np.array(
                    [rng.uniform(-self.cfg.wobble, self.cfg.wobble) for _ in range(3)]
                )
                base += noise
            mats.append(self._quaternion_to_matrix(base))
        return mats

    # ---------------- Projection ---------------- #
    def _project(self, verts: np.ndarray) -> np.ndarray:
        """Perspective project 3D vertices to 2D plane given projection distance."""
        d = self.cfg.projection_distance
        denom = verts[:, [2]] - d
        return verts[:, :2] / denom

    # ---------------- Frame Update ---------------- #
    def _draw_frame(self, frame_index: int):  # return list[Artist] for FuncAnimation
        self.ax.clear()
        rot = self._rotations[frame_index]
        rotated = self.vertices @ rot
        proj = self._project(rotated)
        artists = []
        for a, b in self.edges:
            (line,) = self.ax.plot(
                proj[[a, b], 0],
                proj[[a, b], 1],
                color=self.cfg.edge_color,
                lw=self.cfg.thickness,
            )
            artists.append(line)
        scatter = self.ax.scatter(
            proj[:, 0], proj[:, 1], color=self.cfg.vertex_color, s=25
        )
        artists.append(scatter)
        self.ax.set_aspect("equal", "box")
        self.ax.set_xlim(-0.6, 0.6)
        self.ax.set_ylim(-0.6, 0.6)
        self.ax.axis("off")
        self.ax.set_title(f"Frame {frame_index + 1}/{self.cfg.frames}")
        return artists

    def build_animation(self) -> FuncAnimation:
        return FuncAnimation(
            self.fig,
            self._draw_frame,
            frames=self.cfg.frames,
            interval=1000 / self.cfg.fps,
            blit=False,
            repeat=False,
        )

    def save(self) -> Tuple[bool, float]:
        start = time.time()
        anim = self.build_animation()
        try:
            anim.save(str(self.cfg.output), writer="pillow", fps=self.cfg.fps)
            ok = True
        except Exception as e:  # pragma: no cover
            print(f"Error saving animation: {e}", file=sys.stderr)
            print(
                "Hint: Ensure 'pillow' is installed (pip install pillow)",
                file=sys.stderr,
            )
            ok = False
        finally:
            plt.close(self.fig)
        return ok, time.time() - start


# --------------------------- CLI --------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Generate an animated GIF of a rotating cube (wireframe)."
    )
    p.add_argument(
        "-o",
        "--output",
        default="rotating_cube.gif",
        help="Output GIF path (default rotating_cube.gif)",
    )
    p.add_argument(
        "-f", "--frames", type=int, default=120, help="Number of frames (default 120)"
    )
    p.add_argument("--fps", type=int, default=24, help="Frames per second (default 24)")
    p.add_argument(
        "--edge-color", default="#10b981", help="Edge color (hex or Matplotlib name)"
    )
    p.add_argument(
        "--vertex-color", default="#2563eb", help="Vertex color (default #2563eb)"
    )
    p.add_argument(
        "--thickness", type=float, default=2.0, help="Edge line thickness (default 2.0)"
    )
    p.add_argument(
        "--wobble",
        type=float,
        default=0.0,
        help="Random quaternion noise amplitude (default 0)",
    )
    p.add_argument("--seed", type=int, help="Random seed for deterministic animation")
    p.add_argument(
        "--proj-dist",
        type=float,
        default=-5.0,
        help="Projection distance (default -5.0)",
    )
    p.add_argument(
        "--size",
        type=float,
        nargs=2,
        metavar=("W", "H"),
        default=(8.0, 6.0),
        help="Figure size in inches (W H)",
    )
    p.add_argument("--json", type=Path, help="Write JSON summary stats to path")
    return p


def parse_args(argv: Sequence[str] | None) -> CubeConfig:
    parser = build_parser()
    a = parser.parse_args(argv)
    cfg = CubeConfig(
        frames=a.frames,
        fps=a.fps,
        width=a.size[0],
        height=a.size[1],
        edge_color=a.edge_color,
        vertex_color=a.vertex_color,
        thickness=a.thickness,
        projection_distance=a.proj_dist,
        wobble=a.wobble,
        seed=a.seed,
        json_path=a.json,
        output=Path(a.output),
    )
    try:
        cfg.validate()
    except ValueError as e:
        print(f"Argument error: {e}", file=sys.stderr)
        raise SystemExit(2)
    return cfg


# --------------------------- Orchestration --------------------------- #


def main(argv: Sequence[str] | None = None) -> int:
    cfg = parse_args(argv)
    if cfg.seed is not None:
        random.seed(cfg.seed)
        np.random.seed(cfg.seed % (2**32 - 1))
    animator = CubeAnimator(cfg)
    ok, elapsed = animator.save()
    print(
        f"Frames: {cfg.frames} | FPS: {cfg.fps} | Time: {elapsed:.2f}s | Output: {cfg.output}"
    )
    if cfg.json_path:
        payload = {
            "frames": cfg.frames,
            "fps": cfg.fps,
            "edge_color": cfg.edge_color,
            "vertex_color": cfg.vertex_color,
            "thickness": cfg.thickness,
            "projection_distance": cfg.projection_distance,
            "wobble": cfg.wobble,
            "seed": cfg.seed,
            "output": str(cfg.output),
            "elapsed_sec": round(elapsed, 3),
            "success": ok,
        }
        try:
            with open(cfg.json_path, "w", encoding="utf-8") as fh:
                json.dump(payload, fh, indent=2)
            print(f"JSON summary written to {cfg.json_path}")
        except OSError as e:
            print(f"Warning: Could not write JSON summary: {e}", file=sys.stderr)
    return 0 if ok else 1


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
