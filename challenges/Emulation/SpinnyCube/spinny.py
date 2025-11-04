"""spinny.py – VPython spinning cube demo

This script uses the VPython library to create and display a spinning 3D cube.
It provides a simple, interactive visualization of a 3D object in motion.
"""

from __future__ import annotations

import argparse
import math
import os
import sys
from dataclasses import dataclass
from typing import Any, Optional

# VPython objects are imported dynamically to provide a better error message.
scene: Any | None = None
box: Any | None = None
rate: Any | None = None
vector: Any | None = None

DEFAULT_ROT_SPEED = 0.005  # Radians per frame at 50 FPS.


@dataclass(slots=True)
class Config:
    """Configuration for the spinning cube simulation.

    Attributes:
        rotation_speed: A multiplier for the rotation speed.
        fps: The target frames per second.
        duration: An optional duration to run the simulation for.
        reduced_motion: Whether to run in a reduced-motion mode.
    """

    rotation_speed: float
    fps: int
    duration: Optional[float]
    reduced_motion: bool


def build_parser() -> argparse.ArgumentParser:
    """Builds the command-line argument parser."""
    p = argparse.ArgumentParser(description="VPython spinning cube demo.")
    p.add_argument(
        "--speed", type=float, default=1.0, help="Speed multiplier for rotation."
    )
    p.add_argument("--fps", type=int, default=50, help="Target frames per second.")
    p.add_argument(
        "--duration", type=float, help="Optional duration in seconds to run for."
    )
    p.add_argument(
        "--reduced",
        action="store_true",
        help="Start in reduced-motion mode (slower rotation).",
    )
    return p


def setup_scene() -> None:
    """Imports VPython and sets up the scene."""
    global scene, box, rate, vector

    if scene is None:
        try:
            import vpython
        except ImportError:
            print("Error: The 'vpython' library is required.", file=sys.stderr)
            print("Install it with: pip install vpython", file=sys.stderr)
            raise SystemExit(1)

        scene = vpython.scene
        box = vpython.box
        rate = vpython.rate
        vector = vpython.vector

    scene.title = "Spinny Cube"
    scene.range = 2
    scene.caption = "Drag with the right mouse button to rotate the camera. Scroll to zoom."


def run(config: Config) -> int:
    """Runs the main simulation loop.

    Args:
        config: The configuration for the simulation.

    Returns:
        An integer exit code.
    """
    cube = box()  # Create a default cube at the origin.

    # Set an initial isometric view.
    angle = math.pi / 4
    cube.rotate(angle=angle, axis=vector(1, 0, 0))
    cube.rotate(angle=angle, axis=vector(0, 0, 1))

    base_speed = DEFAULT_ROT_SPEED * config.rotation_speed
    if config.reduced_motion:
        base_speed *= 0.25

    frame_time = 1.0 / max(1, config.fps)
    elapsed = 0.0

    try:
        while True:
            rate(config.fps)  # Limit the loop to the target frame rate.
            cube.rotate(angle=base_speed, axis=vector(0, 1, 0))
            elapsed += frame_time
            if config.duration and elapsed >= config.duration:
                print("Duration reached; exiting.")
                break
    except KeyboardInterrupt:
        print("\nInterrupted by user.")
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        return 1
    return 0


def main(argv: Optional[list[str]] = None) -> int:
    """The main entry point for the script."""
    parser = build_parser()
    args = parser.parse_args(argv)
    reduced_env = os.getenv("SPINNY_REDUCED") == "1"
    config = Config(
        rotation_speed=args.speed,
        fps=args.fps,
        duration=args.duration,
        reduced_motion=args.reduced or reduced_env,
    )
    setup_scene()
    return run(config)


if __name__ == "__main__":
    raise SystemExit(main())
