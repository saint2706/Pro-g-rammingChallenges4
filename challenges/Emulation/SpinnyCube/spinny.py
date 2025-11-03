"""spinny.py – VPython spinning cube demo

Modernizations:
  * argparse for speed factor & optional duration limit
  * Type hints & structured functions
  * Graceful exit with KeyboardInterrupt handling
  * Optional frame rate configuration
  * Reduced motion support via environment variable SPINNY_REDUCED=1
  * Clear inline documentation for educational purposes
"""

from __future__ import annotations

import argparse
import math
import os
import sys
from dataclasses import dataclass
from typing import Any, Optional

scene: Any | None = None
box: Any | None = None
rate: Any | None = None
vector: Any | None = None

DEFAULT_ROT_SPEED = 0.005  # radians per frame at 50 fps (~0.25 rad/sec)


@dataclass(slots=True)
class Config:
    rotation_speed: float
    fps: int
    duration: Optional[float]
    reduced_motion: bool


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="VPython spinning cube demo")
    p.add_argument(
        "--speed", type=float, default=1.0, help="Speed multiplier for rotation"
    )
    p.add_argument("--fps", type=int, default=50, help="Target frames per second")
    p.add_argument(
        "--duration", type=float, help="Optional duration in seconds then exit"
    )
    p.add_argument(
        "--reduced",
        action="store_true",
        help="Start in reduced-motion mode (slower & fewer rotations)",
    )
    return p


def setup_scene() -> None:
    global scene, box, rate, vector

    if scene is None or box is None or rate is None or vector is None:
        try:  # dependency guard
            import vpython
        except ImportError:  # pragma: no cover - environment dependent
            print(
                "Error: The 'vpython' library is required to run this script.",
                file=sys.stderr,
            )
            print("Install with: pip install vpython", file=sys.stderr)
            raise SystemExit(1)

        scene = vpython.scene  # type: ignore[attr-defined]
        box = vpython.box  # type: ignore[attr-defined]
        rate = vpython.rate  # type: ignore[attr-defined]
        vector = vpython.vector  # type: ignore[attr-defined]

    assert scene is not None
    assert box is not None
    assert rate is not None
    assert vector is not None

    scene.title = "Spinny Cube"
    scene.range = 2
    scene.caption = (
        "Drag (right mouse) to rotate camera. Scroll to zoom.\n"
        "Press Ctrl+C in terminal or close window to exit."
    )


def run(config: Config) -> int:
    cube = box()  # Create cube at origin

    # Initial orientation for isometric view
    ang = math.pi / 4
    cube.rotate(angle=ang, axis=vector(1, 0, 0))
    cube.rotate(angle=ang, axis=vector(0, 0, 1))

    base_speed = DEFAULT_ROT_SPEED * config.rotation_speed
    if config.reduced_motion:
        base_speed *= 0.25  # slow down

    frame_time = 1.0 / max(1, config.fps)
    elapsed = 0.0

    try:
        while True:
            rate(config.fps)
            cube.rotate(angle=base_speed, axis=vector(0, 1, 0))
            elapsed += frame_time
            if config.duration is not None and elapsed >= config.duration:
                print("Duration reached; exiting.")
                break
    except KeyboardInterrupt:
        print("\nInterrupted by user.")
    except Exception as e:  # pragma: no cover
        print(f"Unexpected termination: {e}", file=sys.stderr)
        return 1
    return 0


def main(argv: Optional[list[str]] = None) -> int:
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


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
