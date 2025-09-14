"""colorwheel.py - Generate an HSV colour wheel image using colour-science.

Modernization & Added Features:
  * Dataclass `Config` to encapsulate parameters
  * Orientation enumeration with validation (Colour, Matplotlib, Nuke)
  * Optional alpha feathering at edge (--feather) for smoother circular mask
  * CLI options for saving image to file, skipping display, JSON metadata export
  * Verbose mode for insight into processing steps
  * JSON metadata: resolution, method, clipped, feather, output path, elapsed
  * Input validation + clearer errors + exit codes (0 ok, 2 usage error)
  * Modular structure separating generation, plotting, and orientation

Examples:
  python colorwheel.py -s 1024 --method Colour
  python colorwheel.py -s 2048 --no-clip --method Matplotlib --save wheel.png --json meta.json
  python colorwheel.py -s 1500 --feather 6 --save wheel_feathered.png --no-show
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Literal, Optional, Dict, Any

import numpy as np
import matplotlib.pyplot as plt

try:  # dependency guard
    import colour
    from colour.utilities import tstack, tsplit, orient  # type: ignore
    from colour.plotting import colour_style, plot_image  # type: ignore
except ImportError:  # pragma: no cover
    print(
        "Error: The 'colour-science' library is required. Install with: pip install colour-science",
        file=sys.stderr,
    )
    raise SystemExit(2)


class Orientation(str, Enum):
    COLOUR = "Colour"
    MATPLOTLIB = "Matplotlib"
    NUKE = "Nuke"


@dataclass(slots=True)
class Config:
    samples: int = 512
    clip_circle: bool = True
    method: Orientation = Orientation.COLOUR
    save_path: Optional[Path] = None
    json_path: Optional[Path] = None
    feather: int = 0  # pixels for alpha feather
    dpi: int = 100
    show: bool = True
    verbose: bool = False

    def validate(self) -> None:
        if self.samples < 32 or self.samples > 8192:
            raise ValueError("samples must be between 32 and 8192")
        if self.feather < 0 or self.feather > (self.samples // 2):
            raise ValueError("feather must be >=0 and less than half of samples")
        if self.dpi < 40 or self.dpi > 600:
            raise ValueError("dpi must be between 40 and 600")


# ----------------------------- Generation ----------------------------- #


def generate_colour_wheel(cfg: Config) -> np.ndarray:
    """Generate RGBA colour wheel array.

    Steps:
      1. Build coordinate grid in [-1,1]^2.
      2. Convert to polar -> saturation (radius) & hue (angle).
      3. Stack into HSV (value fixed 1) and convert to RGB.
      4. Apply orientation transforms per method.
      5. Clip or square; build alpha (with optional feather edge).

    Returns RGBA float array shape (S,S,4) in range [0,1].
    """
    s = cfg.samples
    lin = np.linspace(-1, 1, s)
    xx, yy = np.meshgrid(lin, lin)
    # radius / saturation & hue angle mapping
    S = np.sqrt(xx**2 + yy**2)
    H = (np.arctan2(xx, yy) + np.pi) / (2 * np.pi)

    HSV = tstack([H, S, np.ones_like(S)])
    RGB = colour.HSV_to_RGB(HSV)

    # Orientation transforms
    if cfg.method == Orientation.MATPLOTLIB:
        RGB = orient(RGB, "90 CW")
        S = orient(S, "90 CW")
    elif cfg.method == Orientation.NUKE:
        RGB = orient(RGB, "Flip")
        RGB = orient(RGB, "90 CW")
        S = orient(S, "Flip")
        S = orient(S, "90 CW")

    # Alpha channel construction
    if cfg.clip_circle:
        A = np.ones_like(S)
        outside = S > 1
        if cfg.feather:
            # Feather: outside still zero; transition region (1-feather_px .. 1)
            r = np.clip((1 - S) * (s / (2 * cfg.feather)), 0, 1)
            A = np.where(S <= 1, np.clip(r, 0, 1), 0)
        else:
            A[outside] = 0
        RGB[S > 1] = 0
    else:
        A = np.ones_like(S)

    R, G, B = tsplit(RGB)
    RGBA = tstack([R, G, B, A])
    return RGBA


# ----------------------------- Plotting ----------------------------- #


def plot_wheel(rgba: np.ndarray, cfg: Config) -> None:
    colour_style()
    plt.style.use("dark_background")
    title = f"HSV Colour Wheel\nMethod: {cfg.method} | Samples: {cfg.samples} | Clipped: {cfg.clip_circle} | Feather: {cfg.feather}px"
    plot_image(rgba, title=title)


# ----------------------------- CLI ----------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Generate & display an HSV colour wheel.")
    p.add_argument(
        "-s",
        "--samples",
        type=int,
        default=512,
        help="Resolution (side length). 32-8192.",
    )
    p.add_argument(
        "--no-clip", dest="clip", action="store_false", help="Do not clip to circle."
    )
    p.add_argument(
        "-m",
        "--method",
        choices=[m.value for m in Orientation],
        default=Orientation.COLOUR.value,
        help="Orientation method.",
    )
    p.add_argument("--save", type=Path, help="Save image to file (PNG recommended).")
    p.add_argument("--json", type=Path, help="Write metadata JSON.")
    p.add_argument(
        "--feather",
        type=int,
        default=0,
        help="Feather width (pixels) for circular alpha edge.",
    )
    p.add_argument("--dpi", type=int, default=120, help="DPI when saving (40-600).")
    p.add_argument(
        "--no-show",
        action="store_true",
        help="Do not display window (headless generation).",
    )
    p.add_argument(
        "-v", "--verbose", action="store_true", help="Verbose progress messages."
    )
    return p


def parse_args(argv: Optional[list[str]]) -> Config:
    parser = build_parser()
    a = parser.parse_args(argv)
    cfg = Config(
        samples=a.samples,
        clip_circle=a.clip,
        method=Orientation(a.method),
        save_path=a.save,
        json_path=a.json,
        feather=a.feather,
        dpi=a.dpi,
        show=not a.no_show,
        verbose=a.verbose,
    )
    try:
        cfg.validate()
    except ValueError as e:
        print(f"Argument error: {e}", file=sys.stderr)
        raise SystemExit(2)
    return cfg


# ----------------------------- Orchestration ----------------------------- #


def main(argv: Optional[list[str]] = None) -> int:
    cfg = parse_args(argv)
    if cfg.verbose:
        print(f"Config: {cfg}")
    t0 = time.time()
    rgba = generate_colour_wheel(cfg)
    elapsed = time.time() - t0
    if cfg.verbose:
        print(f"Generated {cfg.samples}x{cfg.samples} wheel in {elapsed:.3f}s")

    # Save image if requested
    if cfg.save_path:
        try:
            import imageio.v2 as imageio  # lazy import to avoid dependency if unused

            # Convert to 8-bit for saving
            arr8 = np.clip(rgba * 255, 0, 255).astype("uint8")
            imageio.imwrite(cfg.save_path, arr8)
            if cfg.verbose:
                print(f"Saved image to {cfg.save_path}")
        except ImportError:
            print(
                "Warning: imageio not installed. Install with 'pip install imageio' to enable saving.",
                file=sys.stderr,
            )
        except OSError as e:
            print(f"Failed to save image: {e}", file=sys.stderr)

    # Metadata JSON
    if cfg.json_path:
        meta: Dict[str, Any] = {
            "samples": cfg.samples,
            "method": cfg.method.value,
            "clipped": cfg.clip_circle,
            "feather": cfg.feather,
            "elapsed_sec": round(elapsed, 4),
            "saved_image": str(cfg.save_path) if cfg.save_path else None,
        }
        try:
            with open(cfg.json_path, "w", encoding="utf-8") as fh:
                json.dump(meta, fh, indent=2)
            if cfg.verbose:
                print(f"Wrote metadata JSON to {cfg.json_path}")
        except OSError as e:
            print(f"Failed to write JSON: {e}", file=sys.stderr)

    if cfg.show:
        plot_wheel(rgba, cfg)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
