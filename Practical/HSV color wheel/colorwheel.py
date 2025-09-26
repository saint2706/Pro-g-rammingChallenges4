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
from typing import Optional, Dict, Any

import numpy as np
import matplotlib.pyplot as plt

try:  # Optional dependency for colour-science helpers
    import colour  # type: ignore
except ImportError:  # pragma: no cover
    colour = None  # type: ignore

try:  # pragma: no cover - plotting helpers only available with colour-science
    from colour.plotting import colour_style as _colour_style, plot_image as _colour_plot_image  # type: ignore
except ImportError:  # pragma: no cover
    _colour_style = None
    _colour_plot_image = None

try:  # pragma: no cover - prefer colour utilities when present
    from colour.utilities import orient as _colour_orient  # type: ignore
except ImportError:  # pragma: no cover
    _colour_orient = None


COLOUR_SCIENCE_AVAILABLE = colour is not None
COLOUR_SCIENCE_GUIDANCE = (
    "The optional 'colour-science' library is not installed. Install it with "
    "`pip install colour-science` to enable colour-science plotting helpers."
)


def _orient(array: np.ndarray, how: str) -> np.ndarray:
    """Rotate/flip helper mirroring colour.utilities.orient."""

    if _colour_orient is not None:  # pragma: no cover - defer to library implementation
        return _colour_orient(array, how)

    if how == "90 CW":
        return np.rot90(array, k=-1, axes=(0, 1))
    if how == "Flip":
        return np.flipud(array)
    raise ValueError(f"Unsupported orientation operation: {how}")


def _hsv_to_rgb(hsv: np.ndarray) -> np.ndarray:
    """Vectorised HSV→RGB conversion with optional colour-science acceleration."""

    if COLOUR_SCIENCE_AVAILABLE:  # pragma: no branch - fast path when dependency available
        return colour.HSV_to_RGB(hsv)  # type: ignore[attr-defined]

    h = hsv[..., 0]
    s = np.clip(hsv[..., 1], 0.0, 1.0)
    v = np.clip(hsv[..., 2], 0.0, 1.0)

    h6 = (h % 1.0) * 6.0
    i = np.floor(h6).astype(np.int32) % 6
    f = h6 - np.floor(h6)

    p = v * (1.0 - s)
    q = v * (1.0 - f * s)
    t = v * (1.0 - (1.0 - f) * s)

    r = np.empty_like(v)
    g = np.empty_like(v)
    b = np.empty_like(v)

    mask = i == 0
    r[mask], g[mask], b[mask] = v[mask], t[mask], p[mask]

    mask = i == 1
    r[mask], g[mask], b[mask] = q[mask], v[mask], p[mask]

    mask = i == 2
    r[mask], g[mask], b[mask] = p[mask], v[mask], t[mask]

    mask = i == 3
    r[mask], g[mask], b[mask] = p[mask], q[mask], v[mask]

    mask = i == 4
    r[mask], g[mask], b[mask] = t[mask], p[mask], v[mask]

    mask = i == 5
    r[mask], g[mask], b[mask] = v[mask], p[mask], q[mask]

    return np.stack([r, g, b], axis=-1)


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

    HSV = np.stack([H, np.clip(S, 0, 1), np.ones_like(S)], axis=-1, dtype=np.float32)
    RGB = _hsv_to_rgb(HSV)

    # Orientation transforms
    if cfg.method == Orientation.MATPLOTLIB:
        RGB = _orient(RGB, "90 CW")
        S = _orient(S, "90 CW")
    elif cfg.method == Orientation.NUKE:
        RGB = _orient(RGB, "Flip")
        RGB = _orient(RGB, "90 CW")
        S = _orient(S, "Flip")
        S = _orient(S, "90 CW")

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

    rgba = np.stack([RGB[..., 0], RGB[..., 1], RGB[..., 2], A], axis=-1)
    return np.ascontiguousarray(rgba.astype(np.float32))


# ----------------------------- Plotting ----------------------------- #


def plot_wheel(rgba: np.ndarray, cfg: Config) -> None:
    if _colour_style is not None:  # pragma: no branch - apply style if available
        _colour_style()
    plt.style.use("dark_background")
    title = (
        "HSV Colour Wheel\n"
        f"Method: {cfg.method} | Samples: {cfg.samples} | Clipped: {cfg.clip_circle} | Feather: {cfg.feather}px"
    )

    if _colour_plot_image is not None:  # pragma: no branch - prefer colour-science visualisation
        _colour_plot_image(rgba, title=title)
        return

    plt.figure(figsize=(6, 6), dpi=cfg.dpi)
    plt.imshow(np.clip(rgba, 0.0, 1.0))
    plt.title(title)
    plt.axis("off")
    plt.tight_layout()
    plt.show()


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
        if not COLOUR_SCIENCE_AVAILABLE:
            print(COLOUR_SCIENCE_GUIDANCE, file=sys.stderr)
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
