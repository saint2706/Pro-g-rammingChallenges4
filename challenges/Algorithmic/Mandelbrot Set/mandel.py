"""Mandelbrot Set Generator (modernized)
=======================================

Features:
 - Configurable via CLI (size, iterations, center/scale OR explicit bounds).
 - Optional smooth coloring using continuous potential: n + 1 - log(log|z|)/log 2.
 - Save image to file and/or display; headless mode supported.
 - Choice of colormap and optional disabling of the colorbar.
 - Returns normalized float array (0..1) when smoothing enabled for nicer palettes.
 - Explicit dataclass for configuration to ease extension.

Examples:
  python mandel.py --width 1200 --height 800 --max-iter 800 --center -0.75 0 --scale 3.0
  python mandel.py --bounds -2 1 -1.5 1.5 --cmap plasma --save mandel.png
  python mandel.py --smooth --max-iter 1500 --cmap turbo --no-bar --headless --save hi.png

Performance tips:
  - Larger images & high iteration counts scale linearly with width*height*iterations (with early escapes helping).
  - Smooth coloring adds a log + abs per escape update but often visually worth it.
  - Use NumPy wheels (compiled) for speed; consider numba for further optimization if needed.
"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from typing import Optional, Tuple
import sys

import numpy as np
import matplotlib.pyplot as plt


@dataclass
class MandelbrotConfig:
    width: int = 800
    height: int = 600
    max_iter: int = 500
    # Either supply bounds OR center+scale (scale is width of real axis window)
    x_min: Optional[float] = None
    x_max: Optional[float] = None
    y_min: Optional[float] = None
    y_max: Optional[float] = None
    center_x: float = -0.75
    center_y: float = 0.0
    scale: float = 3.0  # Real-axis span when bounds not provided
    smooth: bool = False
    cmap: str = "hot"
    save_path: Optional[str] = None
    show: bool = True
    show_bar: bool = True

    def compute_bounds(self) -> Tuple[float, float, float, float]:
        if None not in (self.x_min, self.x_max, self.y_min, self.y_max):
            return (
                float(self.x_min),
                float(self.x_max),
                float(self.y_min),
                float(self.y_max),
            )  # type: ignore[arg-type]
        # Derive bounds from center & scale keeping aspect ratio
        aspect = self.height / self.width
        half_w = self.scale / 2.0
        half_h = half_w * aspect
        return (
            self.center_x - half_w,
            self.center_x + half_w,
            self.center_y - half_h,
            self.center_y + half_h,
        )

    def validate(self) -> None:
        if self.width <= 0 or self.height <= 0:
            raise ValueError("width/height must be positive")
        if self.max_iter <= 0:
            raise ValueError("max_iter must be positive")
        if self.scale <= 0:
            raise ValueError("scale must be positive")


def generate_mandelbrot(
    cfg: MandelbrotConfig,
) -> Tuple[np.ndarray, Tuple[float, float, float, float]]:
    x_min, x_max, y_min, y_max = cfg.compute_bounds()
    # Create grid of complex numbers
    real = np.linspace(x_min, x_max, cfg.width)
    imag = np.linspace(y_min, y_max, cfg.height)
    c = real[np.newaxis, :] + 1j * imag[:, np.newaxis]
    z = np.zeros_like(c)

    # Array to store iteration counts or smooth values
    if cfg.smooth:
        # store float normalized later
        result = np.full(c.shape, cfg.max_iter, dtype=float)
    else:
        result = np.zeros(c.shape, dtype=int)

    escaped = np.zeros(c.shape, dtype=bool)

    for n in range(cfg.max_iter):
        # Only update points not yet escaped
        mask = ~escaped
        if not mask.any():
            break
        z[mask] = z[mask] * z[mask] + c[mask]
        abs_z = np.abs(z)
        newly_escaped = (abs_z > 2.0) & (~escaped)
        if newly_escaped.any():
            if cfg.smooth:
                # continuous potential coloring
                # n + 1 - log(log|z|)/log 2
                nu = n + 1 - np.log(np.log(abs_z[newly_escaped])) / np.log(2.0)
                result[newly_escaped] = nu
            else:
                result[newly_escaped] = n
            escaped[newly_escaped] = True

    if cfg.smooth:
        # If no point escaped, all values remain max_iter -> treat as fully inside (all 1.0)
        escaped_values = result[escaped]
        if escaped_values.size == 0:
            result[:] = 1.0
        else:
            max_val = escaped_values.max()
            if max_val <= 0:
                result[:] = 1.0
            else:
                norm = np.empty_like(result)
                # Interior points (never escaped) get 1.0
                norm[~escaped] = 1.0
                # Escaped points scaled to (0,1)
                esc = result[escaped] / max_val
                norm[escaped] = esc
                result = norm
    else:
        # Points that never escaped stay at max_iter; others keep their escape iteration
        result[(result == 0) & (~escaped)] = cfg.max_iter

    return result, (x_min, x_max, y_min, y_max)


def plot_mandel(
    result: np.ndarray, bounds: Tuple[float, float, float, float], cfg: MandelbrotConfig
) -> None:
    if not cfg.show and not cfg.save_path:
        return
    x_min, x_max, y_min, y_max = bounds
    plt.figure(figsize=(10, 8))
    im = plt.imshow(
        result,
        cmap=cfg.cmap,
        extent=(x_min, x_max, y_min, y_max),
        origin="lower",
        interpolation="bilinear" if cfg.smooth else "nearest",
    )
    if cfg.show_bar:
        label = "Normalized" if cfg.smooth else "Iterations to Escape"
        plt.colorbar(im, label=label)
    plt.title("Mandelbrot Set" + (" (smooth)" if cfg.smooth else ""))
    plt.xlabel("Re(c)")
    plt.ylabel("Im(c)")
    plt.tight_layout()
    if cfg.save_path:
        try:
            plt.savefig(cfg.save_path, dpi=250)
            print(f"[info] Saved image -> {cfg.save_path}")
        except Exception as e:
            print(f"[error] Could not save image: {e}", file=sys.stderr)
    if cfg.show:
        plt.show()
    else:
        plt.close()


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Generate Mandelbrot set images with optional smooth coloring."
    )
    p.add_argument("--width", type=int, default=800)
    p.add_argument("--height", type=int, default=600)
    p.add_argument("--max-iter", type=int, default=500)
    group = p.add_mutually_exclusive_group()
    group.add_argument(
        "--bounds",
        nargs=4,
        type=float,
        metavar=("XMIN", "XMAX", "YMIN", "YMAX"),
        help="Explicit bounds",
    )
    group.add_argument(
        "--center",
        nargs=2,
        type=float,
        metavar=("CX", "CY"),
        help="Center point (used with --scale)",
    )
    p.add_argument(
        "--scale",
        type=float,
        default=3.0,
        help="Real-axis span when using --center (default 3.0)",
    )
    p.add_argument("--smooth", action="store_true", help="Enable smooth coloring")
    p.add_argument("--cmap", type=str, default="hot", help="Matplotlib colormap name")
    p.add_argument("--save", type=str, help="Path to save image (PNG)")
    p.add_argument(
        "--no-show", action="store_true", help="Do not display window (headless)"
    )
    p.add_argument("--no-bar", action="store_true", help="Disable colorbar")
    return p


def parse_args(argv=None) -> MandelbrotConfig:
    args = build_arg_parser().parse_args(argv)
    cfg = MandelbrotConfig(
        width=args.width,
        height=args.height,
        max_iter=args.max_iter,
        smooth=args.smooth,
        cmap=args.cmap,
        save_path=args.save,
        show=not args.no_show,
        show_bar=not args.no_bar,
    )
    if args.bounds:
        cfg.x_min, cfg.x_max, cfg.y_min, cfg.y_max = args.bounds
    if args.center:
        cfg.center_x, cfg.center_y = args.center
    cfg.scale = args.scale
    cfg.validate()
    return cfg


def main(argv=None):  # pragma: no cover (manual entry)
    cfg = parse_args(argv)
    result, bounds = generate_mandelbrot(cfg)
    plot_mandel(result, bounds, cfg)


if __name__ == "__main__":  # pragma: no cover
    main()
