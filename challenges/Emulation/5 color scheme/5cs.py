"""5cs.py — Dominant Color Palette Extractor
=================================================
Extract the top N dominant colors from an image using K-Means clustering.

Educational Goals:
 - Demonstrate basic image loading and preprocessing (OpenCV, NumPy)
 - Show unsupervised learning with K-Means (scikit-learn)
 - Provide optional performance optimizations (pixel sampling, resize)
 - Offer multiple output formats (RGB, HEX, JSON) and palette saving

Example:
    python 5cs.py -i photo.jpg -n 8 --hex --json --save-palette palette.png \
        --max-pixels 50000 --resize 800x800

The script is safe to import; the CLI only runs under the ``if __name__ == '__main__'`` guard.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from dataclasses import dataclass
from typing import Iterable, List, Tuple

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# Dependency checks (lazy errors are confusing for newcomers)
try:  # pragma: no cover - import side effects not unit-tested
    import cv2  # type: ignore
    from sklearn.cluster import KMeans  # type: ignore
except ImportError as e:  # pragma: no cover
    lib = str(e).split("'")[1]
    print(f"Error: Missing dependency '{lib}'.", file=sys.stderr)
    if lib == "cv2":
        print("Install with: pip install opencv-python", file=sys.stderr)
    else:
        print("Install with: pip install scikit-learn", file=sys.stderr)
    sys.exit(1)


# --------------------------- Data Structures --------------------------- #


@dataclass(slots=True)
class Config:
    """Runtime configuration for palette extraction.

    Attributes:
        image: Path to the input image file.
        num_colors: Number of dominant colors to extract (clusters).
        hex_output: Whether to print colors as hex codes.
        json_output: Whether to emit a JSON summary.
        save_palette: Optional path to save a PNG palette image.
        max_pixels: If set, randomly sample at most this many pixels for K-Means (speed).
        resize: Optional (width, height) to resize image before processing (speeds up & normalizes scale).
    """

    image: str
    num_colors: int = 5
    hex_output: bool = False
    json_output: bool = False
    save_palette: str | None = None
    max_pixels: int | None = None
    resize: Tuple[int, int] | None = None


# --------------------------- Helper Functions -------------------------- #


def load_image(path: str, resize: Tuple[int, int] | None = None) -> np.ndarray:
    """Load an image as RGB.

    Args:
        path: Path to image file.
        resize: Optional (width, height) to resize using area interpolation (good for downscaling).

    Raises:
        FileNotFoundError: If the file does not exist.
        ValueError: If OpenCV cannot decode the file.
    """
    if not os.path.exists(path):
        raise FileNotFoundError(f"Image not found: {path}")
    img = cv2.imread(path)
    if img is None:
        raise ValueError(f"Failed to load image (unsupported/corrupt): {path}")
    if resize:
        w, h = resize
        if w <= 0 or h <= 0:
            raise ValueError("Resize dimensions must be positive integers.")
        img = cv2.resize(img, (w, h), interpolation=cv2.INTER_AREA)
    # Convert BGR -> RGB
    return cv2.cvtColor(img, cv2.COLOR_BGR2RGB)


def sample_pixels(image: np.ndarray, max_pixels: int | None) -> np.ndarray:
    """Return a (N,3) array of pixels, optionally randomly subsampling.

    Subsampling greatly speeds up clustering on large images with negligible palette difference.
    """
    pixels = image.reshape(-1, 3)
    if max_pixels and pixels.shape[0] > max_pixels:
        idx = np.random.choice(pixels.shape[0], size=max_pixels, replace=False)
        pixels = pixels[idx]
    return pixels


def extract_colors(pixels: np.ndarray, k: int) -> np.ndarray:
    """Cluster pixels into k dominant colors using K-Means.

    Returns array of shape (k,3) dtype uint8.
    """
    if k <= 0:
        raise ValueError("num_colors must be > 0")
    # Use an explicit integer for ``n_init`` for compatibility with older scikit-learn versions
    # where the string value "auto" is not supported.
    kmeans = KMeans(n_clusters=k, n_init=10, random_state=42)
    kmeans.fit(pixels)
    return kmeans.cluster_centers_.astype(np.uint8)


def rgb_to_hex(color: Iterable[int]) -> str:
    """Convert an (R,G,B) iterable to '#RRGGBB'."""
    r, g, b = (int(c) for c in color)
    return f"#{r:02X}{g:02X}{b:02X}"


def display_palette(
    colors: np.ndarray, title: str, save_path: str | None = None
) -> None:
    """Render a horizontal color palette with RGB & HEX labels.

    If save_path is provided, saves a PNG and does not block the script unnecessarily.
    """
    num_colors = len(colors)
    fig, ax = plt.subplots(figsize=(max(4, num_colors * 1.4), 1.8), dpi=130)
    for i, color in enumerate(colors):
        rect = patches.Rectangle((i, 0), 1, 1, color=color / 255.0)
        ax.add_patch(rect)
        ax.text(
            i + 0.5,
            -0.15,
            rgb_to_hex(color),
            ha="center",
            va="center",
            fontsize=8,
            fontweight="bold",
        )
        ax.text(
            i + 0.5,
            1.05,
            f"RGB{tuple(int(c) for c in color)}",
            ha="center",
            va="bottom",
            fontsize=7,
        )
    ax.set_xlim(0, num_colors)
    ax.set_ylim(-0.3, 1.2)
    ax.set_axis_off()
    ax.set_title(title, fontsize=10)
    plt.tight_layout()
    if save_path:
        plt.savefig(save_path, bbox_inches="tight")
        print(f"Saved palette image to: {save_path}")
        plt.close(fig)
    else:  # Interactive usage
        plt.show()


def build_json_summary(cfg: Config, colors: np.ndarray) -> str:
    """Return a JSON string summarizing extraction results."""
    payload = {
        "image": cfg.image,
        "num_colors": cfg.num_colors,
        "resize": cfg.resize,
        "max_pixels": cfg.max_pixels,
        "colors": [
            {
                "rgb": [int(c) for c in color],
                "hex": rgb_to_hex(color),
                "index": i,
            }
            for i, color in enumerate(colors)
        ],
    }
    return json.dumps(payload, indent=2)


# --------------------------- Core Workflow ----------------------------- #


def run(cfg: Config) -> np.ndarray:
    """Execute the extraction pipeline and return dominant colors array."""
    image = load_image(cfg.image, resize=cfg.resize)
    pixels = sample_pixels(image, cfg.max_pixels)
    colors = extract_colors(pixels, cfg.num_colors)
    return colors


def parse_resize(value: str) -> Tuple[int, int]:
    """Parse a WxH string like '800x600' into a (w,h) tuple."""
    try:
        w_str, h_str = value.lower().split("x")
        return int(w_str), int(h_str)
    except Exception as e:  # noqa: BLE001
        raise argparse.ArgumentTypeError(
            "Resize must be in WxH format, e.g. 800x800"
        ) from e


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Extract a dominant color palette from an image.",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    default_image = os.path.join(os.path.dirname(__file__), "qwFWZv8.jpg")
    parser.add_argument(
        "-i", "--image", default=default_image, help="Path to input image file"
    )
    parser.add_argument(
        "-n",
        "--num-colors",
        dest="num_colors",
        type=int,
        default=5,
        help="Number of colors to extract",
    )
    parser.add_argument("--hex", action="store_true", help="Print colors as HEX codes")
    parser.add_argument(
        "--json", action="store_true", help="Output JSON summary of palette"
    )
    parser.add_argument(
        "--save-palette", metavar="PATH", help="Save palette visualization to PNG path"
    )
    parser.add_argument(
        "--max-pixels",
        type=int,
        metavar="N",
        help="Randomly sample at most N pixels for clustering",
    )
    parser.add_argument(
        "--resize",
        type=parse_resize,
        metavar="WxH",
        help="Resize image before processing",
    )
    return parser


def main(argv: List[str] | None = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)
    cfg = Config(
        image=args.image,
        num_colors=args.num_colors,
        hex_output=args.hex,
        json_output=args.json,
        save_palette=args.save_palette,
        max_pixels=args.max_pixels,
        resize=args.resize,
    )

    try:
        colors = run(cfg)
    except (FileNotFoundError, ValueError) as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    # Sort colors in a deterministic order (optional aesthetic: by brightness)
    colors = np.array(
        sorted(colors, key=lambda c: 0.2126 * c[0] + 0.7152 * c[1] + 0.0722 * c[2])
    )

    print("Dominant Colors (RGB):")
    for i, color in enumerate(colors):
        if cfg.hex_output:
            print(f" {i+1:>2}. {tuple(int(c) for c in color)}  {rgb_to_hex(color)}")
        else:
            print(f" {i+1:>2}. {tuple(int(c) for c in color)}")

    if cfg.json_output:
        print("\nJSON Summary:")
        print(build_json_summary(cfg, colors))

    if cfg.save_palette:
        display_palette(
            colors,
            f"Dominant Color Palette ({cfg.num_colors} colors)",
            cfg.save_palette,
        )
    else:
        # Only display interactively if not saving (avoids blocking in automated contexts)
        display_palette(colors, f"Dominant Color Palette ({cfg.num_colors} colors)")

    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
