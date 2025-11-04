"""5cs.py — Dominant Color Palette Extractor
=================================================
Extract the top N dominant colors from an image using K-Means clustering.

This script provides a command-line interface to identify and extract the most
prominent colors from an image. It uses the K-Means clustering algorithm
from scikit-learn to group pixels into a specified number of clusters,
representing the dominant colors.

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
    """Load an image and convert it to RGB format.

    Args:
        path: The file path to the image.
        resize: An optional tuple of (width, height) to resize the image.

    Returns:
        A NumPy array representing the image in RGB format.

    Raises:
        FileNotFoundError: If the image file does not exist.
        ValueError: If the image cannot be loaded or if resize dimensions are invalid.
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
        # Use area interpolation for downscaling to avoid artifacts.
        img = cv2.resize(img, (w, h), interpolation=cv2.INTER_AREA)
    # Convert from BGR (OpenCV's default) to RGB.
    return cv2.cvtColor(img, cv2.COLOR_BGR2RGB)


def sample_pixels(image: np.ndarray, max_pixels: int | None) -> np.ndarray:
    """Extract pixels from an image, with optional random subsampling.

    This function reshapes the image into a 2D array of pixels. If `max_pixels`
    is set and the total number of pixels exceeds this value, it will randomly
    sample the pixels to improve performance.

    Args:
        image: A NumPy array representing the image.
        max_pixels: The maximum number of pixels to sample.

    Returns:
        A NumPy array of pixels with shape (N, 3).
    """
    pixels = image.reshape(-1, 3)
    if max_pixels and pixels.shape[0] > max_pixels:
        # Randomly sample pixels to speed up clustering on large images.
        idx = np.random.choice(pixels.shape[0], size=max_pixels, replace=False)
        pixels = pixels[idx]
    return pixels


def extract_colors(pixels: np.ndarray, k: int) -> np.ndarray:
    """Extract k dominant colors from a list of pixels using K-Means clustering.

    Args:
        pixels: A NumPy array of pixels with shape (N, 3).
        k: The number of clusters (dominant colors) to find.

    Returns:
        A NumPy array of the cluster centers (dominant colors) with shape (k, 3).

    Raises:
        ValueError: If `k` is not a positive integer.
    """
    if k <= 0:
        raise ValueError("num_colors must be > 0")
    # n_init is set to 10 to ensure stability, as K-Means is sensitive to initialization.
    kmeans = KMeans(n_clusters=k, n_init=10, random_state=42)
    kmeans.fit(pixels)
    # The cluster centers represent the dominant colors.
    return kmeans.cluster_centers_.astype(np.uint8)


def rgb_to_hex(color: Iterable[int]) -> str:
    """Convert an RGB color to a HEX string.

    Args:
        color: An iterable of three integers representing the R, G, and B values.

    Returns:
        A string representing the color in HEX format (e.g., "#RRGGBB").
    """
    r, g, b = (int(c) for c in color)
    return f"#{r:02X}{g:02X}{b:02X}"


def display_palette(
    colors: np.ndarray, title: str, save_path: str | None = None
) -> None:
    """Display or save a color palette visualization.

    This function creates a horizontal bar of colors with their HEX and RGB
    values labeled. It can either display the palette interactively or save it
    to a file.

    Args:
        colors: A NumPy array of colors to display.
        title: The title of the palette visualization.
        save_path: An optional file path to save the palette image.
    """
    num_colors = len(colors)
    fig, ax = plt.subplots(figsize=(max(4, num_colors * 1.4), 1.8), dpi=130)
    for i, color in enumerate(colors):
        # Create a colored rectangle for each dominant color.
        rect = patches.Rectangle((i, 0), 1, 1, color=color / 255.0)
        ax.add_patch(rect)
        # Add HEX and RGB labels for each color.
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
    """Create a JSON summary of the color extraction results.

    Args:
        cfg: The runtime configuration.
        colors: The extracted dominant colors.

    Returns:
        A JSON-formatted string summarizing the results.
    """
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
    """Execute the color extraction pipeline.

    This function coordinates the process of loading an image, sampling its
    pixels, and extracting the dominant colors.

    Args:
        cfg: The runtime configuration.

    Returns:
        A NumPy array of the dominant colors.
    """
    image = load_image(cfg.image, resize=cfg.resize)
    pixels = sample_pixels(image, cfg.max_pixels)
    colors = extract_colors(pixels, cfg.num_colors)
    return colors


def parse_resize(value: str) -> Tuple[int, int]:
    """Parse a 'WxH' string into a (width, height) tuple for resizing.

    Args:
        value: The string to parse (e.g., "800x600").

    Returns:
        A tuple of integers (width, height).

    Raises:
        argparse.ArgumentTypeError: If the string is not in the correct format.
    """
    try:
        w_str, h_str = value.lower().split("x")
        return int(w_str), int(h_str)
    except Exception as e:  # noqa: BLE001
        raise argparse.ArgumentTypeError(
            "Resize must be in WxH format, e.g. 800x800"
        ) from e


def build_arg_parser() -> argparse.ArgumentParser:
    """Build the command-line argument parser.

    Returns:
        An `argparse.ArgumentParser` instance.
    """
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
    """The main entry point for the script.

    This function parses command-line arguments, runs the color extraction
    pipeline, and prints the results.

    Args:
        argv: A list of command-line arguments.

    Returns:
        An integer exit code (0 for success, 1 for failure).
    """
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

    # Sort colors by brightness for a more aesthetically pleasing and deterministic output.
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

    # Display the palette if requested, but only in interactive mode if not saving.
    if cfg.save_palette:
        display_palette(
            colors,
            f"Dominant Color Palette ({cfg.num_colors} colors)",
            cfg.save_palette,
        )
    else:
        display_palette(colors, f"Dominant Color Palette ({cfg.num_colors} colors)")

    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
