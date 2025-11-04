"""steg_visualizer.py - Inspect the differences between cover and stego images.

This utility loads a pair of images (the original cover image and the
corresponding stego image), computes per-channel bit-plane deltas, and exposes
both textual summaries and visual debugging aids.  It reuses
``image_capacity_chars`` from :mod:`steg` so that capacity information remains
consistent with the embedding CLI.

The tool is careful to fail gracefully when optional visualization dependencies
such as Pillow or Matplotlib are not available or when running in headless
environments (for example in CI).  Tests exercise the JSON metrics path to keep
the core numerical analysis well covered without depending on a GUI backend.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional, Sequence, Tuple

try:  # Pillow is optional at runtime, but required for full functionality.
    from PIL import Image
except ImportError:  # pragma: no cover - exercised via graceful CLI handling.
    Image = None  # type: ignore[assignment]

from steg import image_capacity_chars


CHANNEL_NAMES = ("R", "G", "B")


class VisualizationUnavailable(RuntimeError):
    """Raised when an optional visualization dependency is missing."""


def _ensure_pillow() -> None:
    if Image is None:  # pragma: no cover - executed only when Pillow missing.
        raise VisualizationUnavailable(
            "Pillow (PIL) is required for this command. Install it with 'pip install pillow'."
        )


def load_image(path: str | Path) -> Image.Image:
    """Open an image from *path* and normalise it to RGB."""

    _ensure_pillow()
    try:
        return Image.open(path).convert("RGB")
    except FileNotFoundError as exc:
        raise FileNotFoundError(f"Image not found: {path}") from exc
    except OSError as exc:  # pragma: no cover - depends on corrupt image inputs.
        raise RuntimeError(f"Failed to open {path}: {exc}") from exc


def compute_diff_metrics(
    cover_array: List[List[Iterable[int]]],
    stego_array: List[List[Iterable[int]]],
) -> Tuple[Dict[str, Any], List[List[bool]]]:
    """Compute per-pixel and per-bit differences between two RGB images.

    Parameters
    ----------
    cover_array, stego_array:
        Nested lists shaped ``height x width x 3`` representing the original and
        modified images. Values are expected to be integers in the range
        ``0-255``.

    Returns
    -------
    (metrics, modified_mask)
        ``metrics`` is a JSON-serialisable dictionary describing pixel and
        bit-plane differences. ``modified_mask`` is a matrix of booleans (as
        nested lists) indicating which pixels changed in any channel.
    """

    if len(cover_array) != len(stego_array):
        raise ValueError("Cover and stego images must share dimensions")

    height = len(cover_array)
    width = len(cover_array[0]) if height else 0

    if any(len(row) != width for row in cover_array) or any(
        len(row) != width for row in stego_array
    ):
        raise ValueError("Rows in cover or stego images are not consistently sized")

    modified_mask: List[List[bool]] = [[False] * width for _ in range(height)]
    per_channel: Dict[str, Dict[str, Any]] = {
        name: {
            "modified_pixels": 0,
            "bit_counts": [0] * 8,
            "bits_modified_total": 0,
        }
        for name in CHANNEL_NAMES
    }
    aggregate_bit_counts = [0] * 8

    for y in range(height):
        for x in range(width):
            cover_pixel = tuple(cover_array[y][x])
            stego_pixel = tuple(stego_array[y][x])
            if len(cover_pixel) != 3 or len(stego_pixel) != 3:
                raise ValueError("Pixels must contain three RGB channels")

            changed_here = False
            for channel_index, channel_name in enumerate(CHANNEL_NAMES):
                diff = (cover_pixel[channel_index] ^ stego_pixel[channel_index]) & 0xFF
                if diff:
                    changed_here = True
                    per_channel[channel_name]["modified_pixels"] += 1
                    for bit in range(8):
                        if diff & (1 << bit):
                            per_channel[channel_name]["bit_counts"][bit] += 1
                            aggregate_bit_counts[bit] += 1
            if changed_here:
                modified_mask[y][x] = True

    bits_modified_total = 0
    for channel_info in per_channel.values():
        channel_total = sum(channel_info["bit_counts"])
        channel_info["bits_modified_total"] = channel_total
        bits_modified_total += channel_total

    modified_pixels = sum(1 for row in modified_mask for value in row if value)
    total_pixels = width * height

    metrics: Dict[str, Any] = {
        "width": width,
        "height": height,
        "capacity_chars": image_capacity_chars(width, height),
        "total_pixels": total_pixels,
        "modified_pixels": modified_pixels,
        "modified_ratio": modified_pixels / total_pixels if total_pixels else 0.0,
        "bits_modified_total": bits_modified_total,
        "bit_planes": {
            "aggregate": aggregate_bit_counts,
        },
        "per_channel": per_channel,
        "has_modifications": bool(modified_pixels),
    }
    return metrics, modified_mask


def analyse_image_pair(
    cover_path: str | Path, stego_path: str | Path
) -> Tuple[Image.Image, Image.Image, Dict[str, Any], List[List[bool]]]:
    """Load two images and compute their diff metrics."""

    cover_img = load_image(cover_path)
    stego_img = load_image(stego_path)
    width, height = cover_img.size
    cover_px = cover_img.load()
    stego_px = stego_img.load()
    cover_array = [[cover_px[x, y] for x in range(width)] for y in range(height)]
    stego_array = [[stego_px[x, y] for x in range(width)] for y in range(height)]
    metrics, modified_mask = compute_diff_metrics(cover_array, stego_array)
    metrics.update(
        {
            "cover_path": str(cover_path),
            "stego_path": str(stego_path),
        }
    )
    return cover_img, stego_img, metrics, modified_mask


def _import_matplotlib(show: bool):
    """Import matplotlib with an Agg backend when not displaying figures."""

    try:
        import matplotlib

        if not show:
            matplotlib.use("Agg", force=True)
        import matplotlib.pyplot as plt  # type: ignore

        return plt
    except Exception as exc:  # pragma: no cover - depends on runtime env.
        print(
            f"Matplotlib unavailable or cannot initialise backend: {exc}",
            file=sys.stderr,
        )
        return None


def save_overlay(
    stego_image: Image.Image, modified_mask: List[List[bool]], output_path: str | Path
) -> None:
    """Create and save a semi-transparent red overlay highlighting modified pixels."""

    _ensure_pillow()
    overlay = stego_image.convert("RGBA")
    highlight = Image.new("RGBA", overlay.size, (255, 0, 0, 128))
    width, height = stego_image.size
    mask_image = Image.new("L", (width, height), color=0)
    mask_pixels = mask_image.load()
    for y, row in enumerate(modified_mask):
        for x, value in enumerate(row):
            if value:
                mask_pixels[x, y] = 255
    base = Image.new("RGBA", overlay.size, (0, 0, 0, 0))
    composite = Image.composite(highlight, base, mask_image)
    result = Image.alpha_composite(overlay, composite)
    result.save(output_path)


def render_heatmap(
    modified_mask: List[List[bool]],
    path: Optional[str | Path] = None,
    show: bool = False,
) -> bool:
    """Render a heatmap of modified pixels. Returns ``True`` if rendered."""

    plt = _import_matplotlib(show)
    if plt is None:
        return False

    fig, ax = plt.subplots()
    numeric_mask = [[1.0 if cell else 0.0 for cell in row] for row in modified_mask]
    heatmap = ax.imshow(numeric_mask, cmap="magma", interpolation="nearest")
    ax.set_title("Modified Pixel Heatmap")
    ax.set_xlabel("X coordinate")
    ax.set_ylabel("Y coordinate")
    fig.colorbar(heatmap, ax=ax, shrink=0.8, label="Modified")
    if path:
        fig.savefig(path, dpi=150, bbox_inches="tight")
    if show:
        try:  # pragma: no cover - dependent on GUI availability.
            plt.show()
        except Exception as exc:
            print(f"Unable to display heatmap: {exc}", file=sys.stderr)
    plt.close(fig)
    return True


def render_histogram(
    bit_counts: Iterable[int],
    path: Optional[str | Path] = None,
    show: bool = False,
) -> bool:
    """Render a histogram of bit-plane changes."""

    plt = _import_matplotlib(show)
    if plt is None:
        return False

    bit_indices = list(range(8))
    counts = list(bit_counts)
    fig, ax = plt.subplots()
    ax.bar(bit_indices, counts, color="tab:blue")
    ax.set_xticks(bit_indices)
    ax.set_xlabel("Bit plane (0 = LSB)")
    ax.set_ylabel("Changed bits")
    ax.set_title("Bit-plane Change Histogram")
    if path:
        fig.savefig(path, dpi=150, bbox_inches="tight")
    if show:
        try:  # pragma: no cover - dependent on GUI availability.
            plt.show()
        except Exception as exc:
            print(f"Unable to display histogram: {exc}", file=sys.stderr)
    plt.close(fig)
    return True


def _write_json(metrics: Dict[str, Any], destination: str | Path) -> None:
    output = json.dumps(metrics, indent=2)
    if destination == "-":
        print(output)
        return
    Path(destination).write_text(output + "\n", encoding="utf-8")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Visualise differences between cover and stego images",
    )
    parser.add_argument("cover", help="Path to the original cover image")
    parser.add_argument("stego", help="Path to the stego image")
    parser.add_argument(
        "--json",
        metavar="PATH",
        help="Write metrics as JSON to PATH (use '-' for stdout)",
    )
    parser.add_argument(
        "--export-overlay",
        metavar="PATH",
        help="Save a red overlay highlighting modified pixels",
    )
    parser.add_argument(
        "--save-heatmap",
        metavar="PATH",
        help="Save a heatmap visualisation of modified pixels",
    )
    parser.add_argument(
        "--save-histogram",
        metavar="PATH",
        help="Save a histogram of bit-plane modifications",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Display generated plots interactively when possible",
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)

    try:
        cover_img, stego_img, metrics, modified_mask = analyse_image_pair(
            args.cover, args.stego
        )
    except FileNotFoundError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1
    except VisualizationUnavailable as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1
    except ValueError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1

    if args.export_overlay:
        try:
            save_overlay(stego_img, modified_mask, args.export_overlay)
            print(f"Overlay saved to {args.export_overlay}")
        except (VisualizationUnavailable, OSError) as exc:
            print(f"Failed to export overlay: {exc}", file=sys.stderr)

    if args.save_heatmap:
        if render_heatmap(modified_mask, args.save_heatmap, show=False):
            print(f"Heatmap saved to {args.save_heatmap}")

    if args.save_histogram:
        if render_histogram(
            metrics["bit_planes"]["aggregate"], args.save_histogram, show=False
        ):
            print(f"Histogram saved to {args.save_histogram}")

    if args.show:
        render_heatmap(modified_mask, path=None, show=True)
        render_histogram(metrics["bit_planes"]["aggregate"], path=None, show=True)

    if args.json:
        _write_json(metrics, args.json)
    else:
        summary = (
            f"Modified {metrics['modified_pixels']} / {metrics['total_pixels']} pixels "
            f"({metrics['modified_ratio']:.2%}); total modified bits: {metrics['bits_modified_total']}"
        )
        print(summary)

    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    raise SystemExit(main())
