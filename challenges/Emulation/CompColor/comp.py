"""comp.py — Complementary Color Image Transformer
=================================================
Transforms an image into its *complementary* variant using a
luminance-preserving inversion technique.

This script processes images by applying a complementary color transformation
that preserves the luminance of the original pixels. The formula used is:

    new_channel = min(R,G,B) + max(R,G,B) - original_channel

Unlike a naive inversion, this method produces more harmonious complements.

Features:
 - Batch processing of directories with optional recursion.
 - Preservation of the alpha channel for images with transparency.
 - Generation of a JSON summary with per-image statistics.
 - Safe overwrite behavior to prevent accidental data loss.

Examples:
    # Process a single file.
    python comp.py --single input.jpg output.jpg --json

    # Process a directory of images.
    python comp.py --batch images/ --recursive --suffix _comp --json summary.json

    # Preserve the alpha channel of a PNG.
    python comp.py --single logo.png logo_comp.png --keep-alpha
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Tuple

import numpy as np
from PIL import Image, UnidentifiedImageError


# --------------------------- Data Structures --------------------------- #


@dataclass(slots=True)
class Stats:
    """Stores statistics about the color transformation of an image.

    Attributes:
        mean_before: The mean RGB values before the transformation.
        mean_after: The mean RGB values after the transformation.
        min_before: The minimum RGB values before the transformation.
        max_before: The maximum RGB values before the transformation.
        min_after: The minimum RGB values after the transformation.
        max_after: The maximum RGB values after the transformation.
        path_in: The path to the input image.
        path_out: The path to the output image.
    """
    mean_before: Tuple[float, float, float]
    mean_after: Tuple[float, float, float]
    min_before: Tuple[int, int, int]
    max_before: Tuple[int, int, int]
    min_after: Tuple[int, int, int]
    max_after: Tuple[int, int, int]
    path_in: str
    path_out: str


# --------------------------- Core Logic --------------------------- #


def _compute_complement(data: np.ndarray) -> np.ndarray:
    """Computes the complementary color of an image using a vectorized approach.

    Args:
        data: A NumPy array representing the image data.

    Returns:
        A NumPy array of the transformed image.
    """
    # Use int16 to avoid overflow during the calculation.
    in_data = data.astype(np.int16, copy=False)
    lo = np.amin(in_data, axis=2, keepdims=True)
    hi = np.amax(in_data, axis=2, keepdims=True)
    out = (lo + hi) - in_data
    return np.clip(out, 0, 255).astype(np.uint8, copy=False)


def complement_image(input_path: Path, output_path: Path, keep_alpha: bool) -> Stats:
    """Processes a single image and saves the complementary result.

    Args:
        input_path: The path to the source image.
        output_path: The path to save the transformed image.
        keep_alpha: Whether to preserve the alpha channel.

    Returns:
        A Stats object with before and after color statistics.

    Raises:
        FileNotFoundError: If the input file does not exist.
        UnidentifiedImageError: If the input file is not a valid image.
    """
    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {input_path}")

    with Image.open(input_path) as img:
        mode = img.mode
        alpha = None

        if keep_alpha and ("A" in mode):
            # Preserve the alpha channel if it exists.
            alpha = img.getchannel("A")

        rgb_img = img.convert("RGB")
        arr = np.asarray(rgb_img, dtype=np.uint8)

        # Gather statistics before the transformation.
        min_before = tuple(np.min(arr, axis=(0, 1)))
        max_before = tuple(np.max(arr, axis=(0, 1)))
        mean_before = tuple(np.mean(arr, axis=(0, 1)))

        comp = _compute_complement(arr)

        # Gather statistics after the transformation.
        min_after = tuple(np.min(comp, axis=(0, 1)))
        max_after = tuple(np.max(comp, axis=(0, 1)))
        mean_after = tuple(np.mean(comp, axis=(0, 1)))

        output_path.parent.mkdir(parents=True, exist_ok=True)
        out_img = Image.fromarray(comp, mode="RGB")
        if alpha is not None:
            out_img.putalpha(alpha)
        out_img.save(output_path)

    return Stats(
        mean_before=mean_before,
        mean_after=mean_after,
        min_before=min_before,
        max_before=max_before,
        min_after=min_after,
        max_after=max_after,
        path_in=str(input_path),
        path_out=str(output_path),
    )


# --------------------------- Batch Processing --------------------------- #


def iter_image_files(root: Path, recursive: bool) -> List[Path]:
    """Finds all image files in a directory.

    Args:
        root: The directory to search.
        recursive: Whether to search subdirectories.

    Returns:
        A list of paths to the found image files.
    """
    exts = {".png", ".jpg", ".jpeg", ".bmp", ".tiff", ".gif"}
    if root.is_file():
        return [root] if root.suffix.lower() in exts else []
    pattern = "**/*" if recursive else "*"
    return [p for p in root.glob(pattern) if p.suffix.lower() in exts]


def build_output_path(
    src: Path, dest_dir: Path | None, suffix: str, out_file: Path | None
) -> Path:
    """Constructs the output path for a transformed image.

    Args:
        src: The path to the source image.
        dest_dir: The destination directory for batch processing.
        suffix: The suffix to add to the filename in batch mode.
        out_file: The specified output file for single-image mode.

    Returns:
        The constructed output path.
    """
    if out_file is not None:
        return out_file
    target_dir = dest_dir if dest_dir else src.parent
    if dest_dir:
        dest_dir.mkdir(parents=True, exist_ok=True)
    return target_dir / f"{src.stem}{suffix}{src.suffix}"


# --------------------------- CLI / JSON --------------------------- #


def build_parser() -> argparse.ArgumentParser:
    """Builds the command-line argument parser.

    Returns:
        An ArgumentParser instance.
    """
    p = argparse.ArgumentParser(
        description="Generate complementary color images (single or batch).",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    group = p.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--batch", metavar="PATH", help="Process a directory or a single file"
    )
    group.add_argument(
        "--single", nargs=2, metavar=("IN", "OUT"), help="Single input/output pair"
    )
    p.add_argument(
        "--suffix", default="_comp", help="Suffix for batch output filenames"
    )
    p.add_argument(
        "--dest",
        metavar="DIR",
        help="Directory to place batch outputs (default: alongside inputs)",
    )
    p.add_argument(
        "--recursive",
        action="store_true",
        help="Recurse into subdirectories in batch mode",
    )
    p.add_argument(
        "--keep-alpha", action="store_true", help="Preserve alpha channel if present"
    )
    p.add_argument(
        "--json", metavar="FILE", help="Write JSON summary of processing stats"
    )
    p.add_argument(
        "--force", action="store_true", help="Overwrite existing output files"
    )
    return p


def stats_to_dict(s: Stats) -> Dict[str, object]:
    """Converts a Stats object to a dictionary.

    Args:
        s: The Stats object to convert.

    Returns:
        A dictionary representation of the Stats object.
    """
    return {
        "input": s.path_in,
        "output": s.path_out,
        "mean_before": s.mean_before,
        "mean_after": s.mean_after,
        "min_before": s.min_before,
        "max_before": s.max_before,
        "min_after": s.min_after,
        "max_after": s.max_after,
    }


def main(argv: List[str] | None = None) -> int:
    """The main entry point for the script.

    Args:
        argv: A list of command-line arguments.

    Returns:
        An integer exit code (0 for success).
    """
    parser = build_parser()
    args = parser.parse_args(argv)

    keep_alpha = args.keep_alpha
    force = args.force
    json_path = Path(args.json) if args.json else None
    summary: List[Stats] = []

    try:
        if args.single:
            src, dst = Path(args.single[0]), Path(args.single[1])
            if dst.exists() and not force:
                print(f"Refusing to overwrite: {dst} (use --force)", file=sys.stderr)
                return 2
            stat = complement_image(src, dst, keep_alpha)
            summary.append(stat)
            print(f"Saved: {dst}")
        else:  # Batch mode
            root = Path(args.batch)
            dest_dir = Path(args.dest) if args.dest else None
            files = iter_image_files(root, args.recursive)
            if not files:
                print(f"No image files found in: {root}", file=sys.stderr)
                return 1
            for f in files:
                out_path = build_output_path(f, dest_dir, args.suffix, None)
                if out_path.exists() and not force:
                    print(f"Skipping (exists): {out_path}")
                    continue
                try:
                    stat = complement_image(f, out_path, keep_alpha)
                    summary.append(stat)
                    print(f"Saved: {out_path}")
                except (UnidentifiedImageError, FileNotFoundError) as e:
                    print(f"Warning: Skipping {f} ({e})", file=sys.stderr)

        if json_path:
            data = [stats_to_dict(s) for s in summary]
            json_path.write_text(json.dumps(data, indent=2), encoding="utf-8")
            print(f"Wrote JSON summary to: {json_path}")
    except (UnidentifiedImageError, FileNotFoundError, Exception) as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("\nInterrupted by user.")
        return 130

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
