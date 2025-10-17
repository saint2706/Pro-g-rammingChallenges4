"""comp.py — Complementary Color Image Transformer
=================================================
Transforms an image (or batch of images) into their *complementary* variant
using a luminance-preserving inversion technique:

    new_channel = min(R,G,B) + max(R,G,B) - original_channel

Unlike a naive 255-R style invert, this method mirrors each channel around the
midpoint defined by that pixel's local (min+max). This can produce more
harmonious complements in certain graphic workflows.

Features added in this refactor:
 - Batch processing of a directory (optional recursion)
 - Optional preservation/restoration of alpha channel
 - JSON summary with per-image statistics (mean, min, max before/after)
 - Safe overwrite behavior unless --force specified
 - Deterministic exit codes and structured main() routine

Examples:
    # Single file
    python comp.py input.jpg output.jpg --json

    # Batch (writes *_comp suffix by default)
    python comp.py --batch images/ --recursive --suffix _comp --json summary.json

    # Keep alpha channel (e.g. PNGs with transparency)
    python comp.py logo.png logo_comp.png --keep-alpha
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
    """Vectorized complementary color transform.

    Uses int16 intermediate to avoid uint8 wrap during (lo+hi)-data.
    Expects array shape (H,W,3) with dtype uint8 or int16-compatible.
    Returns uint8 array of same shape.
    """
    in_data = data.astype(np.int16, copy=False)
    lo = np.amin(in_data, axis=2, keepdims=True)
    hi = np.amax(in_data, axis=2, keepdims=True)
    out = (lo + hi) - in_data
    return np.clip(out, 0, 255).astype(np.uint8, copy=False)


def complement_image(input_path: Path, output_path: Path, keep_alpha: bool) -> Stats:
    """Process a single image and write complementary result.

    Args:
        input_path: Source image path.
        output_path: Destination path.
        keep_alpha: If True, preserve original alpha channel.
    Returns:
        Stats object with before/after channel statistics.
    Raises:
        FileNotFoundError, UnidentifiedImageError, RuntimeError on failure.
    """
    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {input_path}")

    with Image.open(input_path) as img:
        mode = img.mode
        alpha = None

        if keep_alpha and ("A" in mode):  # Extract alpha for RGBA / LA / etc.
            alpha = img.getchannel("A")

        # Normalize to RGB for processing
        rgb_img = img.convert("RGB")
        arr = np.asarray(rgb_img, dtype=np.uint8)

        # Gather pre-stats
        mb_arr = np.min(arr, axis=(0, 1))
        xb_arr = np.max(arr, axis=(0, 1))
        avb_arr = np.mean(arr, axis=(0, 1))
        min_before = (int(mb_arr[0]), int(mb_arr[1]), int(mb_arr[2]))
        max_before = (int(xb_arr[0]), int(xb_arr[1]), int(xb_arr[2]))
        mean_before = (float(avb_arr[0]), float(avb_arr[1]), float(avb_arr[2]))

        comp = _compute_complement(arr)

        # Gather post-stats
        ma_arr = np.min(comp, axis=(0, 1))
        xa_arr = np.max(comp, axis=(0, 1))
        ava_arr = np.mean(comp, axis=(0, 1))
        min_after = (int(ma_arr[0]), int(ma_arr[1]), int(ma_arr[2]))
        max_after = (int(xa_arr[0]), int(xa_arr[1]), int(xa_arr[2]))
        mean_after = (float(ava_arr[0]), float(ava_arr[1]), float(ava_arr[2]))

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
    exts = {".png", ".jpg", ".jpeg", ".bmp", ".tiff", ".gif"}
    if root.is_file():
        return [root] if root.suffix.lower() in exts else []
    pattern = "**/*" if recursive else "*"
    return [p for p in root.glob(pattern) if p.suffix.lower() in exts]


def build_output_path(
    src: Path, dest_dir: Path | None, suffix: str, out_file: Path | None
) -> Path:
    if out_file is not None:
        return out_file
    target_dir = dest_dir if dest_dir else src.parent
    return target_dir / f"{src.stem}{suffix}{src.suffix}"


# --------------------------- CLI / JSON --------------------------- #


def build_parser() -> argparse.ArgumentParser:
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
    parser = build_parser()
    args = parser.parse_args(argv)

    keep_alpha = args.keep_alpha
    force = args.force
    json_path = Path(args.json) if args.json else None
    summary: List[Stats] = []

    try:
        if args.single:
            src = Path(args.single[0])
            dst = Path(args.single[1])
            if dst.exists() and not force:
                print(
                    f"Refusing to overwrite existing file: {dst} (use --force)",
                    file=sys.stderr,
                )
                return 2
            stat = complement_image(src, dst, keep_alpha)
            summary.append(stat)
            print(f"Saved: {dst}")
        else:  # batch mode
            root = Path(args.batch)
            dest_dir = Path(args.dest) if args.dest else None
            files = iter_image_files(root, args.recursive)
            if not files:
                print(f"No image files found in: {root}", file=sys.stderr)
                return 1
            for f in files:
                out_path = build_output_path(f, dest_dir, args.suffix, None)
                if out_path.exists() and not force:
                    print(f"Skip (exists): {out_path}")
                    continue
                try:
                    stat = complement_image(f, out_path, keep_alpha)
                    summary.append(stat)
                    print(f"Saved: {out_path}")
                except UnidentifiedImageError:
                    print(
                        f"Warning: Unidentified or corrupt image skipped: {f}",
                        file=sys.stderr,
                    )
                except FileNotFoundError:
                    print(
                        f"Warning: File disappeared before processing: {f}",
                        file=sys.stderr,
                    )

        if json_path:
            data = [stats_to_dict(s) for s in summary]
            json_path.write_text(json.dumps(data, indent=2), encoding="utf-8")
            print(f"Wrote JSON summary to: {json_path}")
    except UnidentifiedImageError as e:
        print(f"Error: Cannot identify image file: {e}", file=sys.stderr)
        return 1
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print("\nInterrupted by user.")
        return 130
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
