"""convert.py - Image to ASCII art converter

Features:
  * Dataclass Config encapsulating parameters
  * Adjustable output width & aspect correction factor
  * Custom character set (dark->light) and optional inversion
  * Vectorized numpy mapping for speed (instead of per-pixel Python loop)
  * Optional JSON summary output (dimensions, chars, duration, output path)
  * Robust CLI with clear help & exit codes (0 success, 1 error)
  * Functions kept compatible for GUI import (resize_image, image_to_ascii, format_ascii_art)

Example:
  python convert.py input.jpg -w 120 -o art.txt
  python convert.py input.png -w 100 -c "@%#*+=-:. " --invert --json meta.json
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional

from PIL import Image, UnidentifiedImageError
import numpy as np

# Default character set used to represent image brightness, from dark to light.
DEFAULT_ASCII_CHARS: List[str] = ["#", "?", "%", ".", "S", "+", "*", ":", ",", "@"]


@dataclass(slots=True)
class Config:
    input_path: Path
    output_path: Optional[Path]
    width: int = 100
    chars: str = "".join(DEFAULT_ASCII_CHARS)
    invert: bool = False
    aspect: float = 0.55  # vertical scaling correction factor
    json_path: Optional[Path] = None

    def validate(self) -> None:
        if self.width < 10 or self.width > 400:
            raise ValueError("width must be between 10 and 400")
        if len(self.chars) < 2:
            raise ValueError("character set must contain at least 2 characters")
        if self.aspect <= 0 or self.aspect > 5:
            raise ValueError("aspect must be >0 and <=5")


# ----------------------------- Core Functions ----------------------------- #


def resize_image(
    image: Image.Image, new_width: int = 100, aspect: float = 0.55
) -> Image.Image:
    """Resizes an image while maintaining aspect ratio with vertical correction.

    aspect parameter compensates for the typical character cell height>width in terminals.
    """
    old_width, old_height = image.size
    new_height = int((old_height / old_width) * new_width * aspect)
    return image.resize((new_width, max(1, new_height)))


def image_to_ascii(
    image: Image.Image, char_set: List[str], invert: bool = False
) -> str:
    """Converts the image to ASCII characters (flat string).

    Uses numpy vectorization for performance.
    """
    # Convert image to grayscale array (0-255)
    arr = np.array(image.convert("L"), dtype=np.uint8)
    if invert:
        arr = 255 - arr
    # Normalize to [0, 1) then scale to indices
    char_len = len(char_set)
    indices = (arr.astype(np.float32) / 256.0 * char_len).clip(0, char_len - 1e-6)
    indices = indices.astype(np.int32)
    mapped = np.vectorize(lambda i: char_set[i])(indices)
    return "".join(mapped.ravel())


def format_ascii_art(ascii_chars: str, width: int) -> str:
    """Formats a flat string of ASCII characters into a multi-line string."""
    return "\n".join(
        ascii_chars[i : i + width] for i in range(0, len(ascii_chars), width)
    )


# ----------------------------- CLI Workflow ----------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Convert an image file to ASCII art.")
    p.add_argument("input_path", help="Path to input image")
    p.add_argument(
        "-o", "--output", dest="output_path", help="Optional output text file path"
    )
    p.add_argument(
        "-w",
        "--width",
        type=int,
        default=100,
        help="Output width in characters (10-400)",
    )
    p.add_argument(
        "-c",
        "--chars",
        default="".join(DEFAULT_ASCII_CHARS),
        help="Custom characters dark->light",
    )
    p.add_argument("--invert", action="store_true", help="Invert brightness mapping")
    p.add_argument(
        "--aspect",
        type=float,
        default=0.55,
        help="Aspect correction factor (default 0.55)",
    )
    p.add_argument("--json", dest="json_path", help="Write JSON summary to path")
    return p


def parse_args(argv: Optional[list[str]]) -> Config:
    parser = build_parser()
    a = parser.parse_args(argv)
    cfg = Config(
        input_path=Path(a.input_path),
        output_path=Path(a.output_path) if a.output_path else None,
        width=a.width,
        chars=a.chars,
        invert=a.invert,
        aspect=a.aspect,
        json_path=Path(a.json_path) if a.json_path else None,
    )
    cfg.validate()
    return cfg


def convert(cfg: Config) -> dict:
    start = time.time()
    with Image.open(cfg.input_path) as img:
        resized_img = resize_image(img, cfg.width, cfg.aspect)
        ascii_chars_flat = image_to_ascii(
            resized_img, list(cfg.chars), invert=cfg.invert
        )
        ascii_art = format_ascii_art(ascii_chars_flat, cfg.width)
    meta = {
        "input": str(cfg.input_path),
        "width": cfg.width,
        "chars": len(cfg.chars),
        "invert": cfg.invert,
        "aspect": cfg.aspect,
        "output": str(cfg.output_path) if cfg.output_path else None,
        "elapsed_sec": round(time.time() - start, 4),
        "dimensions": {"orig": None, "resized": None},
    }
    meta["dimensions"]["resized"] = {
        "width": resized_img.size[0],
        "height": resized_img.size[1],
    }
    return {"ascii": ascii_art, "meta": meta}


# ----------------------------- Main Entry ----------------------------- #


def main(argv: Optional[list[str]] = None) -> int:
    try:
        cfg = parse_args(argv)
        result = convert(cfg)
        ascii_art = result["ascii"]
        meta = result["meta"]

        if cfg.output_path:
            with open(cfg.output_path, "w", encoding="utf-8") as f:
                f.write(ascii_art)
            print(f"ASCII art saved to '{cfg.output_path}'")
        else:
            print("\n--- ASCII Art ---\n")
            print(ascii_art)

        if cfg.json_path:
            try:
                with open(cfg.json_path, "w", encoding="utf-8") as fh:
                    json.dump(meta, fh, indent=2)
            except OSError as e:
                print(f"Warning: Could not write JSON summary: {e}", file=sys.stderr)
        return 0
    except FileNotFoundError:
        print(f"Error: Input file not found.", file=sys.stderr)
    except UnidentifiedImageError:
        print(
            "Error: Cannot identify image file (unsupported or corrupt).",
            file=sys.stderr,
        )
    except ValueError as e:
        print(f"Argument error: {e}", file=sys.stderr)
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
    return 1


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
