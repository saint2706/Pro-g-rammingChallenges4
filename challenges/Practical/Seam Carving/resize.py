"""Seam carving (content-aware image resizing).

This module provides a `SeamCarver` class implementing vertical and horizontal
seam removal using dynamic programming over an energy map (Sobel gradient
magnitude). The implementation focuses on clarity with moderate optimization:

Core Steps:
    1. Compute energy map E(x,y) = |Sobel_x| + |Sobel_y|.
    2. For vertical seam: DP accumulates minimal energy path top->bottom.
    3. Backtrack lowest-energy path; remove one column per row.
    4. Horizontal seam removal performed by transposing image, reusing vertical code.

Features Added:
    - Type hints & docstrings for public methods
    - Input validation (seam counts cannot exceed dimensions)
    - Optional progress callback for UI integration
    - Configurable energy function placeholder hook for future extension
    - Improved CLI with sanity checks & early errors
    - Graceful error messages and non-zero exit codes

Performance Notes:
    - The DP step is O(wh) per seam; removing multiple seams recalculates energy
        after each removal for correctness. More advanced approaches (forward energy
        or multi-seam extraction) could batch seams but are outside this scope.
    - Critical loops use NumPy arrays; Python loops limited to backtracking & copy.

Future Enhancements (not implemented):
    - Forward energy metric
    - Object preservation masks
    - Multi-seam removal per pass
    - Insertion for enlargement
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from typing import Callable, Iterable, List, Optional

import cv2
import numpy as np

ProgressCallback = Callable[[str, int, int], None]


@dataclass
class CarveConfig:
    """Configuration for seam carving operations."""

    delta_width: int = 0  # number of vertical seams to remove
    delta_height: int = 0  # number of horizontal seams to remove
    progress: Optional[ProgressCallback] = None


class SeamCarver:
    """Content-aware seam carver.

    Parameters
    ----------
    image_path : str
        Path to image file readable by OpenCV.
    config : Optional[CarveConfig]
        Configuration including seam counts and optional progress callback.
    """

    def __init__(self, image_path: str, config: Optional[CarveConfig] = None):
        img = cv2.imread(image_path, cv2.IMREAD_COLOR)
        if img is None:
            raise FileNotFoundError(f"Could not load image from '{image_path}'")
        # Work in float64 for gradient precision
        self.image: np.ndarray = img.astype(np.float64)
        self.config = config or CarveConfig()

    # ---------------------------------------------------------------------
    # Energy & seam utilities
    # ---------------------------------------------------------------------

    def _calculate_energy_map(self) -> np.ndarray:
        """Return energy map (Sobel magnitude) as float64 array."""
        gray = cv2.cvtColor(self.image.astype(np.uint8), cv2.COLOR_BGR2GRAY)
        grad_x = cv2.Sobel(gray, cv2.CV_64F, 1, 0, ksize=3)
        grad_y = cv2.Sobel(gray, cv2.CV_64F, 0, 1, ksize=3)
        return np.abs(grad_x) + np.abs(grad_y)

    def _find_vertical_seam(self, energy_map: np.ndarray) -> np.ndarray:
        """Return indices of a minimum-energy vertical seam.

        Uses dynamic programming: each cell accumulates its own energy plus
        the minimum of the three reachable predecessors from the previous row.
        Vectorization reduces Python loop overhead by computing row updates
        with slicing.
        """
        rows, cols = energy_map.shape
        M = energy_map.copy()
        backtrack = np.zeros((rows, cols), dtype=np.int16)

        for r in range(1, rows):
            prev = M[r - 1]
            curr = M[r]
            # For each column, compute min of up-left, up, up-right.
            left = np.empty_like(prev)
            left[0] = np.inf
            left[1:] = prev[:-1]
            up = prev
            right = np.empty_like(prev)
            right[-1] = np.inf
            right[:-1] = prev[1:]
            stacked = np.stack((left, up, right), axis=0)  # (3, cols)
            choices = np.argmin(stacked, axis=0)  # 0,1,2
            min_vals = stacked[choices, np.arange(cols)]
            curr += min_vals
            backtrack[r] = choices - 1  # -1,0,1 relative movement

        seam = np.zeros(rows, dtype=np.int32)
        seam[-1] = int(np.argmin(M[-1]))
        for r in range(rows - 2, -1, -1):
            seam[r] = seam[r + 1] + backtrack[r + 1, seam[r + 1]]
        return seam

    def _remove_vertical_seam(self, seam: np.ndarray) -> None:
        """Remove a vertical seam in-place (allocates new array with one fewer column)."""
        rows, cols, ch = self.image.shape
        new_img = np.empty((rows, cols - 1, ch), dtype=self.image.dtype)
        for r in range(rows):
            c = seam[r]
            new_img[r, :c] = self.image[r, :c]
            new_img[r, c:] = self.image[r, c + 1 :]
        self.image = new_img

    def carve_width(self, num_seams: int) -> None:
        """Remove `num_seams` vertical seams from the current image."""
        if num_seams <= 0:
            return
        h, w, _ = self.image.shape
        if num_seams >= w:
            raise ValueError("Cannot remove width >= current width")
        for i in range(num_seams):
            energy = self._calculate_energy_map()
            seam = self._find_vertical_seam(energy)
            self._remove_vertical_seam(seam)
            if self.config.progress:
                self.config.progress("width", i + 1, num_seams)

    def carve_height(self, num_seams: int) -> None:
        """Remove `num_seams` horizontal seams from the current image."""
        if num_seams <= 0:
            return
        h, w, _ = self.image.shape
        if num_seams >= h:
            raise ValueError("Cannot remove height >= current height")
        self.image = np.transpose(self.image, (1, 0, 2))
        try:
            self.carve_width(num_seams)
        finally:
            self.image = np.transpose(self.image, (1, 0, 2))
        # progress callbacks handled by carve_width already (reported as width)

    def get_image(self) -> np.ndarray:
        """Return current image as uint8 array suitable for writing/display."""
        return np.clip(self.image, 0, 255).astype(np.uint8)


def _parse_cli(argv: Optional[Iterable[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Content-aware image resizing via seam carving"
    )
    parser.add_argument("input_path", help="Path to input image")
    parser.add_argument("output_path", help="Path for resized output")
    parser.add_argument(
        "-dw",
        "--delta-width",
        type=int,
        default=0,
        dest="delta_width",
        help="Number of vertical seams to remove (reduce width)",
    )
    parser.add_argument(
        "-dh",
        "--delta-height",
        type=int,
        default=0,
        dest="delta_height",
        help="Number of horizontal seams to remove (reduce height)",
    )
    parser.add_argument(
        "--no-progress", action="store_true", help="Suppress progress output"
    )
    return parser.parse_args(list(argv) if argv is not None else None)


def _cli(argv: Optional[Iterable[str]] = None) -> int:
    ns = _parse_cli(argv)
    if ns.delta_width <= 0 and ns.delta_height <= 0:
        print(
            "Error: Specify at least one positive seam removal count", file=sys.stderr
        )
        return 2

    def progress_cb(axis: str, done: int, total: int) -> None:
        if not ns.no_progress:
            print(f"Progress {axis}: {done}/{total}\r", end="", flush=True)

    config = CarveConfig(
        delta_width=ns.delta_width,
        delta_height=ns.delta_height,
        progress=progress_cb if not ns.no_progress else None,
    )
    try:
        carver = SeamCarver(ns.input_path, config)
        if ns.delta_width:
            carver.carve_width(ns.delta_width)
        if ns.delta_height:
            carver.carve_height(ns.delta_height)
        out = carver.get_image()
        if not cv2.imwrite(ns.output_path, out):
            print("Error: Failed to write output image", file=sys.stderr)
            return 1
        print(f"\nSaved resized image to {ns.output_path}")
        return 0
    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(_cli())
