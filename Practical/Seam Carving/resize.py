import numpy as np
import cv2
import argparse
import sys
from typing import Optional

class SeamCarver:
    """
    A class to perform content-aware image resizing using the seam carving algorithm.
    """
    def __init__(self, image_path: str):
        self.image = cv2.imread(image_path)
        if self.image is None:
            raise FileNotFoundError(f"Could not load image from '{image_path}'")
        self.image = self.image.astype(np.float64)

    def _calculate_energy_map(self) -> np.ndarray:
        """Calculates the energy of each pixel using the Sobel gradient magnitude."""
        gray = cv2.cvtColor(self.image.astype(np.uint8), cv2.COLOR_BGR2GRAY)
        grad_x = cv2.Sobel(gray, cv2.CV_64F, 1, 0, ksize=3)
        grad_y = cv2.Sobel(gray, cv2.CV_64F, 0, 1, ksize=3)
        return np.abs(grad_x) + np.abs(grad_y)

    def _find_vertical_seam(self, energy_map: np.ndarray) -> List[int]:
        """Finds the lowest-energy vertical seam using dynamic programming."""
        rows, cols = energy_map.shape
        # Create a cumulative energy map (M)
        M = energy_map.copy()
        for r in range(1, rows):
            for c in range(cols):
                # Handle borders
                if c == 0:
                    min_prev_energy = min(M[r-1, c], M[r-1, c+1])
                elif c == cols - 1:
                    min_prev_energy = min(M[r-1, c-1], M[r-1, c])
                else:
                    min_prev_energy = min(M[r-1, c-1], M[r-1, c], M[r-1, c+1])
                M[r, c] += min_prev_energy

        # Backtrack from the bottom to find the seam path
        seam = np.zeros(rows, dtype=np.uint32)
        seam[-1] = np.argmin(M[-1])
        for r in range(rows - 2, -1, -1):
            prev_c = seam[r+1]
            if prev_c == 0:
                seam[r] = np.argmin(M[r, :2])
            elif prev_c == cols - 1:
                seam[r] = np.argmin(M[r, -2:]) + cols - 2
            else:
                seam[r] = np.argmin(M[r, prev_c-1:prev_c+2]) + prev_c - 1
        return seam

    def _remove_vertical_seam(self, seam: List[int]):
        """Removes a given vertical seam from the image."""
        rows, cols, channels = self.image.shape
        new_image = np.zeros((rows, cols - 1, channels), dtype=np.float64)
        for r in range(rows):
            c = seam[r]
            new_image[r, :c] = self.image[r, :c]
            new_image[r, c:] = self.image[r, c+1:]
        self.image = new_image

    def carve_width(self, num_seams: int):
        """Removes a specified number of vertical seams."""
        for _ in range(num_seams):
            energy_map = self._calculate_energy_map()
            seam = self._find_vertical_seam(energy_map)
            self._remove_vertical_seam(seam)

    def carve_height(self, num_seams: int):
        """Removes a specified number of horizontal seams."""
        # Transpose, carve width, then transpose back
        self.image = np.transpose(self.image, (1, 0, 2))
        self.carve_width(num_seams)
        self.image = np.transpose(self.image, (1, 0, 2))

    def get_image(self) -> np.ndarray:
        """Returns the current image as a standard 8-bit array."""
        return self.image.astype(np.uint8)

def main():
    """Main function to run seam carving from the command line."""
    parser = argparse.ArgumentParser(description="Content-aware image resizing using Seam Carving.")
    parser.add_argument("input_path", help="Path to the input image.")
    parser.add_argument("output_path", help="Path to save the resized image.")
    parser.add_argument("-dw", "--delta_width", type=int, default=0,
                        help="Number of vertical seams to remove (reduces width).")
    parser.add_argument("-dh", "--delta_height", type=int, default=0,
                        help="Number of horizontal seams to remove (reduces height).")

    args = parser.parse_args()

    if args.delta_width <= 0 and args.delta_height <= 0:
        print("Error: Please specify a non-zero number of seams to remove.", file=sys.stderr)
        sys.exit(1)

    try:
        print(f"Loading image from '{args.input_path}'...")
        carver = SeamCarver(args.input_path)

        if args.delta_width > 0:
            print(f"Removing {args.delta_width} vertical seam(s)...")
            carver.carve_width(args.delta_width)

        if args.delta_height > 0:
            print(f"Removing {args.delta_height} horizontal seam(s)...")
            carver.carve_height(args.delta_height)

        output_image = carver.get_image()
        cv2.imwrite(args.output_path, output_image)
        print(f"Successfully saved resized image to '{args.output_path}'")

    except FileNotFoundError as e:
        print(f"Error: {e}", file=sys.stderr)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
