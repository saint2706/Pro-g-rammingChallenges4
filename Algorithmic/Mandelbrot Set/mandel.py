import numpy as np
import matplotlib.pyplot as plt
import sys
from typing import Tuple

def generate_mandelbrot_set_vectorized(
    width: int, height: int,
    x_min: float, x_max: float,
    y_min: float, y_max: float,
    max_iter: int
) -> np.ndarray:
    """
    Generates the Mandelbrot set using a vectorized numpy approach for performance.

    Args:
        width: The width of the image in pixels.
        height: The height of the image in pixels.
        x_min, x_max: The range of the real axis (horizontal).
        y_min, y_max: The range of the imaginary axis (vertical).
        max_iter: The maximum number of iterations to determine if a point has escaped.

    Returns:
        A 2D numpy array representing the Mandelbrot set image, where the value
        of each pixel is the number of iterations it took to escape.
    """
    # Create 1D arrays for the real and imaginary parts of the complex plane
    real = np.linspace(x_min, x_max, width)
    imag = np.linspace(y_min, y_max, height)

    # Create a 2D grid of complex numbers 'c' from the real and imaginary parts
    # real[np.newaxis, :] creates a row vector
    # imag[:, np.newaxis] creates a column vector
    # Adding them together via broadcasting creates the complex grid
    c = real[np.newaxis, :] + 1j * imag[:, np.newaxis]

    # Initialize 'z' grid (starts at 0) and the output image grid
    z = np.zeros_like(c)
    image = np.zeros(c.shape, dtype=int)

    # The main vectorized loop
    for n in range(max_iter):
        # Identify points that have not yet escaped (their magnitude is <= 2)
        not_escaped_mask = np.abs(z) <= 2

        # Stop if all points have escaped
        if not not_escaped_mask.any():
            break

        # Update the 'z' values only for the points that have not escaped
        z[not_escaped_mask] = z[not_escaped_mask]**2 + c[not_escaped_mask]

        # Identify points that have just escaped in this iteration
        just_escaped_mask = (np.abs(z) > 2) & (image == 0)
        image[just_escaped_mask] = n

    # Any points that never escaped (image value still 0) are part of the set
    image[image == 0] = max_iter

    return image

def main():
    """
    Main function to configure and generate the Mandelbrot set.
    """
    print("--- Mandelbrot Set Generator ---")

    # Default parameters
    params = {
        "width": 800, "height": 600,
        "x_min": -2.0, "x_max": 1.0,
        "y_min": -1.5, "y_max": 1.5,
        "max_iter": 255
    }

    # Allow overriding defaults with command-line arguments
    if len(sys.argv) > 1: params['width'] = int(sys.argv[1])
    if len(sys.argv) > 2: params['height'] = int(sys.argv[2])
    if len(sys.argv) > 3: params['max_iter'] = int(sys.argv[3])

    print(f"Generating a {params['width']}x{params['height']} image with {params['max_iter']} iterations...")

    mandelbrot_image = generate_mandelbrot_set_vectorized(**params)

    print("Displaying plot...")
    plt.figure(figsize=(10, 8))
    # Use origin='lower' to have the y-axis start from the bottom, which is
    # conventional for mathematical plots. No transpose (.T) is needed.
    plt.imshow(mandelbrot_image, cmap="hot",
               extent=(params['x_min'], params['x_max'], params['y_min'], params['y_max']),
               origin='lower')

    plt.colorbar(label="Iterations to Escape")
    plt.title("Mandelbrot Set", fontsize=16)
    plt.xlabel("Re(c)")
    plt.ylabel("Im(c)")
    plt.show()

if __name__ == "__main__":
    main()
