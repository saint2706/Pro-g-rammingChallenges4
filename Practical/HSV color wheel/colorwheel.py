import numpy as np
import matplotlib.pyplot as plt
import sys
import argparse
from typing import Literal

# Add a dependency check for the 'colour-science' library.
try:
    import colour
except ImportError:
    print("Error: The 'colour-science' library is required.", file=sys.stderr)
    print("Please install it using: pip install colour-science", file=sys.stderr)
    sys.exit(1)

OrientationMethod = Literal["Colour", "Matplotlib", "Nuke"]

def generate_colour_wheel(samples: int = 1024, clip_circle: bool = True, method: OrientationMethod = "Colour") -> np.ndarray:
    """
    Generates the data for an HSV colour wheel.

    Args:
        samples: The resolution of the wheel (e.g., 1024 for 1024x1024).
        clip_circle: If True, the wheel is clipped to a circle.
        method: The orientation method for the final image.
                Can be 'Colour', 'Matplotlib', or 'Nuke'.

    Returns:
        A numpy array containing the RGBA data for the colour wheel image.
    """
    # Create a grid of coordinates from -1 to 1
    xx, yy = np.meshgrid(np.linspace(-1, 1, samples), np.linspace(-1, 1, samples))

    # Convert cartesian coordinates to polar coordinates (Hue and Saturation)
    S = np.sqrt(xx**2 + yy**2)
    H = (np.arctan2(xx, yy) + np.pi) / (np.pi * 2)

    # Create an HSV image (Value is fixed at 1)
    HSV = colour.utilities.tstack([H, S, np.ones(H.shape)])
    # Convert HSV to RGB
    RGB = colour.HSV_to_RGB(HSV)

    # Handle clipping and alpha channel
    if clip_circle:
        RGB[S > 1] = 0
        A = np.where(S > 1, 0, 1)  # Alpha channel is 0 outside the circle
    else:
        A = np.ones(S.shape)

    # Handle different orientation standards
    if method.lower() == "matplotlib":
        RGB = colour.utilities.orient(RGB, "90 CW")
    elif method.lower() == "nuke":
        RGB = colour.utilities.orient(RGB, "Flip")
        RGB = colour.utilities.orient(RGB, "90 CW")

    R, G, B = colour.utilities.tsplit(RGB)

    return colour.utilities.tstack([R, G, B, A])

def main():
    """
    Main function to parse arguments, generate, and display the colour wheel.
    """
    parser = argparse.ArgumentParser(description="Generate and display an HSV colour wheel.")
    parser.add_argument("-s", "--samples", type=int, default=512,
                        help="Resolution of the wheel (e.g., 512 for 512x512). Default is 512.")
    parser.add_argument("--no-clip", action="store_false", dest="clip",
                        help="Generate a square wheel instead of a clipped circle.")
    parser.add_argument("-m", "--method", choices=['Colour', 'Matplotlib', 'Nuke'], default='Colour',
                        help="Specifies the wheel's color orientation. Default is 'Colour'.")

    args = parser.parse_args()

    # Use the colour-science plotting style for a nice look
    colour.plotting.colour_style()
    plt.style.use("dark_background")

    print(f"Generating a {args.samples}x{args.samples} colour wheel...")

    wheel_data = generate_colour_wheel(samples=args.samples, clip_circle=args.clip, method=args.method)

    # Add a title to the plot
    plot_kwargs = {'title': f"HSV Colour Wheel\nMethod: {args.method} | Samples: {args.samples} | Clipped: {args.clip}"}
    colour.plotting.plot_image(wheel_data, **plot_kwargs)

if __name__ == "__main__":
    main()
