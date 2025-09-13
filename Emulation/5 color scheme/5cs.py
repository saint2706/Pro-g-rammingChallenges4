import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import sys
import os
import argparse

# Add dependency checks for a better user experience.
try:
    import cv2
    from sklearn.cluster import KMeans
except ImportError as e:
    library_name = str(e).split("'")[1]
    print(f"Error: The '{library_name}' library is required.", file=sys.stderr)
    if library_name == 'cv2':
        print("Please install it using: pip install opencv-python", file=sys.stderr)
    else:
        print("Please install it using: pip install scikit-learn", file=sys.stderr)
    sys.exit(1)

def get_dominant_colors(image_path: str, num_colors: int) -> np.ndarray:
    """
    Finds the dominant colors in an image using K-Means clustering.

    Args:
        image_path: The path to the input image file.
        num_colors: The number of dominant colors to find.

    Returns:
        A numpy array of shape (num_colors, 3) containing the dominant RGB colors.

    Raises:
        FileNotFoundError: If the image_path does not exist.
        ValueError: If the image cannot be loaded by OpenCV.
    """
    if not os.path.exists(image_path):
        raise FileNotFoundError(f"Image not found at '{image_path}'")

    image = cv2.imread(image_path)
    if image is None:
        raise ValueError(f"Could not load image from '{image_path}'. It may be corrupt or an unsupported format.")

    # Convert the image from BGR (OpenCV's default) to RGB for correct color display.
    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)

    # Reshape the image to be a list of pixels (N_pixels, 3)
    pixels = image.reshape(-1, 3)

    # Apply K-means clustering to find the most common colors.
    # n_init='auto' is the modern default. random_state ensures reproducibility.
    kmeans = KMeans(n_clusters=num_colors, n_init='auto', random_state=42)
    kmeans.fit(pixels)

    # The cluster centers are the dominant colors.
    dominant_colors = kmeans.cluster_centers_.astype(np.uint8)

    return dominant_colors

def display_color_scheme(colors: np.ndarray):
    """
    Displays a list of colors as a horizontal palette using matplotlib.
    """
    num_colors = len(colors)
    fig, ax = plt.subplots(figsize=(num_colors * 1.5, 1.5))

    # Create a rectangle for each color
    for i, color in enumerate(colors):
        # matplotlib colors are expected to be in the [0, 1] range
        rect = patches.Rectangle((i, 0), 1, 1, color=color / 255.0)
        ax.add_patch(rect)
        # Add the RGB value as text
        ax.text(i + 0.5, -0.2, f"RGB: {color}", ha='center', va='center', fontsize=8)

    ax.set_xlim(0, num_colors)
    ax.set_ylim(-0.5, 1) # Adjust ylim to make space for text
    ax.set_title(f"Dominant Color Palette ({num_colors} colors)")
    ax.axis('off')
    plt.tight_layout()
    plt.show()

def main():
    """
    Main function to parse arguments and run the color scheme extractor.
    """
    default_image = os.path.join(os.path.dirname(__file__), "qwFWZv8.jpg")

    parser = argparse.ArgumentParser(description="Extract a dominant color scheme from an image.")
    parser.add_argument("-i", "--image", default=default_image,
                        help=f"Path to the input image. Defaults to '{default_image}'")
    parser.add_argument("-n", "--num_colors", type=int, default=5,
                        help="Number of dominant colors to extract. Defaults to 5.")

    args = parser.parse_args()

    try:
        print(f"Extracting {args.num_colors} dominant colors from '{args.image}'...")
        dominant_colors = get_dominant_colors(args.image, args.num_colors)

        print("\nDominant colors (RGB):")
        print(dominant_colors)

        display_color_scheme(dominant_colors)

    except (ValueError, FileNotFoundError) as e:
        print(f"\nError: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
