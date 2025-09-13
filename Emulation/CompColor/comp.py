import sys
import argparse
from PIL import Image, UnidentifiedImageError
import numpy as np

def complement_image(input_path: str, output_path: str):
    """
    Generates an image with complementary colors using the inversion method.

    The algorithm for the new color is:
    NewColor = (min(R,G,B) + max(R,G,B)) - OriginalColor

    This is applied to each pixel using efficient numpy vectorization.

    Args:
        input_path: The path to the input image file.
        output_path: The path where the output image will be saved.
    """
    try:
        with Image.open(input_path) as img:
            # Ensure the image is in RGB format to handle different modes like RGBA or P
            rgb_img = img.convert("RGB")
            in_data = np.asarray(rgb_img, dtype=np.int16) # Use a larger int type for calculation

            # Find the min and max values along the color channel axis (axis=2)
            lo = np.amin(in_data, axis=2, keepdims=True)
            hi = np.amax(in_data, axis=2, keepdims=True)

            # Apply the inversion formula to the entire array at once
            out_data = (lo + hi) - in_data

            # Clip values to the valid 0-255 range and convert back to uint8
            out_data = np.clip(out_data, 0, 255).astype(np.uint8)

            # Create a new image from the processed data
            out_img = Image.fromarray(out_data)
            out_img.save(output_path)
            print(f"Complementary image successfully saved to '{output_path}'")

    except FileNotFoundError:
        print(f"Error: The input file was not found at '{input_path}'", file=sys.stderr)
        sys.exit(1)
    except UnidentifiedImageError:
        print(f"Error: Cannot identify image file. '{input_path}' may be corrupt or not a valid image.", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)

def main():
    """
    Parses command-line arguments and runs the image complement function.
    """
    parser = argparse.ArgumentParser(
        description="Generate the complementary color version of an image.",
        epilog="Example: python comp.py cat.jpg cat_complemented.jpg"
    )
    parser.add_argument("input_file", help="Path to the input image file.")
    parser.add_argument("output_file", help="Path to save the output image file.")

    args = parser.parse_args()

    complement_image(args.input_file, args.output_file)

if __name__ == "__main__":
    main()
