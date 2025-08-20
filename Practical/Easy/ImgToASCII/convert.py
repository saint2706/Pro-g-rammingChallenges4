from PIL import Image, UnidentifiedImageError
import sys
import os
import argparse
from typing import List

# Default character set used to represent image brightness, from dark to light.
DEFAULT_ASCII_CHARS = ["#", "?", "%", ".", "S", "+", "*", ":", ",", "@"]

def resize_image(image: Image.Image, new_width: int = 100) -> Image.Image:
    """Resizes an image while maintaining its aspect ratio."""
    old_width, old_height = image.size
    aspect_ratio = old_height / old_width
    new_height = int(aspect_ratio * new_width * 0.55) # 0.55 corrects for non-square character aspect ratio
    return image.resize((new_width, new_height))

def image_to_ascii(image: Image.Image, char_set: List[str]) -> str:
    """
    Converts the pixels of a grayscale image to a string of ASCII characters.
    """
    # Convert image to grayscale
    image = image.convert("L")
    pixels = image.getdata()

    # Map each pixel's brightness (0-255) to an ASCII character
    ascii_chars = []
    for pixel_value in pixels:
        index = int(pixel_value / 256 * len(char_set))
        ascii_chars.append(char_set[index])

    return "".join(ascii_chars)

def format_ascii_art(ascii_chars: str, width: int) -> str:
    """Formats a flat string of ASCII characters into a multi-line string."""
    return "\n".join(ascii_chars[i:i + width] for i in range(0, len(ascii_chars), width))

def main():
    """
    Main function to parse arguments and run the image-to-ASCII conversion.
    """
    parser = argparse.ArgumentParser(
        description="Convert an image file to ASCII art.",
        epilog="Example: python convert.py cat.png -w 120 -o output.txt"
    )
    parser.add_argument("input_path", help="Path to the input image file.")
    parser.add_argument("-o", "--output_path", help="Optional path to save the output .txt file.")
    parser.add_argument("-w", "--width", type=int, default=100, help="Width of the output ASCII art in characters.")
    parser.add_argument("-c", "--chars", default="".join(DEFAULT_ASCII_CHARS),
                        help=f"Custom character set from dark to light. Default is '{''.join(DEFAULT_ASCII_CHARS)}'")

    args = parser.parse_args()

    try:
        with Image.open(args.input_path) as img:
            resized_img = resize_image(img, args.width)
            ascii_chars_flat = image_to_ascii(resized_img, list(args.chars))
            ascii_art = format_ascii_art(ascii_chars_flat, args.width)

            if args.output_path:
                with open(args.output_path, "w") as f:
                    f.write(ascii_art)
                print(f"ASCII art successfully saved to '{args.output_path}'")
            else:
                print("\n--- ASCII Art ---\n")
                print(ascii_art)

    except FileNotFoundError:
        print(f"Error: Input file not found at '{args.input_path}'", file=sys.stderr)
    except UnidentifiedImageError:
        print(f"Error: Cannot identify image file. '{args.input_path}' may be corrupt or not a valid image.", file=sys.stderr)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
