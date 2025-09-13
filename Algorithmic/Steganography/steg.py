from PIL import Image
import sys
import argparse

# A unique End-Of-Message marker to signify the end of the hidden data.
EOM_MARKER = "||EOM||"

def text_to_bits(text: str) -> str:
    """Converts a string of text into a string of bits."""
    return ''.join(f"{ord(char):08b}" for char in text)

def bits_to_text(bits: str) -> str:
    """Converts a string of bits back into a string of text."""
    chars = []
    for i in range(0, len(bits), 8):
        byte_bits = bits[i:i+8]
        if len(byte_bits) == 8:
            chars.append(chr(int(byte_bits, 2)))
    return "".join(chars)

def hide_data(image_path: str, secret_message: str, output_path: str):
    """
    Hides a secret message within an image file using LSB steganography.
    """
    print(f"Hiding data in '{image_path}'...")
    try:
        image = Image.open(image_path).convert("RGB")
    except FileNotFoundError:
        print(f"Error: Image file not found at '{image_path}'", file=sys.stderr)
        return

    # Prepare the bit stream to be hidden
    bit_stream = text_to_bits(secret_message + EOM_MARKER)

    if len(bit_stream) > image.width * image.height * 3:
        raise ValueError("Message is too large to be hidden in the provided image.")

    pixels = image.load()
    bit_index = 0

    for y in range(image.height):
        for x in range(image.width):
            if bit_index >= len(bit_stream):
                break

            r, g, b = pixels[x, y]

            # Modify the LSB of the red channel
            if bit_index < len(bit_stream):
                r = (r & 0xFE) | int(bit_stream[bit_index])
                bit_index += 1

            # Modify the LSB of the green channel
            if bit_index < len(bit_stream):
                g = (g & 0xFE) | int(bit_stream[bit_index])
                bit_index += 1

            # Modify the LSB of the blue channel
            if bit_index < len(bit_stream):
                b = (b & 0xFE) | int(bit_stream[bit_index])
                bit_index += 1

            pixels[x, y] = (r, g, b)
        if bit_index >= len(bit_stream):
            break

    image.save(output_path)
    print(f"Data hidden successfully. Output saved to '{output_path}'.")

def extract_data(image_path: str) -> str:
    """
    Extracts a secret message from an image file.
    """
    print(f"Extracting data from '{image_path}'...")
    try:
        image = Image.open(image_path).convert("RGB")
    except FileNotFoundError:
        print(f"Error: Image file not found at '{image_path}'", file=sys.stderr)
        return None

    pixels = image.load()
    extracted_bits = ""

    for y in range(image.height):
        for x in range(image.width):
            r, g, b = pixels[x, y]
            extracted_bits += str(r & 1)
            extracted_bits += str(g & 1)
            extracted_bits += str(b & 1)

    # Convert bits to text and search for the EOM marker
    extracted_text = bits_to_text(extracted_bits)
    eom_index = extracted_text.find(EOM_MARKER)

    if eom_index != -1:
        return extracted_text[:eom_index]
    else:
        return "No hidden message found or message is corrupt."

def create_dummy_image(filepath: str, size: tuple = (100, 100)):
    """Creates a simple dummy image file for demonstration."""
    print(f"Creating a dummy image at '{filepath}'...")
    img = Image.new('RGB', size, color='white')
    img.save(filepath)

def main():
    """Main function to parse command-line arguments and run the tool."""
    parser = argparse.ArgumentParser(description="LSB Steganography Tool")
    subparsers = parser.add_subparsers(dest="command", required=True)

    # --- Hide Command ---
    hide_parser = subparsers.add_parser("hide", help="Hide a message in an image.")
    hide_parser.add_argument("image", help="Path to the input image file.")
    hide_parser.add_argument("output", help="Path for the output image file.")
    hide_parser.add_argument("-m", "--message", help="The secret message to hide.", required=True)

    # --- Extract Command ---
    extract_parser = subparsers.add_parser("extract", help="Extract a message from an image.")
    extract_parser.add_argument("image", help="Path to the image containing the hidden message.")

    # --- Create Dummy Command ---
    create_parser = subparsers.add_parser("create_dummy", help="Create a dummy image for testing.")
    create_parser.add_argument("output", help="Path to save the dummy image.", default="dummy.png", nargs='?')

    args = parser.parse_args()

    if args.command == "hide":
        hide_data(args.image, args.message, args.output)
    elif args.command == "extract":
        extracted_message = extract_data(args.image)
        if extracted_message:
            print(f"\nExtracted Message:\n---\n{extracted_message}\n---")
    elif args.command == "create_dummy":
        create_dummy_image(args.output)

if __name__ == "__main__":
    main()
