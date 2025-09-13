import sys

def text_to_hex(text: str) -> str:
    """
    Converts a string to its hexadecimal representation.
    Each character is converted to its two-digit hex value (based on its
    ordinal value) and separated by a space.

    Args:
        text: The string to convert.

    Returns:
        The space-separated hexadecimal representation of the string.
    """
    return " ".join(f"{ord(char):02x}" for char in text)

def text_to_bin(text: str) -> str:
    """
    Converts a string to its 8-bit binary representation.
    Each character is converted to its 8-bit binary value (based on its
    ordinal value) and separated by a space.

    Args:
        text: The string to convert.

    Returns:
        The space-separated 8-bit binary representation of the string.
    """
    return " ".join(f"{ord(char):08b}" for char in text)

def main():
    """
    Main function to get user input for text and the desired conversion type.
    """
    print("--- Text to Hexadecimal and Binary Converter ---")

    try:
        text_to_convert = input("Enter the text you want to convert: ")
        if not text_to_convert:
            print("No input provided. Exiting.")
            return

        mode = ""
        while mode not in ['hex', 'bin']:
            mode = input("Choose conversion type ('hex' or 'bin'): ").lower().strip()
            if mode not in ['hex', 'bin']:
                print("Invalid choice. Please enter 'hex' or 'bin'.")

        if mode == 'hex':
            result = text_to_hex(text_to_convert)
            print(f"\nHexadecimal representation:\n{result}")
        else:  # mode == 'bin'
            result = text_to_bin(text_to_convert)
            print(f"\nBinary representation:\n{result}")

    except (EOFError, KeyboardInterrupt):
        print("\nOperation cancelled by user.")
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
