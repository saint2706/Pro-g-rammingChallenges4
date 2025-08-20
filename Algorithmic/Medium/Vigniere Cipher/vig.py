import sys
import argparse

def sanitize_key(key: str) -> str:
    """
    Sanitizes the Vigenère key by removing non-alphabetic characters
    and converting it to lowercase.

    Args:
        key: The raw key string.

    Returns:
        The sanitized key.
    """
    return "".join(filter(str.isalpha, key)).lower()

def vigenere_cipher(text: str, key: str, mode: str) -> str:
    """
    Encrypts or decrypts text using the Vigenère cipher.

    This function preserves the case of the original text and passes
    non-alphabetic characters through unchanged.

    Args:
        text: The string to be processed.
        key: The cipher key (will be sanitized).
        mode: The operation mode, either 'encrypt' or 'decrypt'.

    Returns:
        The transformed string.

    Raises:
        ValueError: If the sanitized key is empty or the mode is incorrect.
    """
    if mode not in ['encrypt', 'decrypt']:
        raise ValueError("Mode must be either 'encrypt' or 'decrypt'.")

    sanitized_key = sanitize_key(key)
    if not sanitized_key:
        raise ValueError("Invalid key: The key must contain at least one alphabetic character.")

    processed_text = []
    key_index = 0

    for char in text:
        if char.isalpha():
            # Determine the shift amount from the key
            shift = ord(sanitized_key[key_index % len(sanitized_key)]) - ord('a')

            if mode == 'decrypt':
                shift = -shift

            # Apply the shift, preserving the original case
            if char.isupper():
                start = ord('A')
                processed_char = chr((ord(char) - start + shift) % 26 + start)
            else:  # islower()
                start = ord('a')
                processed_char = chr((ord(char) - start + shift) % 26 + start)

            processed_text.append(processed_char)
            key_index += 1
        else:
            # Pass non-alphabetic characters through unchanged
            processed_text.append(char)

    return "".join(processed_text)

def main():
    """
    Main function to parse command-line arguments and run the Vigenère cipher tool.
    """
    parser = argparse.ArgumentParser(
        description="A tool to encrypt or decrypt text using the Vigenère cipher.",
        epilog="Example: python vig.py encrypt 'my secret key' -t 'Hello World!'"
    )
    parser.add_argument("mode", choices=['encrypt', 'decrypt'], help="The operation to perform: 'encrypt' or 'decrypt'.")
    parser.add_argument("key", help="The secret key for the cipher (e.g., 'secretkey').")
    parser.add_argument("-t", "--text", help="The text to process. If not provided, the tool will read from standard input.")

    args = parser.parse_args()

    if args.text:
        input_text = args.text
    else:
        print("Please enter the text to be processed (press Ctrl-D or Ctrl-Z on a new line to end):")
        try:
            input_text = sys.stdin.read()
        except (KeyboardInterrupt, EOFError):
            print("\nOperation cancelled.")
            sys.exit(0)

    try:
        result = vigenere_cipher(input_text, args.key, args.mode)
        print("\n--- Result ---")
        print(result)
        print("--------------")
    except ValueError as e:
        print(f"\nError: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
