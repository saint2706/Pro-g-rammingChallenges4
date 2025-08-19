import string
import sys

def rot13(text: str) -> str:
    """
    Applies the ROT13 cipher to a text using a direct translation method.
    This method is efficient and self-contained. It correctly handles
    both uppercase and lowercase letters, leaving other characters unchanged.

    Args:
        text: The string to be processed.

    Returns:
        The ROT13 transformed string.
    """
    # Create the translation table for ROT13
    # 'abcdefghijklmnopqrstuvwxyz' -> 'nopqrstuvwxyzabcdefghijklm'
    input_chars = string.ascii_lowercase + string.ascii_uppercase
    output_chars = (string.ascii_lowercase[13:] + string.ascii_lowercase[:13] +
                    string.ascii_uppercase[13:] + string.ascii_uppercase[:13])

    rot13_table = str.maketrans(input_chars, output_chars)

    return text.translate(rot13_table)

def main():
    """
    Main function to get user input and demonstrate the ROT13 cipher.
    It can take input from a command-line argument or an interactive prompt.
    """
    print("--- ROT13 Cipher Tool ---")

    if len(sys.argv) > 1:
        text_to_process = " ".join(sys.argv[1:])
        print(f"Processing text from command-line argument: '{text_to_process}'")
    else:
        try:
            text_to_process = input("Enter text to apply ROT13 to: ")
        except (EOFError, KeyboardInterrupt):
            print("\nNo input provided. Exiting.")
            return

    processed_text = rot13(text_to_process)
    print("\nProcessed text:", processed_text)

    # Demonstrate that applying ROT13 twice reverts the text
    reverted_text = rot13(processed_text)
    print("Applied ROT13 again (reverted):", reverted_text)


if __name__ == "__main__":
    main()
