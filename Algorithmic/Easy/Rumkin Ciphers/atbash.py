import string
import sys

def atbash_cipher(text: str) -> str:
    """
    Applies the Atbash cipher to a text.
    The Atbash cipher is a simple substitution cipher that reverses the alphabet.
    (i.e., A becomes Z, B becomes Y, and so on).
    This implementation handles both uppercase and lowercase letters and
    preserves all non-alphabetic characters.

    Args:
        text: The string to be ciphered.

    Returns:
        The Atbash-ciphered string.
    """
    # Define the normal and reversed alphabets
    alphabet = string.ascii_lowercase + string.ascii_uppercase
    reversed_alphabet = string.ascii_lowercase[::-1] + string.ascii_uppercase[::-1]

    # Create the translation table
    atbash_table = str.maketrans(alphabet, reversed_alphabet)

    return text.translate(atbash_table)

def main():
    """
    Main function to get user input and apply the Atbash cipher.
    """
    print("--- Atbash Cipher Tool ---")
    print("Reverses the alphabet (A->Z, B->Y, etc.). The cipher is its own inverse.")

    if len(sys.argv) > 1:
        text_to_process = " ".join(sys.argv[1:])
        print(f"\nProcessing text from command-line argument: '{text_to_process}'")
    else:
        try:
            text_to_process = input("Enter text to be ciphered: ")
        except (EOFError, KeyboardInterrupt):
            print("\nNo input provided. Exiting.")
            return

    if not text_to_process.strip():
        print("No text entered.")
        return

    processed_text = atbash_cipher(text_to_process)
    print(f"\nAtbash ciphered text: {processed_text}")

    # Atbash is a reciprocal cipher, so applying it twice yields the original text.
    reverted_text = atbash_cipher(processed_text)
    print(f"Applied cipher again (should revert to original): {reverted_text}")

if __name__ == "__main__":
    main()
