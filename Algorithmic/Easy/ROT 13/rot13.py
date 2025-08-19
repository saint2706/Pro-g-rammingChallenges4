import sys
import os

# To import a file from a sibling directory, we need to add its path to sys.path
# This gets the directory of the current file (ROT 13), goes up one level ('..'),
# and then into 'Caesar Cipher'.
caesar_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'Caesar Cipher'))
sys.path.append(caesar_path)

try:
    # Now we can import the function from caesar.py
    from caesar import caesar_cipher
except ImportError:
    print("Error: Could not import the 'caesar_cipher' function from 'caesar.py'.")
    print(f"Please ensure 'caesar.py' is located in: {caesar_path}")
    sys.exit(1)

def rot13(text):
    """
    Applies the ROT13 cipher to a text using the caesar_cipher function with a shift of 13.
    """
    return caesar_cipher(text, 13)

def main():
    """
    Main function to get user input and demonstrate the ROT13 cipher.
    """
    text_to_process = input("Enter text to apply ROT13 to: ")
    processed_text = rot13(text_to_process)
    print("Processed text:", processed_text)

    # As a demonstration, we can show that applying ROT13 twice returns the original text.
    reverted_text = rot13(processed_text)
    print("Applying ROT13 again (should revert to original):", reverted_text)

if __name__ == "__main__":
    main()
