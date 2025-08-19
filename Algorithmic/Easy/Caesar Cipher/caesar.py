import string

def caesar_cipher(text, shift):
    """
    Applies the Caesar cipher to the given text.
    - Handles both uppercase and lowercase letters.
    - Non-alphabetic characters are passed through unchanged.
    - A positive shift encrypts; a negative shift decrypts.
    """
    result = []
    for char in text:
        if 'a' <= char <= 'z':
            start = ord('a')
            new_char_ord = (ord(char) - start + shift) % 26 + start
            result.append(chr(new_char_ord))
        elif 'A' <= char <= 'Z':
            start = ord('A')
            new_char_ord = (ord(char) - start + shift) % 26 + start
            result.append(chr(new_char_ord))
        else:
            result.append(char)
    return "".join(result)


def main():
    """
    Handles the user interaction for encrypting and decrypting messages.
    """
    # Get shift key
    while True:
        try:
            key = int(input("Enter shift key: "))
            break
        except ValueError:
            print("Invalid key. Please enter an integer.")

    # Get text to encrypt
    to_encrypt = input("Enter text to encrypt: ")
    encrypted_text = caesar_cipher(to_encrypt, key)
    print("Encrypted text:", encrypted_text)

    # Get text to decrypt/crack
    to_crack = input("Enter string to be cracked: ")
    # To crack, we simply apply the reverse shift
    decrypted_text = caesar_cipher(to_crack, -key)
    print("Cracked text:", decrypted_text)


if __name__ == "__main__":
    main()
