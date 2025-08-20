import math
import sys

def modular_inverse(a: int, m: int) -> int:
    """
    Calculates the modular multiplicative inverse of a modulo m using the
    Extended Euclidean Algorithm. Returns None if the inverse does not exist.
    """
    if math.gcd(a, m) != 1:
        return None  # Inverse does not exist

    # Use built-in pow for Python 3.8+
    if sys.version_info >= (3, 8):
        try:
            return pow(a, -1, m)
        except ValueError:
            return None

    # Fallback for older Python versions
    m0, x0, x1 = m, 0, 1
    while a > 1:
        q = a // m
        m, a = a % m, m
        x0, x1 = x1 - q * x0, x0
    if x1 < 0:
        x1 += m0
    return x1

def affine_cipher(text: str, key_a: int, key_b: int, mode: str) -> str:
    """
    Encrypts or decrypts text using the Affine cipher.

    Args:
        text: The input string.
        key_a: The 'a' coefficient of the cipher. Must be coprime with 26.
        key_b: The 'b' coefficient (the shift).
        mode: 'encrypt' or 'decrypt'.

    Returns:
        The transformed string.

    Raises:
        ValueError: If key 'a' is not valid or mode is incorrect.
    """
    if mode not in ['encrypt', 'decrypt']:
        raise ValueError("Mode must be 'encrypt' or 'decrypt'.")

    mod_inv_a = modular_inverse(key_a, 26)
    if mod_inv_a is None:
        raise ValueError(f"Key 'a' ({key_a}) is not coprime with 26, so it's not a valid key.")

    result = []
    for char in text:
        if char.isalpha():
            start = ord('a') if char.islower() else ord('A')

            if mode == 'encrypt':
                new_ord = (key_a * (ord(char) - start) + key_b) % 26
            else:  # 'decrypt'
                new_ord = (mod_inv_a * (ord(char) - start - key_b)) % 26

            result.append(chr(start + new_ord))
        else:
            result.append(char)

    return "".join(result)

def main():
    """
    Main function to get user input and perform Affine encryption/decryption.
    """
    print("--- Affine Cipher Tool ---")
    print("Encrypts/decrypts text using the formula: C = (a*P + b) mod 26")

    try:
        text = input("Enter the text: ")
        key_a_str = input("Enter key 'a' (an integer coprime with 26, e.g., 1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25): ")
        key_a = int(key_a_str)
        key_b_str = input("Enter key 'b' (the shift, an integer): ")
        key_b = int(key_b_str)

        mode = ""
        while mode not in ['encrypt', 'decrypt']:
            mode = input("Enter mode ('encrypt' or 'decrypt'): ").lower().strip()
            if mode not in ['encrypt', 'decrypt']:
                print("Invalid mode specified. Please try again.")

        processed_text = affine_cipher(text, key_a, key_b, mode)
        print(f"\n{mode.capitalize()}ed text: {processed_text}")

    except ValueError as e:
        print(f"\nError: {e}", file=sys.stderr)
    except (TypeError, EOFError, KeyboardInterrupt):
        print("\nOperation cancelled.")

if __name__ == "__main__":
    main()
