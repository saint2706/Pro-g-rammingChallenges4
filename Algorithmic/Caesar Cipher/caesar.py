#!/usr/bin/env python3
"""
Advanced Caesar Cipher Implementation

A comprehensive Caesar cipher tool that provides encryption, decryption, and
cryptanalysis capabilities. The Caesar cipher is a substitution cipher where
each letter is shifted by a fixed number of positions in the alphabet.

Features:
- Encrypt/decrypt text with custom shift values
- Brute force attack to crack unknown shifts
- Frequency analysis for cryptanalysis
- Support for multiple alphabets and character sets
- Interactive and command-line interfaces
- Statistical analysis of cipher strength

Example Usage:
    $ python caesar.py encrypt "Hello World" 3
    $ python caesar.py decrypt "Khoor Zruog" 3
    $ python caesar.py crack "Khoor Zruog"
    $ python caesar.py --interactive

Author: Programming Challenges
Date: 2025
Version: 2.0
License: MIT
"""

import argparse
import logging
import string
import sys
from collections import Counter
from enum import Enum
from typing import Dict, List, Optional, Tuple, Union


class CipherMode(Enum):
    """Enumeration of supported cipher operations."""

    ENCRYPT = "encrypt"
    DECRYPT = "decrypt"
    CRACK = "crack"
    ANALYZE = "analyze"


class AlphabetType(Enum):
    """Enumeration of supported alphabet types."""

    LETTERS_ONLY = "letters"
    ALPHANUMERIC = "alphanumeric"
    PRINTABLE = "printable"


# English letter frequency distribution (approximate percentages)
ENGLISH_FREQUENCY = {
    "E": 12.7,
    "T": 9.1,
    "A": 8.2,
    "O": 7.5,
    "I": 7.0,
    "N": 6.7,
    "S": 6.3,
    "H": 6.1,
    "R": 6.0,
    "D": 4.3,
    "L": 4.0,
    "C": 2.8,
    "U": 2.8,
    "M": 2.4,
    "W": 2.4,
    "F": 2.2,
    "G": 2.0,
    "Y": 2.0,
    "P": 1.9,
    "B": 1.3,
    "V": 1.0,
    "K": 0.8,
    "J": 0.15,
    "X": 0.15,
    "Q": 0.10,
    "Z": 0.07,
}


def setup_logging(level: str = "INFO") -> None:
    """
    Configure logging for the application.

    Args:
        level: Logging level (DEBUG, INFO, WARNING, ERROR)
    """
    logging.basicConfig(
        level=getattr(logging, level.upper()),
        format="%(asctime)s - %(levelname)s - %(message)s",
        datefmt="%H:%M:%S",
    )


def caesar_cipher(
    text: str, shift: int, alphabet_type: AlphabetType = AlphabetType.LETTERS_ONLY
) -> str:
    """
    Applies the Caesar cipher to the given text with enhanced alphabet support.

    The Caesar cipher shifts each character by a fixed number of positions in the
    specified alphabet. This implementation supports multiple character sets and
    preserves the case of letters when using letter-only mode.

    Args:
        text: The string to be encrypted or decrypted
        shift: The number of positions to shift each character. Positive values
               encrypt, negative values decrypt. Values outside the alphabet size
               are automatically wrapped using modulo arithmetic.
        alphabet_type: The type of alphabet/character set to use for shifting

    Returns:
        The processed string with characters shifted according to the cipher

    Raises:
        ValueError: If text is not a string or shift is not an integer

    Examples:
        >>> caesar_cipher("Hello", 3)
        'Khoor'
        >>> caesar_cipher("Khoor", -3)  # Decrypt
        'Hello'
        >>> caesar_cipher("Hello123", 3, AlphabetType.ALPHANUMERIC)
        'Khoor456'

    Note:
        - Non-alphabetic characters are preserved when using LETTERS_ONLY mode
        - Case is preserved for letters (A->D, a->d with shift=3)
        - The shift value is normalized using modulo arithmetic
    """
    if not isinstance(text, str):
        raise ValueError("Text must be a string")
    if not isinstance(shift, int):
        raise ValueError("Shift must be an integer")

    if not text:
        return ""

    result = []

    # Define character sets based on alphabet type
    if alphabet_type == AlphabetType.LETTERS_ONLY:
        # Process each character individually for case preservation
        for char in text:
            if char.isalpha():
                # Determine if uppercase or lowercase and get base ASCII value
                is_upper = char.isupper()
                base = ord("A") if is_upper else ord("a")

                # Apply Caesar shift with modulo wrapping
                shifted_value = (ord(char) - base + shift) % 26
                new_char = chr(shifted_value + base)
                result.append(new_char)
            else:
                # Non-alphabetic characters pass through unchanged
                result.append(char)

    elif alphabet_type == AlphabetType.ALPHANUMERIC:
        # Use alphanumeric character set (preserving case for letters)
        for char in text:
            if char.isalnum():
                if char.isalpha():
                    # Handle letters with case preservation
                    is_upper = char.isupper()
                    base = ord("A") if is_upper else ord("a")
                    shifted_value = (ord(char) - base + shift) % 26
                    new_char = chr(shifted_value + base)
                    result.append(new_char)
                elif char.isdigit():
                    # Handle digits (0-9)
                    shifted_value = (ord(char) - ord("0") + shift) % 10
                    new_char = chr(shifted_value + ord("0"))
                    result.append(new_char)
            else:
                # Non-alphanumeric characters pass through unchanged
                result.append(char)

    elif alphabet_type == AlphabetType.PRINTABLE:
        # Use all printable ASCII characters (32-126)
        printable_chars = string.printable[:95]  # Exclude whitespace variations
        for char in text:
            if char in printable_chars:
                char_index = printable_chars.index(char)
                new_index = (char_index + shift) % len(printable_chars)
                result.append(printable_chars[new_index])
            else:
                # Non-printable characters pass through unchanged
                result.append(char)

    return "".join(result)


def calculate_frequency_score(text: str) -> float:
    """
    Calculate how closely text matches English letter frequency distribution.

    This function computes a fitness score based on how well the character
    frequency distribution of the input text matches expected English frequencies.
    Lower scores indicate better matches to English text.

    Args:
        text: The text to analyze for English-like characteristics

    Returns:
        A fitness score where lower values indicate better English matches.
        Typical English text scores around 0.03-0.06.

    Example:
        >>> calculate_frequency_score("Hello World")
        0.045  # Good English text
        >>> calculate_frequency_score("Xukkm Zmkkg")
        0.089  # Poor English match
    """
    if not text:
        return float("inf")

    # Count letter frequencies (case-insensitive)
    letter_count = Counter(char.upper() for char in text if char.isalpha())

    if not letter_count:
        return float("inf")  # No letters to analyze

    total_letters = sum(letter_count.values())

    # Calculate chi-squared statistic comparing to English frequencies
    chi_squared = 0.0
    for letter in string.ascii_uppercase:
        observed_freq = (letter_count.get(letter, 0) / total_letters) * 100
        expected_freq = ENGLISH_FREQUENCY.get(letter, 0)

        # Avoid division by zero
        if expected_freq > 0:
            chi_squared += ((observed_freq - expected_freq) ** 2) / expected_freq

    return chi_squared / 100  # Normalize the score


def brute_force_crack(
    ciphertext: str, alphabet_type: AlphabetType = AlphabetType.LETTERS_ONLY
) -> List[Tuple[int, str, float]]:
    """
    Attempt to crack a Caesar cipher by trying all possible shift values.

    This function tests all possible shift values and ranks the results by how
    closely they match English text frequency patterns. Results are sorted by
    likelihood of being correct English text.

    Args:
        ciphertext: The encrypted text to crack
        alphabet_type: The alphabet type used in the original encryption

    Returns:
        A list of tuples containing (shift_value, decrypted_text, fitness_score)
        sorted by fitness score (best matches first)

    Example:
        >>> results = brute_force_crack("Khoor Zruog")
        >>> best_shift, best_text, score = results[0]
        >>> print(f"Best match: '{best_text}' with shift {best_shift}")
        Best match: 'Hello World' with shift 23
    """
    if not ciphertext:
        return []

    results = []

    # Determine the range of shifts to test based on alphabet type
    if alphabet_type == AlphabetType.LETTERS_ONLY:
        max_shift = 26
    elif alphabet_type == AlphabetType.ALPHANUMERIC:
        max_shift = 26  # Still focus on letter shifts for frequency analysis
    else:  # PRINTABLE
        max_shift = 95

    # Try all possible shift values
    for shift in range(max_shift):
        try:
            decrypted = caesar_cipher(ciphertext, -shift, alphabet_type)
            fitness_score = calculate_frequency_score(decrypted)
            results.append((shift, decrypted, fitness_score))
        except (ValueError, TypeError):
            # Skip invalid shifts
            continue

    # Sort by fitness score (lower is better)
    results.sort(key=lambda x: x[2])

    return results


def analyze_text(text: str) -> Dict[str, Union[int, float, str, Dict[str, int]]]:
    """
    Perform statistical analysis on text for cryptanalysis purposes.

    Args:
        text: The text to analyze

    Returns:
        Dictionary containing various statistical measures of the text
    """
    if not text:
        return {"error": "Empty text provided"}

    # Basic statistics
    total_chars = len(text)
    letters_only = "".join(char for char in text if char.isalpha())
    total_letters = len(letters_only)

    # Character frequency analysis
    char_frequency = Counter(text.upper())
    letter_frequency = Counter(char.upper() for char in text if char.isalpha())

    # Calculate index of coincidence (measure of non-randomness)
    ic = 0.0
    if total_letters > 1:
        for count in letter_frequency.values():
            ic += count * (count - 1)
        ic = ic / (total_letters * (total_letters - 1))

    # English text typically has IC around 0.067
    analysis = {
        "total_characters": total_chars,
        "total_letters": total_letters,
        "letter_percentage": (
            (total_letters / total_chars * 100) if total_chars > 0 else 0
        ),
        "index_of_coincidence": ic,
        "english_likelihood": calculate_frequency_score(text),
        "most_common_letters": dict(letter_frequency.most_common(5)),
        "character_distribution": dict(char_frequency.most_common(10)),
    }

    return analysis


def interactive_mode() -> None:
    """
    Runs the application in interactive mode with a user-friendly interface.

    Provides a comprehensive menu-driven interface for all cipher operations
    including encryption, decryption, cracking, and analysis.
    """
    print("=" * 70)
    print("    Advanced Caesar Cipher Tool - Interactive Mode")
    print("=" * 70)
    print("Features: Encrypt, Decrypt, Brute Force Crack, Frequency Analysis")
    print()

    while True:
        try:
            print("Available operations:")
            print("1. Encrypt text")
            print("2. Decrypt text (with known shift)")
            print("3. Crack cipher (brute force attack)")
            print("4. Analyze text (frequency analysis)")
            print("5. Compare multiple decryptions")
            print("6. Help & Examples")
            print("7. Exit")
            print()

            choice = input("Select an operation (1-7): ").strip()

            if choice == "7":
                print("Goodbye! Stay secure!")
                break
            elif choice == "6":
                show_help_examples()
                continue
            elif choice in ["1", "2", "3", "4", "5"]:
                text_input = input("Enter text: ").strip()
                if not text_input:
                    print("No input provided.")
                    continue

                try:
                    if choice == "1":  # Encrypt
                        shift = int(input("Enter shift value (1-25): "))
                        if not 1 <= shift <= 25:
                            print("Warning: Shift outside typical range (1-25)")
                        result = caesar_cipher(text_input, shift)
                        print(f"\nEncrypted text: {result}")
                        print(f"To decrypt, use shift: {-shift}")

                    elif choice == "2":  # Decrypt
                        shift = int(input("Enter shift value used for encryption: "))
                        result = caesar_cipher(text_input, -shift)
                        print(f"\nDecrypted text: {result}")

                    elif choice == "3":  # Crack
                        print("\nPerforming brute force attack...")
                        results = brute_force_crack(text_input)

                        print(f"\nTop 5 most likely decryptions:")
                        print(f"{'Shift':<6} {'Fitness':<10} {'Decrypted Text'}")
                        print("-" * 60)

                        for i, (shift, decrypted, score) in enumerate(results[:5]):
                            print(f"{shift:<6} {score:<10.3f} {decrypted}")

                        if results:
                            best_shift, best_text, best_score = results[0]
                            print(f"\nBest guess: '{best_text}' (shift: {best_shift})")

                    elif choice == "4":  # Analyze
                        analysis = analyze_text(text_input)
                        print(f"\n--- Text Analysis ---")
                        print(f"Total characters: {analysis['total_characters']}")
                        print(f"Total letters: {analysis['total_letters']}")
                        print(
                            f"Letter percentage: {analysis['letter_percentage']:.1f}%"
                        )
                        print(
                            f"Index of Coincidence: {analysis['index_of_coincidence']:.3f}"
                        )
                        print(
                            f"English likelihood: {analysis['english_likelihood']:.3f}"
                        )
                        print(f"Most common letters: {analysis['most_common_letters']}")

                    elif choice == "5":  # Compare multiple
                        print("\nTesting shifts 1-5:")
                        for test_shift in range(1, 6):
                            decrypted = caesar_cipher(text_input, -test_shift)
                            score = calculate_frequency_score(decrypted)
                            print(
                                f"Shift {test_shift}: {decrypted} (score: {score:.3f})"
                            )

                except ValueError as e:
                    print(f"Error: {e}")
                except Exception as e:
                    logging.error(f"Unexpected error: {e}")
                    print(f"An unexpected error occurred: {e}")

            else:
                print("Invalid choice. Please select 1-7.")

            print("-" * 50)

        except (EOFError, KeyboardInterrupt):
            print("\nOperation cancelled by user. Goodbye!")
            break
        except Exception as e:
            logging.error(f"Unexpected error in interactive mode: {e}")
            print(f"An unexpected error occurred: {e}")


def show_help_examples() -> None:
    """Display help information and usage examples."""
    print("\n" + "=" * 60)
    print("    CAESAR CIPHER HELP & EXAMPLES")
    print("=" * 60)
    print()
    print("The Caesar cipher shifts each letter by a fixed number of positions.")
    print("For example, with shift 3: A→D, B→E, C→F, ..., X→A, Y→B, Z→C")
    print()
    print("EXAMPLES:")
    print("• Encrypt 'HELLO' with shift 3 → 'KHOOR'")
    print("• Decrypt 'KHOOR' with shift 3 → 'HELLO'")
    print("• Crack 'KHOOR' → tries all shifts, finds 'HELLO' as best match")
    print()
    print("FREQUENCY ANALYSIS:")
    print("• Lower fitness scores indicate better English text matches")
    print("• English text typically has fitness scores around 0.03-0.06")
    print("• Index of Coincidence for English ≈ 0.067")
    print()
    print("COMMAND LINE USAGE:")
    print("python caesar.py encrypt 'Hello World' 3")
    print("python caesar.py decrypt 'Khoor Zruog' 3")
    print("python caesar.py crack 'Khoor Zruog'")
    print()


def create_argument_parser() -> argparse.ArgumentParser:
    """
    Creates and configures the command-line argument parser.

    Returns:
        Configured ArgumentParser instance with all supported options
    """
    parser = argparse.ArgumentParser(
        description="Advanced Caesar Cipher Tool with cryptanalysis capabilities",
        epilog="""
Examples:
  %(prog)s encrypt "Hello World" 3
  %(prog)s decrypt "Khoor Zruog" 3  
  %(prog)s crack "Khoor Zruog"
  %(prog)s analyze "This is sample text"
  %(prog)s --interactive
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "mode",
        nargs="?",
        choices=["encrypt", "decrypt", "crack", "analyze"],
        help="Operation mode: encrypt, decrypt, crack, or analyze",
    )

    parser.add_argument(
        "text", nargs="?", help="Text to process (required for command-line mode)"
    )

    parser.add_argument(
        "shift",
        nargs="?",
        type=int,
        help="Shift value (required for encrypt/decrypt modes)",
    )

    parser.add_argument(
        "--alphabet",
        "-a",
        choices=["letters", "alphanumeric", "printable"],
        default="letters",
        help="Character set to use (default: letters)",
    )

    parser.add_argument(
        "--interactive", "-i", action="store_true", help="Run in interactive mode"
    )

    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable verbose output and logging"
    )

    parser.add_argument(
        "--top",
        "-t",
        type=int,
        default=5,
        help="Number of top results to show when cracking (default: 5)",
    )

    return parser


def main() -> None:
    """
    Main function that handles command-line arguments and executes operations.

    Provides both command-line and interactive interfaces for the Caesar cipher tool.
    """
    parser = create_argument_parser()
    args = parser.parse_args()

    # Setup logging
    log_level = "DEBUG" if args.verbose else "INFO"
    setup_logging(log_level)

    # Convert alphabet string to enum
    alphabet_map = {
        "letters": AlphabetType.LETTERS_ONLY,
        "alphanumeric": AlphabetType.ALPHANUMERIC,
        "printable": AlphabetType.PRINTABLE,
    }
    alphabet_type = alphabet_map[args.alphabet]

    # Interactive mode
    if args.interactive:
        interactive_mode()
        return

    # Command-line mode validation
    if not args.mode:
        print("Error: Mode is required for command-line usage.", file=sys.stderr)
        print(
            "Use --interactive for interactive mode, or --help for usage information."
        )
        sys.exit(1)

    if not args.text:
        print("Error: Text is required for command-line usage.", file=sys.stderr)
        sys.exit(1)

    if args.mode in ["encrypt", "decrypt"] and args.shift is None:
        print(f"Error: Shift value is required for {args.mode} mode.", file=sys.stderr)
        sys.exit(1)

    try:
        # Execute the requested operation
        if args.mode == "encrypt":
            result = caesar_cipher(args.text, args.shift, alphabet_type)
            print(f"Encrypted text: {result}")
            if args.verbose:
                print(f"Original: {args.text}")
                print(f"Shift: {args.shift}")
                print(f"Alphabet: {args.alphabet}")

        elif args.mode == "decrypt":
            result = caesar_cipher(args.text, -args.shift, alphabet_type)
            print(f"Decrypted text: {result}")
            if args.verbose:
                print(f"Ciphertext: {args.text}")
                print(f"Shift: {args.shift}")

        elif args.mode == "crack":
            print("Performing brute force attack...")
            results = brute_force_crack(args.text, alphabet_type)

            if results:
                print(f"\nTop {min(args.top, len(results))} most likely decryptions:")
                print(f"{'Rank':<5} {'Shift':<6} {'Fitness':<10} {'Decrypted Text'}")
                print("-" * 60)

                for i, (shift, decrypted, score) in enumerate(results[: args.top], 1):
                    print(f"{i:<5} {shift:<6} {score:<10.3f} {decrypted}")

                best_shift, best_text, best_score = results[0]
                print(
                    f"\nBest guess: '{best_text}' (shift: {best_shift}, fitness: {best_score:.3f})"
                )
            else:
                print("No valid decryptions found.")

        elif args.mode == "analyze":
            analysis = analyze_text(args.text)
            print("Text Analysis Results:")
            print("-" * 30)
            for key, value in analysis.items():
                if key == "error":
                    print(f"Error: {value}")
                elif isinstance(value, dict):
                    print(
                        f"{key.replace('_', ' ').title()}: {dict(list(value.items())[:5])}"
                    )
                elif isinstance(value, float):
                    print(f"{key.replace('_', ' ').title()}: {value:.3f}")
                else:
                    print(f"{key.replace('_', ' ').title()}: {value}")

    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        logging.error(f"Unexpected error: {e}")
        print(f"An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
