#!/usr/bin/env python3
"""
Text Encoding Converter

A comprehensive text encoding utility that converts text to various representations
including hexadecimal, binary, and supports multiple character encodings.

This module provides functions to convert text to:
- Hexadecimal representation
- Binary representation
- Support for different character encodings (UTF-8, UTF-16, ASCII)
- Reverse conversion capabilities

Example:
    $ python txtToHexAndBin.py "Hello" hex
    $ python txtToHexAndBin.py --interactive

Author: Programming Challenges
Date: 2025
Version: 2.0
"""

import sys
import argparse
import logging
from typing import Optional, Union, Literal
from enum import Enum


class ConversionMode(Enum):
    """Enumeration of supported conversion modes."""

    HEX = "hex"
    BINARY = "bin"


class Encoding(Enum):
    """Enumeration of supported character encodings."""

    UTF8 = "utf-8"
    UTF16 = "utf-16"
    ASCII = "ascii"


def setup_logging(level: str = "INFO") -> None:
    """
    Configure logging for the application.

    Args:
        level: Logging level (DEBUG, INFO, WARNING, ERROR)
    """
    logging.basicConfig(
        level=getattr(logging, level.upper()),
        format="%(asctime)s - %(levelname)s - %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S",
    )


def text_to_hex(text: str, encoding: str = "utf-8", separator: str = " ") -> str:
    """
    Converts a string to its hexadecimal representation using specified encoding.

    This function encodes the input text using the specified character encoding
    and then converts each byte to its two-digit hexadecimal representation.

    Args:
        text: The string to convert to hexadecimal
        encoding: Character encoding to use (default: "utf-8")
        separator: String to use between hex values (default: " ")

    Returns:
        The hexadecimal representation of the encoded text

    Raises:
        UnicodeEncodeError: If text cannot be encoded with specified encoding
        ValueError: If encoding is not supported

    Example:
        >>> text_to_hex("Hello")
        '48 65 6c 6c 6f'
        >>> text_to_hex("Hello", separator="")
        '48656c6c6f'
        >>> text_to_hex("Hello", encoding="ascii")
        '48 65 6c 6c 6f'
    """
    if not isinstance(text, str):
        raise TypeError("Input text must be a string")

    if not text:
        return ""

    try:
        # Convert text to bytes using specified encoding
        byte_data = text.encode(encoding)
        # Convert each byte to 2-digit hexadecimal
        hex_values = [f"{byte:02x}" for byte in byte_data]
        return separator.join(hex_values)
    except LookupError as e:
        raise ValueError(f"Unsupported encoding: {encoding}") from e
    except UnicodeEncodeError as e:
        logging.error(f"Cannot encode text with {encoding}: {e}")
        raise


def text_to_bin(text: str, encoding: str = "utf-8", separator: str = " ") -> str:
    """
    Converts a string to its binary representation using specified encoding.

    This function encodes the input text using the specified character encoding
    and then converts each byte to its 8-bit binary representation.

    Args:
        text: The string to convert to binary
        encoding: Character encoding to use (default: "utf-8")
        separator: String to use between binary values (default: " ")

    Returns:
        The binary representation of the encoded text

    Raises:
        UnicodeEncodeError: If text cannot be encoded with specified encoding
        ValueError: If encoding is not supported

    Example:
        >>> text_to_bin("Hi")
        '01001000 01101001'
        >>> text_to_bin("Hi", separator="")
        '0100100001101001'
        >>> text_to_bin("Hi", encoding="ascii")
        '01001000 01101001'
    """
    if not isinstance(text, str):
        raise TypeError("Input text must be a string")

    if not text:
        return ""

    try:
        # Convert text to bytes using specified encoding
        byte_data = text.encode(encoding)
        # Convert each byte to 8-bit binary
        binary_values = [f"{byte:08b}" for byte in byte_data]
        return separator.join(binary_values)
    except LookupError as e:
        raise ValueError(f"Unsupported encoding: {encoding}") from e
    except UnicodeEncodeError as e:
        logging.error(f"Cannot encode text with {encoding}: {e}")
        raise


def hex_to_text(hex_string: str, encoding: str = "utf-8", separator: str = " ") -> str:
    """
    Converts a hexadecimal string back to text using specified encoding.

    Args:
        hex_string: Space-separated hexadecimal values
        encoding: Character encoding to use for decoding
        separator: String that separates hex values

    Returns:
        The decoded text string

    Raises:
        ValueError: If hex string is invalid or cannot be decoded

    Example:
        >>> hex_to_text("48 65 6c 6c 6f")
        'Hello'
    """
    if not hex_string.strip():
        return ""

    try:
        # Split hex string and convert to bytes
        hex_values = (
            hex_string.split(separator)
            if separator
            else [hex_string[i : i + 2] for i in range(0, len(hex_string), 2)]
        )
        byte_data = bytes(int(hex_val, 16) for hex_val in hex_values if hex_val)
        return byte_data.decode(encoding)
    except (ValueError, UnicodeDecodeError) as e:
        raise ValueError(
            f"Invalid hex string or cannot decode with {encoding}: {e}"
        ) from e


def bin_to_text(
    binary_string: str, encoding: str = "utf-8", separator: str = " "
) -> str:
    """
    Converts a binary string back to text using specified encoding.

    Args:
        binary_string: Space-separated binary values (8-bit each)
        encoding: Character encoding to use for decoding
        separator: String that separates binary values

    Returns:
        The decoded text string

    Raises:
        ValueError: If binary string is invalid or cannot be decoded

    Example:
        >>> bin_to_text("01001000 01101001")
        'Hi'
    """
    if not binary_string.strip():
        return ""

    try:
        # Split binary string and convert to bytes
        binary_values = (
            binary_string.split(separator)
            if separator
            else [binary_string[i : i + 8] for i in range(0, len(binary_string), 8)]
        )
        byte_data = bytes(int(bin_val, 2) for bin_val in binary_values if bin_val)
        return byte_data.decode(encoding)
    except (ValueError, UnicodeDecodeError) as e:
        raise ValueError(
            f"Invalid binary string or cannot decode with {encoding}: {e}"
        ) from e


def validate_encoding(encoding: str) -> bool:
    """
    Validates if the given encoding is supported.

    Args:
        encoding: The encoding name to validate

    Returns:
        True if encoding is supported, False otherwise
    """
    try:
        "test".encode(encoding)
        return True
    except LookupError:
        return False


def format_output(data: str, line_length: int = 80) -> str:
    """
    Formats output data for better readability by adding line breaks.

    Args:
        data: The data string to format
        line_length: Maximum characters per line

    Returns:
        Formatted string with line breaks
    """
    if len(data) <= line_length:
        return data

    lines = []
    for i in range(0, len(data), line_length):
        lines.append(data[i : i + line_length])

    return "\n".join(lines)


def interactive_mode() -> None:
    """
    Runs the application in interactive mode with a user-friendly interface.

    Provides a menu-driven interface for text conversion operations.
    """
    print("=" * 60)
    print("    Text Encoding Converter - Interactive Mode")
    print("=" * 60)
    print()

    while True:
        try:
            print("Available operations:")
            print("1. Text to Hexadecimal")
            print("2. Text to Binary")
            print("3. Hexadecimal to Text")
            print("4. Binary to Text")
            print("5. Change encoding (current: utf-8)")
            print("6. Exit")
            print()

            choice = input("Select an operation (1-6): ").strip()

            if choice == "6":
                print("Goodbye!")
                break
            elif choice == "5":
                encoding = input(
                    "Enter encoding (utf-8, ascii, utf-16, etc.): "
                ).strip()
                if validate_encoding(encoding):
                    print(f"Encoding changed to: {encoding}")
                    # Store encoding for subsequent operations
                    current_encoding = encoding
                else:
                    print("Invalid encoding. Keeping current encoding.")
                continue
            elif choice in ["1", "2", "3", "4"]:
                text_input = input("Enter text/data: ").strip()
                if not text_input:
                    print("No input provided.")
                    continue

                try:
                    if choice == "1":
                        result = text_to_hex(text_input)
                        print(f"\nHexadecimal:\n{format_output(result)}")
                    elif choice == "2":
                        result = text_to_bin(text_input)
                        print(f"\nBinary:\n{format_output(result)}")
                    elif choice == "3":
                        result = hex_to_text(text_input)
                        print(f"\nDecoded text: {result}")
                    elif choice == "4":
                        result = bin_to_text(text_input)
                        print(f"\nDecoded text: {result}")

                except (ValueError, TypeError, UnicodeError) as e:
                    print(f"Error: {e}")

            else:
                print("Invalid choice. Please select 1-6.")

            print("-" * 40)

        except (EOFError, KeyboardInterrupt):
            print("\nOperation cancelled by user. Goodbye!")
            break
        except Exception as e:
            logging.error(f"Unexpected error in interactive mode: {e}")
            print(f"An unexpected error occurred: {e}")


def create_argument_parser() -> argparse.ArgumentParser:
    """
    Creates and configures the command-line argument parser.

    Returns:
        Configured ArgumentParser instance
    """
    parser = argparse.ArgumentParser(
        description="Convert text to hexadecimal, binary, and vice versa",
        epilog="""
Examples:
  %(prog)s "Hello World" hex
  %(prog)s "48656c6c6f" hex2text  
  %(prog)s "Hello" bin --encoding ascii
  %(prog)s --interactive
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "text", nargs="?", help="Text to convert (not needed in interactive mode)"
    )

    parser.add_argument(
        "mode",
        nargs="?",
        choices=["hex", "bin", "hex2text", "bin2text"],
        help="Conversion mode: hex, bin, hex2text, bin2text",
    )

    parser.add_argument(
        "--encoding",
        "-e",
        default="utf-8",
        help="Character encoding to use (default: utf-8)",
    )

    parser.add_argument(
        "--separator",
        "-s",
        default=" ",
        help="Separator for output values (default: space)",
    )

    parser.add_argument(
        "--no-format",
        action="store_true",
        help="Disable output formatting (no line breaks)",
    )

    parser.add_argument(
        "--interactive", "-i", action="store_true", help="Run in interactive mode"
    )

    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable verbose logging"
    )

    return parser


def main() -> None:
    """
    Main function that handles command-line arguments and executes conversions.

    Provides both command-line and interactive interfaces for text conversion.
    """
    parser = create_argument_parser()
    args = parser.parse_args()

    # Setup logging
    log_level = "DEBUG" if args.verbose else "INFO"
    setup_logging(log_level)

    # Validate encoding
    if not validate_encoding(args.encoding):
        print(f"Error: Unsupported encoding '{args.encoding}'", file=sys.stderr)
        sys.exit(1)

    # Interactive mode
    if args.interactive:
        interactive_mode()
        return

    # Command-line mode requires both text and mode
    if not args.text or not args.mode:
        print(
            "Error: Text and mode are required for command-line usage.", file=sys.stderr
        )
        print(
            "Use --interactive for interactive mode, or --help for usage information."
        )
        sys.exit(1)

    try:
        # Perform conversion based on mode
        if args.mode == "hex":
            result = text_to_hex(args.text, args.encoding, args.separator)
            print("Hexadecimal representation:")
        elif args.mode == "bin":
            result = text_to_bin(args.text, args.encoding, args.separator)
            print("Binary representation:")
        elif args.mode == "hex2text":
            result = hex_to_text(args.text, args.encoding, args.separator)
            print("Decoded text:")
        elif args.mode == "bin2text":
            result = bin_to_text(args.text, args.encoding, args.separator)
            print("Decoded text:")
        else:
            print(f"Error: Unknown mode '{args.mode}'", file=sys.stderr)
            sys.exit(1)

        # Format and display output
        if args.no_format:
            print(result)
        else:
            print(format_output(result))

        # Display additional information in verbose mode
        if args.verbose:
            logging.info(f"Input: {args.text}")
            logging.info(f"Mode: {args.mode}")
            logging.info(f"Encoding: {args.encoding}")
            logging.info(f"Output length: {len(result)} characters")

    except (ValueError, TypeError, UnicodeError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        logging.error(f"Unexpected error: {e}")
        print(f"An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
