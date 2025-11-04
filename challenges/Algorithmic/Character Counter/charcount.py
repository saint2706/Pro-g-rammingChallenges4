#!/usr/bin/env python3
"""
Character Counter Module

A comprehensive character frequency analysis tool that supports Unicode text,
statistical analysis, and various output formats. This module provides functions
to count character frequencies and analyze text patterns.

Features:
- Unicode-aware character counting
- Statistical analysis (entropy, diversity metrics)
- Case-sensitive and case-insensitive modes
- Character category analysis (letters, digits, punctuation)
- Multiple output formats (text, JSON, CSV)

Example:
    >>> from charcount import get_char_counts, analyze_text_statistics
    >>> counts = get_char_counts("Hello World!")
    >>> stats = analyze_text_statistics("Hello World!")

Author: Programming Challenges
Date: 2025
Version: 2.0
"""

import collections
import json
import logging
import math
import string
import sys
import unicodedata
from collections import Counter
from dataclasses import dataclass, asdict
from enum import Enum
from typing import Dict, List, Optional, Tuple, Union


class CharacterCategory(Enum):
    """Enumeration of character categories for analysis."""

    LETTERS = "letters"
    DIGITS = "digits"
    PUNCTUATION = "punctuation"
    WHITESPACE = "whitespace"
    SYMBOLS = "symbols"
    OTHER = "other"


class OutputFormat(Enum):
    """Enumeration of supported output formats."""

    TEXT = "text"
    JSON = "json"
    CSV = "csv"


@dataclass
class TextStatistics:
    """
    Data class containing comprehensive text analysis statistics.

    Attributes:
        total_characters: Total number of characters in the text
        unique_characters: Number of unique characters
        character_counts: Counter object with character frequencies
        character_categories: Dictionary of counts by character category
        entropy: Shannon entropy of the text (measure of randomness)
        diversity_index: Simpson's diversity index
        most_common: List of most common characters with their counts
        case_sensitive: Whether the analysis was case-sensitive
    """

    total_characters: int
    unique_characters: int
    character_counts: Counter[str]
    character_categories: Dict[str, int]
    entropy: float
    diversity_index: float
    most_common: List[Tuple[str, int]]
    case_sensitive: bool

    def to_dict(self) -> Dict:
        """Convert to dictionary for JSON serialization."""
        result = asdict(self)
        # Convert Counter to regular dict for JSON serialization
        result["character_counts"] = dict(result["character_counts"])
        return result


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


def get_char_counts(text: str, case_sensitive: bool = True) -> Counter[str]:
    """
    Counts the frequency of each character in a given string with options.

    This function provides Unicode-aware character counting with optional
    case-insensitive analysis. It handles all Unicode characters properly
    and provides accurate frequency counts.

    Args:
        text: The input string to analyze (supports Unicode)
        case_sensitive: If False, applies Unicode casefolding before counting

    Returns:
        A Counter object mapping each character to its frequency

    Raises:
        TypeError: If text is not a string

    Examples:
        >>> get_char_counts("Hello")
        Counter({'l': 2, 'H': 1, 'e': 1, 'o': 1})
        >>> get_char_counts("Hello", case_sensitive=False)
        Counter({'l': 2, 'h': 1, 'e': 1, 'o': 1})
        >>> get_char_counts("Straße", case_sensitive=False)
        Counter({'s': 3, 't': 1, 'r': 1, 'a': 1, 'e': 1})
        >>> get_char_counts("Hello 🌍!")  # Unicode support
        Counter({'l': 2, 'H': 1, 'e': 1, 'o': 1, ' ': 1, '🌍': 1, '!': 1})
    """
    if not isinstance(text, str):
        raise TypeError("Input text must be a string")

    if not text:
        return Counter()

    # Apply case transformation if requested
    processed_text = text.casefold() if not case_sensitive else text

    return Counter(processed_text)


def categorize_character(char: str) -> CharacterCategory:
    """
    Categorizes a single character into predefined categories.

    Uses Unicode character properties to accurately categorize characters
    including international letters, symbols, and special characters.

    Args:
        char: A single character to categorize

    Returns:
        CharacterCategory enum value representing the character's category

    Example:
        >>> categorize_character('A')
        <CharacterCategory.LETTERS: 'letters'>
        >>> categorize_character('5')
        <CharacterCategory.DIGITS: 'digits'>
        >>> categorize_character('🌍')
        <CharacterCategory.SYMBOLS: 'symbols'>
    """
    if not char or len(char) != 1:
        return CharacterCategory.OTHER

    # Use Unicode categories for accurate classification
    category = unicodedata.category(char)

    if category.startswith("L"):  # Letter categories (Lu, Ll, Lt, Lm, Lo)
        return CharacterCategory.LETTERS
    elif category.startswith("N"):  # Number categories (Nd, Nl, No)
        return CharacterCategory.DIGITS
    elif category.startswith("P"):  # Punctuation categories
        return CharacterCategory.PUNCTUATION
    elif category.startswith("Z"):  # Separator categories (whitespace)
        return CharacterCategory.WHITESPACE
    elif category.startswith("S"):  # Symbol categories
        return CharacterCategory.SYMBOLS
    else:
        return CharacterCategory.OTHER


def analyze_character_categories(text: str) -> Dict[str, int]:
    """
    Analyzes text and returns counts for each character category.

    Args:
        text: The input text to analyze

    Returns:
        Dictionary mapping category names to their character counts

    Example:
        >>> analyze_character_categories("Hello World! 123")
        {'letters': 10, 'whitespace': 2, 'punctuation': 1, 'digits': 3}
    """
    category_counts = {category.value: 0 for category in CharacterCategory}

    for char in text:
        category = categorize_character(char)
        category_counts[category.value] += 1

    # Remove categories with zero counts for cleaner output
    return {k: v for k, v in category_counts.items() if v > 0}


def calculate_entropy(text: str) -> float:
    """
    Calculates the Shannon entropy of the text.

    Shannon entropy measures the average information content or "randomness"
    of the text. Higher entropy indicates more randomness/diversity.

    Args:
        text: The input text to analyze

    Returns:
        Shannon entropy in bits (log base 2)

    Example:
        >>> calculate_entropy("aaaa")  # Low entropy
        0.0
        >>> calculate_entropy("abcd")  # Higher entropy
        2.0
    """
    if not text:
        return 0.0

    char_counts = Counter(text)
    text_length = len(text)

    # Calculate entropy: H = -Σ(p_i * log2(p_i))
    entropy = 0.0
    for count in char_counts.values():
        probability = count / text_length
        if probability > 0:  # Avoid log(0)
            entropy -= probability * math.log2(probability)

    return entropy


def calculate_diversity_index(text: str) -> float:
    """
    Calculates Simpson's diversity index for the text.

    Simpson's diversity index measures the probability that two randomly
    selected characters are different. Values closer to 1 indicate higher diversity.

    Args:
        text: The input text to analyze

    Returns:
        Simpson's diversity index (0-1, where 1 is most diverse)

    Example:
        >>> calculate_diversity_index("aaaa")  # Low diversity
        0.0
        >>> calculate_diversity_index("abcd")  # High diversity
        0.75
    """
    if not text:
        return 0.0

    char_counts = Counter(text)
    text_length = len(text)

    if text_length <= 1:
        return 0.0

    # Simpson's index: D = 1 - Σ(n_i * (n_i - 1)) / (N * (N - 1))
    sum_ni_squared = sum(count * (count - 1) for count in char_counts.values())
    diversity = 1 - (sum_ni_squared / (text_length * (text_length - 1)))

    return diversity


def analyze_text_statistics(
    text: str, case_sensitive: bool = True, top_n: int = 10
) -> TextStatistics:
    """
    Performs comprehensive statistical analysis of text.

    This function combines all analysis methods to provide a complete
    statistical overview of the input text including frequency counts,
    entropy measures, and character categorization.

    Args:
        text: The input text to analyze
        case_sensitive: Whether to perform case-sensitive analysis
        top_n: Number of most common characters to include in results

    Returns:
        TextStatistics object containing comprehensive analysis results

    Example:
        >>> stats = analyze_text_statistics("Hello World!")
        >>> print(f"Entropy: {stats.entropy:.2f}")
        >>> print(f"Most common: {stats.most_common[:3]}")
    """
    if not isinstance(text, str):
        raise TypeError("Input text must be a string")

    # Get character counts and normalized text
    char_counts = get_char_counts(text, case_sensitive)
    processed_text = text if case_sensitive else text.lower()

    # Calculate statistics
    total_chars = len(processed_text)
    unique_chars = len(char_counts)
    entropy = calculate_entropy(processed_text)
    diversity = calculate_diversity_index(processed_text)
    categories = analyze_character_categories(text)
    most_common = char_counts.most_common(top_n)

    return TextStatistics(
        total_characters=total_chars,
        unique_characters=unique_chars,
        character_counts=char_counts,
        character_categories=categories,
        entropy=entropy,
        diversity_index=diversity,
        most_common=most_common,
        case_sensitive=case_sensitive,
    )


def format_char_for_display(char: str) -> str:
    """
    Formats a character for human-readable display.

    Handles special characters like spaces, tabs, newlines by converting
    them to readable representations while preserving Unicode characters.

    Args:
        char: The character to format

    Returns:
        Human-readable representation of the character

    Example:
        >>> format_char_for_display(' ')
        "' ' (Space)"
        >>> format_char_for_display('\\n')
        "'\\n' (Newline)"
        >>> format_char_for_display('A')
        'A'
    """
    if char == " ":
        return "' ' (Space)"
    elif char == "\n":
        return "'\\n' (Newline)"
    elif char == "\t":
        return "'\\t' (Tab)"
    elif char == "\r":
        return "'\\r' (Carriage Return)"
    elif ord(char) < 32 or ord(char) == 127:  # Control characters
        return f"'\\x{ord(char):02x}' (Control)"
    else:
        return char


def export_results(
    statistics: TextStatistics,
    format_type: OutputFormat,
    filename: Optional[str] = None,
) -> str:
    """
    Exports analysis results in the specified format.

    Args:
        statistics: TextStatistics object to export
        format_type: Output format (TEXT, JSON, or CSV)
        filename: Optional filename to save results (if None, returns string)

    Returns:
        Formatted string of the results

    Raises:
        ValueError: If format_type is unsupported
    """
    if format_type == OutputFormat.TEXT:
        output = _format_text_output(statistics)
    elif format_type == OutputFormat.JSON:
        output = json.dumps(statistics.to_dict(), indent=2, ensure_ascii=False)
    elif format_type == OutputFormat.CSV:
        output = _format_csv_output(statistics)
    else:
        raise ValueError(f"Unsupported format: {format_type}")

    if filename:
        with open(filename, "w", encoding="utf-8") as f:
            f.write(output)
        logging.info(f"Results exported to {filename}")

    return output


def _format_text_output(statistics: TextStatistics) -> str:
    """Format statistics as human-readable text."""
    lines = [
        "=== TEXT ANALYSIS RESULTS ===",
        f"Total characters: {statistics.total_characters:,}",
        f"Unique characters: {statistics.unique_characters:,}",
        f"Shannon entropy: {statistics.entropy:.3f} bits",
        f"Diversity index: {statistics.diversity_index:.3f}",
        f"Case sensitive: {statistics.case_sensitive}",
        "",
        "Character categories:",
    ]

    for category, count in statistics.character_categories.items():
        percentage = (count / statistics.total_characters) * 100
        lines.append(f"  {category.title()}: {count:,} ({percentage:.1f}%)")

    lines.extend(
        [
            "",
            f"Most common characters (top {len(statistics.most_common)}):",
        ]
    )

    for i, (char, count) in enumerate(statistics.most_common, 1):
        char_display = format_char_for_display(char)
        percentage = (count / statistics.total_characters) * 100
        lines.append(f"  {i:2d}. {char_display}: {count:,} ({percentage:.1f}%)")

    return "\n".join(lines)


def _format_csv_output(statistics: TextStatistics) -> str:
    """Format character counts as CSV."""
    lines = ["Character,Count,Percentage"]

    for char, count in statistics.character_counts.most_common():
        percentage = (count / statistics.total_characters) * 100
        # Escape quotes in CSV
        char_escaped = char.replace('"', '""')
        lines.append(f'"{char_escaped}",{count},{percentage:.2f}')

    return "\n".join(lines)


def main() -> None:
    """
    Enhanced main function with comprehensive CLI interface.

    Provides interactive and command-line modes for character analysis
    with multiple output formats and advanced statistical features.
    """
    import argparse

    parser = argparse.ArgumentParser(
        description="Advanced character frequency analysis tool with Unicode support",
        epilog="""
Examples:
  %(prog)s --text "Hello World"
  %(prog)s --file document.txt --case-insensitive
  %(prog)s --interactive
  %(prog)s --text "Sample" --format json --output results.json
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    # Input options
    input_group = parser.add_mutually_exclusive_group()
    input_group.add_argument("--text", "-t", help="Text string to analyze")
    input_group.add_argument("--file", "-f", help="Path to text file to analyze")
    input_group.add_argument(
        "--interactive", "-i", action="store_true", help="Run in interactive mode"
    )

    # Analysis options
    parser.add_argument(
        "--case-insensitive",
        "-c",
        action="store_true",
        help="Perform case-insensitive analysis",
    )
    parser.add_argument(
        "--top",
        "-n",
        type=int,
        default=10,
        help="Number of most common characters to display (default: 10)",
    )

    # Output options
    parser.add_argument(
        "--format",
        choices=["text", "json", "csv"],
        default="text",
        help="Output format (default: text)",
    )
    parser.add_argument(
        "--output", "-o", help="Output file path (if not specified, prints to console)"
    )
    parser.add_argument(
        "--verbose", "-v", action="store_true", help="Enable verbose logging"
    )

    args = parser.parse_args()

    # Setup logging
    log_level = "DEBUG" if args.verbose else "INFO"
    setup_logging(log_level)

    # Interactive mode
    if args.interactive:
        _run_interactive_mode()
        return

    # Get input text
    if args.text:
        input_text = args.text
        source_description = "command-line text"
    elif args.file:
        try:
            with open(args.file, "r", encoding="utf-8") as f:
                input_text = f.read()
            source_description = f"file '{args.file}'"
        except FileNotFoundError:
            print(f"Error: File not found '{args.file}'", file=sys.stderr)
            return
        except Exception as e:
            print(f"Error reading file: {e}", file=sys.stderr)
            return
    else:
        # Fallback to simple input prompt
        try:
            input_text = input("Enter text to analyze: ")
            if not input_text:
                print("No input provided.")
                return
            source_description = "user input"
        except (KeyboardInterrupt, EOFError):
            print("\nOperation cancelled.")
            return

    if not input_text:
        print("No text to analyze.")
        return

    try:
        # Perform analysis
        case_sensitive = not args.case_insensitive
        statistics = analyze_text_statistics(input_text, case_sensitive, args.top)

        # Format output
        format_map = {
            "text": OutputFormat.TEXT,
            "json": OutputFormat.JSON,
            "csv": OutputFormat.CSV,
        }

        output_format = format_map[args.format]
        result = export_results(statistics, output_format, args.output)

        if not args.output:
            print(f"\nAnalysis of {source_description}:")
            print(result)

        if args.verbose:
            logging.info(
                f"Analyzed {len(input_text)} characters from {source_description}"
            )
            logging.info(f"Found {statistics.unique_characters} unique characters")

    except Exception as e:
        logging.error(f"Analysis error: {e}")
        print(f"Error during analysis: {e}", file=sys.stderr)


def _run_interactive_mode() -> None:
    """Runs the enhanced interactive mode with menu options."""
    print("=" * 60)
    print("    Advanced Character Counter - Interactive Mode")
    print("=" * 60)
    print("Unicode-aware character analysis with statistical features")
    print()

    while True:
        try:
            print("Options:")
            print("1. Analyze text string")
            print("2. Analyze text file")
            print("3. Compare case-sensitive vs case-insensitive")
            print("4. Character category breakdown")
            print("5. Help & Examples")
            print("6. Exit")
            print()

            choice = input("Select an option (1-6): ").strip()

            if choice == "6":
                print("Goodbye!")
                break
            elif choice == "5":
                _show_help_examples()
                continue
            elif choice in ["1", "2", "3", "4"]:
                # Get input text
                if choice == "1":
                    text = input("Enter text to analyze: ").strip()
                    if not text:
                        print("No text provided.")
                        continue
                elif choice == "2":
                    filepath = input("Enter file path: ").strip()
                    try:
                        with open(filepath, "r", encoding="utf-8") as f:
                            text = f.read()
                        print(f"Loaded {len(text)} characters from '{filepath}'")
                    except Exception as e:
                        print(f"Error reading file: {e}")
                        continue
                else:
                    text = input("Enter text for comparison: ").strip()
                    if not text:
                        print("No text provided.")
                        continue

                # Perform analysis based on choice
                if choice in ["1", "2"]:
                    case_sensitive = (
                        input("Case-sensitive analysis? (y/N): ")
                        .lower()
                        .startswith("y")
                    )
                    stats = analyze_text_statistics(text, case_sensitive)
                    print("\n" + _format_text_output(stats))

                elif choice == "3":
                    print("\n=== CASE-SENSITIVE ANALYSIS ===")
                    stats_case = analyze_text_statistics(text, case_sensitive=True)
                    print(_format_text_output(stats_case))

                    print("\n=== CASE-INSENSITIVE ANALYSIS ===")
                    stats_nocase = analyze_text_statistics(text, case_sensitive=False)
                    print(_format_text_output(stats_nocase))

                elif choice == "4":
                    categories = analyze_character_categories(text)
                    total = len(text)
                    print(f"\n=== CHARACTER CATEGORIES ===")
                    print(f"Total characters: {total:,}")
                    for category, count in categories.items():
                        percentage = (count / total) * 100
                        print(f"{category.title()}: {count:,} ({percentage:.1f}%)")

            else:
                print("Invalid choice. Please select 1-6.")

            print("-" * 50)

        except (EOFError, KeyboardInterrupt):
            print("\nOperation cancelled. Goodbye!")
            break
        except Exception as e:
            logging.error(f"Interactive mode error: {e}")
            print(f"An error occurred: {e}")


def _show_help_examples() -> None:
    """Display help information and usage examples."""
    print("\n" + "=" * 60)
    print("    CHARACTER COUNTER HELP & EXAMPLES")
    print("=" * 60)
    print()
    print("FEATURES:")
    print("• Unicode-aware character counting")
    print("• Statistical analysis (entropy, diversity)")
    print("• Character categorization (letters, digits, etc.)")
    print("• Case-sensitive and case-insensitive modes")
    print("• Multiple output formats (text, JSON, CSV)")
    print()
    print("EXAMPLES:")
    print("Text: 'Hello World!'")
    print("• Total characters: 12")
    print("• Unique characters: 10")
    print("• Most common: 'l' appears 3 times")
    print("• Categories: 9 letters, 2 whitespace, 1 punctuation")
    print()
    print("ENTROPY & DIVERSITY:")
    print("• Entropy measures randomness (higher = more random)")
    print("• Diversity index shows character variety (0-1, higher = more diverse)")
    print("• 'aaaa' has entropy 0.0 and diversity 0.0")
    print("• 'abcd' has entropy 2.0 and diversity 0.75")
    print()


if __name__ == "__main__":
    main()
