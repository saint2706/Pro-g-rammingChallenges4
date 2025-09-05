import collections
from typing import Counter as CounterType

def get_char_counts(text: str) -> CounterType[str]:
    """
    Counts the frequency of each character in a given string.

    Args:
        text: The input string.

    Returns:
        A collections.Counter object mapping each character to its frequency.
    """
    return collections.Counter(text)

def main():
    """
    Main function to get input from the user and print character counts.
    """
    try:
        sentence = input("Enter a sentence to count its characters: ")
        if not sentence:
            print("No input provided.")
            return

        counts = get_char_counts(sentence)

        print("\nCharacter counts:")
        # Sort by character for consistent output
        for char, count in sorted(counts.items()):
            # Handle special characters like newline for printing
            char_display = repr(char)[1:-1] if char in '\n\r\t' else char
            print(f"'{char_display}': {count}")

    except (KeyboardInterrupt, EOFError):
        print("\nOperation cancelled.")

if __name__ == "__main__":
    main()
