import matplotlib.pyplot as plt
import argparse
import sys
from typing import Counter as CounterType

# Import the logic from the refactored command-line script
try:
    from charcount import get_char_counts
except ImportError:
    print("Error: This script requires 'charcount.py' to be in the same directory.", file=sys.stderr)
    sys.exit(1)

def plot_char_counts(counts: CounterType[str]):
    """
    Creates and displays a bar chart of character frequencies using Matplotlib.

    Args:
        counts: A Counter object mapping characters to their frequencies.
    """
    if not counts:
        print("No characters to plot.")
        return

    # Sort characters by frequency for a cleaner plot
    sorted_items = sorted(counts.items(), key=lambda item: item[1], reverse=True)
    characters = [item[0] for item in sorted_items]
    frequencies = [item[1] for item in sorted_items]

    # Handle special characters for display on the plot's x-axis
    char_labels = []
    for char in characters:
        if char == ' ':
            char_labels.append("' ' (Space)")
        elif char == '\n':
            char_labels.append("'\\n' (Newline)")
        elif char == '\t':
            char_labels.append("'\\t' (Tab)")
        else:
            char_labels.append(char)

    fig, ax = plt.subplots(figsize=(max(10, len(characters) * 0.5), 6))

    ax.bar(char_labels, frequencies, color='skyblue')

    ax.set_ylabel('Frequency')
    ax.set_title('Character Frequency Distribution')
    ax.tick_params(axis='x', rotation=90)

    # Add frequency count on top of each bar
    for i, freq in enumerate(frequencies):
        ax.text(i, freq + 0.1, str(freq), ha='center')

    plt.tight_layout()
    plt.show()

def main():
    """Main function to parse arguments and run the visualizer."""
    parser = argparse.ArgumentParser(description="Visualize the frequency of characters in a given text as a bar chart.")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-t", "--text", help="A string of text to analyze.")
    group.add_argument("-f", "--file", help="Path to a text file to analyze.")

    args = parser.parse_args()

    input_text = ""
    source_name = ""
    if args.text:
        input_text = args.text
        source_name = "the provided string"
    elif args.file:
        try:
            with open(args.file, 'r', encoding='utf-8') as f:
                input_text = f.read()
            source_name = f"the file '{args.file}'"
        except FileNotFoundError:
            print(f"Error: File not found at '{args.file}'", file=sys.stderr)
            sys.exit(1)
        except Exception as e:
            print(f"Error reading file: {e}", file=sys.stderr)
            sys.exit(1)

    print(f"Analyzing character counts from {source_name}...")
    char_counts = get_char_counts(input_text)
    plot_char_counts(char_counts)

if __name__ == "__main__":
    main()
