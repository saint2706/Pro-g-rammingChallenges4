import sys
import argparse
from typing import List, Dict, Any

# Add a dependency check for a better user experience.
try:
    import spacy
except ImportError:
    print("Error: The 'spacy' library is required.", file=sys.stderr)
    print("Please install it using: pip install spacy", file=sys.stderr)
    sys.exit(1)

class SentenceParser:
    """
    A class to parse sentences and extract linguistic features using spaCy.
    """
    def __init__(self, model_name: str = "en_core_web_sm"):
        """
        Initializes the parser by loading a spaCy language model.

        Args:
            model_name: The name of the spaCy model to load.
        """
        self.nlp = None
        try:
            self.nlp = spacy.load(model_name)
            print(f"spaCy model '{model_name}' loaded successfully.")
        except OSError:
            print(f"Error: spaCy model '{model_name}' not found.", file=sys.stderr)
            print(f"Please download it by running: python -m spacy download {model_name}", file=sys.stderr)
            sys.exit(1)

    def parse(self, sentence: str) -> List[Dict[str, Any]]:
        """
        Processes a sentence and extracts token-level linguistic information.

        Args:
            sentence: The input sentence string.

        Returns:
            A list of dictionaries, where each dictionary represents a token
            and its extracted features.
        """
        if not self.nlp:
            raise RuntimeError("spaCy model is not loaded.")

        doc = self.nlp(sentence)

        parsed_data = []
        for token in doc:
            parsed_data.append({
                "text": token.text,
                "pos": token.pos_,
                "dep": token.dep_,
                "head_text": token.head.text,
                "head_pos": token.head.pos_,
            })
        return parsed_data

def display_results(parsed_data: List[Dict[str, Any]]):
    """
    Displays the parsed sentence data in a clean, formatted table.
    """
    if not parsed_data:
        print("No data to display.")
        return

    # Determine column widths for nice formatting
    headers = ["TEXT", "POS", "DEPENDENCY", "HEAD TEXT", "HEAD POS"]
    col_widths = [len(h) for h in headers]
    for row in parsed_data:
        col_widths[0] = max(col_widths[0], len(row["text"]))
        col_widths[1] = max(col_widths[1], len(row["pos"]))
        col_widths[2] = max(col_widths[2], len(row["dep"]))
        col_widths[3] = max(col_widths[3], len(row["head_text"]))
        col_widths[4] = max(col_widths[4], len(row["head_pos"]))

    # Print header
    header_str = (f"{headers[0]:<{col_widths[0]}} | {headers[1]:<{col_widths[1]}} | "
                  f"{headers[2]:<{col_widths[2]}} | {headers[3]:<{col_widths[3]}} | "
                  f"{headers[4]:<{col_widths[4]}}")
    print(header_str)
    print("-" * len(header_str))

    # Print rows
    for row in parsed_data:
        print(f"{row['text']:<{col_widths[0]}} | {row['pos']:<{col_widths[1]}} | "
              f"{row['dep']:<{col_widths[2]}} | {row['head_text']:<{col_widths[3]}} | "
              f"{row['head_pos']:<{col_widths[4]}}")

def main():
    """Main function to parse arguments and analyze a sentence."""
    parser = argparse.ArgumentParser(description="Analyze a sentence to show Part-of-Speech tags and dependency parsing.")
    parser.add_argument("sentence", nargs='?', default="Sachin hit the ball with a bat.",
                        help="The sentence to analyze.")

    args = parser.parse_args()

    parser = SentenceParser()
    parsed_sentence = parser.parse(args.sentence)

    print(f"\n--- Analysis for: '{args.sentence}' ---\n")
    display_results(parsed_sentence)

if __name__ == "__main__":
    main()
