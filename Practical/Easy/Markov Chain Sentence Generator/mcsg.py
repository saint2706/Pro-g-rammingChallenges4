import random
import re
import sys
import os
import argparse
from collections import defaultdict, deque
from typing import List, Dict, Tuple, Deque

class MarkovGenerator:
    """
    A Markov Chain generator for creating sentences based on a source text.
    """
    def __init__(self, state_size: int = 1):
        """
        Initializes the generator.

        Args:
            state_size: The order of the Markov chain (e.g., 1 for single-word states,
                        2 for two-word states).
        """
        if state_size < 1:
            raise ValueError("State size must be at least 1.")
        self.state_size = state_size
        self.model: Dict[Tuple[str, ...], List[str]] = defaultdict(list)
        self._start_states: List[Tuple[str, ...]] = []

    def train(self, text: str):
        """
        Builds the Markov model from a given source text.

        Args:
            text: The source text to build the model from.
        """
        print("Building Markov model...")
        # Tokenize the text into words, removing punctuation and empty strings.
        words = [word.lower() for word in re.split(r'\s+', text) if word]

        if len(words) <= self.state_size:
            print("Warning: Source text is too short for the given state size.", file=sys.stderr)
            return

        # Use a deque as a sliding window for the state
        state: Deque[str] = deque(words[:self.state_size], maxlen=self.state_size)
        self._start_states.append(tuple(state))

        for next_word in words[self.state_size:]:
            self.model[tuple(state)].append(next_word)
            if state[0].endswith('.'): # Consider sentences starting after a period as potential starts
                self._start_states.append(tuple(state))
            state.append(next_word)
        print("Model built successfully.")

    def generate(self, length: int, start_key: Optional[str] = None) -> str:
        """
        Generates a sentence of a specified length using the model.

        Args:
            length: The desired number of words in the sentence.
            start_key: An optional word to start the sentence with.

        Returns:
            A generated sentence as a string.
        """
        if not self.model:
            return "The model has not been trained. Please provide a source text."

        # Choose a starting state
        if start_key:
            # Find a state that starts with the given word
            possible_starts = [s for s in self._start_states if s[0].lower() == start_key.lower()]
            if not possible_starts:
                print(f"Warning: Start word '{start_key}' not found in model starts. Choosing a random start.", file=sys.stderr)
                current_state = random.choice(self._start_states)
            else:
                current_state = random.choice(possible_starts)
        else:
            current_state = random.choice(self._start_states)

        words = list(current_state)

        # Iteratively generate the rest of the sentence
        for _ in range(length - self.state_size):
            next_word_options = self.model.get(current_state)
            if not next_word_options:
                break # Dead end

            next_word = random.choice(next_word_options)
            words.append(next_word)

            # Update the state by sliding the window
            new_state_list = list(current_state)[1:] + [next_word]
            current_state = tuple(new_state_list)

        return " ".join(words)

def main():
    """Main function to parse arguments and run the sentence generator."""
    default_fpath = os.path.join(os.path.dirname(__file__), "crime_and_punishment")

    parser = argparse.ArgumentParser(description="Generate sentences using a Markov Chain model.")
    parser.add_argument("file", nargs='?', default=default_fpath,
                        help=f"Path to the source text file. Defaults to '{default_fpath}'")
    parser.add_argument("-l", "--length", type=int, default=15, help="Number of words per sentence.")
    parser.add_argument("-n", "--num", type=int, default=5, help="Number of sentences to generate.")
    parser.add_argument("-s", "--start", help="An optional word to start the sentences with.")
    parser.add_argument("--state-size", type=int, default=2, help="The order of the Markov chain (e.g., 1 or 2).")

    args = parser.parse_args()

    try:
        with open(args.file, 'r', encoding='utf-8') as f:
            source_text = f.read()
    except FileNotFoundError:
        print(f"Error: Source text file not found at '{args.file}'", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error reading file: {e}", file=sys.stderr)
        sys.exit(1)

    generator = MarkovGenerator(state_size=args.state_size)
    generator.train(source_text)

    print(f"\n--- Generating {args.num} sentences of length {args.length} ---\n")
    for i in range(args.num):
        sentence = generator.generate(args.length, args.start)
        print(f"{i+1}. {sentence}\n")

if __name__ == "__main__":
    main()
