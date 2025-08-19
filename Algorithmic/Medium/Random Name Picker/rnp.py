import random
import sys
import os
from typing import List

def create_default_name_file(filepath: str):
    """Creates a default name file for demonstration purposes."""
    print(f"A name file was not found. Creating a default file at '{filepath}'...")
    default_names = ["Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Heidi", "Ivan", "Judy"]
    try:
        with open(filepath, 'w') as f:
            for name in default_names:
                f.write(name + '\n')
    except IOError as e:
        print(f"Error: Could not create the default name file. {e}", file=sys.stderr)
        sys.exit(1)

def pick_random_names(names_list: List[str], count: int) -> List[str]:
    """
    Picks one or more random names from a list without replacement.

    Args:
        names_list: A list of names to choose from.
        count: The number of unique names to pick.

    Returns:
        A list containing the randomly picked names.

    Raises:
        ValueError: If the list is empty or if the requested count is invalid.
    """
    if not names_list:
        raise ValueError("The list of names cannot be empty.")
    if not 0 < count <= len(names_list):
        raise ValueError(f"Cannot pick {count} names. Please pick a number between 1 and {len(names_list)}.")

    # Use random.sample for efficient, non-repeating random selection.
    return random.sample(names_list, count)

def main():
    """
    Main function to load names from a file and pick a specified number of them.
    """
    print("--- Random Name Picker ---")

    # The script will look for names.txt in the same directory.
    name_file = "names.txt"

    # Create a default name file if it doesn't exist.
    if not os.path.exists(name_file):
        create_default_name_file(name_file)

    try:
        with open(name_file, 'r') as f:
            # Read names, stripping whitespace and ignoring empty lines.
            names = [line.strip() for line in f if line.strip()]

        print(f"Loaded {len(names)} names from '{name_file}'.")

        # Get the number of names to pick from the user.
        try:
            count_str = input("How many random names would you like to pick? (default: 1): ")
            count = int(count_str) if count_str.strip() else 1
        except ValueError:
            print("Invalid number. Defaulting to picking 1 name.")
            count = 1

        picked_names = pick_random_names(names, count)

        print(f"\nHere are your {len(picked_names)} randomly picked name(s):")
        for name in picked_names:
            print(f"  - {name}")

    except FileNotFoundError:
        print(f"Error: The name file '{name_file}' was not found.", file=sys.stderr)
    except ValueError as e:
        print(f"\nError: {e}", file=sys.stderr)
    except (EOFError, KeyboardInterrupt):
        print("\n\nOperation cancelled by user.")

if __name__ == "__main__":
    main()
