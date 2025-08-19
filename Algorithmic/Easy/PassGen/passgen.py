import secrets
import string
import sys

def generate_password(length: int, use_letters: bool, use_digits: bool, use_punctuation: bool) -> str:
    """
    Generates a cryptographically strong random password.
    Args:
        length: The desired length of the password.
        use_letters: Whether to include ASCII letters.
        use_digits: Whether to include digits.
        use_punctuation: Whether to include punctuation symbols.
    Returns:
        A randomly generated password.
    Raises:
        ValueError: If no character types are selected.
    """
    char_pool = ""
    if use_letters:
        char_pool += string.ascii_letters
    if use_digits:
        char_pool += string.digits
    if use_punctuation:
        char_pool += string.punctuation

    if not char_pool:
        raise ValueError("Cannot generate a password with no character types selected.")

    # Ensure the password contains at least one of each selected character type
    password_chars = []
    if use_letters:
        password_chars.append(secrets.choice(string.ascii_letters))
    if use_digits:
        password_chars.append(secrets.choice(string.digits))
    if use_punctuation:
        password_chars.append(secrets.choice(string.punctuation))

    # Fill the rest of the password with random choices from the entire pool
    remaining_length = length - len(password_chars)
    for _ in range(remaining_length):
        password_chars.append(secrets.choice(char_pool))

    # Shuffle the list to ensure character positions are random
    secrets.SystemRandom().shuffle(password_chars)

    return "".join(password_chars)

def get_yes_no_input(prompt: str) -> bool:
    """Helper function to get a 'yes' or 'no' response from the user."""
    while True:
        answer = input(prompt).lower().strip()
        if answer in ['y', 'yes']:
            return True
        if answer in ['n', 'no']:
            return False
        print("Invalid input. Please enter 'y' or 'n'.")

def main():
    """
    Main function to get password criteria from the user and generate a password.
    """
    print("--- Secure Password Generator ---")

    try:
        length_str = input("Enter the desired password length (e.g., 16, min 8): ")
        length = int(length_str)
        if length < 8:
            print("Password length must be at least 8 characters for security.", file=sys.stderr)
            return

        use_letters = get_yes_no_input("Include letters (a-z, A-Z)? (y/n): ")
        use_digits = get_yes_no_input("Include numbers (0-9)? (y/n): ")
        use_punctuation = get_yes_no_input("Include symbols (!, $, #, etc.)? (y/n): ")

        password = generate_password(length, use_letters, use_digits, use_punctuation)
        print(f"\nGenerated Password: {password}")

    except ValueError as e:
        print(f"\nError: {e}", file=sys.stderr)
    except (TypeError, EOFError):
        print("\nInvalid input. Please enter valid options.")
    except KeyboardInterrupt:
        print("\nOperation cancelled by user.")
        sys.exit(0)

if __name__ == "__main__":
    main()
