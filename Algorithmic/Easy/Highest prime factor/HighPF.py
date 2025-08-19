import math
import sys

def highest_prime_factor(n: int) -> int:
    """
    Calculates the largest prime factor of a given integer.

    Args:
        n: An integer greater than 1.

    Returns:
        The largest prime factor of n.

    Raises:
        ValueError: If n is less than 2.
    """
    if n < 2:
        raise ValueError("Input must be an integer greater than 1.")

    max_prime = -1

    # Handle the factor of 2
    while n % 2 == 0:
        max_prime = 2
        n //= 2  # Use integer division

    # Handle odd factors
    # We iterate from 3 up to the square root of the remaining n.
    for i in range(3, int(math.sqrt(n)) + 1, 2):
        while n % i == 0:
            max_prime = i
            n //= i

    # If n is still greater than 2 at the end, it must be a prime number
    # and it will be the largest prime factor.
    if n > 2:
        max_prime = n

    return int(max_prime)

def main():
    """
    Main function to get user input and find the highest prime factor.
    """
    print("Highest Prime Factor Calculator")

    if len(sys.argv) > 1:
        try:
            num = int(sys.argv[1])
        except ValueError:
            print(f"Usage: python {sys.argv[0]} [integer]", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            num_str = input("Enter an integer greater than 1: ")
            if not num_str:
                print("No input provided.")
                return
            num = int(num_str)
        except (ValueError, EOFError):
            print("\nInvalid input. Please enter a valid integer.")
            return

    try:
        result = highest_prime_factor(num)
        print(f"The highest prime factor of {num} is: {result}")
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
