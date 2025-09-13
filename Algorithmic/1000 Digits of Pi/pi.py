import decimal
import sys

def calculate_pi_gauss_legendre(num_digits: int) -> str:
    """
    Calculates Pi to a specified number of decimal places using the
    Gauss-Legendre algorithm. This is an iterative algorithm that
    converges quadratically to Pi.

    Args:
        num_digits: The number of decimal places to calculate.

    Returns:
        A string representation of Pi, including "3." at the start,
        truncated to the requested number of digits.
    """
    if not isinstance(num_digits, int) or num_digits <= 0:
        raise ValueError("Number of digits must be a positive integer.")

    # Set the precision for decimal calculations. Add a few extra digits
    # for safety during intermediate calculations.
    decimal.getcontext().prec = num_digits + 3

    # Initialize the algorithm's state variables as Decimals
    a = decimal.Decimal(1)
    b = decimal.Decimal(1) / decimal.Decimal(2).sqrt()
    t = decimal.Decimal(1) / decimal.Decimal(4)
    p = decimal.Decimal(1)

    a_old = a
    while True:
        a_next = (a + b) / 2
        b = (a * b).sqrt()
        t -= p * (a - a_next) ** 2
        a = a_next
        p *= 2

        # The algorithm converges when 'a' and 'a_old' are indistinguishable
        # at the current precision level.
        if a == a_old:
            break
        a_old = a

    pi = (a + b) ** 2 / (4 * t)

    # Return Pi as a string, truncated to the correct length.
    # The result has `num_digits` after the decimal point, plus "3.".
    return str(pi)[:num_digits + 2]

def main():
    """
    Main function to get user input and calculate Pi to a specified number of digits.
    """
    print("--- Pi Calculator (Gauss-Legendre Algorithm) ---")

    default_digits = 1000
    num_digits = default_digits

    if len(sys.argv) > 1:
        try:
            num_digits = int(sys.argv[1])
        except ValueError:
            print(f"Usage: python {sys.argv[0]} [number_of_digits]", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            digits_str = input(f"Enter the number of decimal places for Pi (default: {default_digits}): ")
            if digits_str:
                num_digits = int(digits_str)
        except (ValueError, EOFError, KeyboardInterrupt):
            print(f"\nInvalid input or operation cancelled. Using default of {default_digits} digits.")

    try:
        print(f"\nCalculating Pi to {num_digits} decimal places...")
        pi_string = calculate_pi_gauss_legendre(num_digits)

        print(f"\nPi to {num_digits} decimal places:")
        # Pretty-print the output for readability
        print(f"{pi_string[0]}.{pi_string[2:]}")

    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
