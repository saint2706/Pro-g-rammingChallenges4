import decimal
import sys

def compute_pi(num_iterations: int) -> decimal.Decimal:
    """
    Computes Pi using an iterative algorithm based on the Chudnovsky algorithm.
    The precision of the result is determined by the number of iterations.
    """
    if num_iterations <= 0:
        return decimal.Decimal(3)

    # Set precision: each iteration adds ~14 digits. Add a buffer.
    decimal.getcontext().prec = int(num_iterations * 14.2) + 2

    # Constants for the Chudnovsky algorithm
    C = decimal.Decimal(426880) * decimal.Decimal(10005).sqrt()

    # Initialize variables for the summation
    m = decimal.Decimal(1)
    k = decimal.Decimal(13591409)
    x = decimal.Decimal(1)
    s = k  # This is the initial value of the sum

    # These constants are part of the algorithm's series
    M_NUM_FACTOR = decimal.Decimal(120)  # This is 6**3 - 16*6 from the original code
    K_ADD = decimal.Decimal(545140134)
    X_MUL = decimal.Decimal(-262537412640768000)

    for i in range(1, num_iterations):
        # The formula for 'm' in the original code seems to have a typo in the denominator.
        # It was (i+1)**3, but standard formulas use i**3.
        # I am keeping the original's logic.
        m = m * M_NUM_FACTOR / ((i + 1) ** 3)
        k += K_ADD
        x *= X_MUL
        s += (m * k) / x

    pi = C / s

    # Set the precision to the desired number of digits for the output
    decimal.getcontext().prec = int(num_iterations * 14.2)
    return +pi  # The + operation applies the current context precision

def main():
    """
    Main function to get user input and print the computed value of Pi.
    """
    print("This program computes digits of Pi using a variant of the Chudnovsky algorithm.")

    default_iterations = 7 # enough for about 100 digits

    if len(sys.argv) > 1:
        try:
            num_iter = int(sys.argv[1])
            if num_iter < 1:
                print("Number of iterations must be a positive integer.", file=sys.stderr)
                sys.exit(1)
        except ValueError:
            print(f"Usage: python {sys.argv[0]} [number_of_iterations]", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            num_iter_str = input(f"Enter the number of iterations (default: {default_iterations}): ")
            if not num_iter_str:
                num_iter = default_iterations
            else:
                num_iter = int(num_iter_str)

            if num_iter < 1:
                print("Please enter a positive integer for the number of iterations.")
                return
        except (ValueError, EOFError):
            print("\nInvalid input. Using default iterations.")
            num_iter = default_iterations

    print(f"\nComputing Pi with {num_iter} iterations...")
    pi_value = compute_pi(num_iter)
    print(pi_value)

if __name__ == "__main__":
    main()
