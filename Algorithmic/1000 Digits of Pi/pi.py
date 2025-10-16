"""
High-precision Pi calculator using the Gauss-Legendre algorithm.

This module provides an efficient implementation of the Gauss-Legendre algorithm
for calculating Pi to arbitrary precision. The algorithm has quadratic convergence,
making it one of the fastest methods for high-precision Pi calculations.

Example:
    Calculate Pi to 100 decimal places:
    $ python pi.py 100

    Or run interactively:
    $ python pi.py

Author: Programming Challenges Repository
Date: 2025
License: MIT
"""

import argparse
import decimal
import logging
import sys
import time
from typing import Optional


def calculate_pi_gauss_legendre(num_digits: int, *, show_progress: bool = False) -> str:
    """
    Calculate Pi to specified precision using the Gauss-Legendre algorithm.

    The Gauss-Legendre algorithm is an iterative method for computing Pi that
    converges quadratically. Each iteration approximately doubles the number of
    correct digits, making it highly efficient for high-precision calculations.

    Algorithm details:
    - Start with initial values: a₀=1, b₀=1/√2, t₀=1/4, p₀=1
    - Iterate: aₙ₊₁ = (aₙ + bₙ)/2, bₙ₊₁ = √(aₙ·bₙ),
               tₙ₊₁ = tₙ - pₙ(aₙ - aₙ₊₁)², pₙ₊₁ = 2pₙ
    - Converge when aₙ ≈ aₙ₋₁ within precision
    - Final result: π ≈ (aₙ + bₙ)²/(4tₙ)

    Args:
        num_digits: Number of decimal places to calculate (must be positive).
                   For example, num_digits=5 returns "3.14159".
        show_progress: If True, display progress information during calculation.
                      Useful for large computations (default: False).

    Returns:
        String representation of Pi with exactly num_digits decimal places.
        Format: "3.141592653..." where the total length is num_digits + 2.

    Raises:
        ValueError: If num_digits is not a positive integer.

    Examples:
        >>> calculate_pi_gauss_legendre(5)
        '3.14159'
        >>> calculate_pi_gauss_legendre(10, show_progress=True)
        'Iteration 1: Precision improved...'
        '3.1415926535'

    Note:
        For very large values (>10,000 digits), calculation may take significant
        time and memory. Consider using show_progress=True to monitor progress.
    """
    # Input validation with descriptive error messages
    if not isinstance(num_digits, int):
        raise ValueError(
            f"num_digits must be an integer, got {type(num_digits).__name__}"
        )
    if num_digits <= 0:
        raise ValueError(f"num_digits must be positive, got {num_digits}")

    # Configure decimal precision with buffer for intermediate calculations
    # We need extra precision to avoid rounding errors during computation
    safety_margin = max(10, num_digits // 100)  # Adaptive safety margin
    decimal.getcontext().prec = num_digits + safety_margin

    if show_progress:
        logging.info(f"Starting Gauss-Legendre calculation for {num_digits} digits")
        logging.info(f"Using precision: {decimal.getcontext().prec}")

    # Initialize algorithm variables with high-precision Decimal arithmetic
    # These correspond to the mathematical definitions in the algorithm
    a = decimal.Decimal(1)  # Arithmetic mean
    b = decimal.Decimal(1) / decimal.Decimal(2).sqrt()  # Geometric mean
    t = decimal.Decimal(1) / decimal.Decimal(4)  # Sum term
    p = decimal.Decimal(1)  # Power of 2

    iteration_count = 0
    previous_a = a

    # Main iteration loop - continues until convergence
    while True:
        iteration_count += 1

        # Store current 'a' for convergence check
        a_current = a

        # Core Gauss-Legendre iteration steps
        a_next = (a + b) / 2  # Update arithmetic mean
        b_next = (a * b).sqrt()  # Update geometric mean
        t_next = t - p * (a - a_next) ** 2  # Update sum term

        # Update all variables for next iteration
        a, b, t = a_next, b_next, t_next
        p *= 2  # Double the power for next iteration

        if show_progress and iteration_count <= 20:  # Limit progress output
            # Calculate approximate number of correct digits (rough estimate)
            convergence_rate = abs(a - previous_a)
            if convergence_rate > 0:
                estimated_digits = -convergence_rate.log10()
                logging.info(
                    f"Iteration {iteration_count}: ~{int(estimated_digits)} correct digits"
                )

        # Convergence test: algorithm stops when successive 'a' values are identical
        # at the current precision level
        if a == previous_a:
            if show_progress:
                logging.info(f"Converged after {iteration_count} iterations")
            break

        previous_a = a_current

        # Safety check to prevent infinite loops (should never happen in practice)
        if iteration_count > 50:
            logging.warning(
                f"Algorithm did not converge after {iteration_count} iterations"
            )
            break

    # Final calculation of Pi using the converged values
    pi_calculated = (a + b) ** 2 / (4 * t)

    # Format result with proper rounding instead of truncation.
    quantizer = decimal.Decimal(1).scaleb(-num_digits)
    rounded = pi_calculated.quantize(quantizer, rounding=decimal.ROUND_HALF_UP)
    return format(rounded, f".{num_digits}f")


def setup_logging(verbose: bool = False) -> None:
    """
    Configure logging for the application.

    Args:
        verbose: If True, enable debug-level logging for detailed output.
    """
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s - %(levelname)s - %(message)s",
        datefmt="%H:%M:%S",
    )


def parse_arguments() -> argparse.Namespace:
    """
    Parse command-line arguments using argparse for modern CLI handling.

    Returns:
        Parsed arguments namespace containing user options.
    """
    parser = argparse.ArgumentParser(
        description="Calculate Pi to arbitrary precision using the Gauss-Legendre algorithm.",
        epilog="""
Examples:
  %(prog)s 100              Calculate Pi to 100 decimal places
  %(prog)s 5000 --progress  Calculate Pi to 5000 places with progress
  %(prog)s --interactive    Run in interactive mode
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "digits",
        type=int,
        nargs="?",
        default=None,
        help="Number of decimal places to calculate (default: interactive mode)",
    )

    parser.add_argument(
        "-p",
        "--progress",
        action="store_true",
        help="Show calculation progress (useful for large computations)",
    )

    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Enable verbose debug output"
    )

    parser.add_argument(
        "-i",
        "--interactive",
        action="store_true",
        help="Force interactive mode even if digits specified",
    )

    parser.add_argument(
        "--format",
        choices=["plain", "scientific", "formatted"],
        default="formatted",
        help="Output format (default: formatted with line breaks)",
    )

    return parser.parse_args()


def format_pi_output(pi_string: str, format_type: str = "formatted") -> str:
    """
    Format Pi output according to specified format type.

    Args:
        pi_string: The calculated Pi string (e.g., "3.14159...")
        format_type: Output format ('plain', 'scientific', 'formatted')

    Returns:
        Formatted Pi string ready for display.
    """
    if format_type == "plain":
        return pi_string
    elif format_type == "scientific":
        # Convert to scientific notation for very long numbers
        if len(pi_string) > 50:
            return f"{pi_string[:20]}...e+0 ({len(pi_string)-2} decimal places)"
        return pi_string
    else:  # formatted
        # Break into readable chunks for better readability
        if len(pi_string) <= 50:
            return pi_string

        # Format as: 3.14159 26535 89793 23846...
        integer_part = pi_string[:2]  # "3."
        decimal_part = pi_string[2:]

        # Group digits in sets of 5 for readability
        formatted_chunks = [
            decimal_part[i : i + 5] for i in range(0, len(decimal_part), 5)
        ]

        # Limit display to first 200 digits for readability
        if len(formatted_chunks) > 40:
            displayed_chunks = formatted_chunks[:40]
            remaining_digits = len(decimal_part) - 200
            return f"{integer_part}{' '.join(displayed_chunks)}... (+{remaining_digits} more digits)"

        return f"{integer_part}{' '.join(formatted_chunks)}"


def get_interactive_input(default_digits: int = 1000) -> int:
    """
    Get user input in interactive mode with proper error handling.

    Args:
        default_digits: Default number of digits if user presses Enter.

    Returns:
        Number of digits requested by user.

    Raises:
        KeyboardInterrupt: If user cancels with Ctrl+C.
    """
    while True:
        try:
            digits_input = input(
                f"Enter number of decimal places for Pi calculation (default: {default_digits}): "
            ).strip()

            if not digits_input:
                return default_digits

            digits = int(digits_input)
            if digits <= 0:
                print("Please enter a positive number.")
                continue
            if digits > 1000000:
                confirm = input(
                    f"Warning: {digits} digits may take a long time. Continue? (y/N): "
                )
                if confirm.lower() not in ["y", "yes"]:
                    continue

            return digits

        except ValueError:
            print("Please enter a valid integer.")
        except (EOFError, KeyboardInterrupt):
            print("\nOperation cancelled.")
            sys.exit(0)


def main() -> None:
    """
    Main application entry point with modern CLI handling and comprehensive features.

    This function orchestrates the entire Pi calculation process:
    1. Parse command-line arguments
    2. Set up logging and progress tracking
    3. Get calculation parameters (interactive or from args)
    4. Perform the calculation with timing
    5. Display results in the requested format
    """
    args = None
    try:
        # Parse command-line arguments
        args = parse_arguments()

        # Set up logging based on user preferences
        setup_logging(verbose=args.verbose)

        # Determine number of digits to calculate
        if args.interactive or args.digits is None:
            print("=== Pi Calculator (Gauss-Legendre Algorithm) ===")
            print(
                "High-precision Pi calculation using quadratically convergent algorithm\n"
            )
            num_digits = get_interactive_input()
        else:
            num_digits = args.digits

        # Validate input range
        if num_digits > 1000000:
            logging.warning(f"Large calculation requested: {num_digits} digits")

        # Perform the calculation with timing
        print(f"\nCalculating Pi to {num_digits:,} decimal places...")
        if args.progress:
            print("Progress will be shown during calculation...\n")

        start_time = time.perf_counter()

        try:
            pi_result = calculate_pi_gauss_legendre(
                num_digits, show_progress=args.progress
            )

            calculation_time = time.perf_counter() - start_time

            # Display results
            print(f"\n{'='*60}")
            print(f"Pi calculated to {num_digits:,} decimal places:")
            print(f"Calculation time: {calculation_time:.3f} seconds")
            print(f"{'='*60}")

            formatted_output = format_pi_output(pi_result, args.format)
            print(f"\n{formatted_output}")

            # Performance statistics
            if args.verbose:
                digits_per_second = (
                    num_digits / calculation_time if calculation_time > 0 else 0
                )
                print(f"\nPerformance: {digits_per_second:,.0f} digits/second")
                print(f"Memory precision used: {decimal.getcontext().prec}")

        except ValueError as e:
            logging.error(f"Calculation error: {e}")
            sys.exit(1)
        except MemoryError:
            logging.error(f"Insufficient memory for {num_digits} digit calculation")
            sys.exit(1)

    except KeyboardInterrupt:
        print("\nCalculation interrupted by user.")
        sys.exit(0)
    except Exception as e:
        logging.error(f"Unexpected error: {e}")
        # Show traceback if verbose mode was enabled
        if args and args.verbose:
            import traceback

            traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
