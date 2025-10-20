#!/usr/bin/env python3
"""
High-Precision Pi Calculator

A high-precision Pi calculator using the Chudnovsky algorithm, one of the fastest
known algorithms for computing Pi. This implementation can compute thousands of
digits with high accuracy and includes modern Python features for usability.

The Chudnovsky algorithm converges extremely rapidly, adding approximately 14.18
decimal digits per iteration. It was used to compute record-breaking digit counts
of Pi and is the algorithm behind many modern Pi calculations.

Features:
- High-precision arbitrary decimal arithmetic
- Progress tracking for long computations
- Multiple output formats (decimal, scientific notation)
- Professional command-line interface
- Comprehensive error handling and validation

Example:
    >>> from DigitPi import compute_pi
    >>> pi = compute_pi(10)
    >>> print(f"Pi to high precision: {pi}")

    Or from command line:
    $ python DigitPi.py --iterations 10 --precision 100 --format decimal

Author: Programming Challenges
Date: 2025
Version: 2.0
Algorithm: Chudnovsky Algorithm (1988)
"""

import argparse
import decimal
import logging
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Iterator, Optional, Union


class OutputFormat(Enum):
    """Enumeration of supported output formats for Pi."""

    DECIMAL = "decimal"
    SCIENTIFIC = "scientific"
    PLAIN = "plain"


class PrecisionMode(Enum):
    """Enumeration of precision calculation modes."""

    AUTO = "auto"  # Automatically calculate based on iterations
    FIXED = "fixed"  # Use a fixed number of decimal places
    MAXIMUM = "maximum"  # Use maximum available precision


@dataclass
class PiConfiguration:
    """
    Configuration parameters for Pi computation.

    Attributes:
        iterations: Number of algorithm iterations (more = higher precision)
        precision: Target decimal precision (number of decimal places)
        precision_mode: How to calculate final precision
        output_format: Format for displaying the result
        show_progress: Whether to display computation progress
        output_file: Optional file path to save results
        verify_digits: Number of known Pi digits to verify against
    """

    iterations: int = 7
    precision: Optional[int] = None
    precision_mode: PrecisionMode = PrecisionMode.AUTO
    output_format: OutputFormat = OutputFormat.DECIMAL
    show_progress: bool = False
    output_file: Optional[Path] = None
    verify_digits: int = 50


# Configure logging
logger = logging.getLogger(__name__)


@dataclass
class ChudnovskyConvergenceStep:
    """Structured representation of a Chudnovsky iteration."""

    iteration: int
    approximation: decimal.Decimal
    series_sum: decimal.Decimal
    elapsed: float


def setup_logging(level: str = "INFO") -> None:
    """
    Configure logging for the application.

    Args:
        level: Logging level (DEBUG, INFO, WARNING, ERROR)
    """
    logging.basicConfig(
        level=getattr(logging, level.upper()),
        format="%(asctime)s - %(levelname)s - %(message)s",
        datefmt="%H:%M:%S",
    )


def estimate_precision(iterations: int) -> int:
    """
    Estimate the number of decimal digits that will be accurate for given iterations.

    The Chudnovsky algorithm adds approximately 14.18 decimal digits per iteration.
    This function provides a conservative estimate.

    Args:
        iterations: Number of algorithm iterations

    Returns:
        Estimated number of accurate decimal digits
    """
    return max(1, int(iterations * 14.18))


def set_decimal_precision(digits: int, buffer: int = 50) -> None:
    """
    Set the decimal module precision with appropriate buffer.

    Args:
        digits: Target number of decimal digits
        buffer: Additional precision buffer to prevent rounding errors
    """
    total_precision = digits + buffer
    decimal.getcontext().prec = total_precision
    logger.debug(
        f"Set decimal precision to {total_precision} digits ({digits} target + {buffer} buffer)"
    )


def compute_pi(
    iterations: int, config: Optional[PiConfiguration] = None
) -> decimal.Decimal:
    """
    Compute Pi using the Chudnovsky algorithm.

    The Chudnovsky algorithm is one of the fastest known algorithms for computing Pi.
    It was discovered by David and Gregory Chudnovsky in 1988 and has been used to
    compute record-breaking numbers of Pi digits.

    Algorithm formula:
    1/π = 12 * Σ(k=0 to ∞) [(-1)^k * (6k)! * (13591409 + 545140134*k)] /
                            [(3k)! * (k!)^3 * 640320^(3k+3/2)]

    Args:
        iterations: Number of algorithm iterations (more iterations = higher precision)
                   Each iteration adds approximately 14.18 decimal digits
        config: Optional configuration object for progress tracking and settings

    Returns:
        High-precision decimal representation of Pi

    Raises:
        ValueError: If iterations is not a positive integer
    """
    if iterations <= 0:
        raise ValueError("Number of iterations must be a positive integer")

    # Initialize configuration if not provided
    if config is None:
        config = PiConfiguration(iterations=iterations)

    # Calculate required precision and configure logging context
    estimated_digits = estimate_precision(iterations)
    working_precision = (
        estimated_digits + 100
    )  # Extra buffer for intermediate calculations

    logger.info(
        f"Computing Pi with {iterations} iterations (estimated {estimated_digits} digits)"
    )

    start_time = None
    if config.show_progress:
        print(f"Starting Pi computation with {iterations} iterations...")
        start_time = time.time()

    progress_interval = max(1, iterations // 10)
    pi_result: Optional[decimal.Decimal] = None

    for step in generate_chudnovsky_convergence(
        iterations, working_precision=working_precision
    ):
        pi_result = step.approximation

        if config.show_progress:
            completed = step.iteration + 1
            if completed == iterations or completed % progress_interval == 0:
                progress = (completed / iterations) * 100
                elapsed = step.elapsed
                if completed > 0 and elapsed > 0:
                    estimated_total = elapsed / (completed / iterations)
                    remaining = max(0.0, estimated_total - elapsed)
                    print(f"Progress: {progress:.1f}% (ETA: {remaining:.1f}s)")
                else:
                    print(f"Progress: {progress:.1f}%")

    if pi_result is None:
        raise RuntimeError("Chudnovsky computation did not produce any iterations")

    if config.show_progress and start_time is not None:
        elapsed = time.time() - start_time
        print(f"Computation completed in {elapsed:.2f} seconds")

    # Set final precision based on configuration
    if config.precision_mode == PrecisionMode.AUTO:
        final_precision = estimated_digits
    elif config.precision_mode == PrecisionMode.FIXED and config.precision:
        final_precision = config.precision
    else:
        final_precision = estimated_digits

    # Apply final precision
    decimal.getcontext().prec = final_precision
    final_pi = +pi_result  # The + operator applies current context precision

    logger.info(f"Pi computed to {final_precision} decimal places")
    return final_pi


def generate_chudnovsky_convergence(
    iterations: int, *, working_precision: Optional[int] = None
) -> Iterator[ChudnovskyConvergenceStep]:
    """Yield successive approximations from the Chudnovsky series."""

    if iterations <= 0:
        raise ValueError("Number of iterations must be a positive integer")

    estimated_digits = estimate_precision(iterations)
    precision = working_precision or estimated_digits + 100

    previous_precision = decimal.getcontext().prec
    set_decimal_precision(precision)

    # Chudnovsky algorithm constants
    constant_multiplier = decimal.Decimal(426880) * decimal.Decimal(10005).sqrt()

    factorial_ratio = decimal.Decimal(1)
    linear_term = decimal.Decimal(13591409)
    exponential_term = decimal.Decimal(1)
    series_sum = linear_term

    factorial_multiplier = decimal.Decimal(545140134)
    exponential_divisor = decimal.Decimal(262537412640768000)

    start = time.perf_counter()

    try:
        # Initial approximation (iteration 0)
        pi_estimate = constant_multiplier / series_sum
        yield ChudnovskyConvergenceStep(
            iteration=0,
            approximation=pi_estimate,
            series_sum=series_sum,
            elapsed=0.0,
        )

        for k in range(1, iterations):
            factorial_ratio *= (
                (6 * k - 5)
                * (6 * k - 4)
                * (6 * k - 3)
                * (6 * k - 2)
                * (6 * k - 1)
                * (6 * k)
            )
            factorial_ratio /= (3 * k - 2) * (3 * k - 1) * (3 * k) * (k**3)

            linear_term += factorial_multiplier
            exponential_term /= exponential_divisor

            current_term = factorial_ratio * linear_term * exponential_term
            if k % 2 == 0:
                series_sum += current_term
            else:
                series_sum -= current_term

            pi_estimate = constant_multiplier / series_sum
            yield ChudnovskyConvergenceStep(
                iteration=k,
                approximation=pi_estimate,
                series_sum=series_sum,
                elapsed=time.perf_counter() - start,
            )
    finally:
        decimal.getcontext().prec = previous_precision


def verify_pi_accuracy(
    computed_pi: decimal.Decimal, known_digits: int = 50
) -> tuple[bool, int]:
    """
    Verify the accuracy of computed Pi against known digits.

    Args:
        computed_pi: The computed Pi value to verify
        known_digits: Number of known Pi digits to compare against

    Returns:
        Tuple of (is_accurate, correct_digits) where:
        - is_accurate: True if all compared digits are correct
        - correct_digits: Number of correct digits found
    """
    # First 100 digits of Pi for verification
    pi_reference = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

    computed_str = str(computed_pi)
    reference_str = pi_reference[
        : min(len(computed_str), known_digits + 2)
    ]  # +2 for "3."

    correct_digits = 0
    for i, (comp, ref) in enumerate(zip(computed_str, reference_str)):
        if comp == ref:
            correct_digits += 1
        else:
            break

    # Subtract 2 for "3." to get actual decimal digits
    decimal_digits_correct = max(0, correct_digits - 2)
    is_accurate = decimal_digits_correct >= known_digits

    return is_accurate, decimal_digits_correct


def format_pi_output(
    pi_value: decimal.Decimal,
    output_format: OutputFormat,
    precision: Optional[int] = None,
) -> str:
    """
    Format Pi value according to specified output format.

    Args:
        pi_value: The Pi value to format
        output_format: Desired output format
        precision: Optional precision override

    Returns:
        Formatted string representation of Pi
    """
    if output_format == OutputFormat.SCIENTIFIC:
        return f"{pi_value:.{precision or 50}E}"
    elif output_format == OutputFormat.PLAIN:
        return str(pi_value)
    else:  # DECIMAL format (default)
        if precision:
            return f"{pi_value:.{precision}f}"
        return str(pi_value)


def save_pi_to_file(
    pi_value: decimal.Decimal, file_path: Path, config: PiConfiguration
) -> None:
    """
    Save Pi computation results to a file.

    Args:
        pi_value: Computed Pi value
        file_path: Path where to save the file
        config: Configuration used for computation
    """
    try:
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(f"Pi Computation Results\n")
            f.write(f"=====================\n\n")
            f.write(f"Algorithm: Chudnovsky Algorithm\n")
            f.write(f"Iterations: {config.iterations}\n")
            f.write(
                f"Estimated Precision: {estimate_precision(config.iterations)} digits\n"
            )
            f.write(f"Output Format: {config.output_format.value}\n")
            f.write(f"Computation Date: {time.strftime('%Y-%m-%d %H:%M:%S')}\n\n")

            # Verify accuracy
            is_accurate, correct_digits = verify_pi_accuracy(
                pi_value, config.verify_digits
            )
            f.write(f"Accuracy Verification: {correct_digits} correct digits")
            if is_accurate:
                f.write(" (VERIFIED)\n\n")
            else:
                f.write(" (WARNING: May contain errors)\n\n")

            f.write(f"Pi Value:\n")
            formatted_pi = format_pi_output(
                pi_value, config.output_format, config.precision
            )
            f.write(formatted_pi)
            f.write("\n")

        logger.info(f"Results saved to: {file_path}")

    except IOError as e:
        logger.error(f"Failed to save results to file: {e}")
        raise


def create_argument_parser() -> argparse.ArgumentParser:
    """
    Create and configure the command-line argument parser.

    Returns:
        Configured ArgumentParser instance
    """
    parser = argparse.ArgumentParser(
        description="High-Precision Pi Calculator using the Chudnovsky Algorithm",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s --iterations 10 --precision 100
  %(prog)s -i 20 --format scientific --output pi_results.txt
  %(prog)s --iterations 5 --progress --verify 30
  %(prog)s -i 15 --precision 200 --format decimal --verbose

Algorithm Information:
  The Chudnovsky algorithm is one of the fastest known algorithms for computing Pi.
  Each iteration adds approximately 14.18 decimal digits of precision.
  
  Iteration Guidelines:
  - 1-5 iterations: ~14-71 digits (suitable for basic calculations)
  - 6-10 iterations: ~85-142 digits (good for most applications)
  - 11-20 iterations: ~156-284 digits (high precision scientific work)
  - 21+ iterations: 300+ digits (research/verification purposes)
        """,
    )

    # Core computation arguments
    parser.add_argument(
        "-i",
        "--iterations",
        type=int,
        default=7,
        help="Number of algorithm iterations (default: %(default)s, ~100 digits)",
    )

    parser.add_argument(
        "-p",
        "--precision",
        type=int,
        help="Target decimal precision (auto-calculated if not specified)",
    )

    parser.add_argument(
        "--precision-mode",
        type=str,
        choices=[mode.value for mode in PrecisionMode],
        default=PrecisionMode.AUTO.value,
        help="Precision calculation mode (default: %(default)s)",
    )

    # Output formatting arguments
    parser.add_argument(
        "-f",
        "--format",
        type=str,
        choices=[fmt.value for fmt in OutputFormat],
        default=OutputFormat.DECIMAL.value,
        help="Output format (default: %(default)s)",
    )

    parser.add_argument("-o", "--output", type=Path, help="Save results to file")

    # Progress and verification arguments
    parser.add_argument(
        "--progress", action="store_true", help="Show computation progress"
    )

    parser.add_argument(
        "--verify",
        type=int,
        default=50,
        help="Number of Pi digits to verify for accuracy (default: %(default)s)",
    )

    # Logging arguments
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Enable verbose logging"
    )

    parser.add_argument(
        "--log-level",
        type=str,
        choices=["DEBUG", "INFO", "WARNING", "ERROR"],
        default="INFO",
        help="Set logging level (default: %(default)s)",
    )

    return parser


def main() -> None:
    """
    Main function to handle command line interface and Pi computation.
    """
    # Parse command line arguments
    parser = create_argument_parser()
    args = parser.parse_args()

    # Setup logging based on arguments
    log_level = "DEBUG" if args.verbose else args.log_level
    setup_logging(log_level)

    # Validate arguments
    if args.iterations < 1:
        logger.error("Number of iterations must be at least 1")
        sys.exit(1)

    if args.precision is not None and args.precision < 1:
        logger.error("Precision must be at least 1")
        sys.exit(1)

    if args.verify < 1:
        logger.error("Verification digit count must be at least 1")
        sys.exit(1)

    # Create configuration
    config = PiConfiguration(
        iterations=args.iterations,
        precision=args.precision,
        precision_mode=PrecisionMode(args.precision_mode),
        output_format=OutputFormat(args.format),
        show_progress=args.progress,
        output_file=args.output,
        verify_digits=args.verify,
    )

    logger.info("Starting Pi computation with Chudnovsky algorithm")
    logger.info(f"Configuration: {args.iterations} iterations, {args.format} format")

    try:
        # Compute Pi
        start_time = time.time()
        pi_value = compute_pi(args.iterations, config)
        computation_time = time.time() - start_time

        # Verify accuracy
        is_accurate, correct_digits = verify_pi_accuracy(pi_value, args.verify)

        # Display results
        print("\n" + "=" * 70)
        print("Pi Computation Results")
        print("=" * 70)
        print(f"Algorithm: Chudnovsky Algorithm")
        print(f"Iterations: {args.iterations}")
        print(f"Computation Time: {computation_time:.3f} seconds")
        print(f"Estimated Precision: {estimate_precision(args.iterations)} digits")
        print(f"Verified Digits: {correct_digits} correct")

        if is_accurate:
            print("✓ Accuracy Verification: PASSED")
        else:
            print("⚠ Accuracy Verification: May contain errors")

        print("\nPi Value:")
        print("-" * 70)
        formatted_pi = format_pi_output(
            pi_value, config.output_format, config.precision
        )
        print(formatted_pi)
        print("-" * 70)

        # Save to file if requested
        if config.output_file:
            save_pi_to_file(pi_value, config.output_file, config)
            print(f"\nResults saved to: {config.output_file}")

        logger.info("Pi computation completed successfully")

    except KeyboardInterrupt:
        logger.warning("Computation interrupted by user")
        print("\nComputation interrupted.")
        sys.exit(1)

    except ValueError as e:
        logger.error(f"Invalid input: {e}")
        print(f"Error: {e}")
        sys.exit(1)

    except Exception as e:
        logger.error(f"Unexpected error during computation: {e}")
        print(f"An unexpected error occurred: {e}")
        if args.verbose:
            import traceback

            traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
