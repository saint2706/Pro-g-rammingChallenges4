# 1000 Digits of Pi

## Problem Statement
Compute \(\pi\) to thousands of decimal places using the Gauss–Legendre iterative refinement method. The script exposes a command line interface for requesting an arbitrary number of digits and prints the resulting decimal expansion.

## Usage
- Run an interactive calculation:
  ```bash
  python pi.py
  ```
- Specify the desired precision explicitly:
  ```bash
  python pi.py 1000
  ```
  The script prints \(\pi\) with the requested number of digits and reports the elapsed time.

## Debugging Tips
- Small precisions (e.g. `python pi.py 10`) should yield `3.1415926536`. Larger requests should begin with the same prefix, making it easy to sanity-check rounding.
- Enable progress logging to monitor convergence:
  ```bash
  python pi.py 500 --verbose
  ```
- There are no automated tests for this module, but you can compare the output against published references (e.g., `mpmath.mp.pi` in a Python shell) to validate additional digits.

## Implementation Notes
- Uses Python's `decimal` module with an adaptive safety margin to keep intermediate precision stable.
- Implements the quadratic-converging Gauss–Legendre iteration, doubling the number of correct digits on each pass.
- Optional progress logging leverages the standard `logging` module.

## Further Reading
- [Gauss–Legendre Algorithm (Wikipedia)](https://en.wikipedia.org/wiki/Gauss%E2%80%93Legendre_algorithm)
- [Borwein & Borwein, *Pi and the AGM* (Academic Press)](https://doi.org/10.1016/C2013-0-07463-5)
