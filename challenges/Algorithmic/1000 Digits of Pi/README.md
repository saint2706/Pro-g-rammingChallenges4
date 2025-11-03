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
- Build the standalone Haskell executable with GHC (or `stack ghci -- Pi.hs` if you prefer a
  project workflow):
  ```bash
  ghc -O2 Pi.hs -o pi-hs
  ```
- Run the Haskell solver with a digit target and optional verbose logging that mirrors the
  Python progress output:
  ```bash
  ./pi-hs --digits 1000
  ./pi-hs --digits 250 --verbose
  ```

## Visualizing Convergence
- Plot the Gauss–Legendre convergence curve and export it as HTML:
  ```bash
  python ../Digits\ of\ Pi/pi_visualizer.py --algorithm gauss-legendre --digits 200 --headless --html-output gauss.html
  ```
- Capture the numerical summary for automated checks:
  ```bash
  python ../Digits\ of\ Pi/pi_visualizer.py --algorithm gauss-legendre --digits 200 --headless --json-summary gauss.json
  ```
- Programmatic access is available through `generate_gauss_legendre_convergence`, which yields dataclass records for each iteration.

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
