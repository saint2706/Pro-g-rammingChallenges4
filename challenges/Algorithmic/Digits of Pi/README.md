# Digits of Pi

## Problem Statement
Generate high-precision approximations of \(\pi\) via the Chudnovsky series, a rapidly converging formula that adds roughly 14 digits of accuracy per iteration. The module lets you script or run a CLI to produce thousands of digits and optionally verify them against reference data.

## Usage

### Python CLI
- Run with sensible defaults:
  ```bash
  python DigitPi.py
  ```
- Request a specific precision and enable progress feedback:
  ```bash
  python DigitPi.py --iterations 10 --precision 1000 --progress
  ```
- Compare the output against the bundled reference digits:
  ```bash
  python DigitPi.py --precision 1000 --verify-file pi_test.txt
  ```

### Haskell CLI
- Compile the executable (requires GHC):
  ```bash
  ghc -O2 DigitPi.hs -o digitpi
  ```
- Compute digits with explicit iteration and precision controls:
  ```bash
  ./digitpi --iterations 8 --precision 200
  ```
- Stream per-iteration updates while solving:
  ```bash
  ./digitpi --iterations 8 --precision 200 --progress
  ```
- Verify against the built-in table (default 50 digits) or a custom reference:
  ```bash
  ./digitpi --iterations 8 --precision 200 --verify-digits 75 --verify-file pi_test.txt
  ```
- Disable verification entirely when benchmarking raw performance:
  ```bash
  ./digitpi --iterations 8 --precision 200 --no-verify
  ```

## Visualizing Convergence
- Animate the iteration-by-iteration error curve with Plotly:
  ```bash
  python pi_visualizer.py --algorithm chudnovsky --iterations 8 --headless --html-output chudnovsky.html
  ```
- Export structured convergence data for tests or further processing:
  ```bash
  python pi_visualizer.py --algorithm chudnovsky --iterations 8 --headless --json-summary chudnovsky.json
  ```
- The visualizer reuses `generate_chudnovsky_convergence` so you can also access the structured steps directly (or use the Haskell CLI for pure terminal output):
  ```python
  >>> from DigitPi import generate_chudnovsky_convergence
  >>> steps = list(generate_chudnovsky_convergence(5))
  >>> steps[0].approximation
  Decimal('3.141592653589793238462643383')
  ```

## Debugging Tips
- For regression checks, compute 100 digits and compare the prefix to the known constant `3.1415926535...`.
- Use the `--progress` flag to see iteration-by-iteration convergence and detect stalls caused by insufficient `decimal` context precision.
- When adjusting parameters, run quick sanity tests by loading digits from `pi_test.txt` and ensuring the `verify_digits` option reports zero mismatches.

## Implementation Notes
- Uses Python's `decimal` module with configurable precision and dataclass-backed configuration.
- Implements the Chudnovsky summation with guard digits to mitigate rounding error.
- Supports multiple output formats (plain, decimal, scientific) and optional file export.

## Further Reading
- [Chudnovsky algorithm (Mathematics of Computation, 1988)](https://doi.org/10.1090/S0025-5718-1989-0965410-5)
- [Bailey, Borwein & Plouffe, "On the Rapid Computation of Various Polylogarithmic Constants" (Mathematics of Computation, 1997)](https://doi.org/10.1090/S0025-5718-97-00856-9)
