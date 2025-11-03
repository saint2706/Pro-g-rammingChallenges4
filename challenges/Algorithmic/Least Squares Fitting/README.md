# Least Squares Fitting

## Problem Statement
Fit a line \(y = mx + b\) to observed data by minimizing the sum of squared residuals (ordinary least squares). The project includes Python, C, and MATLAB variants for experimentation and comparison.

## Usage
- Generate synthetic data and fit a regression line (Python):
  ```bash
  python lsf.py --points 50 --noise 8 --seed 42
  ```
- Provide explicit coordinates via CLI:
  ```bash
  python lsf.py --x 0 1 2 3 --y 1.1 2.9 4.2 6.0 --json
  ```
- Compile the Haskell variant (requires GHC):
  ```bash
  ghc -O2 LSF.hs -o lsf-hs
  ./lsf-hs --points 50 --noise 5 --seed 4 --json summary.json --csv summary.csv --plot-data plot.dat --plot-script plot.gnuplot
  ```
- Supply manual coordinates to the Haskell CLI (space-separated values after each flag):
  ```bash
  ./lsf-hs --x 0 1 2 3 --y 1.1 2.9 4.2 6.0 --csv manual.csv
  ```
- Compile the C sample (requires a C compiler):
  ```bash
  gcc lsf.c -lm -o lsf && ./lsf data.txt
  ```
- MATLAB/Octave users can run `lsf.m` or `lsfc.m` for matrix-based derivations.

## Debugging Tips
- Known dataset: `x = [0, 1, 2]`, `y = [1, 3, 5]` should yield slope `~2` and intercept `1`.
- Run the automated tests:
  ```bash
  pytest test_lsf.py
  ```
- Use `--no-plot` when diagnosing numeric issues in headless environments; re-enable plotting once values look reasonable.

## Implementation Notes
- The Python version leverages NumPy for vectorized sums and optionally matplotlib for visualization.
- The Haskell binary mirrors the CLI ergonomics of the Python script, supporting synthetic datasets, manual coordinate entry, and optional JSON/CSV summaries alongside gnuplot-friendly data dumps.
- Dataclass-backed configuration simplifies reproducible synthetic datasets.
- Error handling guards against degenerate inputs (empty arrays, vertical lines).

## Further Reading
- [Montgomery, Peck & Vining, *Introduction to Linear Regression Analysis*](https://doi.org/10.1002/0470055464)
- [Gauss & Legendre Least Squares History (SIAM Review, 1999)](https://doi.org/10.1137/S003614459833698X)
