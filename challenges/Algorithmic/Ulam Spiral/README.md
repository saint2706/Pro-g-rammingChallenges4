# Ulam Spiral

## Problem Statement
Plot prime numbers on an integer lattice arranged in a spiral, revealing diagonal structures in their distribution. The script builds an \(n \times n\) grid marking prime coordinates and can render or export the visualization.

## Usage
### Python CLI
- Display a default 101×101 spiral:
  ```bash
  python ulam.py --size 101
  ```
- Save a higher resolution image without showing a window:
  ```bash
  python ulam.py --size 301 --no-show --save ulam.png
  ```
- Produce JSON metadata only (no plotting):
  ```bash
  python ulam.py --size 201 --json --no-show
  ```

### Haskell CLI (headless renderer + metadata)
- Install dependencies (one-time) with Cabal:
  ```bash
  cabal install --lib aeson JuicyPixels optparse-applicative vector
  ```
  > Prefer managing dependencies via a `cabal.project`/`stack.yaml` when integrating into a larger build.
- Compile the executable:
  ```bash
  ghc -O2 Ulam.hs -o ulam-hs
  ```
- Generate a 151×151 spiral and write the PNG to disk:
  ```bash
  ./ulam-hs --size 151 --save ulam_151.png
  ```
- Emit JSON metadata only (no image output):
  ```bash
  ./ulam-hs --size 201 --json
  ```
- Combine JSON metadata with image export:
  ```bash
  ./ulam-hs --size 301 --json --save ulam_301.png
  ```

## Debugging Tips
- Odd sizes (101, 201, …) keep the center aligned; even sizes offset the start to `size//2 - 1` so the first steps remain inside the grid—use small sizes like 4 or 6 to verify orientation by hand.
- Compare the prime sieve output against a trusted library (e.g., `sympy.isprime`) when adjusting algorithms.
- If matplotlib is unavailable, the script will skip rendering; install `matplotlib` or pass `--no-show` to silence warnings.

## How It Works
The Python script `ulam.py` generates an Ulam spiral by first creating a grid of numbers in a spiral pattern and then using the Sieve of Eratosthenes to identify which of those numbers are prime. The `generate_ulam_spiral` function creates the spiral, and the `sieve_of_eratosthenes` function efficiently finds all prime numbers up to the maximum number in the spiral.

The script uses `matplotlib` to create a visual representation of the spiral, with prime numbers colored differently from composite numbers. The command-line interface allows you to customize the size of the spiral, the color map, and the output format (image or JSON).

## Implementation Notes
- Employs a NumPy-based Sieve of Eratosthenes to precompute primality up to \(n^2\).
- Walks a discrete spiral (right, up, left, down) incrementing step lengths to assign integers to grid coordinates.
- Optional plotting uses matplotlib with configurable colormaps and figure sizes, while JSON mode reports density statistics.
- The Haskell implementation mirrors the CLI (size/json/save) features and relies on JuicyPixels for PNG output plus `aeson` for JSON serialization.

## Further Reading
- [Ulam, "A Visual Display of Some Properties of the Distribution of Primes" (SIAM Review, 1964)](https://doi.org/10.1137/1006065)
- [Lagarias & Odlyzko, "Effective Versions of the Chebotarev Density Theorem" (Algebraic Number Fields, 1977)](https://doi.org/10.1016/S0304-3975(97)00052-5)
