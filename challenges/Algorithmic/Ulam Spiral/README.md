# Ulam Spiral

## Problem Statement
Plot prime numbers on an integer lattice arranged in a spiral, revealing diagonal structures in their distribution. The script builds an \(n \times n\) grid marking prime coordinates and can render or export the visualization.

## Usage
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

## Debugging Tips
- Odd sizes (101, 201, …) keep the center aligned; even sizes shift the origin—use small sizes like 7 or 9 to verify orientation by hand.
- Compare the prime sieve output against a trusted library (e.g., `sympy.isprime`) when adjusting algorithms.
- If matplotlib is unavailable, the script will skip rendering; install `matplotlib` or pass `--no-show` to silence warnings.

## Implementation Notes
- Employs a NumPy-based Sieve of Eratosthenes to precompute primality up to \(n^2\).
- Walks a discrete spiral (right, up, left, down) incrementing step lengths to assign integers to grid coordinates.
- Optional plotting uses matplotlib with configurable colormaps and figure sizes, while JSON mode reports density statistics.

## Further Reading
- [Ulam, "A Visual Display of Some Properties of the Distribution of Primes" (SIAM Review, 1964)](https://doi.org/10.1137/1006065)
- [Lagarias & Odlyzko, "Effective Versions of the Chebotarev Density Theorem" (Algebraic Number Fields, 1977)](https://doi.org/10.1016/S0304-3975(97)00052-5)
