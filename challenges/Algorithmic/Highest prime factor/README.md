# Highest Prime Factor

## Problem Statement
For a given positive integer \(n \ge 2\), determine its largest prime factor using optimized trial division. The utility accepts single values, batch inputs, or streamed data from stdin.

## Usage
- Compute factors for one or more integers:
  ```bash
  python HighPF.py 13195 600851475143
  ```
- Request JSON-formatted results and timings:
  ```bash
  python HighPF.py --json --timing 42 97 10
  ```
- Pipe values from another program:
  ```bash
  echo 9876543210 | python HighPF.py --stdin
  ```

## Debugging Tips
- Known examples: `600851475143` should return `6857`, while prime inputs should simply echo themselves.
- Run the automated tests:
  ```bash
  pytest test_highpf.py
  ```
- Enable verbose logging (`--verbose`) to observe each factor elimination when diagnosing performance issues on large composites.

## Implementation Notes
- Strips factors of 2 and 3 before iterating potential factors of the form `6k ± 1` up to \(\sqrt{n}\).
- Uses Python's arbitrary-precision integers, so runtime—not overflow—limits computation.
- Provides both CLI parsing and reusable `highest_prime_factor` function for unit tests.

## Further Reading
- [Project Euler Problem 3 Discussion](https://projecteuler.net/problem=3)
- [Crandall & Pomerance, *Prime Numbers: A Computational Perspective*, Chapter 5](https://doi.org/10.1007/978-0-387-49923-3)
