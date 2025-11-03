# FizzBuzz

## Problem Statement
Print the integers from 1 through *n*, substituting `Fizz` for multiples of 3, `Buzz` for multiples of 5, and `FizzBuzz` for numbers divisible by both. This folder contains both Python and C implementations along with tests.

## Usage
- Run the Python implementation:
  ```bash
  python fizzbuzz.py --limit 20
  ```
- Visualise rule coverage with Matplotlib:
  ```bash
  python fizzbuzz_visualizer.py --limit 30 --rule 3:Fizz --rule 5:Buzz --output fizzbuzz.png
  ```
- Compile and run the C version:
  ```bash
  gcc fizzbuzz.c -o fizzbuzz && ./fizzbuzz 20
  ```
- Build the Haskell version (parity with the Python CLI, requires GHC):
  ```bash
  ghc -O2 FizzBuzz.hs -o fizzbuzz-hs
  ./fizzbuzz-hs --limit 30 --rule 3:Fizz --rule 5:Buzz --format json
  ```
  You can also execute it without compiling using `runghc FizzBuzz.hs --limit 30 --format csv`.
- Execute the test suite:
  ```bash
  pytest test_fizzbuzz.py
  ```

## Debugging Tips
- The canonical first 15 outputs should be `1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz`.
- Switch to JSON formatting to inspect sequences programmatically:
  ```bash
  python fizzbuzz.py --limit 15 --format json
  ```
- Generate metadata plus a PNG artefact for a custom rule-set:
  ```bash
  python fizzbuzz_visualizer.py --limit 42 --rule 3:Fizz --rule 5:Buzz --rule 7:Pop \
    --no-numbers --output fizzbuzz.png --metadata fizzbuzz.json
  ```
- When modifying the logic, re-run `pytest` to confirm that edge cases (such as custom divisors via `--rule`) still pass.

## Implementation Notes
- The Python version exposes reusable functions (`fizzbuzz_sequence`) used by the tests.
- Both implementations accept custom start/stop ranges and divisor pairs.
- Command-line flags provide optional JSON/CSV output for integration with other tools.
- The Haskell CLI mirrors the Python options for limit selection, rule overrides, number suppression, and JSON/CSV/plain emitters so existing automations can switch languages without changing flags.

## Further Reading
- ["Why Can't Programmers.. Program?" by Jeff Atwood](https://blog.codinghorror.com/why-cant-programmers-program/)
- [Sedgewick & Wayne, *Introduction to Programming in Python*, Section 1.2 (Conditionals and Loops)](https://introcs.cs.princeton.edu/python/home/)
