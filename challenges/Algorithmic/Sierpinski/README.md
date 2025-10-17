# Sierpinski Triangle

## Problem Statement
Generate an ASCII rendering of the discrete Sierpinski triangle where lattice points satisfy `(x & y) == 0`. The script provides CLI controls for size, character choice, JSON metadata, and optional file output.

## Usage
- Render the default pattern:
  ```bash
  python triangle.py
  ```
- Increase resolution and use a custom character:
  ```bash
  python triangle.py --size 32 --char '#'
  ```
- Export to a text file with JSON metadata:
  ```bash
  python triangle.py --size 32 --json --save tri.txt
  ```

## Debugging Tips
- Sizes that are powers of two produce the classic triangular outline; non-powers still work but will look skewed—use this to verify validation messaging.
- Run the tests:
  ```bash
  pytest test_triangle.py
  ```
  They check generation symmetry and metadata fields.
- Print the generated lines via `generate_sierpinski_lines` in a Python REPL to inspect edge cases without CLI formatting.

## Implementation Notes
- Uses bitwise masking to determine inclusion, avoiding recursion or heavy computation.
- Dataclass configuration enforces valid size/character inputs and reports whether the chosen size is a power of two.
- Generation functions return reusable data structures (list of strings) facilitating tests and downstream formatting.

## Further Reading
- [Sierpinski Triangle (Wolfram MathWorld)](https://mathworld.wolfram.com/SierpinskiSieve.html)
- [Mandelbrot, *Fractals and Chaos*, Chapter 2 (Fractals from Simple Rules)](https://press.princeton.edu/books/paperback/9780691023773/fractals-and-chaos)
