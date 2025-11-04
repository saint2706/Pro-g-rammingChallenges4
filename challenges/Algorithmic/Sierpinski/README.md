# Sierpinski Triangle

## Problem Statement
Generate an ASCII rendering of the discrete Sierpinski triangle where lattice points satisfy `(x & y) == 0`. The script provides CLI controls for size, character choice, JSON metadata, and optional file output.

## Usage
### Python
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

### Haskell
- Compile the executable (or use `runghc` for a one-off run):
  ```bash
  ghc -O2 Sierpinski.hs -o sierpinski
  ./sierpinski
  ```
- Generate a higher resolution triangle with a custom glyph:
  ```bash
  ./sierpinski --size 32 --char '#'
  ```
- Emit JSON metadata identical to the Python script (and optionally persist it):
  ```bash
  ./sierpinski --size 32 --json --save tri.json
  ```
Both CLIs share feature parity—size/character controls, optional JSON emission, and file output follow the same semantics so downstream tooling can swap between implementations without changes.

## Debugging Tips
- Sizes that are powers of two produce the classic triangular outline; non-powers still work but will look skewed—use this to verify validation messaging.
- Run the tests:
  ```bash
  pytest test_triangle.py
  ```
  They check generation symmetry and metadata fields.
- Print the generated lines via `generate_sierpinski_lines` in a Python REPL to inspect edge cases without CLI formatting.

## How It Works
The Python script `triangle.py` generates a Sierpinski triangle using a bitwise AND operation. The core of the algorithm is the `generate_sierpinski_lines` function, which iterates through each point (x, y) of a grid and checks if `(x & y) == 0`. If this condition is true, the point is part of the triangle and is drawn using the specified character.

The script includes a command-line interface for setting the size of the triangle and the character to use. It also supports JSON output for providing metadata about the generated triangle. The `test_triangle.py` file contains unit tests to verify the correctness of the generation logic.

## Implementation Notes
- Uses bitwise masking to determine inclusion, avoiding recursion or heavy computation.
- Dataclass configuration enforces valid size/character inputs and reports whether the chosen size is a power of two.
- Generation functions return reusable data structures (list of strings) facilitating tests and downstream formatting.

## Further Reading
- [Sierpinski Triangle (Wolfram MathWorld)](https://mathworld.wolfram.com/SierpinskiSieve.html)
- [Mandelbrot, *Fractals and Chaos*, Chapter 2 (Fractals from Simple Rules)](https://press.princeton.edu/books/paperback/9780691023773/fractals-and-chaos)
