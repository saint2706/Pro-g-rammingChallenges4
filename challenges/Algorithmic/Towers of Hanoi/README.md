# Towers of Hanoi

## Problem Statement
Solve the Towers of Hanoi puzzle for \(n\) disks, producing the minimal sequence of moves required to relocate a stacked tower from one peg to another using either recursive or iterative strategies.

## Usage
- Print the move sequence for 3 disks:
  ```bash
  python ToH.py --disks 3
  ```
- Use the iterative solver and emit JSON:
  ```bash
  python ToH.py --disks 5 --iterative --json
  ```
- Visualize the state progression (Python GUI helper):
  ```bash
  python ToH_visualizer.py --disks 4
  ```
- Explore the JavaScript demo by opening `ToH.js` in a browser console.

## Debugging Tips
- The known minimal move count is `2^n - 1`; use `--count-only` to verify the solver respects this formula.
- Run the automated tests:
  ```bash
  pytest test_toh.py
  ```
  They validate recursive and iterative generators plus state tracking.
- Enable `--trace` (if available) to print intermediate peg configurations when diagnosing move ordering issues.

## Implementation Notes
- Provides both recursive (`towers_of_hanoi`) and stack-based iterative generators for educational comparison.
- `hanoi_state_generator` yields peg contents after each move for integration with visualizers.
- CLI accepts peg labels, step limits, and JSON output, making scripting and testing straightforward.

## Further Reading
- [Lucas, "Récréations Mathématiques" (1893) – Original Towers of Hanoi puzzle introduction](https://gallica.bnf.fr/ark:/12148/bpt6k99635k)
- [Sedgewick & Wayne, *Algorithms*, Section 1.3 (Stacks and Recursion Exercises)](https://algs4.cs.princeton.edu/home/)
