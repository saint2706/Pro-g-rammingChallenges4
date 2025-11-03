# Towers of Hanoi

## Problem Statement
Solve the Towers of Hanoi puzzle for \(n\) disks, producing the minimal sequence of moves required to relocate a stacked tower from one peg to another using either recursive or iterative strategies.

## Usage
### Python utilities
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

### Haskell CLI (`ToH.hs`)
- Ensure GHC is installed (via [GHCup](https://www.haskell.org/ghcup/) or your package manager).
- Compile once with optimizations:
  ```bash
  ghc -O2 -outputdir build -o toh ToH.hs
  ```
- Run the recursive solver for 4 disks:
  ```bash
  ./toh --disks 4
  ```
- Switch to the iterative solver and export JSON (states included when `--trace` is set):
  ```bash
  ./toh --disks 6 --iterative --json --trace > moves.json 2> trace.log
  ```
  The JSON schema mirrors `ToH.py` (`moves`, `states`, `total_moves`), making it easy to feed into scripts or the JavaScript demo.
- For quick experiments without compiling, use the interpreter directly:
  ```bash
  runhaskell ToH.hs --disks 3 --max-steps 5
  ```

#### Visualizer integration tips
- `--trace` streams peg states to `stderr` in the `A:[...] B:[...] C:[...]` format; redirect this stream into a file and load it from a short Python adapter to feed `HanoiVisualizer` (replace its call to `hanoi_state_generator`).
- When `--json` is enabled alongside `--trace`, the emitted JSON contains a `states` array compatible with the Python visualizer's `hanoi_state_generator`. Parse this array and hand it to the existing drawing routines to replay the Haskell-generated solution without modification to the animation code.

## Debugging Tips
- The known minimal move count is `2^n - 1`; use `--count-only` (Python) or `./toh --count-only` (Haskell) to verify the solver respects this formula.
- Run the automated tests:
  ```bash
  pytest test_toh.py
  ```
  They validate recursive and iterative generators plus state tracking.
- Enable `--trace` to print intermediate peg configurations when diagnosing move ordering issues. In Haskell, these traces go to `stderr`, keeping JSON or move output clean on `stdout`.

## Implementation Notes
- Provides both recursive (`towers_of_hanoi`) and stack-based iterative generators for educational comparison.
- `hanoi_state_generator` yields peg contents after each move for integration with visualizers.
- CLI accepts peg labels, step limits, and JSON output, making scripting and testing straightforward.

## Further Reading
- [Lucas, "Récréations Mathématiques" (1893) – Original Towers of Hanoi puzzle introduction](https://gallica.bnf.fr/ark:/12148/bpt6k99635k)
- [Sedgewick & Wayne, *Algorithms*, Section 1.3 (Stacks and Recursion Exercises)](https://algs4.cs.princeton.edu/home/)
