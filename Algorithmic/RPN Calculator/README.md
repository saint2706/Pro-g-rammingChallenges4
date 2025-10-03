# Reverse Polish Notation Calculator

## Problem Statement
Evaluate arithmetic expressions written in Reverse Polish Notation (postfix notation) supporting arithmetic operators, mathematical functions, constants, and optional trace output.

## Usage
- Evaluate a simple expression:
  ```bash
  python postifx_evaluator.py --expr "3 4 + 2 *"
  ```
- Interpret trigonometric inputs in degrees:
  ```bash
  python postifx_evaluator.py --expr "90 sin" --deg
  ```
- Run in interactive REPL mode:
  ```bash
  python postifx_evaluator.py --repl
  ```

## Debugging Tips
- Stack traces help: pass `--trace` to inspect how the operand stack evolves after each token.
- Known values: `5 !` should return `120`, while `pi 2 / sin` should equal 1.0.
- Automated tests cover parsing and error conditions:
  ```bash
  pytest test_rpn.py
  ```

## Implementation Notes
- Uses dataclass-backed `EvalConfig` to toggle degree mode and tracing.
- Supports unary functions (`sqrt`, `sin`, `log`, etc.), binary operators, factorial, and constants (`pi`, `e`, `tau`).
- CLI allows reading expressions from stdin, command-line arguments, or an interactive loop, with optional JSON output on errors.

## Further Reading
- [Burks, Goldstine & von Neumann, "Preliminary Discussion of the Logical Design of an Electronic Computing Instrument" (1946)](https://doi.org/10.1002/j.1538-7305.1946.tb00423.x)
- [Knuth, *The Art of Computer Programming*, Vol. 1, Section 1.2.1 (Reverse Polish Notation)](https://www-cs-faculty.stanford.edu/~knuth/taocp.html)
