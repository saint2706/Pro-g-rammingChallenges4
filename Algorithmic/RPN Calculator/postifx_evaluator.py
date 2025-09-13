"""postifx_evaluator.py - Enhanced Reverse Polish Notation (RPN) calculator.

Improvements over the original version:
* argparse CLI with multiple input modes and JSON output
* Additional operators: %, unary negation ("neg"), factorial ("!"), power ("^")
* Mathematical constants: pi, tau, e
* Common math functions as tokens: sin, cos, tan, sqrt, log, ln, exp
* Floor division (//) and modulo (%)
* Optional evaluation trace (--trace) to show stack evolution
* Interactive REPL mode (--repl) for repeated calculations

NOTE: Factorial applies to the top of stack as unary operator (integer only).

Examples
--------
  python postifx_evaluator.py --expr "3 4 + 2 *"          -> (3+4)*2 = 14
  python postifx_evaluator.py --expr "90 sin" --deg        -> sin(90°) = 1.0
  python postifx_evaluator.py --expr "5 !"                 -> 120
  echo "2 3 ^ 4 *" | python postifx_evaluator.py --stdin
  python postifx_evaluator.py --repl
  python postifx_evaluator.py --expr "3 0 /" --json        -> error JSON
"""

from __future__ import annotations

import argparse
import json
import math
import operator
import sys
from dataclasses import dataclass
from typing import Callable, Dict, Iterable, List, Optional


Number = float


@dataclass(slots=True)
class EvalConfig:
    degrees: bool = False  # interpret trig inputs as degrees when True
    trace: bool = False


class RPNError(ValueError):
    """Custom exception for RPN evaluation errors."""


def _format_stack(stack: List[Number]) -> str:
    return "[" + ", ".join(f"{x:g}" for x in stack) + "]"


def evaluate_rpn(expression: str, *, config: Optional[EvalConfig] = None) -> Number:
    """Evaluate RPN expression and return numeric result.

    expression: tokens separated by whitespace.
    config: optional EvalConfig controlling interpretation.
    Raises RPNError on malformed expressions.
    """
    if config is None:
        config = EvalConfig()
    tokens = expression.split()
    if not tokens:
        raise RPNError("Empty expression")
    stack: List[Number] = []

    # Binary operators
    bin_ops: Dict[str, Callable[[Number, Number], Number]] = {
        "+": operator.add,
        "-": operator.sub,
        "*": operator.mul,
        "x": operator.mul,
        "/": operator.truediv,
        "//": operator.floordiv,
        "^": operator.pow,
        "%": operator.mod,
    }
    # Unary functions mapping (token -> function)
    unary_funcs: Dict[str, Callable[[Number], Number]] = {
        "neg": operator.neg,
        "sqrt": math.sqrt,
        "exp": math.exp,
        "ln": math.log,
        "log": math.log10,
        "sin": math.sin,
        "cos": math.cos,
        "tan": math.tan,
    }
    constants: Dict[str, Number] = {
        "pi": math.pi,
        "tau": math.tau,
        "e": math.e,
    }

    def apply_trig(x: Number, func: Callable[[Number], Number]) -> Number:
        return func(math.radians(x)) if config.degrees else func(x)

    trace_enabled = config.trace

    for token in tokens:
        try:
            val = float(token)
        except ValueError:
            # constant
            if token in constants:
                stack.append(constants[token])
            # factorial
            elif token == "!":
                if not stack:
                    raise RPNError("Factorial requires one operand")
                n = stack.pop()
                if n < 0 or int(n) != n:
                    raise RPNError("Factorial operand must be a non-negative integer")
                stack.append(math.factorial(int(n)))
            # unary function
            elif token in unary_funcs:
                if not stack:
                    raise RPNError(f"Unary operator '{token}' requires one operand")
                x = stack.pop()
                func = unary_funcs[token]
                if token in {"sin", "cos", "tan"}:
                    x = apply_trig(x, lambda z: z)  # convert degrees if needed
                    # after conversion apply original function
                    stack.append(func(x))
                else:
                    stack.append(func(x))
            # binary op
            elif token in bin_ops:
                if len(stack) < 2:
                    raise RPNError(f"Operator '{token}' requires two operands")
                b = stack.pop()
                a = stack.pop()
                if token in {"/", "//"} and b == 0:
                    raise RPNError("Division by zero")
                try:
                    stack.append(bin_ops[token](a, b))
                except OverflowError:
                    raise RPNError("Overflow in operation")
            else:
                raise RPNError(f"Unknown token '{token}'")
        else:
            stack.append(val)
        if trace_enabled:
            print(f"token={token:>6} stack={_format_stack(stack)}")

    if len(stack) != 1:
        raise RPNError("Malformed expression (items remaining on stack)")
    return stack[0]


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="RPN (Reverse Polish Notation) calculator")
    p.add_argument("--expr", help="RPN expression string")
    p.add_argument("--file", help="Read expression from file (single line)")
    p.add_argument("--stdin", action="store_true", help="Read expression from STDIN")
    p.add_argument("--repl", action="store_true", help="Interactive REPL mode")
    p.add_argument("--json", action="store_true", help="Emit JSON output")
    p.add_argument(
        "--deg", action="store_true", help="Interpret trig inputs as degrees"
    )
    p.add_argument("--trace", action="store_true", help="Print stack after each token")
    return p


def _resolve_expression(args) -> str:
    sources = [bool(args.expr), bool(args.file), args.stdin]
    if sum(sources) > 1:
        raise RPNError("Specify only one of --expr/--file/--stdin (or use --repl)")
    if sum(sources) == 0:
        raise RPNError("No expression source provided (use --expr or --repl)")
    if args.expr:
        return args.expr
    if args.file:
        try:
            with open(args.file, "r", encoding="utf-8") as f:
                return f.read().strip()
        except OSError as e:
            raise RPNError(f"Failed to read file: {e}")
    if args.stdin:
        return sys.stdin.read().strip()
    raise RPNError("Unhandled input source")  # defensive


def repl_loop(cfg: EvalConfig) -> None:
    print("RPN REPL. Type 'quit' or Ctrl-D to exit.")
    while True:
        try:
            line = input("> ").strip()
        except (EOFError, KeyboardInterrupt):
            print()
            return
        if not line:
            continue
        if line.lower() in {"quit", "exit"}:
            return
        try:
            result = evaluate_rpn(line, config=cfg)
            print(result)
        except RPNError as e:
            print(f"Error: {e}")


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = EvalConfig(degrees=args.deg, trace=args.trace)

    if args.repl:
        repl_loop(cfg)
        return 0

    try:
        expr = _resolve_expression(args)
        result = evaluate_rpn(expr, config=cfg)
    except RPNError as e:
        if args.json:
            print(json.dumps({"error": str(e)}, indent=2))
        else:
            print(f"Error: {e}", file=sys.stderr)
        return 1

    if args.json:
        payload = {
            "expression": expr,
            "result": result,
        }
        print(json.dumps(payload, indent=2))
    else:
        print(result)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
