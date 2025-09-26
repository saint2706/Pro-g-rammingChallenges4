"""Core helpers for evaluating graphing calculator expressions."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Dict, Iterable, List, Sequence

import numpy as np
import sympy as sp


@dataclass(slots=True)
class PlotSettings:
    """Configuration used when sampling functions for plotting."""

    x_min: float = -10.0
    x_max: float = 10.0
    samples: int = 1000
    show_derivative: bool = False

    def validate(self) -> None:
        if self.x_min >= self.x_max:
            raise ValueError("x-min must be less than x-max")
        if self.samples < 10 or self.samples > 20000:
            raise ValueError("samples must be between 10 and 20000")


ALLOWED_FUNCS: Dict[str, sp.Function] = {
    name: getattr(sp, name)
    for name in [
        "sin",
        "cos",
        "tan",
        "exp",
        "log",
        "sqrt",
        "abs",
        "asin",
        "acos",
        "atan",
        "sinh",
        "cosh",
        "tanh",
    ]
    if hasattr(sp, name)
}
ALLOWED_SYMBOLS = {"x": sp.Symbol("x")}
SAFE_NAMESPACE = {**ALLOWED_FUNCS, **ALLOWED_SYMBOLS, "E": sp.E, "pi": sp.pi}


@dataclass(slots=True)
class PlotLine:
    label: str
    values: np.ndarray
    linestyle: str = "-"


@dataclass(slots=True)
class PlotComputation:
    x: np.ndarray
    lines: List[PlotLine]
    derivative_lines: List[PlotLine]
    errors: List[str]


def parse_functions(raw: str) -> List[str]:
    """Split raw user input into individual function strings."""

    funcs = [f.strip() for chunk in raw.split("\n") for f in chunk.split(";")]
    return [f for f in funcs if f]


def sympify_expressions(functions: Sequence[str]) -> List[sp.Expr]:
    """Convert strings to SymPy expressions using a restricted namespace."""

    expressions: List[sp.Expr] = []
    for expr in functions:
        try:
            expressions.append(sp.sympify(expr, locals=SAFE_NAMESPACE))
        except (sp.SympifyError, TypeError) as exc:  # pragma: no cover - depends on input
            raise ValueError(f"Cannot parse expression '{expr}': {exc}") from exc
    return expressions


def build_lambda(expression: sp.Expr) -> Callable[[np.ndarray], np.ndarray]:
    """Generate a NumPy-ready callable for the provided SymPy expression."""

    x = ALLOWED_SYMBOLS["x"]
    return sp.lambdify(x, expression, modules="numpy")


def build_lambdas(expressions: Iterable[sp.Expr]) -> List[Callable[[np.ndarray], np.ndarray]]:
    return [build_lambda(expr) for expr in expressions]


def evaluate_functions(functions: Sequence[str], settings: PlotSettings) -> PlotComputation:
    """Evaluate the provided function strings over the configured range."""

    if not functions:
        raise ValueError("No functions provided")

    settings.validate()
    expressions = sympify_expressions(functions)
    x_vals = np.linspace(settings.x_min, settings.x_max, settings.samples)

    lines: List[PlotLine] = []
    derivative_lines: List[PlotLine] = []
    errors: List[str] = []

    for fn_str, expr in zip(functions, expressions):
        try:
            y_vals = _evaluate_expression(expr, x_vals)
            lines.append(PlotLine(label=f"f(x)={fn_str}", values=y_vals))

            if settings.show_derivative:
                derivative_expr = sp.diff(expr, ALLOWED_SYMBOLS["x"])
                derivative_lines.append(
                    PlotLine(
                        label=_format_derivative_label(derivative_expr),
                        values=_evaluate_expression(derivative_expr, x_vals),
                        linestyle="--",
                    )
                )
        except Exception as exc:  # pragma: no cover - depends on input
            errors.append(f"Error plotting {fn_str}: {exc}")

    return PlotComputation(x=x_vals, lines=lines, derivative_lines=derivative_lines, errors=errors)


def _evaluate_expression(expression: sp.Expr, x_vals: np.ndarray) -> np.ndarray:
    fn = build_lambda(expression)
    values = fn(x_vals)
    if np.iscomplexobj(values):
        values = np.real(values)
    values = np.where(np.isfinite(values), values, np.nan)
    return values


def _format_derivative_label(derivative: sp.Expr) -> str:
    repr_text = sp.srepr(derivative)
    if len(repr_text) > 30:
        repr_text = f"{repr_text[:27]}..."
    return f"f'(x)={repr_text}"
