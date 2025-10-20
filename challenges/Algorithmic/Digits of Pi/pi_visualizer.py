#!/usr/bin/env python3
"""Interactive convergence visualizer for the Pi calculators."""

import argparse
import decimal
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional

try:  # Plotly is optional in some test environments
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots

    PLOTLY_AVAILABLE = True
except ModuleNotFoundError:  # pragma: no cover - exercised in CI without Plotly
    go = None  # type: ignore[assignment]
    make_subplots = None  # type: ignore[assignment]
    PLOTLY_AVAILABLE = False

SCRIPT_DIR = Path(__file__).resolve().parent
PARENT_DIR = SCRIPT_DIR.parent

def _ensure_sys_path(path: Path) -> None:
    if str(path) not in sys.path:
        sys.path.insert(0, str(path))


_ensure_sys_path(SCRIPT_DIR)

import DigitPi  # type: ignore  # noqa: E402

_GAUSS_PATH = PARENT_DIR / "1000 Digits of Pi" / "pi.py"

if str(_GAUSS_PATH.parent) not in sys.path:
    sys.path.insert(0, str(_GAUSS_PATH.parent))

import importlib.util

spec = importlib.util.spec_from_file_location("pi_gauss_legendre", _GAUSS_PATH)
if spec is None or spec.loader is None:
    raise ImportError("Unable to load Gauss-Legendre module")
_pi_gauss_module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(_pi_gauss_module)

REFERENCE_PI = decimal.Decimal(
    "3.14159265358979323846264338327950288419716939937510"
)


@dataclass
class ConvergenceDatum:
    algorithm: str
    iteration: int
    approximation: decimal.Decimal
    error: decimal.Decimal
    digits: int
    elapsed: float


def _compute_error(approximation: decimal.Decimal) -> decimal.Decimal:
    with decimal.localcontext() as ctx:
        ctx.prec = max(len(approximation.as_tuple().digits), 60)
        return abs(approximation - REFERENCE_PI)


def _digits_from_error(error: decimal.Decimal) -> int:
    if error.is_zero():
        return len(REFERENCE_PI.as_tuple().digits) - 1

    with decimal.localcontext() as ctx:
        ctx.prec = 60
        log_value = error.log10()
        neg_log = -log_value
        digits_decimal = neg_log.to_integral_value(rounding=decimal.ROUND_FLOOR)
    return max(0, int(digits_decimal))


def get_convergence_data(
    algorithm: str,
    *,
    iterations: Optional[int] = None,
    digits: Optional[int] = None,
) -> List[ConvergenceDatum]:
    algorithm = algorithm.lower()
    data: List[ConvergenceDatum] = []

    if algorithm == "chudnovsky":
        iteration_target = iterations or 7
        for step in DigitPi.generate_chudnovsky_convergence(iteration_target):
            error = _compute_error(step.approximation)
            data.append(
                ConvergenceDatum(
                    algorithm="chudnovsky",
                    iteration=step.iteration,
                    approximation=step.approximation,
                    error=error,
                    digits=_digits_from_error(error),
                    elapsed=step.elapsed,
                )
            )
    elif algorithm == "gauss-legendre":
        digit_target = digits or 50
        generator = _pi_gauss_module.generate_gauss_legendre_convergence
        for step in generator(digit_target):
            error = _compute_error(step.approximation)
            data.append(
                ConvergenceDatum(
                    algorithm="gauss-legendre",
                    iteration=step.iteration,
                    approximation=step.approximation,
                    error=error,
                    digits=_digits_from_error(error),
                    elapsed=step.elapsed,
                )
            )
    else:
        raise ValueError(f"Unsupported algorithm: {algorithm}")

    return data


def _decimal_to_float(value: decimal.Decimal) -> float | None:
    if value.is_zero():
        return None
    try:
        result = float(value)
    except (OverflowError, ValueError):
        return None
    return result if result > 0 else None


def build_figure(data: Iterable[ConvergenceDatum], *, title: str):
    if not PLOTLY_AVAILABLE:
        raise RuntimeError("Plotly is required to build the visualization figure")

    records = list(data)
    iterations = [record.iteration for record in records]
    digits = [record.digits for record in records]
    errors = [record.error for record in records]
    elapsed = [record.elapsed for record in records]

    figure = make_subplots(
        rows=2,
        cols=1,
        shared_xaxes=True,
        specs=[[{"secondary_y": True}], [{}]],
        row_heights=[0.65, 0.35],
    )

    figure.add_trace(
        go.Scatter(
            x=iterations,
            y=digits,
            mode="lines+markers",
            name="Correct digits",
        ),
        row=1,
        col=1,
        secondary_y=False,
    )

    figure.add_trace(
        go.Scatter(
            x=iterations,
            y=[_decimal_to_float(err) for err in errors],
            mode="lines+markers",
            name="Absolute error",
        ),
        row=1,
        col=1,
        secondary_y=True,
    )

    figure.add_trace(
        go.Scatter(
            x=iterations,
            y=elapsed,
            mode="lines+markers",
            name="Elapsed time (s)",
        ),
        row=2,
        col=1,
    )

    figure.update_yaxes(title_text="Correct digits", row=1, col=1, secondary_y=False)
    figure.update_yaxes(
        title_text="Absolute error",
        row=1,
        col=1,
        secondary_y=True,
        type="log",
    )
    figure.update_yaxes(title_text="Elapsed time (s)", row=2, col=1)
    figure.update_xaxes(title_text="Iteration", row=2, col=1)

    frames = []
    for idx, _ in enumerate(records, start=1):
        frames.append(
            go.Frame(
                name=str(idx - 1),
                data=[
                    go.Scatter(
                        x=iterations[:idx],
                        y=digits[:idx],
                        mode="lines+markers",
                    ),
                    go.Scatter(
                        x=iterations[:idx],
                        y=[_decimal_to_float(err) for err in errors[:idx]],
                        mode="lines+markers",
                    ),
                    go.Scatter(
                        x=iterations[:idx],
                        y=elapsed[:idx],
                        mode="lines+markers",
                    ),
                ],
            )
        )

    figure.frames = frames
    if frames:
        figure.update_layout(
            updatemenus=[
                {
                    "type": "buttons",
                    "showactive": False,
                    "buttons": [
                        {
                            "label": "Play",
                            "method": "animate",
                            "args": [None, {"frame": {"duration": 400, "redraw": False}}],
                        },
                        {
                            "label": "Pause",
                            "method": "animate",
                            "args": [[None], {"frame": {"duration": 0}, "mode": "immediate"}],
                        },
                    ],
                }
            ],
            sliders=[
                {
                    "steps": [
                        {
                            "args": [[frame.name], {"mode": "immediate", "frame": {"duration": 0}}],
                            "label": frame.name,
                            "method": "animate",
                        }
                        for frame in frames
                    ],
                    "currentvalue": {"prefix": "Iteration: "},
                }
            ],
        )

    figure.update_layout(title=title)
    return figure


def write_json_summary(
    path: Path,
    *,
    algorithm: str,
    data: Iterable[ConvergenceDatum],
    final_value: str,
) -> None:
    payload = {
        "algorithm": algorithm,
        "final_value": final_value,
        "steps": [
            {
                "iteration": record.iteration,
                "approximation": str(record.approximation),
                "error": str(record.error),
                "digits": record.digits,
                "elapsed": record.elapsed,
            }
            for record in data
        ],
    }
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


def parse_arguments(argv: Optional[Iterable[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Visualize convergence of Pi calculation algorithms.",
    )
    parser.add_argument(
        "--algorithm",
        choices=["chudnovsky", "gauss-legendre"],
        default="chudnovsky",
        help="Algorithm to visualize",
    )
    parser.add_argument(
        "--iterations",
        type=int,
        default=7,
        help="Number of iterations for the Chudnovsky algorithm",
    )
    parser.add_argument(
        "--digits",
        type=int,
        default=50,
        help="Target digits for Gauss-Legendre",
    )
    parser.add_argument(
        "--headless",
        action="store_true",
        help="Run without opening the interactive viewer",
    )
    parser.add_argument(
        "--html-output",
        type=Path,
        help="Optional HTML file to export the visualization",
    )
    parser.add_argument(
        "--json-summary",
        type=Path,
        help="Optional JSON file to store iteration summaries",
    )
    return parser.parse_args(list(argv) if argv is not None else None)


def main(argv: Optional[Iterable[str]] = None) -> int:
    args = parse_arguments(argv)

    if args.algorithm == "chudnovsky":
        convergence = get_convergence_data(
            args.algorithm, iterations=args.iterations
        )
        final_value = str(DigitPi.compute_pi(args.iterations))
        title = f"Chudnovsky convergence ({args.iterations} iterations)"
    else:
        convergence = get_convergence_data(
            args.algorithm, digits=args.digits
        )
        final_value = _pi_gauss_module.calculate_pi_gauss_legendre(args.digits)
        title = f"Gauss-Legendre convergence ({args.digits} digits)"

    figure = build_figure(convergence, title=title) if PLOTLY_AVAILABLE else None

    if args.json_summary:
        write_json_summary(
            args.json_summary,
            algorithm=args.algorithm,
            data=convergence,
            final_value=final_value,
        )

    if args.html_output:
        if figure is None:
            raise SystemExit("Plotly is required for HTML export")
        figure.write_html(str(args.html_output), include_plotlyjs="cdn")

    if not args.headless and figure is not None:
        figure.show()

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
