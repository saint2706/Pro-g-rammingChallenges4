"""Matrix arithmetic utilities with explanatory output and CLI/GUI front-ends."""

from __future__ import annotations

import argparse
import ast
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, List, Sequence

import numpy as np

EPSILON = 1e-10

if TYPE_CHECKING:  # pragma: no cover - only imported for type checking
    from matplotlib.figure import Figure


class MatrixError(Exception):
    """Base exception for matrix arithmetic errors."""


class SingularMatrixError(MatrixError):
    """Raised when an inverse is requested for a singular matrix."""


@dataclass
class OperationResult:
    """Container for a matrix computation result and any explanatory steps."""

    value: np.ndarray | float
    steps: List[str]

    def format(self, precision: int = 6) -> str:
        if isinstance(self.value, np.ndarray):
            return format_matrix(self.value, precision=precision)
        return f"{self.value:.{precision}f}"


def ensure_2d(matrix: np.ndarray) -> np.ndarray:
    if matrix.ndim != 2:
        raise MatrixError("Matrices must be two-dimensional.")
    return matrix


def parse_literal_or_file(text_or_path: str | Path) -> str:
    """Return text from a literal string or from a path if it exists."""
    if isinstance(text_or_path, Path):
        candidate = text_or_path
    else:
        candidate = Path(text_or_path)
    if candidate.exists():
        return candidate.read_text(encoding="utf-8")
    return str(text_or_path)


def parse_matrix_text(text: str) -> np.ndarray:
    """Parse a matrix from a string literal."""
    try:
        data = ast.literal_eval(text)
    except (ValueError, SyntaxError) as exc:
        raise MatrixError(f"Could not parse matrix data: {exc}") from exc
    array = np.array(data, dtype=float)
    return ensure_2d(array)


def parse_matrix(text_or_path: str | Path) -> np.ndarray:
    """Parse a matrix either from a literal string or a file path."""
    text = parse_literal_or_file(text_or_path)
    return parse_matrix_text(text)


def format_matrix(matrix: np.ndarray, precision: int = 6) -> str:
    rows = [
        "[ " + ", ".join(f"{value:.{precision}f}" for value in row) + " ]"
        for row in matrix
    ]
    return "[\n  " + ",\n  ".join(rows) + "\n]"


def add_matrices(
    a: np.ndarray, b: np.ndarray, explain: bool = False
) -> OperationResult:
    a = ensure_2d(np.array(a, dtype=float))
    b = ensure_2d(np.array(b, dtype=float))
    if a.shape != b.shape:
        raise MatrixError("Matrices must share the same shape for addition.")
    result = a + b
    steps: List[str] = []
    if explain:
        for i in range(a.shape[0]):
            for j in range(a.shape[1]):
                steps.append(
                    f"Entry ({i + 1},{j + 1}): {a[i, j]:.6g} + {b[i, j]:.6g} = {result[i, j]:.6g}"
                )
    return OperationResult(result, steps)


def multiply_matrices(
    a: np.ndarray, b: np.ndarray, explain: bool = False
) -> OperationResult:
    a = ensure_2d(np.array(a, dtype=float))
    b = ensure_2d(np.array(b, dtype=float))
    if a.shape[1] != b.shape[0]:
        raise MatrixError("Inner dimensions must agree for multiplication.")
    result = a @ b
    steps: List[str] = []
    if explain:
        for i in range(a.shape[0]):
            for j in range(b.shape[1]):
                parts = []
                for k in range(a.shape[1]):
                    parts.append(f"({a[i, k]:.6g}×{b[k, j]:.6g})")
                steps.append(
                    f"Entry ({i + 1},{j + 1}): "
                    + " + ".join(parts)
                    + f" = {result[i, j]:.6g}"
                )
    return OperationResult(result, steps)


def _find_pivot(matrix: np.ndarray, start_row: int, col: int) -> int:
    pivot_row = max(
        range(start_row, matrix.shape[0]), key=lambda r: abs(matrix[r, col])
    )
    if abs(matrix[pivot_row, col]) < EPSILON:
        return -1
    return pivot_row


def determinant(matrix: np.ndarray, explain: bool = False) -> OperationResult:
    matrix = ensure_2d(np.array(matrix, dtype=float))
    if matrix.shape[0] != matrix.shape[1]:
        raise MatrixError("Determinant is defined only for square matrices.")
    n = matrix.shape[0]
    working = matrix.copy()
    det = 1.0
    sign = 1
    steps: List[str] = []
    for col in range(n):
        pivot_row = _find_pivot(working, col, col)
        if pivot_row == -1:
            if explain:
                steps.append(f"Column {col + 1}: pivot is zero; determinant is 0.")
            return OperationResult(0.0, steps)
        if pivot_row != col:
            working[[col, pivot_row]] = working[[pivot_row, col]]
            sign *= -1
            if explain:
                steps.append(
                    f"Swap row {col + 1} with row {pivot_row + 1} (sign flip)."
                )
        pivot = working[col, col]
        det *= pivot
        if explain:
            steps.append(
                f"Pivot at ({col + 1},{col + 1}) = {pivot:.6g}; partial det = {det * sign:.6g}"
            )
        for row in range(col + 1, n):
            factor = working[row, col] / pivot
            if abs(factor) < EPSILON:
                continue
            working[row, col:] -= factor * working[col, col:]
            if explain:
                steps.append(f"R{row + 1} ← R{row + 1} - ({factor:.6g})·R{col + 1}")
    det *= sign
    if explain:
        steps.append(f"Final determinant = {det:.6g}")
    return OperationResult(det, steps)


def inverse(matrix: np.ndarray, explain: bool = False) -> OperationResult:
    matrix = ensure_2d(np.array(matrix, dtype=float))
    if matrix.shape[0] != matrix.shape[1]:
        raise MatrixError("Inverse is defined only for square matrices.")
    n = matrix.shape[0]
    augmented = np.hstack([matrix, np.eye(n)])
    steps: List[str] = []
    for col in range(n):
        pivot_row = _find_pivot(augmented, col, col)
        if pivot_row == -1:
            if explain:
                steps.append(f"Column {col + 1}: pivot is zero; matrix is singular.")
            raise SingularMatrixError("Matrix is singular; inverse does not exist.")
        if pivot_row != col:
            augmented[[col, pivot_row]] = augmented[[pivot_row, col]]
            if explain:
                steps.append(f"Swap row {col + 1} with row {pivot_row + 1}.")
        pivot = augmented[col, col]
        if abs(pivot) < EPSILON:
            if explain:
                steps.append(
                    f"Pivot at ({col + 1},{col + 1}) is zero after swap; singular."
                )
            raise SingularMatrixError("Matrix is singular; inverse does not exist.")
        augmented[col] = augmented[col] / pivot
        if explain:
            steps.append(f"Scale row {col + 1} by 1/{pivot:.6g} to make pivot 1.")
        for row in range(n):
            if row == col:
                continue
            factor = augmented[row, col]
            if abs(factor) < EPSILON:
                continue
            augmented[row] -= factor * augmented[col]
            if explain:
                steps.append(f"R{row + 1} ← R{row + 1} - ({factor:.6g})·R{col + 1}")
    inverse_matrix = augmented[:, n:]
    if explain:
        steps.append("Right block is now the inverse matrix.")
    return OperationResult(inverse_matrix, steps)


def visualize_transformation(
    matrix: np.ndarray,
    output: str | None = None,
    *,
    return_figure: bool = False,
) -> str | "Figure":
    matrix = ensure_2d(np.array(matrix, dtype=float))
    if matrix.shape != (2, 2):
        raise MatrixError("Visualisation currently supports only 2×2 matrices.")
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    square = np.array(
        [
            [0, 0],
            [1, 0],
            [1, 1],
            [0, 1],
            [0, 0],
        ]
    )
    transformed = (matrix @ square.T).T

    fig, ax = plt.subplots()
    ax.plot(square[:, 0], square[:, 1], label="Unit square", linestyle="--")
    ax.plot(transformed[:, 0], transformed[:, 1], label="Transformed", linewidth=2)
    ax.set_aspect("equal")
    ax.set_title("Linear transformation of unit square")
    ax.legend()
    ax.grid(True, linestyle=":", alpha=0.6)

    output_path: str | None
    if output is not None:
        output_path = output
    elif not return_figure:
        output_path = "matrix_transformation.png"
    else:
        output_path = None

    if output_path is not None:
        fig.savefig(output_path, bbox_inches="tight", dpi=200)

    if return_figure:
        return fig

    plt.close(fig)
    return output_path or ""


def compute_operation(
    operation: str,
    matrix_a: np.ndarray,
    matrix_b: np.ndarray | None = None,
    *,
    explain: bool = False,
) -> OperationResult:
    """Dispatch helper shared by the CLI and Streamlit front-ends."""

    operation = operation.lower()
    if operation == "add":
        if matrix_b is None:
            raise MatrixError("Addition requires a second matrix.")
        return add_matrices(matrix_a, matrix_b, explain=explain)
    if operation == "multiply":
        if matrix_b is None:
            raise MatrixError("Multiplication requires a second matrix.")
        return multiply_matrices(matrix_a, matrix_b, explain=explain)
    if operation == "determinant":
        return determinant(matrix_a, explain=explain)
    if operation == "inverse":
        return inverse(matrix_a, explain=explain)
    raise MatrixError(f"Unsupported operation '{operation}'.")


def _print_steps(steps: Sequence[str]) -> None:
    if steps:
        print("\nSteps:")
        for idx, step in enumerate(steps, start=1):
            print(f"  {idx}. {step}")


def run_cli(argv: Sequence[str] | None = None) -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--precision",
        type=int,
        default=4,
        help="Decimal precision for output formatting.",
    )
    parser.add_argument(
        "--gui",
        action="store_true",
        help="Launch the optional Tk GUI instead of the CLI workflow.",
    )
    subparsers = parser.add_subparsers(dest="command")

    add_parser = subparsers.add_parser("add", help="Add two matrices.")
    add_parser.add_argument(
        "--matrix-a", required=True, help="Left matrix literal or path."
    )
    add_parser.add_argument(
        "--matrix-b", required=True, help="Right matrix literal or path."
    )
    add_parser.add_argument(
        "--explain", action="store_true", help="Print step-by-step explanations."
    )

    mul_parser = subparsers.add_parser("multiply", help="Multiply two matrices.")
    mul_parser.add_argument(
        "--matrix-a", required=True, help="Left matrix literal or path."
    )
    mul_parser.add_argument(
        "--matrix-b", required=True, help="Right matrix literal or path."
    )
    mul_parser.add_argument(
        "--explain", action="store_true", help="Print step-by-step explanations."
    )

    det_parser = subparsers.add_parser(
        "determinant", help="Compute the determinant of a matrix."
    )
    det_parser.add_argument(
        "--matrix", required=True, help="Matrix literal or path to file containing one."
    )
    det_parser.add_argument(
        "--explain", action="store_true", help="Print step-by-step explanations."
    )

    inv_parser = subparsers.add_parser(
        "inverse", help="Compute the inverse of a matrix."
    )
    inv_parser.add_argument(
        "--matrix", required=True, help="Matrix literal or path to file containing one."
    )
    inv_parser.add_argument(
        "--explain", action="store_true", help="Print step-by-step explanations."
    )

    viz_parser = subparsers.add_parser(
        "visualize", help="Visualise a 2x2 matrix acting on the unit square."
    )
    viz_parser.add_argument("--matrix", required=True, help="Matrix literal or path.")
    viz_parser.add_argument(
        "--output", help="Image file path (defaults to matrix_transformation.png)."
    )

    args = parser.parse_args(argv)

    if args.gui:
        launch_gui()
        return

    if not args.command:
        parser.print_help()
        return

    precision: int = args.precision

    try:
        if args.command in {"add", "multiply"}:
            matrix_a = parse_matrix(args.matrix_a)
            matrix_b = parse_matrix(args.matrix_b)
            result = compute_operation(
                args.command,
                matrix_a,
                matrix_b,
                explain=args.explain,
            )
        elif args.command in {"determinant", "inverse"}:
            matrix = parse_matrix(args.matrix)
            result = compute_operation(
                args.command,
                matrix,
                explain=args.explain,
            )
        elif args.command == "visualize":
            matrix = parse_matrix(args.matrix)
            path = visualize_transformation(matrix, output=args.output)
            print(f"Saved visualisation to {path}")
            return
        else:
            parser.error(f"Unknown command {args.command}")
            return
    except MatrixError as exc:
        parser.exit(status=1, message=f"Error: {exc}\n")

    print(result.format(precision=precision))
    _print_steps(result.steps)


def launch_gui() -> None:
    import tkinter as tk
    from tkinter import messagebox, ttk

    root = tk.Tk()
    root.title("Matrix Arithmetic Toolkit")

    operation_var = tk.StringVar(value="add")

    def _make_matrix_entry(parent: tk.Widget, title: str) -> tk.Text:
        frame = ttk.LabelFrame(parent, text=title)
        frame.pack(fill="both", expand=True, padx=5, pady=5)
        text_widget = tk.Text(frame, width=40, height=5)
        text_widget.pack(fill="both", expand=True)
        return text_widget

    matrices_frame = ttk.Frame(root)
    matrices_frame.pack(fill="both", expand=True)

    matrix_a_entry = _make_matrix_entry(matrices_frame, "Matrix A (or single matrix)")
    matrix_b_entry = _make_matrix_entry(matrices_frame, "Matrix B (for add/multiply)")

    ttk.Label(root, text="Operation:").pack(anchor="w", padx=5)
    operation_menu = ttk.OptionMenu(
        root, operation_var, "add", "add", "multiply", "determinant", "inverse"
    )
    operation_menu.pack(fill="x", padx=5, pady=5)

    explain_var = tk.BooleanVar(value=True)
    ttk.Checkbutton(
        root, text="Show step-by-step explanation", variable=explain_var
    ).pack(anchor="w", padx=5)

    output_frame = ttk.LabelFrame(root, text="Result")
    output_frame.pack(fill="both", expand=True, padx=5, pady=5)
    result_text = tk.Text(output_frame, width=60, height=12)
    result_text.pack(fill="both", expand=True)

    def compute() -> None:
        result_text.delete("1.0", tk.END)
        try:
            matrix_a = parse_matrix(matrix_a_entry.get("1.0", tk.END).strip())
        except MatrixError as exc:
            messagebox.showerror("Matrix Error", f"Matrix A: {exc}")
            return
        matrix_b = None
        if operation_var.get() in {"add", "multiply"}:
            try:
                matrix_b = parse_matrix(matrix_b_entry.get("1.0", tk.END).strip())
            except MatrixError as exc:
                messagebox.showerror("Matrix Error", f"Matrix B: {exc}")
                return
        try:
            if operation_var.get() == "add" and matrix_b is not None:
                result = add_matrices(matrix_a, matrix_b, explain=explain_var.get())
            elif operation_var.get() == "multiply" and matrix_b is not None:
                result = multiply_matrices(
                    matrix_a, matrix_b, explain=explain_var.get()
                )
            elif operation_var.get() == "determinant":
                result = determinant(matrix_a, explain=explain_var.get())
            elif operation_var.get() == "inverse":
                result = inverse(matrix_a, explain=explain_var.get())
            else:
                raise MatrixError("Unsupported operation selection.")
        except MatrixError as exc:
            messagebox.showerror("Matrix Error", str(exc))
            return
        result_text.insert(tk.END, result.format())
        if result.steps:
            result_text.insert(tk.END, "\n\nSteps:\n")
            for idx, step in enumerate(result.steps, start=1):
                result_text.insert(tk.END, f"{idx}. {step}\n")

    ttk.Button(root, text="Compute", command=compute).pack(pady=5)
    ttk.Button(root, text="Quit", command=root.destroy).pack(pady=2)

    root.mainloop()


if __name__ == "__main__":
    run_cli()
