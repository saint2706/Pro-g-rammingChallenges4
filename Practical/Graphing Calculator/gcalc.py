"""gcalc.py - Interactive Graphing Calculator

Modernized Features:
  * Multiple function plotting (separate by ';' or newlines)
  * Dataclass `PlotSettings` encapsulating range, samples, derivative toggle
  * Safer parsing: restricted SymPy namespace (basic math only) to mitigate arbitrary code risk
  * Clear separation: parse -> build lambdas -> evaluate -> plot
  * Samples control & validation; dynamic axis labeling
  * Derivative optional per global toggle still (applied to each function)
  * Export graph to PNG button
  * Status bar for quick feedback instead of only dialogs
  * Robust error handling with granular messages
  * Inline comments & docstrings for clarity

Usage Notes:
  Enter expressions using SymPy syntax, e.g.:
    sin(x); cos(x)
    exp(-x**2)\nlog(x)
  Accepts standard functions: sin, cos, tan, exp, log, sqrt, abs, asin, acos, atan, sinh, cosh, tanh
"""

from __future__ import annotations

import tkinter as tk
from tkinter import ttk, messagebox, filedialog
from dataclasses import dataclass
from typing import List, Callable, Tuple, Dict

import numpy as np
import sympy as sp
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# ----------------------------- Data Model ----------------------------- #


@dataclass(slots=True)
class PlotSettings:
    x_min: float = -10.0
    x_max: float = 10.0
    samples: int = 1000
    show_derivative: bool = False

    def validate(self) -> None:
        if self.x_min >= self.x_max:
            raise ValueError("x-min must be less than x-max")
        if self.samples < 10 or self.samples > 20000:
            raise ValueError("samples must be between 10 and 20000")


# Allowed symbols for safer sympify environment
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

# ----------------------------- Main Application ----------------------------- #


class GraphingCalculator:
    """Graphing calculator GUI using Tkinter + Matplotlib + SymPy."""

    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Graphing Calculator")
        self.settings = PlotSettings()
        self._create_widgets()
        self._initial_plot_setup()

    # ------------------ UI Construction ------------------ #
    def _create_widgets(self) -> None:
        # Figure / Canvas
        self.fig, self.ax = plt.subplots(figsize=(8, 6))
        self.canvas = FigureCanvasTkAgg(self.fig, master=self.master)
        self.canvas_widget = self.canvas.get_tk_widget()
        self.canvas_widget.pack(side=tk.TOP, fill=tk.BOTH, expand=1)

        # Function input frame
        fn_frame = ttk.Frame(self.master)
        fn_frame.pack(pady=8, padx=10, fill=tk.X)
        ttk.Label(fn_frame, text="f(x) =").pack(side=tk.LEFT, padx=(0, 5))
        self.function_entry = tk.Text(fn_frame, width=50, height=3)
        self.function_entry.pack(side=tk.LEFT, expand=True, fill=tk.X)
        self.function_entry.bind("<Control-Return>", lambda e: self.plot_functions())
        self.function_entry.bind("<Return>", lambda e: self.plot_functions())

        btn_frame = ttk.Frame(fn_frame)
        btn_frame.pack(side=tk.LEFT, padx=6)
        self.plot_button = ttk.Button(
            btn_frame, text="Plot", command=self.plot_functions
        )
        self.plot_button.pack(fill=tk.X)
        self.clear_button = ttk.Button(btn_frame, text="Clear", command=self.clear_plot)
        self.clear_button.pack(pady=4, fill=tk.X)
        self.export_button = ttk.Button(
            btn_frame, text="Export PNG", command=self.export_png
        )
        self.export_button.pack(fill=tk.X)

        # Options frame
        opt = ttk.Frame(self.master)
        opt.pack(pady=4, padx=10, fill=tk.X)
        self.derivative_var = tk.BooleanVar(value=False)
        ttk.Checkbutton(opt, text="Show Derivative", variable=self.derivative_var).pack(
            side=tk.LEFT
        )

        ttk.Label(opt, text="x-min:").pack(side=tk.LEFT, padx=(20, 3))
        self.x_min_entry = ttk.Entry(opt, width=6)
        self.x_min_entry.insert(0, str(self.settings.x_min))
        self.x_min_entry.pack(side=tk.LEFT)

        ttk.Label(opt, text="x-max:").pack(side=tk.LEFT, padx=(10, 3))
        self.x_max_entry = ttk.Entry(opt, width=6)
        self.x_max_entry.insert(0, str(self.settings.x_max))
        self.x_max_entry.pack(side=tk.LEFT)

        ttk.Label(opt, text="Samples:").pack(side=tk.LEFT, padx=(10, 3))
        self.samples_entry = ttk.Entry(opt, width=7)
        self.samples_entry.insert(0, str(self.settings.samples))
        self.samples_entry.pack(side=tk.LEFT)

        # Status bar
        self.status_var = tk.StringVar(value="Enter function(s) and press Plot")
        status = ttk.Label(
            self.master, textvariable=self.status_var, anchor="w", relief=tk.SUNKEN
        )
        status.pack(side=tk.BOTTOM, fill=tk.X)

    def _initial_plot_setup(self) -> None:
        self.ax.set_xlabel("x")
        self.ax.set_ylabel("y")
        self.ax.set_title("Enter function(s) to plot")
        self.ax.grid(True)
        self.ax.axhline(0, color="black", linewidth=0.5)
        self.ax.axvline(0, color="black", linewidth=0.5)
        self.canvas.draw()

    # ------------------ Helpers ------------------ #
    def _update_settings_from_ui(self) -> None:
        try:
            self.settings.x_min = float(self.x_min_entry.get())
            self.settings.x_max = float(self.x_max_entry.get())
            self.settings.samples = int(self.samples_entry.get())
            self.settings.show_derivative = self.derivative_var.get()
            self.settings.validate()
        except ValueError as e:
            raise ValueError(f"Invalid settings: {e}")

    def _parse_functions(self, raw: str) -> List[str]:
        # Split on semicolons or newlines, strip whitespace, ignore empties
        funcs = [f.strip() for chunk in raw.split("\n") for f in chunk.split(";")]
        return [f for f in funcs if f]

    def _sympify(self, expr: str) -> sp.Expr:
        # Use restricted namespace for safety
        try:
            return sp.sympify(expr, locals=SAFE_NAMESPACE)
        except (sp.SympifyError, TypeError) as e:
            raise ValueError(f"Cannot parse expression '{expr}': {e}")

    def _build_lambda(self, expression: sp.Expr) -> Callable[[np.ndarray], np.ndarray]:
        x = ALLOWED_SYMBOLS["x"]
        return sp.lambdify(x, expression, modules="numpy")

    # ------------------ Actions ------------------ #
    def clear_plot(self) -> None:
        self.ax.clear()
        self.function_entry.delete("1.0", tk.END)
        self._initial_plot_setup()
        self.status_var.set("Cleared.")

    def export_png(self) -> None:
        path = filedialog.asksaveasfilename(
            defaultextension=".png", filetypes=[("PNG Image", "*.png")]
        )
        if not path:
            return
        try:
            self.fig.savefig(path, dpi=150)
            self.status_var.set(f"Exported to {path}")
        except Exception as e:  # pragma: no cover (UI specific)
            messagebox.showerror("Export Error", f"Could not export figure: {e}")

    def plot_functions(self) -> None:
        raw = self.function_entry.get("1.0", tk.END).strip()
        if not raw:
            self.status_var.set("No input.")
            return
        try:
            self._update_settings_from_ui()
            fn_strings = self._parse_functions(raw)
            if not fn_strings:
                self.status_var.set("No valid functions found.")
                return
            expressions = [self._sympify(s) for s in fn_strings]
            lambdas = [self._build_lambda(expr) for expr in expressions]

            # Prepare domain
            x_vals = np.linspace(
                self.settings.x_min, self.settings.x_max, self.settings.samples
            )

            # Clear and re-init axes
            self.ax.clear()
            self.ax.grid(True)
            self.ax.axhline(0, color="black", linewidth=0.5)
            self.ax.axvline(0, color="black", linewidth=0.5)

            derivative_flag = self.settings.show_derivative
            x_symbol = ALLOWED_SYMBOLS["x"]
            for s, expr, fn in zip(fn_strings, expressions, lambdas):
                try:
                    y_vals = fn(x_vals)
                    # Filter invalid (complex or nan)
                    if np.iscomplexobj(y_vals):
                        y_vals = np.real(y_vals)
                    y_vals = np.where(np.isfinite(y_vals), y_vals, np.nan)
                    self.ax.plot(x_vals, y_vals, label=f"f(x)={s}")

                    if derivative_flag:
                        deriv_expr = sp.diff(expr, x_symbol)
                        deriv_fn = self._build_lambda(deriv_expr)
                        d_vals = deriv_fn(x_vals)
                        if np.iscomplexobj(d_vals):
                            d_vals = np.real(d_vals)
                        d_vals = np.where(np.isfinite(d_vals), d_vals, np.nan)
                        self.ax.plot(
                            x_vals,
                            d_vals,
                            linestyle="--",
                            label=f"f'(x)={sp.srepr(deriv_expr)[:25]}...",
                        )
                except Exception as inner_e:  # continue plotting others
                    self.ax.text(
                        0.02,
                        0.95,
                        f"Error plotting {s}: {inner_e}",
                        transform=self.ax.transAxes,
                        fontsize=8,
                        color="red",
                    )

            self.ax.set_xlabel("x")
            self.ax.set_ylabel("y")
            self.ax.set_title("Graph")
            self.ax.legend(loc="upper right", fontsize="small")
            self.canvas.draw()
            self.status_var.set(f"Plotted {len(fn_strings)} function(s).")
        except ValueError as e:
            messagebox.showerror("Input Error", str(e))
            self.status_var.set("Input error.")
        except Exception as e:  # pragma: no cover
            messagebox.showerror("Error", f"Unexpected error: {e}")
            self.status_var.set("Error.")


# ----------------------------- Entrypoint ----------------------------- #


def main() -> None:
    root = tk.Tk()
    app = GraphingCalculator(root)
    root.mainloop()


if __name__ == "__main__":  # pragma: no cover
    main()
