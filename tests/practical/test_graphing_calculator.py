"""Regression tests for the Graphing Calculator Tkinter UI."""

from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

matplotlib = pytest.importorskip(
    "matplotlib", reason="Matplotlib required for Graphing Calculator UI"
)
matplotlib.use("Agg", force=True)


try:  # pragma: no cover - module loading guard
    import tkinter as tk
except ModuleNotFoundError:  # pragma: no cover - platform dependent
    tk = None  # type: ignore


def _load_graphing_calculator():
    """Load the graphing calculator module despite the space in its path."""

    module_path = (
        Path(__file__).resolve().parents[2]
        / "challenges/Practical/Graphing Calculator/gcalc.py"
    )
    spec = importlib.util.spec_from_file_location("graphing_calculator", module_path)
    if spec is None or spec.loader is None:  # pragma: no cover - safety check
        raise RuntimeError("Unable to load graphing calculator module")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.mark.skipif(tk is None, reason="Tkinter not available")
def test_text_widget_allows_multiline_before_plot():
    """Ensure <Return> inserts a newline and only Ctrl+Return triggers plotting."""

    module = _load_graphing_calculator()
    GraphingCalculator = module.GraphingCalculator

    try:
        root = tk.Tk()
    except tk.TclError as exc:  # pragma: no cover - environment guard
        pytest.skip(f"Tk not available: {exc}")

    root.withdraw()
    calculator = GraphingCalculator(root)

    calls: list[str] = []

    def fake_plot() -> None:
        calls.append("plot")

    # Swap out the heavy plotting logic with a stub for the test.
    calculator.plot_functions = fake_plot  # type: ignore[assignment]

    text = calculator.function_entry
    text.delete("1.0", tk.END)
    text.insert("1.0", "sin(x)")
    root.update()

    # Plain Return should only insert a newline, not trigger plotting.
    text.event_generate("<Return>")
    root.update()
    text.insert(tk.END, "cos(x)")
    root.update()

    assert calls == []
    assert text.get("1.0", "end-1c") == "sin(x)\ncos(x)"

    # Ctrl+Return remains bound to the plotting command.
    text.event_generate("<Control-Return>")
    root.update()
    assert calls == ["plot"]

    root.destroy()
