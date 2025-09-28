"""Streamlit front-end for the Graphing Calculator."""

from __future__ import annotations

import io

import matplotlib.pyplot as plt
import streamlit as st

from plot_core import PlotSettings, evaluate_functions, parse_functions


def render() -> None:
    """Render the Streamlit interface for plotting functions."""

    st.title("Graphing Calculator")
    st.write(
        "Enter one or more functions of `x` (separate with new lines or semicolons)."
    )

    with st.form("graph-form"):
        functions_input = st.text_area(
            "Functions",
            value="sin(x)\ncos(x)",
            height=140,
            help="Example: sin(x); cos(x)",
        )
        col1, col2, col3 = st.columns(3)
        with col1:
            x_min = st.number_input("x-min", value=-10.0)
        with col2:
            x_max = st.number_input("x-max", value=10.0)
        with col3:
            samples = st.number_input(
                "Samples", min_value=10, max_value=20000, value=1000
            )

        show_derivative = st.checkbox("Show derivative", value=False)
        submitted = st.form_submit_button("Plot")

    if not submitted:
        st.info("Adjust settings and press Plot to generate a graph.")
        return

    functions = parse_functions(functions_input)
    if not functions:
        st.warning("Please enter at least one function.")
        return

    settings = PlotSettings(
        x_min=float(x_min),
        x_max=float(x_max),
        samples=int(samples),
        show_derivative=show_derivative,
    )

    try:
        result = evaluate_functions(functions, settings)
    except ValueError as exc:
        st.error(str(exc))
        return

    fig, ax = plt.subplots(figsize=(8, 6))
    ax.grid(True)
    ax.axhline(0, color="black", linewidth=0.5)
    ax.axvline(0, color="black", linewidth=0.5)

    for line in result.lines:
        ax.plot(result.x, line.values, label=line.label, linestyle=line.linestyle)

    for line in result.derivative_lines:
        ax.plot(result.x, line.values, label=line.label, linestyle=line.linestyle)

    for idx, error in enumerate(result.errors):
        ax.text(
            0.02,
            0.95 - idx * 0.05,
            error,
            transform=ax.transAxes,
            fontsize=8,
            color="red",
        )

    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_title("Graph")
    if result.lines or result.derivative_lines:
        ax.legend(loc="upper right", fontsize="small")

    st.pyplot(fig)

    buffer = io.BytesIO()
    fig.savefig(buffer, format="png", dpi=150)
    plt.close(fig)
    buffer.seek(0)
    st.download_button(
        label="Download PNG",
        data=buffer.getvalue(),
        file_name="graph.png",
        mime="image/png",
    )

    if result.errors:
        st.warning("\n".join(result.errors))


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover - manual execution helper
    render()
