"""Streamlit front-end for the Vector Product demo."""
from __future__ import annotations

import sys
from pathlib import Path
from typing import Tuple

import matplotlib.pyplot as plt
import streamlit as st

CURRENT_DIR = Path(__file__).resolve().parent
if str(CURRENT_DIR) not in sys.path:
    sys.path.append(str(CURRENT_DIR))

from vector import cross, dot, magnitude, plot_vectors, prepare_vector  # noqa: E402


def _default_vectors(dim: int) -> Tuple[str, str]:
    if dim == 2:
        return "3 4", "1 -2"
    return "3 4 5", "1 -2 3"


def render() -> None:
    """Render the interactive Streamlit page."""

    st.title("Vector Product Explorer")
    st.write(
        "Use this tool to experiment with dot and cross products. "
        "Enter two vectors, choose the dimension, and optionally visualise them."
    )

    dim = st.radio("Vector dimension", options=(2, 3), index=1, horizontal=True)
    default_v1, default_v2 = _default_vectors(dim)

    col1, col2 = st.columns(2)
    with col1:
        vector1_text = st.text_input("Vector 1", value=default_v1, help="Comma or space separated")
    with col2:
        vector2_text = st.text_input("Vector 2", value=default_v2, help="Comma or space separated")

    show_magnitude = st.checkbox("Show magnitudes", value=True)
    show_plot = st.checkbox("Show plot", value=True)

    try:
        v1 = prepare_vector(vector1_text, dim)
        v2 = prepare_vector(vector2_text, dim)
    except ValueError as exc:
        st.error(f"Invalid vector input: {exc}")
        return

    dot_val = dot(v1, v2)
    cross_val = cross(v1, v2)

    st.subheader("Results")
    st.write(f"**Dot product:** {dot_val}")
    if dim == 2:
        st.write(f"**Cross product (z-component):** {cross_val}")
    else:
        st.write(f"**Cross product:** {cross_val}")

    if show_magnitude:
        st.write(f"|v1| = {magnitude(v1):.4g}")
        st.write(f"|v2| = {magnitude(v2):.4g}")

    if show_plot:
        fig = plot_vectors(v1, v2, cross_val, show=False)
        st.pyplot(fig)
        plt.close(fig)


def main() -> None:
    render()


if __name__ == "__main__":
    main()
