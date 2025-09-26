"""Streamlit front-end for the Matrix Arithmetic toolkit."""
from __future__ import annotations

import json
from typing import Optional

import numpy as np
import streamlit as st

from matrix_arithmetic import (
    MatrixError,
    compute_operation,
    parse_matrix_text,
    visualize_transformation,
)


st.set_page_config(page_title="Matrix Arithmetic", page_icon="🧮", layout="wide")

st.title("Matrix Arithmetic Toolkit")
st.write(
    "Carry out common matrix operations, inspect step-by-step workings, and "
    "download the results for later use. Matrices can be entered manually or "
    "uploaded from a text/JSON file containing a literal such as ``[[1, 2], "
    "[3, 4]]``."
)


def _matrix_input(label: str, key_prefix: str) -> str:
    """Collect raw matrix text from the user via manual entry or upload."""

    container = st.container()
    container.markdown(f"**{label}**")
    mode = container.radio(
        "Input method",
        ("Type or paste", "Upload file"),
        key=f"{key_prefix}_mode",
        horizontal=True,
    )
    if mode == "Upload file":
        uploaded = container.file_uploader(
            "Choose a file containing a Python-style matrix literal",
            type=["txt", "json", "csv"],
            key=f"{key_prefix}_upload",
        )
        if uploaded is not None:
            try:
                return uploaded.getvalue().decode("utf-8")
            except UnicodeDecodeError:
                container.error("Could not decode the uploaded file as UTF-8 text.")
        return ""
    return container.text_area(
        "Enter a literal such as [[1, 2], [3, 4]]",
        key=f"{key_prefix}_text",
        height=120,
    )


def _parse_matrix_input(raw_text: str, label: str) -> np.ndarray:
    if not raw_text.strip():
        raise MatrixError(f"{label} is required.")
    return parse_matrix_text(raw_text.strip())


operation_names = {
    "Add": "add",
    "Multiply": "multiply",
    "Determinant": "determinant",
    "Inverse": "inverse",
}


with st.form("matrix_form", clear_on_submit=False):
    selection = st.selectbox("Operation", list(operation_names.keys()))
    precision = st.slider("Decimal precision", min_value=2, max_value=10, value=6)
    explain = st.checkbox("Show explanations where available", value=True)
    visualize = st.checkbox("Show 2×2 visualisation of the resulting matrix", value=False)

    raw_a = _matrix_input("Matrix A", "matrix_a")
    raw_b: Optional[str] = None
    if operation_names[selection] in {"add", "multiply"}:
        raw_b = _matrix_input("Matrix B", "matrix_b")

    submitted = st.form_submit_button("Compute")


if submitted:
    op_key = operation_names[selection]
    try:
        matrix_a = _parse_matrix_input(raw_a, "Matrix A")
        matrix_b = None
        if raw_b is not None:
            matrix_b = _parse_matrix_input(raw_b, "Matrix B")
        result = compute_operation(op_key, matrix_a, matrix_b, explain=explain)
    except MatrixError as exc:
        st.error(str(exc))
    else:
        st.subheader("Result")
        formatted = result.format(precision=precision)
        st.code(formatted, language="text")

        if result.steps:
            with st.expander("Computation steps", expanded=True):
                for idx, step in enumerate(result.steps, start=1):
                    st.markdown(f"{idx}. {step}")

        payload: dict[str, object] = {
            "operation": op_key,
            "result": result.value.tolist() if isinstance(result.value, np.ndarray) else result.value,
            "steps": result.steps,
            "precision": precision,
        }
        json_payload = json.dumps(payload, indent=2)
        st.download_button(
            "Download result as JSON",
            data=json_payload,
            file_name="matrix_result.json",
            mime="application/json",
        )

        text_lines = [f"Operation: {op_key}", "", "Result:", formatted]
        if result.steps:
            text_lines.extend(["", "Steps:"])
            text_lines.extend(f"{idx}. {step}" for idx, step in enumerate(result.steps, start=1))
        st.download_button(
            "Download result as text",
            data="\n".join(text_lines),
            file_name="matrix_result.txt",
            mime="text/plain",
        )

        if visualize:
            if isinstance(result.value, np.ndarray) and result.value.shape == (2, 2):
                fig = visualize_transformation(result.value, return_figure=True)
                st.pyplot(fig)
                try:
                    import matplotlib.pyplot as plt

                    plt.close(fig)
                except Exception:
                    pass
            else:
                st.warning("Visualisation is only available for 2×2 matrix results.")
