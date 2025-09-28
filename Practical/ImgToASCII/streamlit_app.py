"""Streamlit front-end for the Image to ASCII converter."""

from __future__ import annotations

import html
import json
from pathlib import Path

import streamlit as st

from convert import Config, DEFAULT_ASCII_CHARS, convert


def _build_config(
    *,
    source_name: str,
    width: int,
    chars: str,
    invert: bool,
    aspect: float,
) -> Config:
    cfg = Config(
        input_path=Path(source_name or "uploaded"),
        output_path=None,
        width=width,
        chars=chars,
        invert=invert,
        aspect=aspect,
        json_path=None,
    )
    cfg.validate()
    return cfg


def _render_ascii(ascii_art: str) -> None:
    st.markdown(
        "<div style='font-family:monospace; white-space:pre; overflow-x:auto;'>"
        f"{html.escape(ascii_art)}"
        "</div>",
        unsafe_allow_html=True,
    )


def render() -> None:
    st.set_page_config(page_title="Image to ASCII", layout="wide")
    st.title("Image to ASCII Converter")

    uploaded = st.file_uploader(
        "Upload an image",
        type=["png", "jpg", "jpeg", "bmp", "gif", "webp", "tiff"],
    )

    width = st.slider("Width (characters)", min_value=10, max_value=400, value=100)
    default_chars = "".join(DEFAULT_ASCII_CHARS)
    chars = st.text_input(
        "Character set (dark → light)",
        value=default_chars,
        help="Provide at least two characters ordered from darkest to lightest.",
    )
    invert = st.checkbox("Invert brightness", value=False)
    aspect = st.slider(
        "Aspect correction", min_value=0.1, max_value=5.0, value=0.55, step=0.05
    )

    convert_now = uploaded is not None and len(chars) >= 2

    if uploaded and len(chars) < 2:
        st.warning("The character set must contain at least two characters.")

    if not convert_now:
        return

    try:
        cfg = _build_config(
            source_name=uploaded.name or "uploaded",
            width=width,
            chars=chars,
            invert=invert,
            aspect=aspect,
        )
    except ValueError as exc:
        st.error(str(exc))
        return

    uploaded.seek(0)
    result = convert(cfg, image_source=uploaded)
    ascii_art = result["ascii"]
    meta = result["meta"]

    st.subheader("ASCII Art")
    _render_ascii(ascii_art)

    st.subheader("Metadata")
    st.json(meta)

    base_name = Path(uploaded.name).stem or "ascii_art"
    ascii_bytes = ascii_art.encode("utf-8")
    st.download_button(
        label="Download ASCII text",
        data=ascii_bytes,
        file_name=f"{base_name}.txt",
        mime="text/plain",
    )

    json_bytes = json.dumps(meta, indent=2).encode("utf-8")
    st.download_button(
        label="Download metadata (JSON)",
        data=json_bytes,
        file_name=f"{base_name}_meta.json",
        mime="application/json",
    )


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover
    render()
