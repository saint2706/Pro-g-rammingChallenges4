"""Streamlit front-end for the Paint clone using a drawable canvas widget."""
from __future__ import annotations

from io import BytesIO

import numpy as np
import streamlit as st
from PIL import Image
from streamlit_drawable_canvas import st_canvas

from settings_util import CanvasSettings, VALID_SHAPES, rgba_from_hex

SHAPE_TO_DRAWING_MODE = {
    "freehand": "freedraw",
    "line": "line",
    "rectangle": "rect",
    "oval": "circle",
}


def _prepare_download(image_array: np.ndarray) -> bytes:
    """Convert the canvas numpy array into PNG bytes."""

    image = Image.fromarray(image_array.astype("uint8"), mode="RGBA")
    buffer = BytesIO()
    image.save(buffer, format="PNG")
    buffer.seek(0)
    return buffer.getvalue()


def main() -> None:
    st.set_page_config(page_title="Paint Clone (Streamlit)", layout="wide")
    st.title("🖌️ Paint Clone — Streamlit Edition")
    st.write(
        "Draw with familiar controls from the Tkinter app using an in-browser canvas. "
        "Use the sidebar to tweak your brush, then download your work as a PNG."
    )

    defaults = CanvasSettings()

    with st.sidebar:
        st.header("Brush & Canvas Settings")
        stroke_color = st.color_picker("Stroke colour", defaults.fg)
        tool = st.selectbox("Tool", options=VALID_SHAPES, index=VALID_SHAPES.index(defaults.shape))
        stroke_width = st.slider("Stroke width", min_value=1, max_value=25, value=defaults.stroke_width)
        fill_enabled = st.checkbox("Fill shapes", value=defaults.fill)
        background_color = st.color_picker("Canvas background", defaults.bg)

    fill_color = rgba_from_hex(stroke_color, 0.5 if fill_enabled else 0.0)
    drawing_mode = SHAPE_TO_DRAWING_MODE.get(tool, "freedraw")

    canvas_result = st_canvas(
        fill_color=fill_color,
        stroke_width=stroke_width,
        stroke_color=stroke_color,
        background_color=background_color,
        width=defaults.width,
        height=defaults.height,
        drawing_mode=drawing_mode,
        key="paint-canvas",
        update_streamlit=True,
        display_toolbar=False,
    )

    image_data = canvas_result.image_data
    if image_data is not None:
        png_bytes = _prepare_download(image_data)
        st.subheader("Canvas Preview")
        st.image(image_data, use_column_width=True)
        st.download_button(
            label="Download drawing as PNG",
            data=png_bytes,
            file_name="paint_clone.png",
            mime="image/png",
        )
    else:
        st.info("Start drawing on the canvas to enable PNG downloads.")


if __name__ == "__main__":
    main()
