"""Streamlit interface for the HSV colour wheel generator."""

from __future__ import annotations

import json
from io import BytesIO
from pathlib import Path
from typing import Any

import numpy as np
import streamlit as st

from colorwheel import (
    COLOUR_SCIENCE_AVAILABLE,
    COLOUR_SCIENCE_GUIDANCE,
    Config,
    Orientation,
    generate_colour_wheel,
)

try:  # Optional dependency for PNG export
    from PIL import Image
except ImportError:  # pragma: no cover
    Image = None  # type: ignore[misc, assignment]


def render() -> None:
    """Render the HSV colour wheel application."""

    st.set_page_config(page_title="HSV Colour Wheel", page_icon="🎨", layout="wide")
    st.title("HSV Colour Wheel")

    if not COLOUR_SCIENCE_AVAILABLE:
        st.info(COLOUR_SCIENCE_GUIDANCE)

    st.markdown(
        "Generate HSV colour wheels interactively. Adjust the parameters on the left and "
        "use the download buttons to export the PNG image or JSON metadata."
    )

    with st.sidebar:
        st.header("Configuration")
        samples = st.slider("Samples", min_value=64, max_value=4096, value=512, step=32)
        clip_circle = st.toggle("Clip to circle", value=True)
        orientation_label = st.selectbox(
            "Orientation",
            options=[orientation.value for orientation in Orientation],
            index=0,
        )
        orientation = Orientation(orientation_label)
        feather = st.slider(
            "Feather (px)",
            min_value=0,
            max_value=max(0, samples // 2),
            value=0,
            step=1,
        )
        dpi = st.slider(
            "DPI for downloads", min_value=72, max_value=600, value=150, step=6
        )

    config = Config(
        samples=samples,
        clip_circle=clip_circle,
        method=orientation,
        feather=feather,
        dpi=dpi,
        show=False,
    )

    try:
        config.validate()
    except ValueError as exc:
        st.error(f"Configuration error: {exc}")
        st.stop()

    wheel = generate_colour_wheel(config)

    st.subheader("Preview")
    st.image(
        wheel,
        caption=(
            f"{config.samples}×{config.samples} | {config.method.value} | "
            f"Clipped: {config.clip_circle} | Feather: {config.feather}px"
        ),
        clamp=True,
    )

    metadata: dict[str, Any] = {
        "samples": config.samples,
        "clip_circle": config.clip_circle,
        "orientation": config.method.value,
        "feather": config.feather,
        "dpi": config.dpi,
        "dtype": str(wheel.dtype),
        "min": float(np.min(wheel)),
        "max": float(np.max(wheel)),
        "colour_science_available": COLOUR_SCIENCE_AVAILABLE,
    }

    col_png, col_json = st.columns(2)

    with col_png:
        st.subheader("Image export")
        if Image is None:
            st.warning(
                "PNG export requires the optional Pillow dependency. Install it with "
                "`pip install pillow` to enable image downloads."
            )
        else:
            arr8 = np.clip(wheel * 255.0, 0, 255).astype(np.uint8)
            png_buffer = BytesIO()
            Image.fromarray(arr8, mode="RGBA").save(
                png_buffer,
                format="PNG",
                dpi=(config.dpi, config.dpi),
            )
            png_name = (
                f"colour-wheel-{config.samples}px-{config.method.value.lower()}.png"
            )
            st.download_button(
                "Download PNG",
                data=png_buffer.getvalue(),
                file_name=png_name,
                mime="image/png",
            )

    with col_json:
        st.subheader("Metadata")
        meta_buffer = BytesIO(json.dumps(metadata, indent=2).encode("utf-8"))
        json_name = Path("colour-wheel-metadata.json").name
        st.download_button(
            "Download JSON",
            data=meta_buffer.getvalue(),
            file_name=json_name,
            mime="application/json",
        )
        st.json(metadata)


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover - manual execution helper
    render()
