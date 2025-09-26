"""Streamlit front-end for exploring the Bismuth fractal."""

from __future__ import annotations

import io
import math
from typing import Iterable, List

import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
import streamlit as st

from bismuth import (
    Config,
    DEFAULT_PALETTE,
    generate_hexes,
    parse_palette,
)


def _hex_vertices(x: float, y: float, side: float) -> List[List[float]]:
    """Return the 2D vertices for a pointy-topped hexagon."""

    return [
        [x + side * math.cos(math.radians(30 + 60 * i)),
         y + side * math.sin(math.radians(30 + 60 * i))]
        for i in range(6)
    ]


def _hex_bounds(hexes: Iterable[tuple[float, float, float, str]]) -> tuple[float, float, float, float]:
    cos30 = math.cos(math.radians(30))
    min_x = min(x - side * cos30 for x, _, side, _ in hexes)
    max_x = max(x + side * cos30 for x, _, side, _ in hexes)
    min_y = min(y - side for _, y, side, _ in hexes)
    max_y = max(y + side for _, y, side, _ in hexes)
    return min_x, max_x, min_y, max_y


def render() -> None:
    st.set_page_config(page_title="Bismuth Fractal", page_icon="🔷")
    st.title("Bismuth Fractal Explorer")

    col_left, col_right = st.columns(2)
    with col_left:
        level = st.slider("Recursion depth", min_value=1, max_value=7, value=4)
        shuffle = st.checkbox("Shuffle palette", value=False)
    with col_right:
        size = st.slider(
            "Initial hex side length",
            min_value=10.0,
            max_value=200.0,
            value=120.0,
            step=5.0,
        )
        palette_text = st.text_input(
            "Palette (comma-separated)",
            value=",".join(DEFAULT_PALETTE),
            help="Provide hex colour codes separated by commas. Leave blank for the default palette.",
        )

    palette_arg = palette_text.strip() or None
    palette = parse_palette(palette_arg)
    cfg = Config(
        level=level,
        size=size,
        palette=palette,
        shuffle_palette=shuffle,
        animate=False,
        frame_interval=1,
        export_path=None,
    )

    hexes = generate_hexes(cfg)

    min_x, max_x, min_y, max_y = _hex_bounds(hexes)
    span = max(max_x - min_x, max_y - min_y)
    margin = span * 0.05 if span else 1.0

    fig, ax = plt.subplots(figsize=(6, 6), facecolor="black")
    ax.set_aspect("equal")
    ax.axis("off")
    ax.set_facecolor("black")

    for x, y, side, color in hexes:
        patch = Polygon(_hex_vertices(x, y, side), closed=True, facecolor=color, edgecolor=color)
        ax.add_patch(patch)

    ax.set_xlim(min_x - margin, max_x + margin)
    ax.set_ylim(min_y - margin, max_y + margin)

    buffer = io.BytesIO()
    fig.savefig(buffer, format="png", facecolor=fig.get_facecolor(), bbox_inches="tight")
    buffer.seek(0)

    st.pyplot(fig)
    st.download_button(
        "Download PNG",
        data=buffer.getvalue(),
        file_name="bismuth_fractal.png",
        mime="image/png",
    )
    st.caption(f"Rendered {len(hexes)} hexes (estimated {cfg.estimated_hexes}).")

    plt.close(fig)


if __name__ == "__main__":  # pragma: no cover
    render()
