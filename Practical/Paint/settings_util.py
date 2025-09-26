"""Shared configuration helpers for the Paint apps."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Tuple

DEFAULT_CANVAS_WIDTH = 800
DEFAULT_CANVAS_HEIGHT = 600
DEFAULT_BACKGROUND_COLOR = "#FFFFFF"
DEFAULT_FOREGROUND_COLOR = "#000000"
DEFAULT_STROKE_WIDTH = 3
DEFAULT_SHAPE = "freehand"
DEFAULT_FILL = False

VALID_SHAPES: Tuple[str, ...] = ("freehand", "line", "rectangle", "oval")


@dataclass(slots=True)
class CanvasSettings:
    """Basic dimensions and drawing defaults shared across UIs."""

    width: int = DEFAULT_CANVAS_WIDTH
    height: int = DEFAULT_CANVAS_HEIGHT
    bg: str = DEFAULT_BACKGROUND_COLOR
    fg: str = DEFAULT_FOREGROUND_COLOR
    stroke_width: int = DEFAULT_STROKE_WIDTH
    shape: str = DEFAULT_SHAPE
    fill: bool = DEFAULT_FILL


def rgba_from_hex(hex_color: str, alpha: float) -> str:
    """Convert ``#RRGGBB`` colors to Streamlit-friendly ``rgba()`` strings."""

    hex_color = hex_color.lstrip("#")
    if len(hex_color) != 6:
        raise ValueError("Expected a 6-character hex color (e.g. #AABBCC)")
    r = int(hex_color[0:2], 16)
    g = int(hex_color[2:4], 16)
    b = int(hex_color[4:6], 16)
    alpha = max(0.0, min(alpha, 1.0))
    return f"rgba({r}, {g}, {b}, {alpha:.2f})"
