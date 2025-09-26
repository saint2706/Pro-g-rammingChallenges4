"""Unified Streamlit entry point for the Practical project suite."""

from __future__ import annotations

import importlib.util
import sys
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from types import ModuleType
from typing import Callable, Dict, List

import streamlit as st

BASE_DIR = Path(__file__).resolve().parent


@dataclass(frozen=True)
class ToolSpec:
    """Metadata describing a Streamlit tool and how to load it."""

    key: str
    title: str
    path: Path
    description: str


TOOLS: List[ToolSpec] = [
    ToolSpec(
        key="bellman-ford",
        title="Bellman–Ford Simulation",
        path=BASE_DIR / "Bellman Ford Simulation" / "streamlit_app.py",
        description="Step through the Bellman–Ford shortest path algorithm with visualised relaxation steps.",
    ),
    ToolSpec(
        key="bismuth-fractal",
        title="Bismuth Fractal",
        path=BASE_DIR / "Bismuth Fractal" / "streamlit_app.py",
        description="Generate colourful recursive bismuth fractal art and download the rendered image.",
    ),
    ToolSpec(
        key="graphing-calculator",
        title="Graphing Calculator",
        path=BASE_DIR / "Graphing Calculator" / "streamlit_app.py",
        description="Plot one or more mathematical functions (with optional derivatives) over configurable domains.",
    ),
    ToolSpec(
        key="hsv-wheel",
        title="HSV Colour Wheel",
        path=BASE_DIR / "HSV color wheel" / "streamlit_app.py",
        description="Create HSV colour wheel images, inspect metadata, and export PNG/JSON assets.",
    ),
    ToolSpec(
        key="ip-tracking",
        title="IP Tracking Visualisation",
        path=BASE_DIR / "IP Tracking visualization" / "streamlit_app.py",
        description="Fetch geolocation data for IP addresses and explore the results on an interactive map.",
    ),
    ToolSpec(
        key="ip-url-obscurifier",
        title="IP URL Obscurifier",
        path=BASE_DIR / "IP URL Obscurifier" / "streamlit_app.py",
        description="Encode or decode IPv4 values and URLs into mixed-base disguises with downloadable outputs.",
    ),
    ToolSpec(
        key="image-converter",
        title="Image Converter",
        path=BASE_DIR / "Image Converter" / "streamlit_app.py",
        description="Convert, resize, and download images in common formats while preserving metadata when possible.",
    ),
    ToolSpec(
        key="img-to-ascii",
        title="Image → ASCII",
        path=BASE_DIR / "ImgToASCII" / "streamlit_app.py",
        description="Transform uploaded images into ASCII art with tunable character sets and metadata exports.",
    ),
    ToolSpec(
        key="markdown-editor",
        title="Markdown Editor",
        path=BASE_DIR / "Markdown Editor" / "streamlit_app.py",
        description="Author Markdown content with HTML templates, live preview, and optional session autosave.",
    ),
    ToolSpec(
        key="matrix-arithmetic",
        title="Matrix Arithmetic",
        path=BASE_DIR / "Matrix Arithmetic" / "streamlit_app.py",
        description="Compute matrix operations (add, multiply, determinant, inverse) with step-by-step explanations.",
    ),
    ToolSpec(
        key="old-school-demo",
        title="Old School Demo Timeline",
        path=BASE_DIR / "Old School cringe" / "streamlit_app.py",
        description="Browse the demoscene-inspired effect timeline powering the retro multi-effect demo.",
    ),
    ToolSpec(
        key="paint-clone",
        title="Paint Clone",
        path=BASE_DIR / "Paint" / "streamlit_app.py",
        description="Sketch directly in the browser using familiar Paint tools and export your drawing as a PNG.",
    ),
    ToolSpec(
        key="radix-converter",
        title="Radix Base Converter",
        path=BASE_DIR / "Radix Base Converter" / "streamlit_app.py",
        description="Convert values between bases, build batch conversion tables, and export CSV/text reports.",
    ),
    ToolSpec(
        key="vector-product",
        title="Vector Product Explorer",
        path=BASE_DIR / "Vector Product" / "streamlit_app.py",
        description="Experiment with dot and cross products, inspect magnitudes, and view plots of the inputs.",
    ),
    ToolSpec(
        key="verlet-cloth",
        title="Verlet Cloth Playground",
        path=BASE_DIR / "Verlet Cloth" / "streamlit_app.py",
        description="Simulate a cloth mesh, preview frames, and download JSON/GIF exports of the animation.",
    ),
]

TOOLS_BY_KEY: Dict[str, ToolSpec] = {tool.key: tool for tool in TOOLS}
_IMPORTED_MODULES: Dict[str, ModuleType] = {}
_SYS_PATH_CACHE: set[str] = set()


def _ensure_import(tool: ToolSpec) -> ModuleType:
    module = _IMPORTED_MODULES.get(tool.key)
    if module is not None:
        return module

    module_name = f"practical_streamlit_{tool.key.replace('-', '_')}"
    spec = importlib.util.spec_from_file_location(module_name, tool.path)
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load module spec for {tool.path}")

    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module

    parent_dir = str(tool.path.parent.resolve())
    if parent_dir not in _SYS_PATH_CACHE:
        sys.path.insert(0, parent_dir)
        _SYS_PATH_CACHE.add(parent_dir)

    spec.loader.exec_module(module)
    _IMPORTED_MODULES[tool.key] = module
    return module


@lru_cache(maxsize=None)
def _resolve_render(tool_key: str) -> Callable[[], None]:
    tool = TOOLS_BY_KEY[tool_key]
    module = _ensure_import(tool)
    render = getattr(module, "render", None)
    if not callable(render):  # pragma: no cover - defensive guard for unexpected modules
        raise AttributeError(f"Module {tool.path} does not expose a callable render()")
    return render


def main() -> None:
    st.sidebar.title("Practical Streamlit Hub")
    st.sidebar.write("Select a tool to launch its Streamlit interface.")

    options = [tool.key for tool in TOOLS]
    selected_key = st.sidebar.radio(
        "Available tools",
        options=options,
        format_func=lambda key: TOOLS_BY_KEY[key].title,
    )
    selected_tool = TOOLS_BY_KEY[selected_key]

    st.sidebar.markdown(f"_{selected_tool.description}_")
    st.sidebar.divider()
    st.sidebar.caption("Switch between tools using the radio buttons above.")

    render = _resolve_render(selected_key)
    render()


if __name__ == "__main__":  # pragma: no cover - streamlit entry point
    main()

