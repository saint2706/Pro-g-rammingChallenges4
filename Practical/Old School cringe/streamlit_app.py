"""Streamlit UI for the Old School multi-effect demo timeline."""
from __future__ import annotations

import inspect
import sys
from functools import lru_cache
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

import streamlit as st

CURRENT_DIR = Path(__file__).resolve().parent
if str(CURRENT_DIR) not in sys.path:
    sys.path.append(str(CURRENT_DIR))

import retro_demo as retro  # noqa: E402


def _resolve_effect_class(factory: Callable[[retro.DemoConfig], retro.Effect]) -> Optional[type[retro.Effect]]:
    """Best-effort resolution of the Effect subclass used by ``factory``."""

    effect_cls: Optional[type[retro.Effect]] = None
    code = getattr(factory, "__code__", None)
    if code is not None:
        for name in code.co_names:
            candidate = getattr(retro, name, None)
            if inspect.isclass(candidate) and issubclass(candidate, retro.Effect):
                effect_cls = candidate
                break

    if effect_cls is None:
        closure = getattr(factory, "__closure__", None)
        if closure:
            for cell in closure:
                try:
                    value = cell.cell_contents
                except ValueError:
                    continue
                if inspect.isclass(value) and issubclass(value, retro.Effect):
                    effect_cls = value
                    break

    return effect_cls


def _extract_segments(timeline: retro.Timeline) -> List[Dict[str, Any]]:
    """Extract display metadata for each timeline segment."""

    extracted: List[Dict[str, Any]] = []
    for state in timeline.segments:
        spec = state.spec
        info: Dict[str, Any] = {
            "name": spec.name,
            "duration": spec.duration,
            "fade_in": spec.fade_in,
            "fade_out": spec.fade_out,
            "metadata": dict(spec.metadata),
            "lines": None,
            "effect_class": None,
            "description": [],
        }

        effect_cls = _resolve_effect_class(spec.factory)
        docstring_added = False
        if effect_cls is not None:
            info["effect_class"] = effect_cls.__name__
            if effect_cls.__doc__:
                info["description"].append(inspect.cleandoc(effect_cls.__doc__))
                docstring_added = True

        closure = getattr(spec.factory, "__closure__", None)
        if closure:
            for cell in closure:
                try:
                    value = cell.cell_contents
                except ValueError:
                    continue

                if isinstance(value, dict):
                    info["metadata"].update(value)
                elif isinstance(value, list) and value and all(isinstance(line, str) for line in value):
                    info["lines"] = value
                elif (
                    effect_cls is None
                    and inspect.isclass(value)
                    and issubclass(value, retro.Effect)
                ):
                    info["effect_class"] = value.__name__
                    if value.__doc__ and not docstring_added:
                        info["description"].append(inspect.cleandoc(value.__doc__))
                        docstring_added = True

        if info["metadata"]:
            for key, value in info["metadata"].items():
                pretty_key = key.replace("_", " ").title()
                info["description"].append(f"**{pretty_key}:** {value}")

        extracted.append(info)

    return extracted


@lru_cache(maxsize=1)
def _load_timeline() -> retro.Timeline:
    cfg = retro.DemoConfig()
    return retro.build_timeline(cfg)


def render() -> None:
    """Render the Streamlit interface."""

    st.set_page_config(page_title="Old School Demo Timeline", layout="wide")
    st.title("Old School Multi-Effect Timeline")
    st.write(
        "Inspect the demoscene-inspired effects that make up the retro demo. "
        "Each segment lists its duration, fade windows, and contextual notes."
    )

    timeline = _load_timeline()
    segments = _extract_segments(timeline)

    for seg in segments:
        header = f"{seg['name']} — {seg['duration']:.1f}s"
        with st.expander(header, expanded=True):
            if seg["effect_class"]:
                st.write(f"**Effect class:** `{seg['effect_class']}`")
            st.write(f"Fade-in: {seg['fade_in']:.1f}s · Fade-out: {seg['fade_out']:.1f}s")

            if seg["description"]:
                for paragraph in seg["description"]:
                    st.write(paragraph)
            else:
                st.info("No additional description available for this segment.")

            if seg["lines"]:
                st.markdown("**Scroller text:**")
                for line in seg["lines"]:
                    st.markdown(f"- {line}")

    st.caption("Data sourced from retro_demo.py's timeline configuration.")


__all__ = ["render"]


if __name__ == "__main__":
    render()
