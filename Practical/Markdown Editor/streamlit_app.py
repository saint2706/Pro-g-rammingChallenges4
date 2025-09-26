"""Streamlit front-end for the Markdown editor conversion utilities."""
from __future__ import annotations

from datetime import datetime
import re
from typing import Dict, Sequence

import streamlit as st
from streamlit.components.v1 import html

from .converter import (
    ConversionResult,
    convert_markdown,
    convert_to_xml,
    list_template_choices,
    load_template_manager,
)

DEFAULT_DOCUMENT = """---
title: Streamlit Demo
author: Markdown Maker
template: default
---

# Welcome!

Start typing in the editor on the left. Front matter metadata is optional but
lets you pick templates, set the author line, and include custom `<head>`
snippets.

## Live Preview

- Use **bold**, *italic*, `code`, and fenced blocks.
- Select different templates to change the presentation instantly.
- Export HTML or XML using the buttons below the preview pane.
"""

THEME_STYLES: Dict[str, str] = {
    "Light": """
        <style>
        .stApp, .block-container { background: #f8fafc; color: #0f172a; }
        .stTextArea textarea { background: #ffffff; color: #0f172a; }
        </style>
    """,
    "Dark": """
        <style>
        .stApp, .block-container { background: #0f172a; color: #e2e8f0; }
        .stTextArea textarea { background: #1e293b; color: #e2e8f0; }
        </style>
    """,
}


def _initialise_state(templates: Sequence[str]) -> None:
    """Populate ``st.session_state`` defaults including autosaved drafts."""

    autosaved = st.session_state.get("autosaved_draft")
    default_template = templates[0] if templates else ""
    if autosaved and autosaved.get("template") in templates:
        default_template = autosaved["template"]

    if "markdown_text" not in st.session_state:
        st.session_state.markdown_text = (
            autosaved.get("content") if autosaved and autosaved.get("content") else DEFAULT_DOCUMENT
        )
    if "template_name" not in st.session_state:
        st.session_state.template_name = default_template
    if "theme" not in st.session_state:
        st.session_state.theme = "Light"
    if "autosave_enabled" not in st.session_state:
        st.session_state.autosave_enabled = autosaved is not None


def _slugify(value: str) -> str:
    slug = re.sub(r"[^a-zA-Z0-9]+", "-", value).strip("-")
    return slug or "document"


def _autosave(result: ConversionResult, enabled: bool) -> None:
    if not enabled:
        return
    st.session_state.autosaved_draft = {
        "content": st.session_state.markdown_text,
        "template": result.template_used or st.session_state.template_name,
        "timestamp": datetime.utcnow().isoformat(timespec="seconds"),
    }


def render() -> None:
    """Render the Streamlit application."""

    st.set_page_config(page_title="Markdown Editor", layout="wide")

    manager = load_template_manager()
    template_choices = list_template_choices(manager)
    template_names = [name for name, _label in template_choices]
    template_labels = {name: label for name, label in template_choices}

    _initialise_state(template_names)

    st.title("Markdown Editor (Streamlit)")

    if template_names:
        try:
            current_index = template_names.index(st.session_state.template_name)
        except ValueError:
            current_index = 0
            st.session_state.template_name = template_names[0]
    else:
        current_index = 0

    with st.sidebar:
        st.header("Appearance")
        theme = st.radio("Theme", options=["Light", "Dark"], key="theme")
        st.markdown(THEME_STYLES.get(theme, ""), unsafe_allow_html=True)

        st.header("Templates")
        if template_names:
            st.selectbox(
                "Template",
                options=template_names,
                format_func=lambda name: template_labels.get(name, name.title()),
                key="template_name",
                index=current_index,
            )
        else:
            st.info("Drop HTML template files into the templates/ folder to enable rendering.")

        st.header("Session")
        st.checkbox("Autosave draft to session", key="autosave_enabled", value=st.session_state.autosave_enabled)
        if st.session_state.get("autosaved_draft"):
            saved = st.session_state.autosaved_draft
            st.caption(f"Autosaved {saved['timestamp']} UTC")
            if st.button("Clear autosaved draft"):
                st.session_state.pop("autosaved_draft", None)

    editor_col, preview_col = st.columns([1, 1])

    with editor_col:
        st.subheader("Markdown Source")
        st.text_area(
            "Markdown",
            key="markdown_text",
            height=520,
        )

    with preview_col:
        st.subheader("HTML Preview")
        if not template_names:
            st.warning("No templates found. The preview will show raw Markdown conversion without template styling.")

    result = convert_markdown(
        st.session_state.markdown_text,
        template_manager=manager,
        preferred_template=st.session_state.template_name or None,
    )

    _autosave(result, st.session_state.autosave_enabled)

    with preview_col:
        html(result.html, height=540, scrolling=True)

        html_filename = f"{_slugify(result.metadata.get('title', 'document'))}.html"
        xml_payload = convert_to_xml(result)
        xml_filename = f"{_slugify(result.metadata.get('title', 'document'))}.xml"

        st.download_button(
            "Download HTML",
            data=result.html,
            file_name=html_filename,
            mime="text/html",
        )
        st.download_button(
            "Download XML",
            data=xml_payload,
            file_name=xml_filename,
            mime="application/xml",
        )

    with editor_col:
        if result.template_used and result.template_used != st.session_state.template_name:
            st.info(
                "Front matter requested template '%s'. Rendering honours that selection." % result.template_used
            )


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover - manual execution helper
    render()
