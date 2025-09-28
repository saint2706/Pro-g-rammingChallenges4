"""Utilities for parsing Markdown documents with YAML-style front matter,
applying HTML templates, and producing HTML/XML outputs.

The functions here are intentionally importable by the GUI and the unit tests.
They avoid GUI dependencies so they can be reused in headless pipelines.
"""

from __future__ import annotations

from dataclasses import dataclass
from html import escape
from pathlib import Path
from string import Template
from typing import Dict, Iterable, Optional, Sequence, Tuple
import re
import xml.etree.ElementTree as ET

try:  # Prefer the markdown package when available.
    import markdown as _markdown
except ImportError:  # pragma: no cover - exercised in CI if markdown is absent.
    _markdown = None


@dataclass(frozen=True)
class TemplateDefinition:
    """Represents an HTML template available to the editor."""

    name: str
    label: str
    body: Template


class TemplateManager:
    """Loads and renders HTML templates stored in a directory."""

    def __init__(self, template_dir: Path):
        self.template_dir = template_dir
        self._templates: Dict[str, TemplateDefinition] = {}
        self.refresh()

    def refresh(self) -> None:
        """Reload template files from disk."""
        self._templates.clear()
        if not self.template_dir.exists():
            return
        for file in sorted(self.template_dir.glob("*.html")):
            name = file.stem
            label = name.replace("_", " ").title()
            content = file.read_text(encoding="utf-8")
            self._templates[name] = TemplateDefinition(name, label, Template(content))

    def names(self) -> Iterable[str]:
        return self._templates.keys()

    def labels(self) -> Dict[str, str]:
        return {name: definition.label for name, definition in self._templates.items()}

    def choices(self) -> Sequence[Tuple[str, str]]:
        """Return template name/label pairs in a deterministic order."""

        return [
            (name, definition.label) for name, definition in self._templates.items()
        ]

    def render(
        self,
        template_name: str,
        *,
        html_content: str,
        metadata: Dict[str, str],
    ) -> str:
        """Render ``html_content`` inside the requested template."""
        definition = self._templates.get(template_name)
        if definition is None and self._templates:
            # Fallback to the first template available.
            definition = next(iter(self._templates.values()))
        if definition is None:
            # No templates available: return the bare content.
            return html_content

        author = metadata.get("author", "")
        template_vars = {
            "title": metadata.get("title", "Untitled Document"),
            "author": author,
            "author_line": f"<p class='byline'><em>{author}</em></p>" if author else "",
            "styles": metadata.get("styles", ""),
            "extra_head": metadata.get("extra_head", ""),
            "content": html_content,
        }
        return definition.body.safe_substitute(template_vars)


@dataclass
class ConversionResult:
    """Container for conversion outputs."""

    metadata: Dict[str, str]
    body_markdown: str
    html: str
    template_used: Optional[str]


def parse_front_matter(text: str) -> Tuple[Dict[str, str], str]:
    """Extract YAML-like front matter from ``text``.

    Supports ``key: value`` pairs until the closing ``---`` delimiter. Values are
    stripped but otherwise left as-is to avoid surprising the author.
    """

    lines = text.splitlines()
    if not lines or lines[0].strip() != "---":
        return {}, text

    metadata: Dict[str, str] = {}
    body_start_index: Optional[int] = None
    for index, line in enumerate(lines[1:], start=1):
        if line.strip() == "---":
            body_start_index = index + 1
            break
        if not line.strip() or line.strip().startswith("#"):
            # Allow comments inside front matter.
            continue
        if ":" in line:
            key, value = line.split(":", 1)
            metadata[key.strip()] = value.strip()
    if body_start_index is None:
        return {}, text
    body_lines = lines[body_start_index:]
    return metadata, "\n".join(body_lines).lstrip("\n")


def _markdown_to_html(markdown_text: str) -> str:
    """Convert Markdown to HTML, preferring the external ``markdown`` package."""

    if _markdown is not None:
        md = _markdown.Markdown(
            extensions=["fenced_code", "tables", "codehilite"],
            output_format="xhtml1",
        )
        return md.convert(markdown_text)

    # Fallback conversion that supports a subset of Markdown syntax. We focus on
    # headings, emphasis, code spans, and paragraphs to keep tests meaningful.
    html_lines = []
    in_code_block = False
    for raw_line in markdown_text.splitlines():
        line = raw_line.rstrip()
        if line.startswith("```"):
            if not in_code_block:
                html_lines.append("<pre><code>")
                in_code_block = True
            else:
                html_lines.append("</code></pre>")
                in_code_block = False
            continue
        if in_code_block:
            html_lines.append(escape(line))
            continue
        if not line:
            html_lines.append("<p></p>")
            continue
        heading_match = re.match(r"^(#{1,6})\s+(.*)", line)
        if heading_match:
            level = len(heading_match.group(1))
            content = heading_match.group(2)
            html_lines.append(f"<h{level}>{_inline_format(content)}</h{level}>")
            continue
        html_lines.append(f"<p>{_inline_format(line)}</p>")
    if in_code_block:
        html_lines.append("</code></pre>")
    return "\n".join(html_lines)


def _inline_format(text: str) -> str:
    """Apply very small subset of Markdown inline formatting for the fallback."""

    text = re.sub(r"\*\*(.+?)\*\*", r"<strong>\1</strong>", text)
    text = re.sub(r"\*(.+?)\*", r"<em>\1</em>", text)
    text = re.sub(r"`([^`]+)`", r"<code>\1</code>", text)
    text = re.sub(r"\[(.+?)\]\((.+?)\)", r"<a href='\2'>\1</a>", text)
    return text


def convert_markdown(
    text: str,
    *,
    template_manager: TemplateManager,
    preferred_template: Optional[str] = None,
) -> ConversionResult:
    """Convert Markdown (with optional front matter) into HTML."""

    metadata, body = parse_front_matter(text)
    template_name = metadata.get("template") or preferred_template
    available = list(template_manager.names())
    if available:
        if template_name not in available:
            template_name = available[0]
    body_html = _markdown_to_html(body)
    html = template_manager.render(
        template_name or "", html_content=body_html, metadata=metadata
    )
    return ConversionResult(
        metadata=metadata, body_markdown=body, html=html, template_used=template_name
    )


def convert_to_xml(result: ConversionResult) -> str:
    """Serialise the conversion result into a small XML document."""

    root = ET.Element("document")
    meta = ET.SubElement(root, "front_matter")
    for key, value in sorted(result.metadata.items()):
        node = ET.SubElement(meta, key)
        node.text = value

    template_node = ET.SubElement(root, "template")
    template_node.text = result.template_used or ""

    content = ET.SubElement(root, "content_html")
    content.text = result.html

    return ET.tostring(root, encoding="unicode")


BASE_DIR = Path(__file__).resolve().parent
DEFAULT_TEMPLATE_DIR = BASE_DIR / "templates"


def load_template_manager(template_dir: Optional[Path] = None) -> TemplateManager:
    """Instantiate a :class:`TemplateManager` for ``template_dir``.

    The desktop and Streamlit applications both rely on this helper to lazily
    construct a manager without importing any Tkinter modules. When
    ``template_dir`` is ``None`` the default template folder bundled with the
    project is used.
    """

    directory = template_dir or DEFAULT_TEMPLATE_DIR
    return TemplateManager(directory)


def list_template_choices(
    template_manager: TemplateManager,
) -> Sequence[Tuple[str, str]]:
    """Expose available template ``(name, label)`` pairs for UI bindings."""

    return template_manager.choices()


def convert_markdown_to_html(
    text: str,
    *,
    template_manager: TemplateManager,
    preferred_template: Optional[str] = None,
) -> str:
    """Convenience wrapper returning rendered HTML only."""

    return convert_markdown(
        text,
        template_manager=template_manager,
        preferred_template=preferred_template,
    ).html


def convert_markdown_to_xml(
    text: str,
    *,
    template_manager: TemplateManager,
    preferred_template: Optional[str] = None,
) -> str:
    """Convert Markdown input directly to the XML payload used for exports."""

    result = convert_markdown(
        text,
        template_manager=template_manager,
        preferred_template=preferred_template,
    )
    return convert_to_xml(result)


__all__ = [
    "ConversionResult",
    "TemplateDefinition",
    "TemplateManager",
    "convert_markdown_to_html",
    "convert_markdown_to_xml",
    "convert_markdown",
    "convert_to_xml",
    "DEFAULT_TEMPLATE_DIR",
    "list_template_choices",
    "load_template_manager",
    "parse_front_matter",
]
