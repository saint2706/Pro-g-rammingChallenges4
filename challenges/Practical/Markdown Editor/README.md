# Markdown Editor

A desktop-focused Markdown editor built with Tkinter. It provides inline syntax highlighting, live HTML preview, and quick export tools so you can focus on writing content rather than wiring up tooling.

![Markdown editor screenshot placeholder](../programming%20challenges.png)

## Feature Overview

| Area | Highlights |
| ---- | ---------- |
| Editing | • Familiar Ctrl+N/Ctrl+O/Ctrl+S workflow<br>• Front matter detection with live metadata display<br>• Fast rule-based syntax highlighting for headings, emphasis, code fences, and links |
| Preview | • Side-by-side live HTML preview updates as you type<br>• Template aware (front matter `template` key selects a template automatically)<br>• Renders using [`tkhtmlview`](https://github.com/Andereoo/TkinterWeb) when installed; falls back to HTML source preview otherwise |
| Export | • Save Markdown, HTML, or XML from the same session<br>• HTML export renders through the selected template<br>• XML export captures front matter + rendered HTML for pipeline integration |
| Conversion utilities | • `converter.py` exposes reusable helpers for parsing front matter, rendering templates, and producing XML<br>• Tested with `unittest` for confidence |

## Installation

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt  # optional global requirements
pip install -r "challenges/Practical/Markdown Editor/requirements.txt"
```

> `tkhtmlview` is optional but recommended for a rendered preview pane. Without it, the preview shows generated HTML markup.

## Usage

```bash
python "challenges/Practical/Markdown Editor/editor.py"
```

### Keyboard Shortcuts

| Shortcut | Action |
| -------- | ------ |
| `Ctrl+N` | New document |
| `Ctrl+O` | Open Markdown file |
| `Ctrl+S` | Save document |
| `Ctrl+Shift+S` | Save As |
| `Ctrl+E` | Export rendered HTML |
| `Ctrl+Shift+E` | Export XML package |
| `Ctrl+L` | Toggle between light and dark application themes |
| `F5` | Force refresh of syntax highlighting & preview |

### Theming & Templates

- **Application themes**: choose _Light_ or _Dark_ from the `View → Theme` menu (or press `Ctrl+L`).
- **Content templates**: templates live in `templates/`. The current selection is shown in the toolbar combo box. You can:
  - Set `template: dark` (or another name) in front matter to auto-select it.
  - Drop in new `.html` files with `$title`, `$content`, `$styles`, and `$extra_head` placeholders to extend the gallery.

### Front Matter

Start documents with YAML-style front matter delimited by `---` to pre-fill metadata:

```markdown
---
title: Release Notes
author: Jane Dev
template: dark
extra_head: <meta name="robots" content="noindex">
---

# Release Notes
...
```

Fields are injected into templates and also appear in XML exports.

## Export Formats

- **Markdown**: standard `.md` editing, saved via `File → Save`.
- **HTML**: generated through the currently selected template; ideal for quick publishing.
- **XML**: structured container with `<front_matter>` and `<content_html>` nodes, so build pipelines can extract metadata or HTML safely.

## Tests

```bash
python -m unittest discover "challenges/Practical/Markdown Editor/tests"
```

## Folder Layout

```
challenges/Practical/Markdown Editor/
├── converter.py          # Front matter parsing + conversion utilities (unit tested)
├── editor.py             # Tkinter GUI application
├── requirements.txt      # Focused dependencies (markdown, tkhtmlview optional)
├── templates/            # HTML templates (light + dark)
└── tests/test_converter.py
```

## Live Preview Tips

- Press `F5` if you disabled auto-refresh or want to re-run conversions manually.
- If preview rendering looks raw, install `tkhtmlview` to enable full HTML styling.

Enjoy writing!

## Additional Solutions

Looking for alternative stacks? Explore the implementations under `solutions/`:

- `solutions/python_flask` – Flask-powered web editor with live preview.
- `solutions/node_cli` – Node.js CLI that converts Markdown to HTML from the terminal.
