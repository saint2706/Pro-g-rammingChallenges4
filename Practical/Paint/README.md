# Paint Clone

Two front-ends share a common configuration:

- `clone.py` — the original Tkinter desktop paint program with undo/redo and PNG export.
- `streamlit_app.py` — a web UI powered by Streamlit and `streamlit-drawable-canvas` that mirrors the brush controls.

## Requirements

Install the Practical extras from the project root to grab Pillow and friends:

```bash
python -m pip install -e .[practical]
```

For the Streamlit variant you also need the drawing widget:

```bash
python -m pip install streamlit streamlit-drawable-canvas
```

## Usage

Desktop app:

```bash
python clone.py
```

Streamlit app:

```bash
streamlit run streamlit_app.py
```

Both versions share defaults (canvas size, colours, stroke width) via `settings_util.py` so tweaks stay consistent.
