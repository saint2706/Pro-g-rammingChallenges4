# Pixel Editor

A lightweight pixel-art editor built with Tkinter. It focuses on clarity and hackability: the data model is pure Python, tests cover the core canvas & palette logic, and the GUI layers on top with animation-aware import/export helpers.

## Features

- **Layer-based painting** – create, reorder, rename, and toggle layers for non-destructive editing.
- **Palette management** – seeded with common tones, supports adding custom colors, reordering, and sampling from the canvas.
- **Zoom & grid** – zoom from 1× to 64× with optional grid overlay so individual pixels are always easy to target.
- **Undo/redo** – unlimited history (bounded by memory) of pixel-level edits.
- **Frame timeline** – duplicate frames, onion-skin overlays, and animation preview playback.
- **Import/export** – open PNG sprite sheets, export PNG or GIF (with duration) sprite strips, and JSON project files for round-tripping.

## Getting Started

```bash
python pixel_editor.py
```

Tkinter ships with the Python standard library, but image import/export requires Pillow. Install the consolidated dependencies from `Practical/requirements.txt` or simply run:

```bash
pip install Pillow
```

## Keyboard Shortcuts

| Action | Shortcut |
| ------ | -------- |
| Toggle pen/eraser | `E` |
| Undo / Redo | `Ctrl+Z` / `Ctrl+Shift+Z` |
| Zoom in / out | `Ctrl+=` / `Ctrl+-` |
| Next / Previous frame | `.` / `,` |
| Play / Pause animation | `Space` |
| Sample color (eyedropper) | Hold `Alt` while clicking |

Mouse wheel zooms the viewport (with Ctrl for finer steps). Use the Up/Down palette buttons to reorder swatches.

## Workflow Examples

1. **Create a new sprite:**
   - File → New → set canvas size (default 32×32) and frame count.
   - Use the palette buttons to select colors; left-click to paint, right-click to erase.
   - Add layers for shading or outlines; toggle onion skinning to view the previous frame.

2. **Import an existing sprite sheet:**
   - File → Import Sprite Sheet → choose PNG or GIF.
   - Pick frame dimensions and stride; frames populate the timeline for refinement.

3. **Export animation:**
   - File → Export → choose PNG strip or GIF animation.
   - Optionally set frame duration (ms) for GIF output.

## Project Structure

```
Pixel Editor/
├── core.py          # Data model: Palette, PixelLayer, PixelFrame, PixelDocument
├── io_utils.py      # Import/export helpers for PNG/GIF sprite sheets
├── pixel_editor.py  # Tkinter GUI application
└── tests/
    └── test_core.py
```

The GUI intentionally separates model logic from view concerns so you can script conversions or write alternate interfaces using `core.py` alone.

## Future Ideas

- Tile map mode with per-cell metadata.
- More export layouts (texture atlas packing, aseprite JSON).
- Batch palette swaps.

Contributions are welcome—open an issue or PR with ideas!
