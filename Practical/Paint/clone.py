"""clone.py - Simple Tkinter paint application (modernized).

Features:
  * Dataclass `Settings` encapsulates initial configuration
  * Toolbar with: color picker, shape selector, stroke width slider, fill toggle, Undo, Clear, Save
  * Shapes: freehand (smoothed), line, rectangle, oval
  * Rubber-band (live preview) for geometric shapes while dragging
  * History stack enables undo of last drawn element (multi-segment freehand counts as one)
  * Optional filled shapes
  * Keyboard shortcuts: Ctrl+S (save), Ctrl+Z (undo), Delete (clear canvas)
  * Graceful PNG save: attempts Pillow -> PNG, else falls back to PostScript export
  * Type hints & inline documentation for new contributors

Design Notes:
  * For performance, freehand drawing adds small line segments only while mouse moves; they are grouped per stroke for undo.
  * Live preview prevents intermediate shape artifacts by drawing a temporary item that is deleted on mouse release.
  * The canvas is resolution independent; saving via PostScript then converting to PNG if Pillow is present keeps dependencies optional.
"""

from __future__ import annotations

import tempfile
import tkinter as tk
from dataclasses import dataclass
from pathlib import Path
from tkinter import colorchooser, filedialog, messagebox
from typing import List, Optional

try:  # Optional Pillow import for PNG saving
    from PIL import Image  # type: ignore

    PIL_AVAILABLE = True
except Exception:  # pragma: no cover - absence path
    PIL_AVAILABLE = False
    Image = None  # type: ignore

# --------------------------- Settings --------------------------- #


@dataclass(slots=True)
class Settings:
    width: int = 800
    height: int = 600
    bg: str = "white"
    fg: str = "black"
    stroke_width: int = 3
    shape: str = "freehand"  # freehand | line | rectangle | oval
    fill: bool = False


# --------------------------- Application --------------------------- #


class PaintApp:
    """A lightweight paint-like program with undo, save, and basic shapes."""

    VALID_SHAPES = ("freehand", "line", "rectangle", "oval")

    def __init__(self, root: tk.Tk, settings: Optional[Settings] = None):
        self.root = root
        self.settings = settings or Settings()
        self.root.title("Paint Clone")

        # Internal state
        self.current_color: str = self.settings.fg
        self.preview_item: int | None = None  # temporary shape during drag
        self.start_x: int | None = None
        self.start_y: int | None = None
        self.last_x: int | None = None  # for freehand chaining
        self.last_y: int | None = None
        self.current_stroke_ids: List[int] = (
            []
        )  # ids composing the active freehand stroke
        self.history: List[List[int]] = []  # stack of shape/stroke id groups for undo

        # Build UI
        self._build_toolbar()
        self._build_canvas()
        self._bind_shortcuts()

    # ---------------- UI Construction ---------------- #
    def _build_toolbar(self) -> None:
        self.toolbar = tk.Frame(self.root, padx=4, pady=2)
        self.toolbar.pack(side=tk.TOP, fill=tk.X)

        self.color_btn = tk.Button(
            self.toolbar, text="Color", command=self.choose_color, fg=self.current_color
        )
        self.color_btn.pack(side=tk.LEFT)

        tk.Label(self.toolbar, text="Shape").pack(side=tk.LEFT, padx=(8, 2))
        self.shape_var = tk.StringVar(value=self.settings.shape)
        self.shape_menu = tk.OptionMenu(
            self.toolbar, self.shape_var, *self.VALID_SHAPES
        )
        self.shape_menu.config(width=9)
        self.shape_menu.pack(side=tk.LEFT)

        tk.Label(self.toolbar, text="Width").pack(side=tk.LEFT, padx=(8, 2))
        self.stroke_var = tk.IntVar(value=self.settings.stroke_width)
        self.stroke_slider = tk.Scale(
            self.toolbar,
            from_=1,
            to=25,
            orient=tk.HORIZONTAL,
            showvalue=True,
            variable=self.stroke_var,
            length=120,
        )
        self.stroke_slider.pack(side=tk.LEFT)

        self.fill_var = tk.BooleanVar(value=self.settings.fill)
        self.fill_chk = tk.Checkbutton(
            self.toolbar, text="Fill", variable=self.fill_var
        )
        self.fill_chk.pack(side=tk.LEFT, padx=(8, 2))

        self.undo_btn = tk.Button(self.toolbar, text="Undo", command=self.undo)
        self.undo_btn.pack(side=tk.LEFT)
        self.clear_btn = tk.Button(
            self.toolbar, text="Clear", command=self.clear_canvas
        )
        self.clear_btn.pack(side=tk.LEFT)
        self.save_btn = tk.Button(self.toolbar, text="Save", command=self.save_dialog)
        self.save_btn.pack(side=tk.LEFT)

        self.status = tk.Label(self.toolbar, text="Ready", anchor="w")
        self.status.pack(side=tk.LEFT, padx=10)

    def _build_canvas(self) -> None:
        self.canvas = tk.Canvas(
            self.root,
            bg=self.settings.bg,
            width=self.settings.width,
            height=self.settings.height,
        )
        self.canvas.pack(expand=True, fill=tk.BOTH)
        self.canvas.bind("<ButtonPress-1>", self.on_press)
        self.canvas.bind("<B1-Motion>", self.on_drag)
        self.canvas.bind("<ButtonRelease-1>", self.on_release)

    def _bind_shortcuts(self) -> None:
        self.root.bind("<Control-s>", lambda e: self.save_dialog())
        self.root.bind("<Control-S>", lambda e: self.save_dialog())
        self.root.bind("<Control-z>", lambda e: self.undo())
        self.root.bind("<Control-Z>", lambda e: self.undo())
        self.root.bind("<Delete>", lambda e: self.clear_canvas())

    # ---------------- Event Handlers ---------------- #
    def on_press(self, event: tk.Event) -> None:
        """Record anchor point and reset stroke state."""
        self.start_x, self.start_y = event.x, event.y
        self.last_x, self.last_y = event.x, event.y
        self.current_stroke_ids.clear()
        if self.preview_item is not None:
            self.canvas.delete(self.preview_item)
            self.preview_item = None

    def on_drag(self, event: tk.Event) -> None:
        shape = self._shape()
        stroke = int(self.stroke_var.get())
        color = self.current_color
        if shape == "freehand":
            if self.last_x is not None and self.last_y is not None:
                line_id = self.canvas.create_line(
                    self.last_x,
                    self.last_y,
                    event.x,
                    event.y,
                    fill=color,
                    width=stroke,
                    capstyle=tk.ROUND,
                    smooth=True,
                )
                self.current_stroke_ids.append(line_id)
            self.last_x, self.last_y = event.x, event.y
            return
        # Geometric shape preview
        if self.start_x is None or self.start_y is None:
            return
        if self.preview_item is not None:
            self.canvas.delete(self.preview_item)
        fill = color if self.fill_var.get() else ""
        if shape == "line":
            self.preview_item = self.canvas.create_line(
                self.start_x, self.start_y, event.x, event.y, fill=color, width=stroke
            )
        elif shape == "rectangle":
            self.preview_item = self.canvas.create_rectangle(
                self.start_x,
                self.start_y,
                event.x,
                event.y,
                outline=color,
                width=stroke,
                fill=fill,
            )
        elif shape == "oval":
            self.preview_item = self.canvas.create_oval(
                self.start_x,
                self.start_y,
                event.x,
                event.y,
                outline=color,
                width=stroke,
                fill=fill,
            )

    def on_release(self, event: tk.Event) -> None:
        shape = self._shape()
        stroke = int(self.stroke_var.get())
        color = self.current_color
        if shape == "freehand":
            # Group entire stroke as single history entry
            if self.current_stroke_ids:
                self.history.append(self.current_stroke_ids.copy())
            self.current_stroke_ids.clear()
            self.last_x = self.last_y = None
            return
        if self.start_x is None or self.start_y is None:
            return
        if self.preview_item is not None:
            self.canvas.delete(self.preview_item)
            self.preview_item = None
        fill = color if self.fill_var.get() else ""
        ids: List[int] = []
        if shape == "line":
            ids.append(
                self.canvas.create_line(
                    self.start_x,
                    self.start_y,
                    event.x,
                    event.y,
                    fill=color,
                    width=stroke,
                )
            )
        elif shape == "rectangle":
            ids.append(
                self.canvas.create_rectangle(
                    self.start_x,
                    self.start_y,
                    event.x,
                    event.y,
                    outline=color,
                    width=stroke,
                    fill=fill,
                )
            )
        elif shape == "oval":
            ids.append(
                self.canvas.create_oval(
                    self.start_x,
                    self.start_y,
                    event.x,
                    event.y,
                    outline=color,
                    width=stroke,
                    fill=fill,
                )
            )
        if ids:
            self.history.append(ids)
        self.start_x = self.start_y = None
        self._set_status(f"Shapes: {len(self.history)}")

    # ---------------- Commands ---------------- #
    def choose_color(self) -> None:
        color = colorchooser.askcolor(initialcolor=self.current_color)[1]
        if color:
            self.current_color = color
            self.color_btn.configure(fg=color)

    def undo(self) -> None:
        if not self.history:
            self._set_status("Nothing to undo")
            return
        ids = self.history.pop()
        for _id in ids:
            self.canvas.delete(_id)
        self._set_status("Undid last action")

    def clear_canvas(self) -> None:
        self.canvas.delete("all")
        self.history.clear()
        self._set_status("Canvas cleared")

    def save_dialog(self) -> None:
        filetypes = [
            ("PNG Image", "*.png"),
            ("PostScript", "*.ps"),
            ("All Files", "*.*"),
        ]
        path_str = filedialog.asksaveasfilename(
            defaultextension=".png", filetypes=filetypes, title="Save Image"
        )
        if not path_str:
            return
        path = Path(path_str)
        try:
            self._save_image(path)
            self._set_status(f"Saved {path.name}")
        except Exception as e:  # pragma: no cover
            messagebox.showerror("Save Failed", f"Could not save file:\n{e}")
            self._set_status("Save failed")

    # ---------------- Saving Logic ---------------- #
    def _save_image(self, path: Path) -> None:
        # Always produce a PostScript first (vector). Then convert to PNG if requested & Pillow present.
        # If Pillow not available and user asked for PNG, we store a PS file next to it.
        ps_temp = path.with_suffix(".ps") if path.suffix.lower() == ".png" else path
        self.canvas.postscript(file=ps_temp, colormode="color")
        if path.suffix.lower() != ".png":
            return  # user wants .ps
        if not PIL_AVAILABLE:
            messagebox.showwarning(
                "Pillow Missing",
                "Pillow not installed; saved PostScript instead of PNG.",
            )
            return
        # Convert PS -> PNG
        assert PIL_AVAILABLE and Image is not None  # for type checkers
        img = Image.open(ps_temp)
        img.save(path, "PNG")
        # Optionally remove ps intermediate if different name
        if ps_temp != path and ps_temp.exists():
            try:  # best-effort cleanup
                ps_temp.unlink()
            except OSError:
                pass

    # ---------------- Helpers ---------------- #
    def _shape(self) -> str:
        val = self.shape_var.get().strip().lower()
        if val not in self.VALID_SHAPES:
            val = "freehand"
            self.shape_var.set(val)
        return val

    def _set_status(self, text: str) -> None:
        self.status.configure(text=text)

    # ---------------- Run Loop ---------------- #


def main() -> int:
    root = tk.Tk()
    app = PaintApp(root)
    root.mainloop()
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
