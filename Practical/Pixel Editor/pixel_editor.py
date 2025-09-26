"""Tkinter pixel art editor GUI."""

from __future__ import annotations

import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple

try:  # Allow running as a script without installation
    if __package__ is None or __package__ == "":
        sys.path.append(str(Path(__file__).resolve().parent))
except NameError:  # pragma: no cover - __file__ may be undefined in some environments
    pass

import tkinter as tk
from tkinter import colorchooser, filedialog, messagebox, simpledialog, ttk

from PIL import Image, ImageTk

from core import PaletteIndex, PixelDocument, TRANSPARENT, composite_to_rgba
from io_utils import SpriteSheetSpec, export_gif, export_png_sprite_sheet, import_sprite_sheet


@dataclass
class PixelAction:
    frame_index: int
    layer_index: int
    x: int
    y: int
    before: PaletteIndex
    after: PaletteIndex

    def undo(self, document: PixelDocument) -> None:
        document.frames[self.frame_index].layers[self.layer_index].set_pixel(self.x, self.y, self.before)

    def redo(self, document: PixelDocument) -> None:
        document.frames[self.frame_index].layers[self.layer_index].set_pixel(self.x, self.y, self.after)


class UndoManager:
    def __init__(self) -> None:
        self._undo_stack: List[PixelAction] = []
        self._redo_stack: List[PixelAction] = []

    def push(self, action: PixelAction) -> None:
        self._undo_stack.append(action)
        self._redo_stack.clear()

    def can_undo(self) -> bool:
        return bool(self._undo_stack)

    def can_redo(self) -> bool:
        return bool(self._redo_stack)

    def undo(self, document: PixelDocument) -> Optional[PixelAction]:
        if not self._undo_stack:
            return None
        action = self._undo_stack.pop()
        action.undo(document)
        self._redo_stack.append(action)
        return action

    def redo(self, document: PixelDocument) -> Optional[PixelAction]:
        if not self._redo_stack:
            return None
        action = self._redo_stack.pop()
        action.redo(document)
        self._undo_stack.append(action)
        return action


class PixelEditorApp(tk.Tk):
    def __init__(self) -> None:
        super().__init__()
        self.title("Pixel Editor")
        self.geometry("1100x720")

        self.document = PixelDocument(32, 32)
        self.current_frame_index = 0
        self.current_layer_index = 0
        self.current_color_index: PaletteIndex = 1  # white by default
        self.tool = tk.StringVar(value="pen")
        self.zoom = tk.IntVar(value=16)
        self.grid_visible = tk.BooleanVar(value=True)
        self.onion_skin = tk.BooleanVar(value=False)
        self.animation_delay = tk.IntVar(value=120)
        self.is_playing = False
        self.undo_manager = UndoManager()
        self._photo: Optional[ImageTk.PhotoImage] = None

        self._build_ui()
        self._bind_shortcuts()
        self._refresh_all()

    # ------------------------------------------------------------------ UI BUILDERS
    def _build_ui(self) -> None:
        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=1)

        root = ttk.Frame(self)
        root.grid(row=0, column=0, sticky="nsew")
        root.columnconfigure(0, weight=3)
        root.columnconfigure(1, weight=1)
        root.rowconfigure(0, weight=1)

        # Canvas area
        canvas_frame = ttk.Frame(root)
        canvas_frame.grid(row=0, column=0, sticky="nsew")
        canvas_frame.columnconfigure(0, weight=1)
        canvas_frame.rowconfigure(0, weight=1)

        self.canvas = tk.Canvas(canvas_frame, background="#1e1e1e")
        self.canvas.grid(row=0, column=0, sticky="nsew")
        self.canvas.bind("<ButtonPress-1>", self._on_canvas_press)
        self.canvas.bind("<B1-Motion>", self._on_canvas_drag)
        self.canvas.bind("<ButtonPress-3>", self._on_canvas_press)
        self.canvas.bind("<B3-Motion>", self._on_canvas_drag)
        self.canvas.bind("<Motion>", self._on_canvas_hover)
        self.canvas.bind("<Leave>", lambda _: self.status_var.set(""))
        self.canvas.bind("<MouseWheel>", self._on_mouse_wheel)

        # Controls column
        sidebar = ttk.Notebook(root)
        sidebar.grid(row=0, column=1, sticky="nsew")

        palette_tab = ttk.Frame(sidebar)
        layers_tab = ttk.Frame(sidebar)
        frames_tab = ttk.Frame(sidebar)
        settings_tab = ttk.Frame(sidebar)
        sidebar.add(palette_tab, text="Palette")
        sidebar.add(layers_tab, text="Layers")
        sidebar.add(frames_tab, text="Frames")
        sidebar.add(settings_tab, text="Settings")

        # Palette tab
        palette_tab.columnconfigure(0, weight=1)
        self.palette_list = tk.Listbox(palette_tab, exportselection=False)
        self.palette_list.grid(row=0, column=0, sticky="nsew", padx=4, pady=4)
        self.palette_list.bind("<<ListboxSelect>>", self._on_palette_select)

        palette_buttons = ttk.Frame(palette_tab)
        palette_buttons.grid(row=1, column=0, sticky="ew", padx=4, pady=4)
        ttk.Button(palette_buttons, text="Add", command=self._add_color).grid(row=0, column=0, padx=2, pady=2)
        ttk.Button(palette_buttons, text="Remove", command=self._remove_color).grid(row=0, column=1, padx=2, pady=2)
        ttk.Button(palette_buttons, text="Up", command=lambda: self._shift_palette(-1)).grid(row=0, column=2, padx=2, pady=2)
        ttk.Button(palette_buttons, text="Down", command=lambda: self._shift_palette(1)).grid(row=0, column=3, padx=2, pady=2)

        # Layers tab
        layers_tab.columnconfigure(0, weight=1)
        self.layers_list = tk.Listbox(layers_tab, exportselection=False)
        self.layers_list.grid(row=0, column=0, sticky="nsew", padx=4, pady=4)
        self.layers_list.bind("<<ListboxSelect>>", self._on_layer_select)
        self.layers_list.bind("<Double-1>", lambda _: self._rename_layer())

        layer_buttons = ttk.Frame(layers_tab)
        layer_buttons.grid(row=1, column=0, sticky="ew", padx=4, pady=4)
        ttk.Button(layer_buttons, text="Add", command=self._add_layer).grid(row=0, column=0, padx=2, pady=2)
        ttk.Button(layer_buttons, text="Duplicate", command=self._duplicate_layer).grid(row=0, column=1, padx=2, pady=2)
        ttk.Button(layer_buttons, text="Delete", command=self._delete_layer).grid(row=0, column=2, padx=2, pady=2)
        ttk.Button(layer_buttons, text="Up", command=lambda: self._shift_layer(-1)).grid(row=1, column=0, padx=2, pady=2)
        ttk.Button(layer_buttons, text="Down", command=lambda: self._shift_layer(1)).grid(row=1, column=1, padx=2, pady=2)
        ttk.Button(layer_buttons, text="Toggle", command=self._toggle_layer_visibility).grid(row=1, column=2, padx=2, pady=2)

        # Frames tab
        frames_tab.columnconfigure(0, weight=1)
        self.frames_list = tk.Listbox(frames_tab, exportselection=False)
        self.frames_list.grid(row=0, column=0, sticky="nsew", padx=4, pady=4)
        self.frames_list.bind("<<ListboxSelect>>", self._on_frame_select)
        self.frames_list.bind("<Double-1>", lambda _: self._rename_frame())

        frame_buttons = ttk.Frame(frames_tab)
        frame_buttons.grid(row=1, column=0, sticky="ew", padx=4, pady=4)
        ttk.Button(frame_buttons, text="Add", command=self._add_frame).grid(row=0, column=0, padx=2, pady=2)
        ttk.Button(frame_buttons, text="Duplicate", command=self._duplicate_frame).grid(row=0, column=1, padx=2, pady=2)
        ttk.Button(frame_buttons, text="Delete", command=self._delete_frame).grid(row=0, column=2, padx=2, pady=2)
        ttk.Button(frame_buttons, text="Play", command=self._toggle_playback).grid(row=1, column=0, padx=2, pady=2)
        ttk.Button(frame_buttons, text="Prev", command=lambda: self._step_frame(-1)).grid(row=1, column=1, padx=2, pady=2)
        ttk.Button(frame_buttons, text="Next", command=lambda: self._step_frame(1)).grid(row=1, column=2, padx=2, pady=2)

        # Settings tab
        settings_tab.columnconfigure(0, weight=1)
        zoom_frame = ttk.Frame(settings_tab)
        zoom_frame.grid(row=0, column=0, sticky="ew", padx=4, pady=4)
        ttk.Label(zoom_frame, text="Zoom").grid(row=0, column=0, sticky="w")
        zoom_scale = ttk.Scale(zoom_frame, from_=1, to=64, orient="horizontal", command=self._on_zoom_change)
        zoom_scale.set(self.zoom.get())
        zoom_scale.grid(row=1, column=0, sticky="ew")

        ttk.Checkbutton(settings_tab, text="Show Grid", variable=self.grid_visible, command=self._render_canvas).grid(
            row=1, column=0, sticky="w", padx=4, pady=4
        )
        ttk.Checkbutton(settings_tab, text="Onion Skin", variable=self.onion_skin, command=self._render_canvas).grid(
            row=2, column=0, sticky="w", padx=4, pady=4
        )
        ttk.Label(settings_tab, text="Playback delay (ms)").grid(row=3, column=0, sticky="w", padx=4, pady=4)
        ttk.Spinbox(settings_tab, from_=30, to=1000, increment=10, textvariable=self.animation_delay).grid(
            row=4, column=0, sticky="ew", padx=4, pady=4
        )

        tool_frame = ttk.Frame(settings_tab)
        tool_frame.grid(row=5, column=0, sticky="ew", padx=4, pady=4)
        ttk.Label(tool_frame, text="Tool").grid(row=0, column=0, sticky="w")
        ttk.Radiobutton(tool_frame, text="Pen", value="pen", variable=self.tool).grid(row=1, column=0, sticky="w")
        ttk.Radiobutton(tool_frame, text="Eraser", value="eraser", variable=self.tool).grid(row=1, column=1, sticky="w")

        export_frame = ttk.Frame(settings_tab)
        export_frame.grid(row=6, column=0, sticky="ew", padx=4, pady=4)
        ttk.Button(export_frame, text="Export PNG", command=self._export_png).grid(row=0, column=0, padx=2, pady=2)
        ttk.Button(export_frame, text="Export GIF", command=self._export_gif).grid(row=0, column=1, padx=2, pady=2)
        ttk.Button(export_frame, text="Import Sprite Sheet", command=self._import_sprite_sheet).grid(
            row=1, column=0, columnspan=2, padx=2, pady=2
        )

        # Status bar
        self.status_var = tk.StringVar(value="")
        status = ttk.Label(self, textvariable=self.status_var, anchor="w")
        status.grid(row=1, column=0, sticky="ew", padx=4, pady=2)

        # Menu
        menu_bar = tk.Menu(self)
        self.config(menu=menu_bar)

        file_menu = tk.Menu(menu_bar, tearoff=False)
        file_menu.add_command(label="New", command=self._new_project)
        file_menu.add_command(label="Save Project", command=self._save_project)
        file_menu.add_command(label="Open Project", command=self._open_project)
        file_menu.add_separator()
        file_menu.add_command(label="Export PNG", command=self._export_png)
        file_menu.add_command(label="Export GIF", command=self._export_gif)
        file_menu.add_separator()
        file_menu.add_command(label="Quit", command=self.destroy)
        menu_bar.add_cascade(label="File", menu=file_menu)

    # ------------------------------------------------------------------ SHORTCUTS
    def _bind_shortcuts(self) -> None:
        self.bind("<Control-z>", lambda event: self._undo())
        self.bind("<Control-Shift-Z>", lambda event: self._redo())
        self.bind("<Control-y>", lambda event: self._redo())
        self.bind("e", lambda event: self._toggle_tool())
        self.bind("<Control-e>", lambda event: self._toggle_tool())
        self.bind(".", lambda event: self._step_frame(1))
        self.bind(",", lambda event: self._step_frame(-1))
        self.bind("<space>", lambda event: self._toggle_playback())
        self.bind("<Control-plus>", lambda event: self._increment_zoom(1))
        self.bind("<Control-minus>", lambda event: self._increment_zoom(-1))

    # ------------------------------------------------------------------ PALETTE OPS
    def _on_palette_select(self, event: tk.Event[tk.Listbox]) -> None:  # type: ignore[name-defined]
        selection = self.palette_list.curselection()
        if selection:
            self.current_color_index = selection[0]

    def _add_color(self) -> None:
        color = colorchooser.askcolor(title="Pick color")
        if not color or not color[1]:
            return
        index = self.document.add_palette_color(color[1])
        self.current_color_index = index
        self._refresh_palette()
        self._render_canvas()

    def _remove_color(self) -> None:
        selection = self.palette_list.curselection()
        if not selection:
            return
        index = selection[0]
        try:
            self.document.remove_palette_color(index)
        except IndexError:
            return
        self.current_color_index = max(0, min(self.current_color_index, len(self.document.palette.colors) - 1))
        self._refresh_palette()
        self._render_canvas()

    def _shift_palette(self, direction: int) -> None:
        selection = self.palette_list.curselection()
        if not selection:
            return
        index = selection[0]
        target = index + direction
        if 0 <= target < len(self.document.palette.colors):
            self.document.swap_palette_colors(index, target)
            self.current_color_index = target
            self._refresh_palette()
            self.palette_list.selection_set(target)
            self._render_canvas()

    # ------------------------------------------------------------------ LAYER OPS
    def _refresh_layers(self) -> None:
        self.layers_list.delete(0, tk.END)
        frame = self.document.frames[self.current_frame_index]
        for i, layer in enumerate(frame.layers):
            visibility = "👁" if layer.visible else "🚫"
            label = f"{visibility} {layer.name}"
            self.layers_list.insert(tk.END, label)
        if frame.layers:
            index = min(self.current_layer_index, len(frame.layers) - 1)
            self.layers_list.selection_set(index)
            self.current_layer_index = index

    def _on_layer_select(self, event: tk.Event[tk.Listbox]) -> None:  # type: ignore[name-defined]
        selection = self.layers_list.curselection()
        if selection:
            self.current_layer_index = selection[0]

    def _add_layer(self) -> None:
        frame = self.document.frames[self.current_frame_index]
        layer = frame.add_layer()
        self.current_layer_index = frame.layers.index(layer)
        self._refresh_layers()
        self._render_canvas()

    def _duplicate_layer(self) -> None:
        frame = self.document.frames[self.current_frame_index]
        original = frame.layers[self.current_layer_index]
        clone = original.clone(name=f"{original.name} copy")
        frame.layers.insert(self.current_layer_index + 1, clone)
        self.current_layer_index += 1
        self._refresh_layers()
        self._render_canvas()

    def _delete_layer(self) -> None:
        frame = self.document.frames[self.current_frame_index]
        try:
            frame.remove_layer(self.current_layer_index)
        except ValueError as exc:
            messagebox.showwarning("Layer", str(exc))
            return
        self.current_layer_index = max(0, self.current_layer_index - 1)
        self._refresh_layers()
        self._render_canvas()

    def _shift_layer(self, direction: int) -> None:
        frame = self.document.frames[self.current_frame_index]
        index = self.current_layer_index
        target = index + direction
        if 0 <= target < len(frame.layers):
            frame.layers[index], frame.layers[target] = frame.layers[target], frame.layers[index]
            self.current_layer_index = target
            self._refresh_layers()
            self._render_canvas()

    def _toggle_layer_visibility(self) -> None:
        frame = self.document.frames[self.current_frame_index]
        layer = frame.layers[self.current_layer_index]
        layer.visible = not layer.visible
        self._refresh_layers()
        self._render_canvas()

    def _rename_layer(self) -> None:
        frame = self.document.frames[self.current_frame_index]
        layer = frame.layers[self.current_layer_index]
        name = simpledialog.askstring("Rename Layer", "Name", initialvalue=layer.name)
        if name:
            layer.name = name
            self._refresh_layers()

    # ------------------------------------------------------------------ FRAME OPS
    def _refresh_frames(self) -> None:
        self.frames_list.delete(0, tk.END)
        for idx, _frame in enumerate(self.document.frames):
            self.frames_list.insert(tk.END, f"Frame {idx + 1}")
        if self.document.frames:
            index = min(self.current_frame_index, len(self.document.frames) - 1)
            self.frames_list.selection_set(index)
            self.current_frame_index = index

    def _on_frame_select(self, event: tk.Event[tk.Listbox]) -> None:  # type: ignore[name-defined]
        selection = self.frames_list.curselection()
        if selection:
            self.current_frame_index = selection[0]
            self.current_layer_index = min(self.current_layer_index, len(self.document.frames[self.current_frame_index].layers) - 1)
            self._refresh_layers()
            self._render_canvas()

    def _add_frame(self) -> None:
        frame = self.document.add_frame(clone_index=self.current_frame_index)
        self.current_frame_index = self.document.frames.index(frame)
        self._refresh_frames()
        self.frames_list.selection_set(self.current_frame_index)
        self._render_canvas()

    def _duplicate_frame(self) -> None:
        self._add_frame()

    def _delete_frame(self) -> None:
        try:
            self.document.remove_frame(self.current_frame_index)
        except ValueError as exc:
            messagebox.showwarning("Frame", str(exc))
            return
        self.current_frame_index = max(0, self.current_frame_index - 1)
        self._refresh_frames()
        self._render_canvas()

    def _rename_frame(self) -> None:
        name = simpledialog.askstring("Rename Frame", "Label", initialvalue=self.frames_list.get(self.current_frame_index))
        if name:
            self.frames_list.delete(self.current_frame_index)
            self.frames_list.insert(self.current_frame_index, name)

    def _toggle_playback(self) -> None:
        self.is_playing = not self.is_playing
        if self.is_playing:
            self._play_next_frame()

    def _play_next_frame(self) -> None:
        if not self.is_playing:
            return
        self.current_frame_index = (self.current_frame_index + 1) % len(self.document.frames)
        self._refresh_frames()
        self._render_canvas()
        delay = max(30, self.animation_delay.get())
        self.after(delay, self._play_next_frame)

    def _step_frame(self, direction: int) -> None:
        self.current_frame_index = (self.current_frame_index + direction) % len(self.document.frames)
        self._refresh_frames()
        self._render_canvas()

    # ------------------------------------------------------------------ CANVAS RENDERING
    def _render_canvas(self) -> None:
        composite = self.document.composite_frame(self.current_frame_index)
        rgba = composite_to_rgba(composite, self.document.palette)

        if self.onion_skin.get() and len(self.document.frames) > 1:
            prev_index = (self.current_frame_index - 1) % len(self.document.frames)
            prev = self.document.composite_frame(prev_index)
            for y in range(len(prev)):
                for x in range(len(prev[0])):
                    if rgba[y][x][3] == 0 and prev[y][x] != TRANSPARENT:
                        color = self.document.palette.get(prev[y][x])
                        r = int(color[1:3], 16)
                        g = int(color[3:5], 16)
                        b = int(color[5:7], 16)
                        rgba[y][x] = ((r + 255) // 2, (g + 255) // 2, (b + 255) // 2, 160)

        base = Image.new("RGBA", (self.document.width, self.document.height))
        flat = [tuple(pixel) for row in rgba for pixel in row]
        base.putdata(flat)
        zoomed = base.resize((self.document.width * self.zoom.get(), self.document.height * self.zoom.get()), Image.NEAREST)

        self.canvas.delete("all")
        self._photo = ImageTk.PhotoImage(zoomed)
        self.canvas.config(width=zoomed.width, height=zoomed.height)
        self.canvas.create_image(0, 0, anchor="nw", image=self._photo)

        if self.grid_visible.get() and self.zoom.get() >= 4:
            w = zoomed.width
            h = zoomed.height
            for x in range(0, w + 1, self.zoom.get()):
                self.canvas.create_line(x, 0, x, h, fill="#333333")
            for y in range(0, h + 1, self.zoom.get()):
                self.canvas.create_line(0, y, w, y, fill="#333333")

    # ------------------------------------------------------------------ CANVAS INTERACTION
    def _canvas_to_pixel(self, event: tk.Event[tk.Canvas]) -> Optional[Tuple[int, int]]:  # type: ignore[name-defined]
        x = int(event.x // self.zoom.get())
        y = int(event.y // self.zoom.get())
        if 0 <= x < self.document.width and 0 <= y < self.document.height:
            return x, y
        return None

    def _on_canvas_hover(self, event: tk.Event[tk.Canvas]) -> None:  # type: ignore[name-defined]
        coords = self._canvas_to_pixel(event)
        if coords:
            self.status_var.set(f"x={coords[0]} y={coords[1]} frame={self.current_frame_index + 1}")
        else:
            self.status_var.set("")

    def _on_canvas_press(self, event: tk.Event[tk.Canvas]) -> None:  # type: ignore[name-defined]
        coords = self._canvas_to_pixel(event)
        if not coords:
            return
        if event.state & 0x0008:  # Alt for eyedropper
            self._sample_color(*coords)
            return
        if event.num == 3:
            self._apply_pixel(coords[0], coords[1], TRANSPARENT)
        else:
            value = self.current_color_index if self.tool.get() == "pen" else TRANSPARENT
            self._apply_pixel(coords[0], coords[1], value)

    def _on_canvas_drag(self, event: tk.Event[tk.Canvas]) -> None:  # type: ignore[name-defined]
        coords = self._canvas_to_pixel(event)
        if not coords:
            return
        value = self.current_color_index if (self.tool.get() == "pen" and event.num == 1) else TRANSPARENT
        self._apply_pixel(coords[0], coords[1], value, record_undo=False)

    def _apply_pixel(self, x: int, y: int, value: PaletteIndex, record_undo: bool = True) -> None:
        frame = self.document.frames[self.current_frame_index]
        layer = frame.layers[self.current_layer_index]
        previous = layer.get_pixel(x, y)
        if previous == value:
            return
        layer.set_pixel(x, y, value)
        if record_undo:
            action = PixelAction(self.current_frame_index, self.current_layer_index, x, y, previous, value)
            self.undo_manager.push(action)
        self._render_canvas()

    def _sample_color(self, x: int, y: int) -> None:
        composite = self.document.composite_frame(self.current_frame_index)
        value = composite[y][x]
        if value != TRANSPARENT:
            self.current_color_index = value
            self.palette_list.selection_clear(0, tk.END)
            self.palette_list.selection_set(value)

    def _on_mouse_wheel(self, event: tk.Event[tk.Canvas]) -> None:  # type: ignore[name-defined]
        delta = 1 if event.delta > 0 else -1
        self._increment_zoom(delta)

    def _increment_zoom(self, delta: int) -> None:
        new_zoom = min(64, max(1, self.zoom.get() + delta))
        self.zoom.set(new_zoom)
        self._render_canvas()

    def _on_zoom_change(self, value: str) -> None:
        try:
            zoom = int(float(value))
        except ValueError:
            return
        self.zoom.set(max(1, min(64, zoom)))
        self._render_canvas()

    # ------------------------------------------------------------------ UNDO/REDO
    def _undo(self) -> None:
        if self.undo_manager.undo(self.document):
            self._render_canvas()

    def _redo(self) -> None:
        if self.undo_manager.redo(self.document):
            self._render_canvas()

    def _toggle_tool(self) -> None:
        self.tool.set("eraser" if self.tool.get() == "pen" else "pen")

    # ------------------------------------------------------------------ FILE OPS
    def _new_project(self) -> None:
        width = simpledialog.askinteger("New", "Width", initialvalue=self.document.width, minvalue=1, maxvalue=256)
        if width is None:
            return
        height = simpledialog.askinteger("New", "Height", initialvalue=self.document.height, minvalue=1, maxvalue=256)
        if height is None:
            return
        frames = simpledialog.askinteger("New", "Frame count", initialvalue=len(self.document.frames), minvalue=1, maxvalue=64)
        if frames is None:
            return
        self.document = PixelDocument(width, height)
        for _ in range(frames - 1):
            self.document.add_frame()
        self.current_frame_index = 0
        self.current_layer_index = 0
        self.undo_manager = UndoManager()
        self._refresh_all()

    def _export_png(self) -> None:
        path = filedialog.asksaveasfilename(defaultextension=".png", filetypes=[("PNG", "*.png")])
        if not path:
            return
        try:
            export_png_sprite_sheet(self.document, Path(path))
        except Exception as exc:
            messagebox.showerror("Export", str(exc))

    def _export_gif(self) -> None:
        path = filedialog.asksaveasfilename(defaultextension=".gif", filetypes=[("GIF", "*.gif")])
        if not path:
            return
        try:
            export_gif(self.document, Path(path), duration=self.animation_delay.get())
        except Exception as exc:
            messagebox.showerror("Export", str(exc))

    def _import_sprite_sheet(self) -> None:
        path = filedialog.askopenfilename(filetypes=[("Images", "*.png;*.gif;*.bmp;*.jpg;*.jpeg")])
        if not path:
            return
        frame_width = simpledialog.askinteger("Import", "Frame width", initialvalue=self.document.width, minvalue=1)
        if frame_width is None:
            return
        frame_height = simpledialog.askinteger("Import", "Frame height", initialvalue=self.document.height, minvalue=1)
        if frame_height is None:
            return
        spec = SpriteSheetSpec(frame_width, frame_height)
        try:
            document = import_sprite_sheet(Path(path), spec, palette=self.document.palette)
        except Exception as exc:
            messagebox.showerror("Import", str(exc))
            return
        self.document = document
        self.current_frame_index = 0
        self.current_layer_index = 0
        self.undo_manager = UndoManager()
        self._refresh_all()

    def _save_project(self) -> None:
        path = filedialog.asksaveasfilename(defaultextension=".json", filetypes=[("Pixel Project", "*.json")])
        if not path:
            return
        data = {
            "width": self.document.width,
            "height": self.document.height,
            "palette": self.document.palette.colors,
            "frames": [
                {
                    "layers": [
                        {
                            "name": layer.name,
                            "visible": layer.visible,
                            "pixels": layer.pixels,
                        }
                        for layer in frame.layers
                    ]
                }
                for frame in self.document.frames
            ],
        }
        Path(path).write_text(json.dumps(data))

    def _open_project(self) -> None:
        path = filedialog.askopenfilename(filetypes=[("Pixel Project", "*.json")])
        if not path:
            return
        try:
            data = json.loads(Path(path).read_text())
        except Exception as exc:
            messagebox.showerror("Open", f"Failed to load project: {exc}")
            return

        try:
            document = PixelDocument(data["width"], data["height"], frames=[])
            document.palette.colors = data.get("palette", document.palette.colors)
            for frame_data in data["frames"]:
                frame = document.add_frame()
                frame.layers.clear()
                for layer_data in frame_data["layers"]:
                    layer = frame.add_layer(name=layer_data.get("name", "Layer"))
                    layer.visible = layer_data.get("visible", True)
                    layer.pixels = [row[:] for row in layer_data["pixels"]]
            document.frames.pop(0)  # remove the placeholder frame created by add_frame()
        except Exception as exc:
            messagebox.showerror("Open", f"Invalid project file: {exc}")
            return

        self.document = document
        self.current_frame_index = 0
        self.current_layer_index = 0
        self.undo_manager = UndoManager()
        self._refresh_all()

    # ------------------------------------------------------------------ REFRESH HELPERS
    def _refresh_palette(self) -> None:
        self.palette_list.delete(0, tk.END)
        for color in self.document.palette.colors:
            self.palette_list.insert(tk.END, color)
        if self.document.palette.colors:
            index = min(self.current_color_index, len(self.document.palette.colors) - 1)
            self.palette_list.selection_set(index)
            self.current_color_index = index

    def _refresh_all(self) -> None:
        self._refresh_palette()
        self._refresh_layers()
        self._refresh_frames()
        self._render_canvas()


def main() -> None:
    app = PixelEditorApp()
    app.mainloop()


if __name__ == "__main__":
    main()
