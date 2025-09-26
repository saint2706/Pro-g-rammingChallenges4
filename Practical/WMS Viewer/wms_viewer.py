"""Tkinter-based desktop client for the WMS viewer challenge."""
from __future__ import annotations

import io
from pathlib import Path
from typing import Optional

from PIL import Image, ImageTk
import tkinter as tk
from tkinter import ttk, messagebox

from wms_engine import (
    AppConfig,
    LayerConfig,
    MapViewState,
    Projection,
    ServerConfig,
    WMSCache,
    WMSClient,
    ensure_cache_dir,
    load_config,
)


class WMSViewerApp:
    """Main application controller for the Tkinter GUI."""

    def __init__(self, root: tk.Tk, config: AppConfig, cache_path: Path) -> None:
        self.root = root
        self.root.title("OGC WMS Desktop Viewer")
        self.config = config
        self.cache = WMSCache(cache_path)
        self.client = WMSClient(self.cache)

        self._server_var = tk.StringVar()
        self._layer_var = tk.StringVar()

        self._projection: Optional[Projection] = None
        self._view_state: Optional[MapViewState] = None
        self._current_image: Optional[ImageTk.PhotoImage] = None
        self._drag_last: Optional[tuple[int, int]] = None

        self._build_ui()
        self._initialize_state()
        self._update_map()

    # ------------------------------------------------------------------ UI setup
    def _build_ui(self) -> None:
        control_frame = ttk.Frame(self.root, padding=4)
        control_frame.pack(side=tk.TOP, fill=tk.X)

        ttk.Label(control_frame, text="Server:").pack(side=tk.LEFT)
        self.server_combo = ttk.Combobox(
            control_frame,
            textvariable=self._server_var,
            state="readonly",
            values=[server.name for server in self.config.servers],
        )
        self.server_combo.pack(side=tk.LEFT, padx=(4, 12))
        self.server_combo.bind("<<ComboboxSelected>>", self._on_server_change)

        ttk.Label(control_frame, text="Layer:").pack(side=tk.LEFT)
        self.layer_combo = ttk.Combobox(control_frame, textvariable=self._layer_var, state="readonly")
        self.layer_combo.pack(side=tk.LEFT, padx=(4, 12))
        self.layer_combo.bind("<<ComboboxSelected>>", self._on_layer_change)

        ttk.Button(control_frame, text="+", width=3, command=lambda: self._on_zoom(1)).pack(side=tk.LEFT)
        ttk.Button(control_frame, text="-", width=3, command=lambda: self._on_zoom(-1)).pack(side=tk.LEFT, padx=(4, 0))

        self.status_var = tk.StringVar(value="")
        self.status_label = ttk.Label(self.root, textvariable=self.status_var, anchor=tk.W)
        self.status_label.pack(side=tk.BOTTOM, fill=tk.X, padx=4, pady=2)

        self.canvas = tk.Canvas(self.root, width=self.config.view.width, height=self.config.view.height, bg="#1f1f1f")
        self.canvas.pack(fill=tk.BOTH, expand=True)
        self.canvas.bind("<Configure>", self._on_resize)
        self.canvas.bind("<ButtonPress-1>", self._on_drag_start)
        self.canvas.bind("<B1-Motion>", self._on_drag_move)
        self.canvas.bind("<ButtonRelease-1>", self._on_drag_end)
        self.canvas.bind("<MouseWheel>", self._on_mouse_wheel)
        # Linux scroll events
        self.canvas.bind("<Button-4>", lambda event: self._on_zoom(1))
        self.canvas.bind("<Button-5>", lambda event: self._on_zoom(-1))

    # ------------------------------------------------------------------ State init
    def _initialize_state(self) -> None:
        if not self.config.servers:
            messagebox.showerror("Configuration error", "No servers configured")
            self.root.destroy()
            return
        default_server = self.config.servers[0]
        self._server_var.set(default_server.name)
        self._populate_layers(default_server)
        self._projection = Projection(default_server.crs)
        self._view_state = MapViewState(
            width=self.config.view.width,
            height=self.config.view.height,
            projection=self._projection,
            zoom=self.config.view.default_zoom,
            min_zoom=self.config.view.min_zoom,
            max_zoom=self.config.view.max_zoom,
            center_lon=self.config.view.default_center[0],
            center_lat=self.config.view.default_center[1],
        )

    # ------------------------------------------------------------------ Helpers
    def _populate_layers(self, server: ServerConfig) -> None:
        self.layer_combo["values"] = [layer.title or layer.name for layer in server.layers]
        default_layer = server.layers[0]
        self._layer_var.set(default_layer.title or default_layer.name)

    def _current_server(self) -> ServerConfig:
        name = self._server_var.get()
        for server in self.config.servers:
            if server.name == name:
                return server
        return self.config.servers[0]

    def _current_layer(self) -> LayerConfig:
        title = self._layer_var.get()
        server = self._current_server()
        for layer in server.layers:
            if layer.title == title or layer.name == title:
                return layer
        return server.layers[0]

    # ------------------------------------------------------------------ Event handlers
    def _on_server_change(self, event: tk.Event) -> None:  # type: ignore[override]
        server = self._current_server()
        self._projection = Projection(server.crs)
        if self._view_state:
            lon, lat = self._view_state.center_lonlat()
            self._view_state = MapViewState(
                width=self._view_state.width,
                height=self._view_state.height,
                projection=self._projection,
                zoom=self._view_state.zoom,
                min_zoom=self.config.view.min_zoom,
                max_zoom=self.config.view.max_zoom,
                center_lon=lon,
                center_lat=lat,
            )
        else:
            self._view_state = MapViewState(
                width=self.config.view.width,
                height=self.config.view.height,
                projection=self._projection,
            )
        self._populate_layers(server)
        self._update_map()

    def _on_layer_change(self, event: tk.Event) -> None:  # type: ignore[override]
        self._update_map()

    def _on_zoom(self, delta: int) -> None:
        if not self._view_state:
            return
        self._view_state.zoom_by(delta)
        self._update_map()

    def _on_mouse_wheel(self, event: tk.Event) -> None:  # type: ignore[override]
        delta = 1 if event.delta > 0 else -1
        self._on_zoom(delta)

    def _on_resize(self, event: tk.Event) -> None:  # type: ignore[override]
        if not self._view_state:
            return
        if event.width != self._view_state.width or event.height != self._view_state.height:
            self._view_state.resize(event.width, event.height)
            self._update_map()

    def _on_drag_start(self, event: tk.Event) -> None:  # type: ignore[override]
        self._drag_last = (event.x, event.y)

    def _on_drag_move(self, event: tk.Event) -> None:  # type: ignore[override]
        if not self._view_state or self._drag_last is None:
            return
        last_x, last_y = self._drag_last
        dx = event.x - last_x
        dy = event.y - last_y
        self._view_state.pan_pixels(dx, dy)
        self._drag_last = (event.x, event.y)
        self._update_map()

    def _on_drag_end(self, event: tk.Event) -> None:  # type: ignore[override]
        self._drag_last = None

    # ------------------------------------------------------------------ Map refresh
    def _update_map(self) -> None:
        if not self._view_state:
            return
        server = self._current_server()
        layer = self._current_layer()
        bbox = self._view_state.bounding_box()
        size = (self._view_state.width, self._view_state.height)

        try:
            image_bytes = self.client.fetch_map(server, layer, bbox, size)
        except Exception as exc:  # pragma: no cover - user-visible dialog
            messagebox.showerror("WMS Error", str(exc))
            return

        image = Image.open(io.BytesIO(image_bytes))
        self._current_image = ImageTk.PhotoImage(image)
        self.canvas.delete("all")
        self.canvas.create_image(0, 0, anchor=tk.NW, image=self._current_image)

        lon, lat = self._view_state.center_lonlat()
        self.status_var.set(
            f"{server.name} – {layer.title or layer.name} | Zoom {self._view_state.zoom} | "
            f"Center: {lon:.3f}, {lat:.3f} | {server.attribution}"
        )


def main(config_path: Optional[Path] = None) -> None:
    base_path = Path(__file__).parent
    config_file = config_path or (base_path / "config.yaml")
    app_config = load_config(config_file)
    cache_path = ensure_cache_dir(base_path)

    root = tk.Tk()
    WMSViewerApp(root, app_config, cache_path)
    root.mainloop()


if __name__ == "__main__":
    main()
