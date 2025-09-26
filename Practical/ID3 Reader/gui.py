"""Simple Tkinter GUI for the ID3 Reader."""
from __future__ import annotations

import importlib.util
import tkinter as tk
from pathlib import Path
from tkinter import filedialog, messagebox, ttk
from typing import Optional

MODULE_PATH = Path(__file__).resolve().with_name("id3_reader.py")
SPEC = importlib.util.spec_from_file_location("id3_reader_gui_helper", MODULE_PATH)
id3_reader = importlib.util.module_from_spec(SPEC)
assert SPEC and SPEC.loader
SPEC.loader.exec_module(id3_reader)  # type: ignore[arg-type]

ID3Metadata = id3_reader.ID3Metadata
export_csv = id3_reader.export_csv
export_json = id3_reader.export_json
parse_id3 = id3_reader.parse_id3


class ID3ReaderApp(tk.Tk):
    def __init__(self) -> None:
        super().__init__()
        self.title("ID3 Reader")
        self.geometry("680x420")

        self.metadata: Optional[ID3Metadata] = None

        self._build_widgets()

    def _build_widgets(self) -> None:
        toolbar = tk.Frame(self)
        toolbar.pack(fill=tk.X, padx=6, pady=4)

        open_btn = tk.Button(toolbar, text="Open File…", command=self.open_file)
        open_btn.pack(side=tk.LEFT, padx=(0, 6))

        export_json_btn = tk.Button(toolbar, text="Export JSON", command=self.export_json)
        export_json_btn.pack(side=tk.LEFT, padx=(0, 6))

        export_csv_btn = tk.Button(toolbar, text="Export CSV", command=self.export_csv)
        export_csv_btn.pack(side=tk.LEFT)

        self.tree = ttk.Treeview(self, columns=("value",), show="tree")
        self.tree.pack(fill=tk.BOTH, expand=True, padx=6, pady=(0, 6))

        self.status = tk.StringVar(value="Select an MP3 file to inspect.")
        status_bar = tk.Label(self, textvariable=self.status, anchor="w")
        status_bar.pack(fill=tk.X, padx=6, pady=(0, 6))

    def open_file(self) -> None:
        file_path = filedialog.askopenfilename(
            title="Open MP3", filetypes=(("MP3 files", "*.mp3"), ("All files", "*.*"))
        )
        if not file_path:
            return
        try:
            self.metadata = parse_id3(file_path)
        except Exception as exc:  # noqa: BLE001 - display to user
            messagebox.showerror("ID3 Reader", str(exc))
            return
        self._populate_tree(self.metadata)
        versions = ", ".join(self.metadata.versions)
        self.status.set(f"Loaded {Path(file_path).name} ({versions}).")

    def export_json(self) -> None:
        if not self._ensure_metadata():
            return
        output_path = filedialog.asksaveasfilename(
            title="Export JSON", defaultextension=".json", filetypes=(("JSON", "*.json"),)
        )
        if not output_path:
            return
        export_json([self.metadata], output_path)
        self.status.set(f"Exported JSON to {output_path}.")

    def export_csv(self) -> None:
        if not self._ensure_metadata():
            return
        output_path = filedialog.asksaveasfilename(
            title="Export CSV", defaultextension=".csv", filetypes=(("CSV", "*.csv"),)
        )
        if not output_path:
            return
        export_csv([self.metadata], output_path)
        self.status.set(f"Exported CSV to {output_path}.")

    def _populate_tree(self, metadata: ID3Metadata) -> None:
        self.tree.delete(*self.tree.get_children())

        root_id = self.tree.insert("", tk.END, text=Path(metadata.file_path).name)
        versions_id = self.tree.insert(root_id, tk.END, text=f"Versions: {', '.join(metadata.versions)}")
        self.tree.item(root_id, open=True)
        self.tree.item(versions_id, open=True)

        tags_id = self.tree.insert(root_id, tk.END, text="Normalised Tags")
        for key, value in sorted(metadata.tags.items()):
            self.tree.insert(tags_id, tk.END, text=f"{key}: {value}")

        raw_id = self.tree.insert(root_id, tk.END, text="Raw Frames")
        for frame_id, values in sorted(metadata.raw_frames.items()):
            frame_node = self.tree.insert(raw_id, tk.END, text=frame_id)
            for value in values:
                display = value if len(value) < 80 else value[:77] + "…"
                self.tree.insert(frame_node, tk.END, text=display)

    def _ensure_metadata(self) -> bool:
        if self.metadata is None:
            messagebox.showwarning("ID3 Reader", "Load a file before exporting.")
            return False
        return True


def launch() -> None:
    app = ID3ReaderApp()
    app.mainloop()


if __name__ == "__main__":
    launch()
