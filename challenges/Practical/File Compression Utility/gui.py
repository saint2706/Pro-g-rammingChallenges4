"""Tkinter GUI for the File Compression Utility project."""

from __future__ import annotations

import queue
import sys
import threading
from pathlib import Path
from typing import Iterable

try:
    import tkinter as tk
    from tkinter import ttk, filedialog, messagebox
except ImportError as exc:  # pragma: no cover - environments without Tk
    raise SystemExit("Tkinter is required to run the GUI.") from exc

try:  # Optional drag-and-drop support
    from tkinterdnd2 import DND_FILES, TkinterDnD

    DND_AVAILABLE = True
except Exception:  # pragma: no cover - fallback path
    TkinterDnD = None  # type: ignore
    DND_FILES = None  # type: ignore
    DND_AVAILABLE = False

# Allow running from repository root by augmenting sys.path when executed directly.
if __package__ in {None, ""}:
    sys.path.append(str(Path(__file__).resolve().parent))

from backend import (
    ArchiveFormat,
    ArchiveManager,
    ArchiveOperationError,
    ProgressEvent,
)  # noqa: E402


class CompressionApp:
    """Encapsulates the Tkinter interface and behaviour."""

    def __init__(self) -> None:
        root_cls = TkinterDnD.Tk if DND_AVAILABLE else Tk  # type: ignore[attr-defined]
        self.root = root_cls()
        self.root.title("File Compression Utility")
        self.root.geometry("840x520")
        self.root.minsize(760, 480)

        self.progress_queue: "queue.Queue[object]" = queue.Queue()
        self.active_thread: threading.Thread | None = None

        self._build_widgets()
        self.root.after(100, self._poll_progress)

        if not DND_AVAILABLE:
            self.status_var.set(
                "Drag-and-drop unavailable. Install 'tkinterdnd2' for drop support."
            )

    # ------------------------------------------------------------------
    # UI construction helpers
    def _build_widgets(self) -> None:
        self.status_var = tk.StringVar()
        status = ttk.Label(self.root, textvariable=self.status_var, foreground="#444")
        status.pack(fill="x", padx=12, pady=(8, 0))

        notebook = ttk.Notebook(self.root)
        notebook.pack(fill="both", expand=True, padx=10, pady=10)

        self.compress_frame = ttk.Frame(notebook)
        self.extract_frame = ttk.Frame(notebook)
        notebook.add(self.compress_frame, text="Create Archive")
        notebook.add(self.extract_frame, text="Extract Archive")

        self._build_compress_tab()
        self._build_extract_tab()

        self.progress_var = tk.DoubleVar(value=0.0)
        self.progress_bar = ttk.Progressbar(
            self.root, variable=self.progress_var, maximum=1.0
        )
        self.progress_bar.pack(fill="x", padx=12, pady=(0, 10))

    # ------------------------------------------------------------------
    def _build_compress_tab(self) -> None:
        frame = self.compress_frame

        list_frame = ttk.LabelFrame(frame, text="Files & Folders")
        list_frame.pack(fill="both", expand=True, padx=10, pady=10)

        self.sources_box = tk.Listbox(list_frame, height=12)
        self.sources_box.pack(
            side="left", fill="both", expand=True, padx=(0, 8), pady=8
        )

        if DND_AVAILABLE:
            self.sources_box.drop_target_register(DND_FILES)  # type: ignore[attr-defined]
            self.sources_box.dnd_bind("<<Drop>>", self._on_drop_sources)

        button_frame = ttk.Frame(list_frame)
        button_frame.pack(side="left", fill="y", pady=8)

        add_files_btn = ttk.Button(
            button_frame, text="Add Files", command=self._add_files
        )
        add_files_btn.pack(fill="x", pady=2)

        add_dir_btn = ttk.Button(
            button_frame, text="Add Folder", command=lambda: self._add_directories()
        )
        add_dir_btn.pack(fill="x", pady=2)

        remove_btn = ttk.Button(
            button_frame, text="Remove Selected", command=self._remove_selected
        )
        remove_btn.pack(fill="x", pady=2)

        clear_btn = ttk.Button(button_frame, text="Clear", command=self._clear_sources)
        clear_btn.pack(fill="x", pady=2)

        dest_frame = ttk.LabelFrame(frame, text="Destination")
        dest_frame.pack(fill="x", padx=10, pady=(0, 10))

        self.dest_var = tk.StringVar()
        dest_entry = ttk.Entry(dest_frame, textvariable=self.dest_var)
        dest_entry.pack(side="left", fill="x", expand=True, padx=(8, 4), pady=8)

        browse_dest_btn = ttk.Button(
            dest_frame, text="Browse", command=self._choose_destination
        )
        browse_dest_btn.pack(side="left", padx=(0, 8))

        name_frame = ttk.Frame(frame)
        name_frame.pack(fill="x", padx=10, pady=(0, 10))

        ttk.Label(name_frame, text="Archive name:").pack(side="left", padx=(0, 6))
        self.archive_name_var = tk.StringVar(value="archive")
        name_entry = ttk.Entry(name_frame, textvariable=self.archive_name_var)
        name_entry.pack(side="left", fill="x", expand=True)

        ttk.Label(name_frame, text="Preset:").pack(side="left", padx=(10, 6))
        self.format_var = tk.StringVar()
        self.format_options = {
            "ZIP (Stored)": ArchiveFormat.ZIP_STORED,
            "ZIP (Deflated)": ArchiveFormat.ZIP_DEFLATED,
            "ZIP (BZip2)": ArchiveFormat.ZIP_BZIP2,
            "ZIP (LZMA)": ArchiveFormat.ZIP_LZMA,
            "TAR (Uncompressed)": ArchiveFormat.TAR,
            "TAR.GZ (gzip)": ArchiveFormat.TAR_GZ,
            "TAR.BZ2 (bzip2)": ArchiveFormat.TAR_BZ2,
            "TAR.XZ (xz)": ArchiveFormat.TAR_XZ,
        }
        self.format_combo = ttk.Combobox(
            name_frame,
            textvariable=self.format_var,
            values=list(self.format_options.keys()),
            state="readonly",
        )
        self.format_combo.set("ZIP (Deflated)")
        self.format_combo.pack(side="left", width=140)

        create_btn = ttk.Button(
            frame, text="Create Archive", command=self._start_create
        )
        create_btn.pack(padx=10, pady=(0, 12), anchor="e")

    # ------------------------------------------------------------------
    def _build_extract_tab(self) -> None:
        frame = self.extract_frame

        archive_frame = ttk.LabelFrame(frame, text="Archive")
        archive_frame.pack(fill="x", padx=10, pady=10)

        self.archive_path_var = tk.StringVar()
        archive_entry = ttk.Entry(archive_frame, textvariable=self.archive_path_var)
        archive_entry.pack(side="left", fill="x", expand=True, padx=(8, 4), pady=8)

        if DND_AVAILABLE:
            archive_entry.drop_target_register(DND_FILES)  # type: ignore[attr-defined]
            archive_entry.dnd_bind("<<Drop>>", self._on_drop_archive)

        browse_archive_btn = ttk.Button(
            archive_frame, text="Browse", command=self._choose_archive
        )
        browse_archive_btn.pack(side="left", padx=(0, 8))

        output_frame = ttk.LabelFrame(frame, text="Destination")
        output_frame.pack(fill="x", padx=10, pady=(0, 10))

        self.extract_dest_var = tk.StringVar()
        output_entry = ttk.Entry(output_frame, textvariable=self.extract_dest_var)
        output_entry.pack(side="left", fill="x", expand=True, padx=(8, 4), pady=8)

        browse_output_btn = ttk.Button(
            output_frame, text="Browse", command=self._choose_extract_destination
        )
        browse_output_btn.pack(side="left", padx=(0, 8))

        extract_btn = ttk.Button(frame, text="Extract", command=self._start_extract)
        extract_btn.pack(padx=10, pady=(0, 12), anchor="e")

    # ------------------------------------------------------------------
    # Event handlers and helpers
    def _add_files(self) -> None:
        files = filedialog.askopenfilenames(
            parent=self.root, title="Select files to compress"
        )
        self._extend_sources(Path(f) for f in files)

    def _add_directories(self) -> None:
        directory = filedialog.askdirectory(parent=self.root, title="Select folder")
        if directory:
            self._extend_sources([Path(directory)])

    def _extend_sources(self, new_paths: Iterable[Path]) -> None:
        existing = {
            Path(self.sources_box.get(idx)) for idx in range(self.sources_box.size())
        }
        for path in new_paths:
            if path and path not in existing:
                self.sources_box.insert("end", str(path))

    def _remove_selected(self) -> None:
        selection = list(self.sources_box.curselection())
        selection.reverse()
        for idx in selection:
            self.sources_box.delete(idx)

    def _clear_sources(self) -> None:
        self.sources_box.delete(0, "end")

    def _choose_destination(self) -> None:
        folder = filedialog.askdirectory(
            parent=self.root, title="Select destination folder"
        )
        if folder:
            self.dest_var.set(folder)

    def _choose_archive(self) -> None:
        archive = filedialog.askopenfilename(parent=self.root, title="Select archive")
        if archive:
            self.archive_path_var.set(archive)

    def _choose_extract_destination(self) -> None:
        folder = filedialog.askdirectory(
            parent=self.root, title="Select destination folder"
        )
        if folder:
            self.extract_dest_var.set(folder)

    def _start_create(self) -> None:
        if self.active_thread and self.active_thread.is_alive():
            messagebox.showinfo("Busy", "An operation is already in progress.")
            return

        sources = [
            Path(self.sources_box.get(i)) for i in range(self.sources_box.size())
        ]
        if not sources:
            messagebox.showerror(
                "Validation", "Add at least one file or folder to compress."
            )
            return

        dest_folder = Path(self.dest_var.get() or ".")
        name = self.archive_name_var.get().strip() or "archive"
        selected_label = self.format_var.get()
        fmt = self.format_options.get(selected_label)
        if fmt is None:
            messagebox.showerror("Preset", "Select a compression preset.")
            return

        destination = dest_folder / f"{name}{fmt.extension()}"

        self._run_async(
            lambda: ArchiveManager.create_archive(
                sources=sources,
                destination=destination,
                archive_format=fmt,
                progress_callback=self.progress_queue.put,
            ),
            success_message=f"Archive created at {destination}",
        )

    def _start_extract(self) -> None:
        if self.active_thread and self.active_thread.is_alive():
            messagebox.showinfo("Busy", "An operation is already in progress.")
            return

        archive_path = Path(self.archive_path_var.get())
        if not archive_path:
            messagebox.showerror("Validation", "Choose an archive to extract.")
            return

        destination = Path(self.extract_dest_var.get() or ".")

        self._run_async(
            lambda: ArchiveManager.extract_archive(
                archive_path=archive_path,
                destination=destination,
                progress_callback=self.progress_queue.put,
            ),
            success_message=f"Extracted into {destination}",
        )

    def _run_async(self, task, success_message: str) -> None:
        self.progress_var.set(0.0)
        self.status_var.set("Working…")

        def worker() -> None:
            try:
                task()
            except ArchiveOperationError as exc:
                self.progress_queue.put(exc)
            except Exception as exc:  # pragma: no cover - unexpected path
                self.progress_queue.put(ArchiveOperationError(str(exc)))
            else:
                self.progress_queue.put(success_message)

        self.active_thread = threading.Thread(target=worker, daemon=True)
        self.active_thread.start()

    def _poll_progress(self) -> None:
        try:
            while True:
                item = self.progress_queue.get_nowait()
                if isinstance(item, ProgressEvent):
                    self.progress_var.set(item.fraction)
                    self.status_var.set(f"{item.phase.value.title()}: {item.filename}")
                elif isinstance(item, ArchiveOperationError):
                    messagebox.showerror("Archive error", str(item))
                    self.status_var.set(str(item))
                    self.progress_var.set(0.0)
                elif isinstance(item, str):
                    messagebox.showinfo("Success", item)
                    self.status_var.set(item)
                    self.progress_var.set(1.0)
        except queue.Empty:
            pass
        finally:
            self.root.after(100, self._poll_progress)

    def _on_drop_sources(self, event) -> None:  # type: ignore[override]
        paths = self._parse_drop_event(event.data)
        self._extend_sources(Path(p) for p in paths)

    def _on_drop_archive(self, event) -> None:  # type: ignore[override]
        paths = self._parse_drop_event(event.data)
        if paths:
            self.archive_path_var.set(paths[0])

    @staticmethod
    def _parse_drop_event(data: str) -> list[str]:
        if not data:
            return []
        # tkinterdnd2 separates paths by spaces but wraps ones containing spaces in braces.
        results: list[str] = []
        current = []
        brace_level = 0
        for char in data:
            if char == "{":
                brace_level += 1
                if brace_level == 1:
                    current = []
                    continue
            if char == "}":
                brace_level -= 1
                if brace_level == 0:
                    results.append("".join(current))
                    current = []
                    continue
            if brace_level > 0:
                current.append(char)
            elif char == " ":
                if current:
                    results.append("".join(current))
                    current = []
            else:
                current.append(char)
        if current:
            results.append("".join(current))
        return results

    def run(self) -> None:
        self.root.mainloop()


def main() -> None:
    app = CompressionApp()
    app.run()


if __name__ == "__main__":
    main()
