"""Autosave Text Editor (Tkinter)

Modernized features:
  * Type hints & dataclass-based configuration
  * Centralized autosave interval & hashing for change detection (reduces disk writes)
  * Atomic file writes (write to temp then replace) to avoid partial saves
  * Status bar with richer messages (saved time, unsaved indicator)
  * Window title reflects current file & dirty state
  * Graceful new file / open file handling
  * Extensible structure for future features (shortcuts, settings)

Usage:
  python aste.py            # launch editor with default 20s autosave
  python aste.py --interval 5

Recent Additions:
  - CLI flags: --read-only, --encoding, --backup-dir, --max-backups
  - Backup rotation directory with retention controls
  - Keyboard shortcuts (Ctrl+S/O/N, Ctrl+Shift+S)
"""

from __future__ import annotations

import argparse
import contextlib
import datetime as dt
import hashlib
import os
import shutil
import tempfile
from dataclasses import dataclass
from pathlib import Path
import sys
import tkinter as tk
from tkinter import filedialog, messagebox
from typing import Optional

# --------------------------- Configuration --------------------------- #


@dataclass(slots=True)
class Config:
    autosave_interval_ms: int = 20_000  # 20 seconds
    read_only: bool = False
    encoding: str = "utf-8"
    backup_dir: Optional[Path] = None
    max_backups: int = 5
    initial_file: Optional[Path] = None


# --------------------------- Editor Class --------------------------- #


class AutosaveTextEditor:
    """A simple text editor with automatic save functionality.

    Autosave triggers only when content hash changes since last save to reduce
    unnecessary disk writes. Saves are atomic: write to a temp file then replace
    target, minimizing corruption risk.
    """

    def __init__(self, master: tk.Tk, config: Config):
        self.master = master
        self.config = config

        self.master.title("Autosave Text Editor")
        self.master.geometry("800x600")

        self.text_area = tk.Text(self.master, wrap="word", undo=True)
        self.text_area.pack(expand=True, fill="both")

        self._build_menu()
        self._bind_shortcuts()

        self.status_bar = tk.Label(
            self.master, text="Ready", bd=1, relief=tk.SUNKEN, anchor=tk.W
        )
        self.status_bar.pack(side=tk.BOTTOM, fill=tk.X)

        self.current_file: Optional[Path] = None
        self._last_hash: str = ""
        self._dirty: bool = False
        self._suppress_modified: bool = False
        self.encoding = self.config.encoding
        self.backup_dir = self.config.backup_dir
        self.max_backups = max(0, self.config.max_backups)

        # Event bindings
        self.text_area.bind("<<Modified>>", self._on_modified)
        self.master.protocol("WM_DELETE_WINDOW", self._on_close)

        self._apply_read_only_state()

        if self.config.initial_file:
            self._load_startup_file(self.config.initial_file)

        # Kick off autosave loop
        self._schedule_autosave()

    # ---------------------- UI Construction ---------------------- #
    def _build_menu(self) -> None:
        self.menu_bar = tk.Menu(self.master)
        self.master.config(menu=self.menu_bar)

        file_menu = tk.Menu(self.menu_bar, tearoff=0)
        file_menu.add_command(label="New", command=self.new_file)
        file_menu.add_command(label="Open...", command=self.open_file)
        file_menu.add_command(label="Save", command=self.save_file)
        file_menu.add_command(label="Save As...", command=self.save_as)
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self._on_close)
        self.menu_bar.add_cascade(label="File", menu=file_menu)
        self.file_menu = file_menu

    def _bind_shortcuts(self) -> None:
        shortcut_map = {
            ("<Control-s>", "<Control-S>"): self._shortcut_save,
            ("<Control-Shift-s>", "<Control-Shift-S>"): self._shortcut_save_as,
            ("<Control-o>", "<Control-O>"): self._shortcut_open,
            ("<Control-n>", "<Control-N>"): self._shortcut_new,
        }
        for sequences, handler in shortcut_map.items():
            for seq in sequences:
                self.master.bind_all(seq, handler)

    def _apply_read_only_state(self) -> None:
        if not self.config.read_only:
            return
        self.text_area.config(state=tk.DISABLED)
        for label in ("New", "Save", "Save As..."):
            with contextlib.suppress(tk.TclError):
                self.file_menu.entryconfig(label, state="disabled")
        self._set_status("Read-only mode: editing and saving disabled.")

    def _load_startup_file(self, file_path: Path) -> None:
        if not file_path.exists():
            self._set_status(f"File not found: {file_path}")
            return
        self._load_file_path(file_path)

    def _shortcut_save(self, event) -> str:
        self.save_file()
        return "break"

    def _shortcut_save_as(self, event) -> str:
        self.save_as()
        return "break"

    def _shortcut_open(self, event) -> str:
        self.open_file()
        return "break"

    def _shortcut_new(self, event) -> str:
        self.new_file()
        return "break"

    # ---------------------- File Operations ---------------------- #
    def new_file(self) -> None:
        if self.config.read_only:
            self._set_status("Read-only mode: cannot create new files.")
            return
        if not self._maybe_discard_changes():
            return
        self._set_text_content("")
        self.current_file = None
        self._mark_clean()
        self._update_title()

    def open_file(self) -> None:
        if not self._maybe_discard_changes():
            return
        file_path_str = filedialog.askopenfilename(
            defaultextension=".txt",
            filetypes=[("Text Files", "*.txt"), ("All Files", "*.*")],
        )
        if file_path_str:
            file_path = Path(file_path_str)
            self._load_file_path(file_path)

    def save_file(self) -> None:
        if self.config.read_only:
            self._set_status("Read-only mode: saves disabled.")
            return
        if self.current_file is None:
            self.save_as()
            return
        self._write_to_path(self.current_file)

    def save_as(self) -> None:
        if self.config.read_only:
            self._set_status("Read-only mode: saves disabled.")
            return
        date_str = dt.datetime.now().strftime("%Y-%m-%d")
        default_filename = f"document_{date_str}.txt"
        file_path_str = filedialog.asksaveasfilename(
            defaultextension=".txt",
            filetypes=[("Text Files", "*.txt"), ("All Files", "*.*")],
            initialfile=default_filename,
        )
        if file_path_str:
            self.current_file = Path(file_path_str)
            self._write_to_path(self.current_file)

    def _set_text_content(self, content: str) -> None:
        restore_state = self.text_area.cget("state") == tk.DISABLED
        self._suppress_modified = True
        try:
            if restore_state:
                self.text_area.config(state=tk.NORMAL)
            self.text_area.delete(1.0, tk.END)
            self.text_area.insert(tk.END, content)
        finally:
            if restore_state:
                self.text_area.config(state=tk.DISABLED)
            self._suppress_modified = False
        self.text_area.edit_modified(False)

    def _load_file_path(self, file_path: Path) -> None:
        try:
            content = file_path.read_text(encoding=self.encoding)
        except Exception as e:  # pragma: no cover
            messagebox.showerror("Open Error", f"Could not open file:\n{e}")
            return
        self._set_text_content(content)
        self.current_file = file_path
        self._last_hash = self._hash_content(content)
        self._mark_clean()
        self._update_title()
        self._set_status(f"Opened: {file_path.name}")

    def _maybe_create_backup(self, path: Path) -> None:
        if self.config.read_only or self.max_backups <= 0:
            return
        if not path.exists() or not path.is_file():
            return
        backup_root = self.backup_dir
        if backup_root is None:
            return
        try:
            backup_root = backup_root.expanduser().resolve()
        except OSError:
            return
        backup_root.mkdir(parents=True, exist_ok=True)
        fingerprint = hashlib.sha1(str(path.resolve()).encode("utf-8")).hexdigest()[:8]
        subdir = backup_root / f"{path.stem}_{fingerprint}"
        subdir.mkdir(parents=True, exist_ok=True)
        timestamp = dt.datetime.now().strftime("%Y%m%d_%H%M%S")
        suffix = path.suffix
        backup_name = f"{path.stem}_{timestamp}{suffix}" if suffix else f"{path.stem}_{timestamp}"
        backup_path = subdir / backup_name
        try:
            shutil.copy2(path, backup_path)
        except OSError as err:  # pragma: no cover
            self._set_status(f"Backup failed: {err}")
            return
        backups = sorted(
            subdir.iterdir(), key=lambda p: p.stat().st_mtime, reverse=True
        )
        for old in backups[self.max_backups :]:
            with contextlib.suppress(OSError):
                old.unlink()

    def _write_to_path(self, path: Path) -> None:
        content = self.text_area.get(1.0, tk.END)
        new_hash = self._hash_content(content)
        if new_hash == self._last_hash and not self._dirty:
            self._set_status("No changes to save")
            return
        self._maybe_create_backup(path)
        try:
            tmp_dir = path.parent if path.parent.exists() else Path(".")
            with tempfile.NamedTemporaryFile(
                "w", delete=False, dir=tmp_dir, encoding=self.encoding
            ) as tmp:
                tmp.write(content)
                tmp_name = tmp.name
            os.replace(tmp_name, path)  # atomic on most OSes
        except Exception as e:  # pragma: no cover
            messagebox.showerror("Save Error", f"Could not save file:\n{e}")
            return
        self._last_hash = new_hash
        self._mark_clean()
        self._update_title()
        self._set_status(f"Saved at {dt.datetime.now().strftime('%H:%M:%S')}")

    # ---------------------- Autosave Logic ---------------------- #
    def _schedule_autosave(self) -> None:
        self.master.after(self.config.autosave_interval_ms, self._autosave_tick)

    def _autosave_tick(self) -> None:
        if self.config.read_only:
            self._schedule_autosave()
            return
        content = self.text_area.get(1.0, tk.END)
        new_hash = self._hash_content(content)
        if self.current_file and (new_hash != self._last_hash):
            self._write_to_path(self.current_file)
        self._schedule_autosave()

    # ---------------------- Dirty State ---------------------- #
    def _on_modified(self, event) -> None:  # tkinter passes event
        # Reset the modified flag so future modifications trigger again
        self.text_area.edit_modified(False)
        if self._suppress_modified or self.config.read_only:
            return
        self._dirty = True
        self._update_title()

    def _mark_clean(self) -> None:
        self._dirty = False
        self.text_area.edit_modified(False)

    # ---------------------- Utilities ---------------------- #
    @staticmethod
    def _hash_content(text: str) -> str:
        return hashlib.sha256(text.encode("utf-8")).hexdigest()

    def _maybe_discard_changes(self) -> bool:
        if self._dirty:
            resp = messagebox.askyesnocancel(
                "Unsaved Changes",
                "Discard unsaved changes? (Yes = discard, No = cancel operation)",
            )
            if resp is None:  # cancel
                return False
            if resp is False:
                # attempt save
                self.save_file()
        return True

    def _update_title(self) -> None:
        name = self.current_file.name if self.current_file else "Untitled"
        dirty = "*" if self._dirty else ""
        self.master.title(f"{name}{dirty} - Autosave Text Editor")

    def _set_status(self, msg: str) -> None:
        self.status_bar.config(text=msg)
        # Clear after 5 seconds
        self.master.after(5000, lambda: self.status_bar.config(text=""))

    def _on_close(self) -> None:
        if not self._maybe_discard_changes():
            return
        self.master.destroy()


# --------------------------- CLI Entry --------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Autosave Text Editor")
    p.add_argument(
        "--interval",
        type=int,
        default=20,
        help="Autosave interval seconds (default 20)",
    )
    p.add_argument(
        "--read-only",
        action="store_true",
        help="Open editor in read-only mode (no modifications or saves)",
    )
    p.add_argument(
        "--encoding",
        default="utf-8",
        help="Text encoding used for reading and writing files",
    )
    p.add_argument(
        "--backup-dir",
        type=Path,
        help="Directory to store rotated backups (default: ~/.autosave_backups)",
    )
    p.add_argument(
        "--max-backups",
        type=int,
        default=5,
        help="Number of rotated backups to retain (0 disables backups)",
    )
    p.add_argument("file", nargs="?", type=Path, help="Optional file to open on launch")
    return p


def main(argv: Optional[list[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    interval_ms = max(1, args.interval) * 1000
    backup_dir = args.backup_dir.expanduser() if args.backup_dir else None
    if args.max_backups > 0:
        backup_dir = backup_dir or (Path.home() / ".autosave_backups")
    else:
        backup_dir = None
    initial_file = args.file.expanduser() if args.file else None
    cfg = Config(
        autosave_interval_ms=interval_ms,
        read_only=args.read_only,
        encoding=args.encoding,
        backup_dir=backup_dir,
        max_backups=max(0, args.max_backups),
        initial_file=initial_file,
    )

    root = tk.Tk()
    AutosaveTextEditor(root, cfg)
    try:
        root.mainloop()
    except KeyboardInterrupt:
        print("Interrupted.")
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
