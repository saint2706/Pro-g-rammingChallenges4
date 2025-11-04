"""pdftag_gui.py - Tkinter GUI front-end for modernized PDF metadata tagger.

Features:
  * Browse for PDF & auto-populate existing metadata
  * Editable fields: Title, Author, Subject, Keywords
  * Overwrite toggle (instead of merge)
  * JSON summary optional export
  * Status bar for user feedback
  * Validation & error dialogs

Requires `pdftag.py` (modern version) in same directory.
"""

from __future__ import annotations

import sys
import tkinter as tk
from pathlib import Path
from tkinter import filedialog, messagebox
from typing import Dict

try:
    from pdftag import (
        PDFMetadata,
        TagConfig,
        read_metadata,
        update_pdf,
        create_output_path,
        PYPDF_AVAILABLE,
        PYPDF3_AVAILABLE,
    )
except Exception as e:  # pragma: no cover
    print("Error: This GUI requires the updated 'pdftag.py' script.", file=sys.stderr)
    print(e, file=sys.stderr)
    sys.exit(1)


class PdfTaggerGUI:
    FIELD_ORDER = ["title", "author", "subject", "keywords"]

    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("PDF Metadata Tagger")
        self.input_path_var = tk.StringVar()
        self.json_path_var = tk.StringVar()
        self.overwrite_var = tk.BooleanVar(value=False)

        self.entries: Dict[str, tk.Entry] = {}

        self._build_ui()
        self._update_backend_status()

    # ---------------- UI Construction ---------------- #
    def _build_ui(self) -> None:
        main = tk.Frame(self.master, padx=10, pady=10)
        main.pack(fill=tk.BOTH, expand=True)

        # File selection
        file_frame = tk.Frame(main)
        file_frame.pack(fill=tk.X, pady=4)
        tk.Label(file_frame, text="PDF File:").pack(side=tk.LEFT)
        self.file_entry = tk.Entry(
            file_frame, textvariable=self.input_path_var, width=50
        )
        self.file_entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
        tk.Button(file_frame, text="Browse...", command=self.browse_file).pack(
            side=tk.LEFT
        )

        # Metadata fields
        meta_frame = tk.LabelFrame(main, text="Metadata", padx=10, pady=10)
        meta_frame.pack(fill=tk.X, pady=8)
        for row, field in enumerate(self.FIELD_ORDER):
            tk.Label(meta_frame, text=field.capitalize() + ":").grid(
                row=row, column=0, sticky=tk.W, pady=2
            )
            e = tk.Entry(meta_frame, width=60)
            e.grid(row=row, column=1, sticky=tk.EW, padx=5)
            self.entries[field] = e
        meta_frame.columnconfigure(1, weight=1)

        # Options frame
        opt = tk.Frame(main)
        opt.pack(fill=tk.X, pady=4)
        tk.Checkbutton(
            opt, text="Overwrite (do not merge)", variable=self.overwrite_var
        ).pack(side=tk.LEFT)
        tk.Label(opt, text="JSON Summary:").pack(side=tk.LEFT, padx=(12, 2))
        json_entry = tk.Entry(opt, textvariable=self.json_path_var, width=25)
        json_entry.pack(side=tk.LEFT)
        tk.Button(opt, text="Browse", command=self.browse_json).pack(
            side=tk.LEFT, padx=2
        )

        # Action buttons
        action = tk.Frame(main)
        action.pack(fill=tk.X, pady=10)
        self.apply_btn = tk.Button(
            action, text="Apply & Save", command=self.apply_tags, width=18
        )
        self.apply_btn.pack(side=tk.LEFT)
        self.list_btn = tk.Button(
            action, text="List Metadata", command=self.list_metadata
        )
        self.list_btn.pack(side=tk.LEFT, padx=6)
        self.clear_btn = tk.Button(
            action, text="Clear Fields", command=self.clear_fields
        )
        self.clear_btn.pack(side=tk.LEFT)

        # Status bar
        self.status = tk.Label(main, text="Ready", anchor="w")
        self.status.pack(fill=tk.X, pady=(8, 0))

    # ---------------- Helpers ---------------- #
    def _set_status(self, text: str) -> None:
        self.status.configure(text=text)

    def _update_backend_status(self) -> None:
        if not (PYPDF_AVAILABLE or PYPDF3_AVAILABLE):
            self._set_status("No PDF backend found. Install 'pypdf'.")
            self.apply_btn.config(state=tk.DISABLED)
            self.list_btn.config(state=tk.DISABLED)
        else:
            backend = "pypdf" if PYPDF_AVAILABLE else "PyPDF3"
            self._set_status(f"Backend: {backend}")

    def clear_fields(self) -> None:
        for e in self.entries.values():
            e.delete(0, tk.END)
        self._set_status("Fields cleared")

    def browse_file(self) -> None:
        path = filedialog.askopenfilename(
            title="Select PDF", filetypes=(("PDF Files", "*.pdf"), ("All Files", "*.*"))
        )
        if not path:
            return
        self.input_path_var.set(path)
        self.populate_existing_metadata(Path(path))

    def browse_json(self) -> None:
        path = filedialog.asksaveasfilename(
            defaultextension=".json",
            filetypes=(("JSON", "*.json"), ("All Files", "*.*")),
        )
        if not path:
            return
        self.json_path_var.set(path)

    def populate_existing_metadata(self, pdf_path: Path) -> None:
        try:
            meta = read_metadata(pdf_path)
        except Exception as e:
            messagebox.showwarning("Read Failed", f"Could not read metadata:\n{e}")
            self._set_status("Metadata read failed")
            return
        mapping = meta.to_pdf_dict()
        # Convert keys like /Title to lower simple names
        key_map = {
            "/Title": "title",
            "/Author": "author",
            "/Subject": "subject",
            "/Keywords": "keywords",
        }
        for k, v in mapping.items():
            simple = key_map.get(k)
            if simple and simple in self.entries:
                self.entries[simple].delete(0, tk.END)
                self.entries[simple].insert(0, v)
        self._set_status("Metadata loaded")

    # ---------------- Actions ---------------- #
    def list_metadata(self) -> None:
        path = self.input_path_var.get()
        if not path:
            messagebox.showerror("Error", "Select a PDF first.")
            return
        try:
            meta = read_metadata(Path(path))
        except Exception as e:
            messagebox.showerror("Error", f"Failed to read metadata:\n{e}")
            return
        data = meta.to_pdf_dict()
        if not data:
            messagebox.showinfo("Metadata", "No metadata present.")
            return
        lines = [f"{k[1:]}: {v}" for k, v in data.items()]
        messagebox.showinfo("Metadata", "\n".join(lines))

    def apply_tags(self) -> None:
        path = self.input_path_var.get()
        if not path:
            messagebox.showerror("Error", "Select a PDF first.")
            return
        pdf_path = Path(path)
        if not pdf_path.exists():
            messagebox.showerror("Error", "File does not exist.")
            return
        meta_obj = PDFMetadata(
            title=self.entries["title"].get() or None,
            author=self.entries["author"].get() or None,
            subject=self.entries["subject"].get() or None,
            keywords=self.entries["keywords"].get() or None,
        )
        if not meta_obj.to_pdf_dict():
            messagebox.showwarning(
                "Nothing to Apply", "Enter at least one metadata field."
            )
            return
        output = create_output_path(pdf_path)
        json_summary = (
            Path(self.json_path_var.get()) if self.json_path_var.get() else None
        )
        cfg = TagConfig(
            input_pdf=pdf_path,
            output_pdf=output,
            metadata=meta_obj,
            overwrite=self.overwrite_var.get(),
            list_only=False,
            json_summary=json_summary,
        )
        summary = update_pdf(cfg)
        if json_summary and summary.get("success"):
            try:
                with open(json_summary, "w", encoding="utf-8") as fh:
                    import json

                    json.dump({"mode": "gui-update", **summary}, fh, indent=2)
            except OSError as e:
                messagebox.showwarning("JSON", f"Could not write JSON summary:\n{e}")
        if not summary.get("success"):
            messagebox.showerror(
                "Error", f"Update failed:\n{summary.get('error', 'Unknown error')}"
            )
            self._set_status("Update failed")
            return
        messagebox.showinfo(
            "Success", f"Metadata updated. Saved to:\n{summary['output']}"
        )
        self._set_status("Update successful")


# ---------------- Main ---------------- #


def main() -> int:
    root = tk.Tk()
    app = PdfTaggerGUI(root)
    root.mainloop()
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
