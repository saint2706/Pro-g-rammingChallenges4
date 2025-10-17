"""Tkinter-based Markdown editor with live preview and export helpers."""

from __future__ import annotations

import json
import tkinter as tk
from tkinter import filedialog, messagebox, ttk
from tkinter.scrolledtext import ScrolledText
from pathlib import Path
from typing import Optional
import re

from .converter import (
    ConversionResult,
    TemplateManager,
    convert_markdown,
    convert_to_xml,
)

try:  # Optional dependency that renders HTML in Tkinter.
    from tkhtmlview import HTMLScrolledText  # type: ignore
except Exception:  # pragma: no cover - graceful fallback when unavailable.
    HTMLScrolledText = None

APP_TITLE = "Markdown Editor"
BASE_DIR = Path(__file__).resolve().parent
TEMPLATE_DIR = BASE_DIR / "templates"
AUTOSAVE_INTERVAL_MS = 5000

THEMES = {
    "Light": {
        "background": "#ffffff",
        "foreground": "#111827",
        "insertbackground": "#2563eb",
        "selectbackground": "#bfdbfe",
        "linehighlight": "#f1f5f9",
        "heading": "#0f172a",
        "bold": "#1d4ed8",
        "italic": "#2563eb",
        "code": "#0f172a",
        "link": "#2563eb",
    },
    "Dark": {
        "background": "#0f172a",
        "foreground": "#e2e8f0",
        "insertbackground": "#38bdf8",
        "selectbackground": "#1d4ed8",
        "linehighlight": "#1e293b",
        "heading": "#38bdf8",
        "bold": "#facc15",
        "italic": "#f472b6",
        "code": "#f59e0b",
        "link": "#93c5fd",
    },
}


class MarkdownEditorApp:
    def __init__(self, root: tk.Tk):
        self.root = root
        self.root.title(APP_TITLE)
        self.root.geometry("1200x720")

        self.template_manager = TemplateManager(TEMPLATE_DIR)
        self.current_path: Optional[Path] = None
        self.current_result: Optional[ConversionResult] = None
        self.template_var = tk.StringVar()
        self.theme_var = tk.StringVar(value="Light")
        self.status_var = tk.StringVar(value="Ready")
        self._template_choices: list[tuple[str, str]] = []

        self.highlight_job: Optional[str] = None
        self.preview_job: Optional[str] = None
        self.autosave_job: Optional[str] = None

        self._build_menu()
        self._build_toolbar()
        self._build_body()
        self._build_status_bar()

        self._apply_theme("Light")
        self._schedule_autosave()

        self.root.bind("<Control-n>", lambda _event: self.new_file())
        self.root.bind("<Control-o>", lambda _event: self.open_file())
        self.root.bind("<Control-s>", lambda _event: self.save_file())
        self.root.bind("<Control-S>", lambda _event: self.save_file(save_as=True))
        self.root.bind("<Control-e>", lambda _event: self.export_html())
        self.root.bind("<Control-E>", lambda _event: self.export_xml())
        self.root.bind("<Control-l>", lambda _event: self.toggle_theme())
        self.root.bind("<F5>", lambda _event: self.refresh_now())

    # UI building helpers -------------------------------------------------
    def _build_menu(self) -> None:
        menubar = tk.Menu(self.root)

        file_menu = tk.Menu(menubar, tearoff=False)
        file_menu.add_command(label="New", command=self.new_file, accelerator="Ctrl+N")
        file_menu.add_command(
            label="Open…", command=self.open_file, accelerator="Ctrl+O"
        )
        file_menu.add_command(
            label="Save", command=self.save_file, accelerator="Ctrl+S"
        )
        file_menu.add_command(
            label="Save As…",
            command=lambda: self.save_file(save_as=True),
            accelerator="Ctrl+Shift+S",
        )
        file_menu.add_separator()
        file_menu.add_command(
            label="Export HTML…", command=self.export_html, accelerator="Ctrl+E"
        )
        file_menu.add_command(
            label="Export XML…", command=self.export_xml, accelerator="Ctrl+Shift+E"
        )
        file_menu.add_separator()
        file_menu.add_command(label="Exit", command=self.root.quit)
        menubar.add_cascade(label="File", menu=file_menu)

        view_menu = tk.Menu(menubar, tearoff=False)
        view_menu.add_radiobutton(
            label="Light", command=lambda: self._apply_theme("Light"), value="Light"
        )
        view_menu.add_radiobutton(
            label="Dark", command=lambda: self._apply_theme("Dark"), value="Dark"
        )
        menubar.add_cascade(label="View", menu=view_menu)

        self.root.config(menu=menubar)

    def _build_toolbar(self) -> None:
        toolbar = ttk.Frame(self.root, padding=(8, 4))
        toolbar.pack(side=tk.TOP, fill=tk.X)

        ttk.Label(toolbar, text="Template:").pack(side=tk.LEFT, padx=(0, 4))
        self.template_combo = ttk.Combobox(
            toolbar,
            textvariable=self.template_var,
            state="readonly",
            width=24,
        )
        self.template_combo.pack(side=tk.LEFT)
        self.template_combo.bind(
            "<<ComboboxSelected>>", lambda _event: self._on_template_selected()
        )

        self._sync_template_combo()

        ttk.Button(toolbar, text="Refresh", command=self.refresh_now).pack(
            side=tk.LEFT, padx=6
        )
        ttk.Button(
            toolbar, text="Templates Folder", command=self._open_templates_folder
        ).pack(side=tk.LEFT)

    def _build_body(self) -> None:
        paned = ttk.PanedWindow(self.root, orient=tk.HORIZONTAL)
        paned.pack(fill=tk.BOTH, expand=True)

        self.editor = ScrolledText(paned, undo=True, wrap=tk.WORD)
        self.editor.pack(fill=tk.BOTH, expand=True)
        self.editor.bind("<<Modified>>", self._on_modified)
        paned.add(self.editor, weight=3)

        if HTMLScrolledText is not None:
            self.preview = HTMLScrolledText(
                paned, html="<h2>Preview</h2><p>Start typing…</p>"
            )
        else:
            self.preview = ScrolledText(paned, state=tk.DISABLED, wrap=tk.WORD)
            self.preview.configure(height=10)
            self.preview.configure(cursor="arrow")
            self.preview.configure(insertbackground="#000000")
            self.preview.configure(tabs=(12,))
            self.preview.configure(background="#f1f5f9")
            self.preview.configure(foreground="#0f172a")
            self.preview.configure(font=("Consolas", 11))
            self._update_preview_fallback(
                "<h2>Preview</h2>\n<p>Install tkhtmlview for rendered output.</p>"
            )
        paned.add(self.preview, weight=2)

    def _build_status_bar(self) -> None:
        status = ttk.Label(self.root, textvariable=self.status_var, anchor=tk.W)
        status.pack(side=tk.BOTTOM, fill=tk.X)

    # Events ---------------------------------------------------------------
    def _on_modified(self, _event: tk.Event) -> None:
        if self.editor.edit_modified():
            self.editor.edit_modified(False)
            self._schedule_highlight()
            self._schedule_preview()

    def _on_template_selected(self) -> None:
        label_to_name = {label: name for name, label in self._template_choices}
        selected_label = self.template_var.get()
        template_name = label_to_name.get(selected_label)
        if template_name:
            self.current_result = None  # Force regenerate with new template.
            self.refresh_now(preferred_template=template_name)

    # Highlighting --------------------------------------------------------
    def _schedule_highlight(self) -> None:
        if self.highlight_job:
            self.root.after_cancel(self.highlight_job)
        self.highlight_job = self.root.after(120, self._highlight_markdown)

    def _highlight_markdown(self) -> None:
        text = self.editor.get("1.0", tk.END)
        for tag in ("heading", "bold", "italic", "code", "link", "line"):  # reset
            self.editor.tag_remove(tag, "1.0", tk.END)

        theme = THEMES[self.theme_var.get()]
        self.editor.tag_configure(
            "heading", foreground=theme["heading"], font=("Segoe UI", 13, "bold")
        )
        self.editor.tag_configure(
            "bold", foreground=theme["bold"], font=("Segoe UI", 11, "bold")
        )
        self.editor.tag_configure(
            "italic", foreground=theme["italic"], font=("Segoe UI", 11, "italic")
        )
        self.editor.tag_configure(
            "code", foreground=theme["code"], font=("Consolas", 11)
        )
        self.editor.tag_configure("link", foreground=theme["link"], underline=True)
        self.editor.tag_configure("line", background=theme["linehighlight"])

        for match in re.finditer(r"^.*$", text, re.MULTILINE):
            start = f"1.0+{match.start()}c"
            end = f"1.0+{match.end()}c"
            if match.group(0).strip():
                self.editor.tag_add("line", start, end)

        for pattern, tag in [
            (r"^#{1,6}.+$", "heading"),
            (r"\*\*(.+?)\*\*", "bold"),
            (r"(?<!\*)\*(?!\*)(.+?)(?<!\*)\*(?!\*)", "italic"),
            (r"`([^`]+)`", "code"),
            (r"```.+?```", "code"),
            (r"\[(.+?)\]\((.+?)\)", "link"),
        ]:
            for match in re.finditer(pattern, text, re.MULTILINE | re.DOTALL):
                start = f"1.0+{match.start()}c"
                end = f"1.0+{match.end()}c"
                self.editor.tag_add(tag, start, end)

    # Preview -------------------------------------------------------------
    def _schedule_preview(self) -> None:
        if self.preview_job:
            self.root.after_cancel(self.preview_job)
        self.preview_job = self.root.after(200, self.refresh_now)

    def refresh_now(self, preferred_template: Optional[str] = None) -> None:
        self.template_manager.refresh()
        self._sync_template_combo()
        text = self.editor.get("1.0", tk.END).rstrip()
        if not text.strip():
            self._update_preview("<p><em>Start typing to see the preview…</em></p>")
            self.status_var.set("Empty document")
            return

        template_name = preferred_template or self._template_name_from_label(
            self.template_var.get()
        )
        try:
            result = convert_markdown(
                text,
                template_manager=self.template_manager,
                preferred_template=template_name,
            )
        except (
            Exception
        ) as exc:  # pragma: no cover - defensive; conversion errors are rare.
            self.status_var.set(f"Conversion error: {exc}")
            return

        self.current_result = result
        if result.template_used and result.template_used != template_name:
            self.template_var.set(
                self.template_manager.labels().get(
                    result.template_used, result.template_used
                )
            )

        self._update_preview(result.html)
        meta_summary = (
            ", ".join(f"{key}: {value}" for key, value in result.metadata.items())
            or "No front matter"
        )
        self.status_var.set(f"Preview updated • {meta_summary}")

    def _update_preview(self, html: str) -> None:
        if HTMLScrolledText is not None and isinstance(self.preview, HTMLScrolledText):
            self.preview.set_html(html)
        else:
            self._update_preview_fallback(html)

    def _update_preview_fallback(self, html: str) -> None:
        self.preview.configure(state=tk.NORMAL)
        self.preview.delete("1.0", tk.END)
        self.preview.insert(tk.END, html)
        self.preview.configure(state=tk.DISABLED)

    def _sync_template_combo(self) -> None:
        labels = sorted(
            self.template_manager.labels().items(), key=lambda item: item[1]
        )
        self._template_choices = labels
        human_labels = [label for _, label in labels]
        self.template_combo.configure(values=human_labels)
        if human_labels and self.template_var.get() not in human_labels:
            self.template_var.set(human_labels[0])

    # File operations -----------------------------------------------------
    def new_file(self) -> None:
        if not self._confirm_discard_changes():
            return
        self.editor.delete("1.0", tk.END)
        self.current_path = None
        self.status_var.set("New document")

    def open_file(self) -> None:
        if not self._confirm_discard_changes():
            return
        path = filedialog.askopenfilename(
            filetypes=[("Markdown", "*.md"), ("All files", "*.*")]
        )
        if not path:
            return
        data = Path(path).read_text(encoding="utf-8")
        self.editor.delete("1.0", tk.END)
        self.editor.insert("1.0", data)
        self.current_path = Path(path)
        self.status_var.set(f"Opened {self.current_path.name}")
        self.refresh_now()

    def save_file(self, save_as: bool = False) -> None:
        if self.current_path is None or save_as:
            path = filedialog.asksaveasfilename(
                defaultextension=".md",
                filetypes=[("Markdown", "*.md"), ("All files", "*.*")],
            )
            if not path:
                return
            self.current_path = Path(path)
        data = self.editor.get("1.0", tk.END)
        self.current_path.write_text(data.rstrip() + "\n", encoding="utf-8")
        self.status_var.set(f"Saved {self.current_path.name}")

    def export_html(self) -> None:
        if self.current_result is None:
            self.refresh_now()
        if self.current_result is None:
            messagebox.showinfo(
                APP_TITLE, "Nothing to export yet – type some Markdown first."
            )
            return
        path = filedialog.asksaveasfilename(
            defaultextension=".html", filetypes=[("HTML", "*.html")]
        )
        if not path:
            return
        Path(path).write_text(self.current_result.html, encoding="utf-8")
        self.status_var.set(f"Exported HTML to {Path(path).name}")

    def export_xml(self) -> None:
        if self.current_result is None:
            self.refresh_now()
        if self.current_result is None:
            messagebox.showinfo(
                APP_TITLE, "Nothing to export yet – type some Markdown first."
            )
            return
        xml_payload = convert_to_xml(self.current_result)
        path = filedialog.asksaveasfilename(
            defaultextension=".xml", filetypes=[("XML", "*.xml")]
        )
        if not path:
            return
        Path(path).write_text(xml_payload, encoding="utf-8")
        self.status_var.set(f"Exported XML to {Path(path).name}")

    # Utilities -----------------------------------------------------------
    def _confirm_discard_changes(self) -> bool:
        content = self.editor.get("1.0", tk.END).strip()
        if not content:
            return True
        if self.current_path is None:
            prompt = "Discard the current unsaved document?"
        else:
            prompt = f"Discard changes to {self.current_path.name}?"
        return messagebox.askyesno(APP_TITLE, prompt)

    def _template_name_from_label(self, label: str) -> Optional[str]:
        for name, human in self.template_manager.labels().items():
            if human == label:
                return name
        return None

    def _apply_theme(self, theme_name: str) -> None:
        theme = THEMES[theme_name]
        self.theme_var.set(theme_name)
        self.editor.configure(
            background=theme["background"],
            foreground=theme["foreground"],
            insertbackground=theme["insertbackground"],
            selectbackground=theme["selectbackground"],
        )
        if HTMLScrolledText is None:
            self.preview.configure(
                background=theme["linehighlight"], foreground=theme["foreground"]
            )
        self._highlight_markdown()

    def toggle_theme(self) -> None:
        next_theme = "Dark" if self.theme_var.get() == "Light" else "Light"
        self._apply_theme(next_theme)

    def _open_templates_folder(self) -> None:
        path = TEMPLATE_DIR
        try:
            if tk.TkVersion >= 8.6:
                self.root.tk.call("::tk::dialog::file::ShowFolder", path)
        except Exception:
            # Portable fallback – show a message containing the path.
            messagebox.showinfo(APP_TITLE, f"Templates live at:\n{path}")

    def _schedule_autosave(self) -> None:
        if self.autosave_job:
            self.root.after_cancel(self.autosave_job)
        self.autosave_job = self.root.after(
            AUTOSAVE_INTERVAL_MS, self._autosave_snapshot
        )

    def _autosave_snapshot(self) -> None:
        content = self.editor.get("1.0", tk.END).strip()
        if content:
            scratch = BASE_DIR / ".autosave.json"
            snapshot = {
                "path": str(self.current_path) if self.current_path else None,
                "content": content,
            }
            scratch.write_text(json.dumps(snapshot, indent=2), encoding="utf-8")
            self.status_var.set("Autosaved draft")
        self._schedule_autosave()

    # Application entry point ---------------------------------------------
    def run(self) -> None:
        self.refresh_now()
        self.root.mainloop()


def main() -> None:
    root = tk.Tk()
    app = MarkdownEditorApp(root)
    app.run()


if __name__ == "__main__":
    main()
