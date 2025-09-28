"""Tkinter GUI wrapper for the Practical Password Manager."""

from __future__ import annotations

import argparse
import json
import tkinter as tk
from tkinter import filedialog, messagebox, simpledialog
from pathlib import Path
from typing import Optional

from vault import PasswordVault, VaultEntry, generate_password


class PasswordManagerApp:
    def __init__(self, root: tk.Tk, vault: PasswordVault):
        self.root = root
        self.vault = vault
        self.root.title("Password Manager")
        self._build_ui()
        self.refresh_entries()

    # ------------------------------------------------------------------
    def _build_ui(self) -> None:
        self.listbox = tk.Listbox(self.root, width=80)
        self.listbox.grid(
            row=0, column=0, columnspan=4, sticky="nsew", padx=10, pady=10
        )
        self.listbox.bind("<<ListboxSelect>>", lambda _event: self._on_select())

        scrollbar = tk.Scrollbar(
            self.root, orient=tk.VERTICAL, command=self.listbox.yview
        )
        scrollbar.grid(row=0, column=4, sticky="ns", pady=10)
        self.listbox.configure(yscrollcommand=scrollbar.set)

        btn_add = tk.Button(self.root, text="Add Entry", command=self.add_entry)
        btn_add.grid(row=1, column=0, padx=5, pady=5, sticky="ew")
        self.btn_view = tk.Button(
            self.root, text="View", command=self.view_entry, state=tk.DISABLED
        )
        self.btn_view.grid(row=1, column=1, padx=5, pady=5, sticky="ew")
        self.btn_edit = tk.Button(
            self.root, text="Edit", command=self.edit_entry, state=tk.DISABLED
        )
        self.btn_edit.grid(row=1, column=2, padx=5, pady=5, sticky="ew")
        self.btn_delete = tk.Button(
            self.root, text="Delete", command=self.delete_entry, state=tk.DISABLED
        )
        self.btn_delete.grid(row=1, column=3, padx=5, pady=5, sticky="ew")

        btn_generate = tk.Button(
            self.root, text="Generate Password", command=self.copy_generated_password
        )
        btn_generate.grid(row=2, column=0, padx=5, pady=5, sticky="ew")
        btn_export = tk.Button(
            self.root, text="Export JSON", command=self.export_entries
        )
        btn_export.grid(row=2, column=1, padx=5, pady=5, sticky="ew")
        btn_audit = tk.Button(self.root, text="View Audit Log", command=self.show_audit)
        btn_audit.grid(row=2, column=2, padx=5, pady=5, sticky="ew")
        btn_refresh = tk.Button(self.root, text="Refresh", command=self.refresh_entries)
        btn_refresh.grid(row=2, column=3, padx=5, pady=5, sticky="ew")

        self.root.columnconfigure(0, weight=1)
        self.root.columnconfigure(1, weight=1)
        self.root.columnconfigure(2, weight=1)
        self.root.columnconfigure(3, weight=1)
        self.root.rowconfigure(0, weight=1)

    # ------------------------------------------------------------------
    def refresh_entries(self) -> None:
        self.listbox.delete(0, tk.END)
        self.entries = self.vault.list_entries()
        for entry in self.entries:
            display = f"{entry.name} ({entry.username}) [{entry.category}]"
            self.listbox.insert(tk.END, display)
        self._on_select()

    def _get_selected_entry(self) -> Optional[tuple[int, VaultEntry]]:
        selection = self.listbox.curselection()
        if not selection:
            return None
        index = selection[0]
        return index, self.entries[index]

    def _on_select(self) -> None:
        has_selection = bool(self.listbox.curselection())
        state = tk.NORMAL if has_selection else tk.DISABLED
        for button in (self.btn_view, self.btn_edit, self.btn_delete):
            button.configure(state=state)

    def add_entry(self) -> None:
        details = self._prompt_entry()
        if not details:
            return
        self.vault.add_entry(**details)
        self.refresh_entries()

    def edit_entry(self) -> None:
        selection = self._get_selected_entry()
        if not selection:
            return
        index, entry = selection
        details = self._prompt_entry(entry)
        if not details:
            return
        self.vault.update_entry(entry.entry_id, **details)
        self.refresh_entries()
        self.listbox.selection_set(index)

    def delete_entry(self) -> None:
        selection = self._get_selected_entry()
        if not selection:
            return
        index, entry = selection
        if messagebox.askyesno("Delete", f"Delete entry '{entry.name}'?"):
            self.vault.delete_entry(entry.entry_id)
            self.refresh_entries()

    def view_entry(self) -> None:
        selection = self._get_selected_entry()
        if not selection:
            return
        _index, entry = selection
        info = json.dumps(entry.to_dict(), indent=2, ensure_ascii=False)
        messagebox.showinfo("Entry Details", info)

    def copy_generated_password(self) -> None:
        password = generate_password()
        self.root.clipboard_clear()
        self.root.clipboard_append(password)
        messagebox.showinfo("Password Generated", "Password copied to clipboard.")

    def export_entries(self) -> None:
        path = filedialog.asksaveasfilename(
            title="Export entries",
            defaultextension=".json",
            filetypes=[("JSON", "*.json")],
        )
        if not path:
            return
        entries = [entry.to_dict() for entry in self.vault.list_entries()]
        Path(path).write_text(
            json.dumps(entries, indent=2, ensure_ascii=False), encoding="utf-8"
        )
        messagebox.showinfo("Export Complete", f"Exported {len(entries)} entries.")

    def show_audit(self) -> None:
        events = self.vault.audit_log()
        if not events:
            messagebox.showinfo("Audit Log", "No events recorded yet.")
            return
        text = "\n".join(
            f"{e.timestamp} | {e.action} | {e.entry_id or '-'} | {e.detail}"
            for e in events
        )
        messagebox.showinfo("Audit Log", text)

    # ------------------------------------------------------------------
    def _prompt_entry(self, entry=None) -> Optional[dict]:
        name = simpledialog.askstring(
            "Entry", "Name", initialvalue=getattr(entry, "name", ""), parent=self.root
        )
        if not name:
            return None
        username = simpledialog.askstring(
            "Entry",
            "Username",
            initialvalue=getattr(entry, "username", ""),
            parent=self.root,
        )
        if username is None:
            return None
        password = simpledialog.askstring(
            "Entry",
            "Password (leave blank to generate)",
            initialvalue=getattr(entry, "password", ""),
            show="*",
            parent=self.root,
        )
        if password is None:
            return None
        if not password:
            password = generate_password()
        category = simpledialog.askstring(
            "Entry",
            "Category",
            initialvalue=getattr(entry, "category", "general"),
            parent=self.root,
        )
        if category is None:
            return None
        notes = simpledialog.askstring(
            "Entry", "Notes", initialvalue=getattr(entry, "notes", ""), parent=self.root
        )
        if notes is None:
            return None
        return {
            "name": name,
            "username": username,
            "password": password,
            "category": category or "general",
            "notes": notes,
        }


def prompt_for_vault(args: argparse.Namespace) -> PasswordVault:
    path = Path(args.vault)
    if not path.exists():
        if messagebox.askyesno("Vault", f"Vault not found at {path}. Create it?"):
            password = _prompt_password(confirm=True)
            return PasswordVault.create(path, password)
        raise SystemExit("Vault not created")
    password = args.password or _prompt_password()
    return PasswordVault.load(path, password)


def _prompt_password(confirm: bool = False) -> str:
    while True:
        password = simpledialog.askstring("Vault Password", "Master password", show="*")
        if not password:
            continue
        if confirm:
            confirm_pw = simpledialog.askstring(
                "Vault Password", "Confirm password", show="*"
            )
            if password != confirm_pw:
                messagebox.showerror("Password", "Passwords do not match")
                continue
        return password


def main() -> None:
    parser = argparse.ArgumentParser(description="Password Manager GUI")
    parser.add_argument("--vault", default="vault.pm")
    parser.add_argument("--password")
    args = parser.parse_args()

    root = tk.Tk()
    try:
        vault = prompt_for_vault(args)
    except Exception as exc:  # pragma: no cover - GUI prompt path
        messagebox.showerror("Vault", str(exc))
        root.destroy()
        return

    app = PasswordManagerApp(root, vault)
    root.mainloop()


if __name__ == "__main__":
    main()
