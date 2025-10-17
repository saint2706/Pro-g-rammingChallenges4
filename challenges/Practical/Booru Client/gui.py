"""Tkinter GUI for browsing Booru posts."""

from __future__ import annotations

import io
import tkinter as tk
from pathlib import Path
from tkinter import messagebox, ttk
from typing import Dict, List

try:
    from PIL import Image, ImageTk  # type: ignore
except Exception:  # pragma: no cover - pillow optional at runtime
    Image = None  # type: ignore
    ImageTk = None  # type: ignore

from booru_client import BooruClient, CacheManager, Post


class BooruBrowser:
    def __init__(
        self,
        *,
        booru: str,
        cache_dir: Path,
        download_dir: Path,
        page_size: int,
        cache_ttl: int,
    ) -> None:
        self.root = tk.Tk()
        self.root.title(f"Booru Client – {booru}")
        self.page_size = page_size
        self.page = 1
        self.posts: List[Post] = []
        self.preview_cache: Dict[int, tk.PhotoImage] = {}

        cache = CacheManager(cache_dir, ttl=cache_ttl)
        self.client = BooruClient(booru, cache=cache, download_dir=download_dir)

        self._build_ui()
        self._bind_events()

    # ------------------------------- UI ------------------------------------
    def _build_ui(self) -> None:
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(1, weight=1)

        control_frame = ttk.Frame(self.root)
        control_frame.grid(row=0, column=0, sticky="ew", padx=10, pady=5)
        control_frame.columnconfigure(1, weight=1)

        ttk.Label(control_frame, text="Tags:").grid(row=0, column=0, sticky="w")
        self.tags_var = tk.StringVar()
        self.tags_entry = ttk.Entry(control_frame, textvariable=self.tags_var)
        self.tags_entry.grid(row=0, column=1, sticky="ew", padx=(5, 10))

        ttk.Label(control_frame, text="Rating:").grid(row=0, column=2, sticky="w")
        self.rating_var = tk.StringVar(value="any")
        self.rating_combo = ttk.Combobox(
            control_frame,
            textvariable=self.rating_var,
            values=["any", "safe", "questionable", "explicit"],
            width=14,
            state="readonly",
        )
        self.rating_combo.grid(row=0, column=3, sticky="w")
        self.rating_combo.current(0)

        self.search_button = ttk.Button(
            control_frame, text="Search", command=self.refresh
        )
        self.search_button.grid(row=0, column=4, padx=(10, 0))

        main_frame = ttk.Frame(self.root)
        main_frame.grid(row=1, column=0, sticky="nsew")
        main_frame.columnconfigure(1, weight=1)
        main_frame.rowconfigure(0, weight=1)

        self.results = tk.Listbox(main_frame, height=20)
        self.results.grid(row=0, column=0, sticky="nsw")

        self.preview_panel = ttk.Frame(main_frame)
        self.preview_panel.grid(row=0, column=1, sticky="nsew")
        self.preview_panel.columnconfigure(0, weight=1)

        self.preview_label = ttk.Label(
            self.preview_panel, text="Select a post to preview", anchor="center"
        )
        self.preview_label.grid(row=0, column=0, sticky="nwe", padx=10, pady=10)

        self.metadata_text = tk.Text(self.preview_panel, height=10, wrap="word")
        self.metadata_text.grid(row=1, column=0, sticky="nsew", padx=10, pady=(0, 10))
        self.metadata_text.configure(state="disabled")

        nav_frame = ttk.Frame(self.root)
        nav_frame.grid(row=2, column=0, sticky="ew", padx=10, pady=5)
        nav_frame.columnconfigure(1, weight=1)

        self.prev_button = ttk.Button(
            nav_frame, text="◀ Previous", command=self.prev_page
        )
        self.prev_button.grid(row=0, column=0, sticky="w")

        self.page_label = ttk.Label(nav_frame, text="Page 1")
        self.page_label.grid(row=0, column=1)

        self.next_button = ttk.Button(nav_frame, text="Next ▶", command=self.next_page)
        self.next_button.grid(row=0, column=2, sticky="e")

        self.download_button = ttk.Button(
            nav_frame, text="Download Selected", command=self.download_selected
        )
        self.download_button.grid(row=0, column=3, padx=(10, 0))

    def _bind_events(self) -> None:
        self.results.bind("<<ListboxSelect>>", lambda event: self.show_preview())
        self.tags_entry.bind("<Return>", lambda event: self.refresh())

    # ----------------------------- Callbacks -------------------------------
    def refresh(self) -> None:
        rating = self.rating_var.get()
        rating_filter = None if rating == "any" else rating
        tags = [tag for tag in self.tags_var.get().split() if tag]
        try:
            posts = self.client.search_posts(
                tags=tags, rating=rating_filter, limit=self.page_size, page=self.page
            )
        except Exception as exc:  # pragma: no cover - UI feedback only
            messagebox.showerror("Search failed", str(exc))
            return

        self.posts = posts
        self.results.delete(0, tk.END)
        for post in posts:
            self.results.insert(
                tk.END, f"#{post.id} [{post.rating}] {' '.join(post.tags[:5])}"
            )
        self.page_label.config(text=f"Page {self.page}")
        self.preview_label.configure(image="", text="Select a post to preview")
        self.metadata_text.configure(state="normal")
        self.metadata_text.delete("1.0", tk.END)
        self.metadata_text.configure(state="disabled")

    def show_preview(self) -> None:
        if not self.posts:
            return
        selection = self.results.curselection()
        if not selection:
            return
        post = self.posts[selection[0]]
        self._update_preview(post)
        self._update_metadata(post)

    def _update_metadata(self, post: Post) -> None:
        self.metadata_text.configure(state="normal")
        self.metadata_text.delete("1.0", tk.END)
        meta = f"ID: {post.id}\nRating: {post.rating}\nSource: {post.source}\nTags: {' '.join(post.tags)}"
        self.metadata_text.insert("1.0", meta)
        self.metadata_text.configure(state="disabled")

    def _update_preview(self, post: Post) -> None:
        if Image is None or ImageTk is None:
            self.preview_label.configure(text="Pillow not available for previews")
            return
        if post.id in self.preview_cache:
            self.preview_label.configure(image=self.preview_cache[post.id])
            return

        url = post.preview_url or post.file_url
        try:
            self.client.rate_limiter.wait()
            response = self.client.session.get(url, timeout=30)
            response.raise_for_status()
            image = Image.open(io.BytesIO(response.content))
            image.thumbnail((400, 400))
            photo = ImageTk.PhotoImage(image)
            self.preview_cache[post.id] = photo
            self.preview_label.configure(image=photo)
        except Exception as exc:  # pragma: no cover - UI feedback only
            self.preview_label.configure(text=f"Failed to load preview: {exc}")

    def download_selected(self) -> None:
        selection = self.results.curselection()
        if not selection:
            messagebox.showinfo("Download", "Select at least one post to download")
            return
        for index in selection:
            post = self.posts[index]
            try:
                path = self.client.download_post(post)
                messagebox.showinfo("Download", f"Saved to {path}")
            except Exception as exc:  # pragma: no cover - UI feedback only
                messagebox.showerror("Download failed", str(exc))

    def next_page(self) -> None:
        self.page += 1
        self.refresh()

    def prev_page(self) -> None:
        if self.page > 1:
            self.page -= 1
            self.refresh()

    def run(self) -> None:
        self.root.mainloop()


def launch_gui(
    *, booru: str, cache_dir: Path, download_dir: Path, page_size: int, cache_ttl: int
) -> None:
    browser = BooruBrowser(
        booru=booru,
        cache_dir=cache_dir,
        download_dir=download_dir,
        page_size=page_size,
        cache_ttl=cache_ttl,
    )
    browser.run()


if __name__ == "__main__":  # pragma: no cover - manual GUI launch
    launch_gui(
        booru="danbooru",
        cache_dir=Path("cache"),
        download_dir=Path("downloads"),
        page_size=20,
        cache_ttl=900,
    )
