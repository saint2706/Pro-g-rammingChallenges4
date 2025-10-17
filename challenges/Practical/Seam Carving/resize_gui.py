import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from PIL import Image, ImageTk
import threading
import queue
import sys
import os
from typing import Optional

# Import logic (expects resize.py in same directory)
try:
    from resize import SeamCarver, CarveConfig
except Exception as e:
    print(f"Error: This GUI requires the 'resize.py' script: {e}", file=sys.stderr)
    sys.exit(1)


class SeamCarvingGUI:
    """Tkinter GUI for seam carving (content‑aware image resizing).

    Features:
      - Image load dialog
      - Inputs for number of vertical (width) & horizontal (height) seams to remove
      - Background thread processing with progress polling queue
      - Cancel operation button
      - Validation for excessive seam removal
      - Dual panels: original vs processed
    """

    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Seam Carving Content-Aware Resizing")

        self.original_image_path: Optional[str] = None
        self.original_img_pil: Optional[Image.Image] = None
        self.processed_img_pil: Optional[Image.Image] = None
        self._worker_thread: Optional[threading.Thread] = None
        self._cancel_flag = threading.Event()

        self.gui_queue: "queue.Queue[tuple[str, object]]" = queue.Queue()

        self._create_widgets()

    def _create_widgets(self) -> None:
        # --- Main Frames ---
        top_frame = ttk.Frame(self.master, padding="5")
        top_frame.pack(fill=tk.X)

        image_frame = ttk.Frame(self.master, padding="5")
        image_frame.pack(fill=tk.BOTH, expand=True)

        # --- Top Controls ---
        ttk.Button(top_frame, text="Load Image...", command=self.load_image).pack(
            side=tk.LEFT, padx=5
        )

        ttk.Label(top_frame, text="Remove Width (px):").pack(side=tk.LEFT, padx=(10, 2))
        self.width_entry = ttk.Entry(top_frame, width=5)
        self.width_entry.insert(0, "50")
        self.width_entry.pack(side=tk.LEFT)

        ttk.Label(top_frame, text="Remove Height (px):").pack(
            side=tk.LEFT, padx=(10, 2)
        )
        self.height_entry = ttk.Entry(top_frame, width=5)
        self.height_entry.insert(0, "0")
        self.height_entry.pack(side=tk.LEFT)

        self.carve_button = ttk.Button(
            top_frame,
            text="Start Carving",
            command=self.start_carving,
            state=tk.DISABLED,
        )
        self.carve_button.pack(side=tk.LEFT, padx=10)
        self.cancel_button = ttk.Button(
            top_frame, text="Cancel", command=self.cancel_carving, state=tk.DISABLED
        )
        self.cancel_button.pack(side=tk.LEFT)

        # --- Image Display ---
        self.original_panel = ttk.Label(
            image_frame, text="Original Image", relief="solid"
        )
        self.original_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=5)

        self.processed_panel = ttk.Label(
            image_frame, text="Processed Image", relief="solid"
        )
        self.processed_panel.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=5)

        # --- Status Bar ---
        self.status_var = tk.StringVar(value="Please load an image to begin.")
        ttk.Label(
            self.master, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W
        ).pack(side=tk.BOTTOM, fill=tk.X)

    def load_image(self) -> None:
        filepath = filedialog.askopenfilename(
            title="Select an Image",
            filetypes=[("Image Files", "*.png *.jpg *.jpeg *.bmp")],
        )
        if not filepath:
            return

        self.original_image_path = filepath
        self.original_img_pil = Image.open(self.original_image_path)
        self.display_image(self.original_img_pil, self.original_panel)

        self.processed_panel.configure(image="")  # Clear processed panel
        self.processed_panel.image = None  # type: ignore[attr-defined]

        self.status_var.set(f"Loaded: {os.path.basename(filepath)}")
        self.carve_button.config(state=tk.NORMAL)

    def display_image(self, pil_image: Image.Image, panel: ttk.Label) -> None:
        """Resizes and displays a PIL image on a Tkinter Label."""
        panel_w, panel_h = 400, 400  # Fixed panel size for display
        img_copy = pil_image.copy()
        img_copy.thumbnail((panel_w, panel_h), Image.Resampling.LANCZOS)

        img_tk = ImageTk.PhotoImage(img_copy)
        panel.configure(image=img_tk)
        panel.image = img_tk  # type: ignore[attr-defined]  # Keep a reference

    def start_carving(self) -> None:
        """Validate inputs and launch background carving thread."""
        if not self.original_image_path:
            messagebox.showerror("Error", "No image loaded.")
            return

        try:
            delta_w = int(self.width_entry.get())
            delta_h = int(self.height_entry.get())
            if delta_w < 0 or delta_h < 0:
                raise ValueError()
        except ValueError:
            messagebox.showerror(
                "Input Error",
                "Please enter valid, non-negative integers for width and height removal.",
            )
            return

        # Dimension validation
        if self.original_img_pil is None:  # safety guard
            messagebox.showerror("State Error", "Original image not loaded correctly.")
            return
        ow, oh = self.original_img_pil.size
        if delta_w >= ow or delta_h >= oh:
            messagebox.showerror(
                "Input Error",
                "Cannot remove seams equal/greater than image dimensions.",
            )
            return

        self.carve_button.config(state=tk.DISABLED)
        self.cancel_button.config(state=tk.NORMAL)
        self._cancel_flag.clear()

        self._worker_thread = threading.Thread(
            target=self.run_carver_thread,
            args=(delta_w, delta_h),
            daemon=True,
        )
        self._worker_thread.start()
        self.master.after(100, self.process_queue)

    def run_carver_thread(self, delta_w: int, delta_h: int) -> None:
        """Background worker: performs seam carving, updating queue for GUI thread."""
        try:
            self.gui_queue.put(("status", "Loading image for carving..."))

            def progress(axis: str, done: int, total: int):
                if self._cancel_flag.is_set():
                    raise RuntimeError("Operation cancelled")
                self.gui_queue.put(("status", f"Removing {axis} seams {done}/{total}"))

            # self.original_image_path is ensured non-None earlier; narrow type for type-checkers
            assert (
                self.original_image_path is not None
            ), "original_image_path unexpectedly None"
            carver = SeamCarver(
                self.original_image_path, CarveConfig(progress=progress)
            )

            if delta_w > 0:
                self.gui_queue.put(("status", f"Carving {delta_w} vertical seams..."))
                carver.carve_width(delta_w)

            if delta_h > 0:
                self.gui_queue.put(("status", f"Carving {delta_h} horizontal seams..."))
                carver.carve_height(delta_h)

            # Convert result from OpenCV BGR to PIL RGB
            result_bgr = carver.get_image()
            result_rgb = result_bgr[:, :, ::-1]  # BGR to RGB
            self.processed_img_pil = Image.fromarray(result_rgb)

            self.gui_queue.put(("image", self.processed_img_pil))
            self.gui_queue.put(("status", "Carving complete!"))

        except RuntimeError as e:
            self.gui_queue.put(("status", str(e)))
        except Exception as e:  # unexpected
            self.gui_queue.put(("error", f"An error occurred: {e}"))
        finally:
            self.gui_queue.put(("done", None))

    def process_queue(self) -> None:
        """Checks the queue for messages from the worker thread and updates the GUI."""
        try:
            message_type, data = self.gui_queue.get_nowait()
            if message_type == "status":
                self.status_var.set(str(data))
            elif message_type == "image":
                if isinstance(data, Image.Image):
                    self.display_image(data, self.processed_panel)
            elif message_type == "error":
                messagebox.showerror("Processing Error", str(data))
            elif message_type == "done":
                self.carve_button.config(state=tk.NORMAL)
                self.cancel_button.config(state=tk.DISABLED)
                return  # Stop polling
        except queue.Empty:
            pass  # No message, do nothing

        self.master.after(100, self.process_queue)

    def cancel_carving(self) -> None:
        """Signal cancellation; worker will raise and cleanup."""
        self._cancel_flag.set()
        self.status_var.set("Cancelling...")


def main():
    root = tk.Tk()
    app = SeamCarvingGUI(root)
    root.mainloop()


if __name__ == "__main__":
    main()
