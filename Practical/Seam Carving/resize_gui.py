import tkinter as tk
from tkinter import ttk, filedialog, messagebox
from PIL import Image, ImageTk
import threading
import queue
import sys
import os

# Import the logic from the refactored command-line script
try:
    from resize import SeamCarver
except ImportError:
    print("Error: This GUI requires the 'resize.py' script to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class SeamCarvingGUI:
    """A Tkinter GUI for the Seam Carving application."""
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Seam Carving Content-Aware Resizing")

        self.original_image_path = None
        self.original_img_pil = None
        self.processed_img_pil = None

        self.gui_queue = queue.Queue()

        self._create_widgets()

    def _create_widgets(self):
        # --- Main Frames ---
        top_frame = ttk.Frame(self.master, padding="5")
        top_frame.pack(fill=tk.X)

        image_frame = ttk.Frame(self.master, padding="5")
        image_frame.pack(fill=tk.BOTH, expand=True)

        # --- Top Controls ---
        ttk.Button(top_frame, text="Load Image...", command=self.load_image).pack(side=tk.LEFT, padx=5)

        ttk.Label(top_frame, text="Remove Width (px):").pack(side=tk.LEFT, padx=(10, 2))
        self.width_entry = ttk.Entry(top_frame, width=5)
        self.width_entry.insert(0, "50")
        self.width_entry.pack(side=tk.LEFT)

        ttk.Label(top_frame, text="Remove Height (px):").pack(side=tk.LEFT, padx=(10, 2))
        self.height_entry = ttk.Entry(top_frame, width=5)
        self.height_entry.insert(0, "0")
        self.height_entry.pack(side=tk.LEFT)

        self.carve_button = ttk.Button(top_frame, text="Start Carving", command=self.start_carving, state=tk.DISABLED)
        self.carve_button.pack(side=tk.LEFT, padx=10)

        # --- Image Display ---
        self.original_panel = ttk.Label(image_frame, text="Original Image", relief="solid")
        self.original_panel.pack(side=tk.LEFT, fill=tk.BOTH, expand=True, padx=5)

        self.processed_panel = ttk.Label(image_frame, text="Processed Image", relief="solid")
        self.processed_panel.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True, padx=5)

        # --- Status Bar ---
        self.status_var = tk.StringVar(value="Please load an image to begin.")
        ttk.Label(self.master, textvariable=self.status_var, relief=tk.SUNKEN, anchor=tk.W).pack(side=tk.BOTTOM, fill=tk.X)

    def load_image(self):
        filepath = filedialog.askopenfilename(title="Select an Image", filetypes=[("Image Files", "*.png *.jpg *.jpeg *.bmp")])
        if not filepath: return

        self.original_image_path = filepath
        self.original_img_pil = Image.open(self.original_image_path)
        self.display_image(self.original_img_pil, self.original_panel)

        self.processed_panel.configure(image='') # Clear processed panel
        self.processed_panel.image = None

        self.status_var.set(f"Loaded: {os.path.basename(filepath)}")
        self.carve_button.config(state=tk.NORMAL)

    def display_image(self, pil_image: Image.Image, panel: ttk.Label):
        """Resizes and displays a PIL image on a Tkinter Label."""
        panel_w, panel_h = 400, 400 # Fixed panel size for display
        img_copy = pil_image.copy()
        img_copy.thumbnail((panel_w, panel_h), Image.Resampling.LANCZOS)

        img_tk = ImageTk.PhotoImage(img_copy)
        panel.configure(image=img_tk)
        panel.image = img_tk # Keep a reference

    def start_carving(self):
        """Validates input and starts the carving process in a background thread."""
        if not self.original_image_path:
            messagebox.showerror("Error", "No image loaded.")
            return

        try:
            delta_w = int(self.width_entry.get())
            delta_h = int(self.height_entry.get())
            if delta_w < 0 or delta_h < 0: raise ValueError()
        except ValueError:
            messagebox.showerror("Input Error", "Please enter valid, non-negative integers for width and height removal.")
            return

        self.carve_button.config(state=tk.DISABLED)

        thread = threading.Thread(
            target=self.run_carver_thread,
            args=(delta_w, delta_h),
            daemon=True
        )
        thread.start()
        self.master.after(100, self.process_queue)

    def run_carver_thread(self, delta_w: int, delta_h: int):
        """The background task that runs the seam carving algorithm."""
        try:
            self.gui_queue.put(("status", "Loading image for carving..."))
            carver = SeamCarver(self.original_image_path)

            if delta_w > 0:
                self.gui_queue.put(("status", f"Carving {delta_w} vertical seams..."))
                carver.carve_width(delta_w)

            if delta_h > 0:
                self.gui_queue.put(("status", f"Carving {delta_h} horizontal seams..."))
                carver.carve_height(delta_h)

            # Convert result from OpenCV BGR to PIL RGB
            result_bgr = carver.get_image()
            result_rgb = result_bgr[:, :, ::-1] # BGR to RGB
            self.processed_img_pil = Image.fromarray(result_rgb)

            self.gui_queue.put(("image", self.processed_img_pil))
            self.gui_queue.put(("status", "Carving complete!"))

        except Exception as e:
            self.gui_queue.put(("error", f"An error occurred: {e}"))
        finally:
            self.gui_queue.put(("done", None))

    def process_queue(self):
        """Checks the queue for messages from the worker thread and updates the GUI."""
        try:
            message_type, data = self.gui_queue.get_nowait()
            if message_type == "status":
                self.status_var.set(data)
            elif message_type == "image":
                self.display_image(data, self.processed_panel)
            elif message_type == "error":
                messagebox.showerror("Processing Error", data)
            elif message_type == "done":
                self.carve_button.config(state=tk.NORMAL)
                return # Stop polling
        except queue.Empty:
            pass # No message, do nothing

        self.master.after(100, self.process_queue)

def main():
    root = tk.Tk()
    app = SeamCarvingGUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()
