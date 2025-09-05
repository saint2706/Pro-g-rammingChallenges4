import tkinter as tk
from tkinter import filedialog, messagebox
import os

# Import the logic from the refactored command-line script
try:
    from pdftag import update_pdf_metadata, create_output_path
except ImportError:
    print("Error: This GUI requires the 'pdftag.py' script to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class PdfTaggerGUI:
    """
    A Tkinter GUI for the PDF Tagger tool.
    """
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("PDF Metadata Tagger")

        self.input_path = tk.StringVar()

        self._create_widgets()

    def _create_widgets(self):
        """Creates and lays out all the GUI widgets."""
        main_frame = tk.Frame(self.master, padx=10, pady=10)
        main_frame.pack(fill=tk.BOTH, expand=True)

        # --- File Selection ---
        file_frame = tk.Frame(main_frame)
        file_frame.pack(fill=tk.X, pady=5)

        tk.Label(file_frame, text="PDF File:").pack(side=tk.LEFT)
        self.file_entry = tk.Entry(file_frame, textvariable=self.input_path, width=50)
        self.file_entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)
        tk.Button(file_frame, text="Browse...", command=self.browse_file).pack(side=tk.LEFT)

        # --- Metadata Entry ---
        metadata_frame = tk.LabelFrame(main_frame, text="Metadata", padx=10, pady=10)
        metadata_frame.pack(fill=tk.X, pady=10)

        self.entries = {}
        fields = ["Title", "Author", "Subject", "Keywords"]
        for i, field in enumerate(fields):
            tk.Label(metadata_frame, text=f"{field}:").grid(row=i, column=0, sticky=tk.W, pady=2)
            entry = tk.Entry(metadata_frame, width=60)
            entry.grid(row=i, column=1, sticky=tk.EW, padx=5)
            self.entries[field] = entry

        metadata_frame.columnconfigure(1, weight=1)

        # --- Action Button ---
        self.tag_button = tk.Button(main_frame, text="Apply Tags and Save", command=self.apply_tags)
        self.tag_button.pack(pady=10)

    def browse_file(self):
        """Opens a file dialog to select a PDF file."""
        filepath = filedialog.askopenfilename(
            title="Select a PDF file",
            filetypes=(("PDF Files", "*.pdf"), ("All files", "*.*"))
        )
        if filepath:
            self.input_path.set(filepath)

    def apply_tags(self):
        """Gathers metadata from the UI and calls the backend logic to tag the PDF."""
        input_file = self.input_path.get()
        if not input_file:
            messagebox.showerror("Error", "Please select an input PDF file.")
            return

        if not os.path.exists(input_file):
            messagebox.showerror("Error", f"File not found:\n{input_file}")
            return

        metadata = {}
        for field, entry in self.entries.items():
            value = entry.get()
            if value:
                # PyPDF3 expects keys like '/Title', '/Author'
                metadata[f'/{field}'] = value

        if not metadata:
            messagebox.showwarning("Warning", "No metadata was entered. Nothing to apply.")
            return

        output_file = create_output_path(input_file)

        # This will print success/error messages to the console
        update_pdf_metadata(input_file, output_file, metadata)

        messagebox.showinfo("Success", f"Metadata applied.\nSaved new file to:\n{output_file}")


def main():
    """Main function to create the Tkinter window and run the app."""
    root = tk.Tk()
    app = PdfTaggerGUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()
