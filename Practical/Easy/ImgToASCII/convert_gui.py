import tkinter as tk
from tkinter import filedialog, messagebox, scrolledtext
from PIL import Image, ImageTk
import os
import sys

# Import the logic from the refactored command-line script
try:
    from convert import resize_image, image_to_ascii, format_ascii_art, DEFAULT_ASCII_CHARS
except ImportError:
    print("Error: This GUI requires the 'convert.py' script to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class ImgToAsciiGUI:
    """
    A Tkinter GUI for the Image to ASCII Art converter.
    """
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Image to ASCII Art Converter")

        self.input_path = ""
        self.ascii_art_result = ""

        self._create_widgets()

    def _create_widgets(self):
        """Creates and lays out all the GUI widgets."""
        main_frame = tk.Frame(self.master, padx=10, pady=10)
        main_frame.pack(fill=tk.BOTH, expand=True)

        # --- Top Controls Frame ---
        controls_frame = tk.Frame(main_frame)
        controls_frame.pack(fill=tk.X, pady=5)

        tk.Button(controls_frame, text="Open Image...", command=self.browse_file).pack(side=tk.LEFT, padx=(0,10))

        tk.Label(controls_frame, text="Width:").pack(side=tk.LEFT)
        self.width_entry = tk.Entry(controls_frame, width=5)
        self.width_entry.insert(0, "100")
        self.width_entry.pack(side=tk.LEFT, padx=5)

        tk.Label(controls_frame, text="Characters:").pack(side=tk.LEFT)
        self.chars_entry = tk.Entry(controls_frame, width=20)
        self.chars_entry.insert(0, "".join(DEFAULT_ASCII_CHARS))
        self.chars_entry.pack(side=tk.LEFT, fill=tk.X, expand=True, padx=5)

        self.convert_button = tk.Button(controls_frame, text="Convert", command=self.convert_image)
        self.convert_button.pack(side=tk.LEFT, padx=5)

        # --- Result Display ---
        result_frame = tk.Frame(main_frame)
        result_frame.pack(fill=tk.BOTH, expand=True, pady=10)

        self.result_text = scrolledtext.ScrolledText(result_frame, wrap=tk.WORD, font=("Courier New", 8))
        self.result_text.pack(fill=tk.BOTH, expand=True)

        # --- Bottom Save Button ---
        self.save_button = tk.Button(main_frame, text="Save to .txt file...", command=self.save_to_file, state=tk.DISABLED)
        self.save_button.pack(pady=5)

    def browse_file(self):
        """Opens a file dialog to select an image file."""
        filepath = filedialog.askopenfilename(
            title="Select an Image",
            filetypes=(
                ("Image Files", "*.jpg *.jpeg *.png *.bmp *.gif"),
                ("All files", "*.*")
            )
        )
        if filepath:
            self.input_path = filepath
            self.master.title(f"Image to ASCII - {os.path.basename(filepath)}")
            self.result_text.delete('1.0', tk.END)
            self.result_text.insert(tk.INSERT, f"Loaded image: {filepath}\n\nClick 'Convert' to generate ASCII art.")
            self.save_button.config(state=tk.DISABLED)

    def convert_image(self):
        """Performs the image to ASCII conversion and displays the result."""
        if not self.input_path:
            messagebox.showerror("Error", "Please open an image file first.")
            return

        try:
            width = int(self.width_entry.get())
            char_set = list(self.chars_entry.get())
            if not char_set:
                raise ValueError("Character set cannot be empty.")

            with Image.open(self.input_path) as img:
                resized_img = resize_image(img, width)
                ascii_chars_flat = image_to_ascii(resized_img, char_set)
                self.ascii_art_result = format_ascii_art(ascii_chars_flat, width)

            self.result_text.delete('1.0', tk.END)
            self.result_text.insert(tk.INSERT, self.ascii_art_result)
            self.save_button.config(state=tk.NORMAL)

        except ValueError as e:
            messagebox.showerror("Input Error", f"Invalid input: {e}")
        except Exception as e:
            messagebox.showerror("Error", f"An error occurred during conversion: {e}")

    def save_to_file(self):
        """Saves the generated ASCII art to a text file."""
        if not self.ascii_art_result:
            messagebox.showwarning("Warning", "Nothing to save. Please convert an image first.")
            return

        output_path = filedialog.asksaveasfilename(
            title="Save ASCII Art As",
            defaultextension=".txt",
            filetypes=(("Text Files", "*.txt"), ("All files", "*.*")),
            initialfile=f"{os.path.splitext(os.path.basename(self.input_path))[0]}_ascii.txt"
        )

        if output_path:
            try:
                with open(output_path, 'w') as f:
                    f.write(self.ascii_art_result)
                messagebox.showinfo("Success", f"ASCII art saved to\n{output_path}")
            except Exception as e:
                messagebox.showerror("Error", f"Failed to save file: {e}")

def main():
    """Main function to create the Tkinter window and run the app."""
    root = tk.Tk()
    app = ImgToAsciiGUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()
