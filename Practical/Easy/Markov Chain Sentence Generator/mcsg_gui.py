import tkinter as tk
from tkinter import ttk, filedialog, messagebox, scrolledtext
import threading
import sys
import os

# Import the logic from the refactored command-line script
try:
    from mcsg import MarkovGenerator
except ImportError:
    print("Error: This GUI requires the 'mcsg.py' script to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class MarkovGUI:
    """
    A Tkinter GUI for the Markov Chain Sentence Generator.
    """
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Markov Chain Sentence Generator")

        self.generator = None
        self.source_text_path = ""

        self._create_widgets()

    def _create_widgets(self):
        main_frame = ttk.Frame(self.master, padding="10")
        main_frame.pack(fill=tk.BOTH, expand=True)

        # --- Source Text Frame ---
        source_frame = ttk.LabelFrame(main_frame, text="Source Text", padding="5")
        source_frame.pack(fill=tk.X, pady=5)

        self.load_button = ttk.Button(source_frame, text="Load Text File...", command=self.load_file)
        self.load_button.pack(pady=5)
        self.source_label = ttk.Label(source_frame, text="No file loaded.")
        self.source_label.pack(pady=5)

        # --- Generation Controls ---
        controls_frame = ttk.LabelFrame(main_frame, text="Generation Options", padding="5")
        controls_frame.pack(fill=tk.X, pady=5)

        ttk.Label(controls_frame, text="Sentence Length:").grid(row=0, column=0, padx=5, pady=2, sticky=tk.W)
        self.length_entry = ttk.Entry(controls_frame, width=5)
        self.length_entry.insert(0, "15")
        self.length_entry.grid(row=0, column=1, padx=5, sticky=tk.W)

        ttk.Label(controls_frame, text="Number of Sentences:").grid(row=0, column=2, padx=5, pady=2, sticky=tk.W)
        self.num_entry = ttk.Entry(controls_frame, width=5)
        self.num_entry.insert(0, "5")
        self.num_entry.grid(row=0, column=3, padx=5, sticky=tk.W)

        ttk.Label(controls_frame, text="Start Word (optional):").grid(row=1, column=0, padx=5, pady=2, sticky=tk.W)
        self.start_entry = ttk.Entry(controls_frame)
        self.start_entry.grid(row=1, column=1, columnspan=3, padx=5, sticky=tk.EW)

        self.generate_button = ttk.Button(controls_frame, text="Generate", command=self.start_generation, state=tk.DISABLED)
        self.generate_button.grid(row=2, column=0, columnspan=4, pady=10)

        # --- Results Display ---
        results_frame = ttk.LabelFrame(main_frame, text="Generated Sentences", padding="5")
        results_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        self.result_text = scrolledtext.ScrolledText(results_frame, wrap=tk.WORD, font=("Georgia", 12))
        self.result_text.pack(fill=tk.BOTH, expand=True)

        # --- Status Bar ---
        self.status_bar = ttk.Label(self.master, text="Ready.", padding="2")
        self.status_bar.pack(side=tk.BOTTOM, fill=tk.X)

    def load_file(self):
        """Opens a file dialog to select a source text file."""
        filepath = filedialog.askopenfilename(title="Select a Text File", filetypes=(("Text Files", "*.txt"), ("All files", "*.*")))
        if not filepath:
            return

        self.source_text_path = filepath
        self.source_label.config(text=f"Loaded: {os.path.basename(filepath)}")
        self.status_bar.config(text="File loaded. Click 'Generate' to build the model.")
        self.generate_button.config(state=tk.NORMAL)
        self.result_text.delete('1.0', tk.END)

    def start_generation(self):
        """Starts the model training and sentence generation in a background thread."""
        if not self.source_text_path:
            messagebox.showerror("Error", "Please load a source text file first.")
            return

        try:
            length = int(self.length_entry.get())
            num_sentences = int(self.num_entry.get())
            start_word = self.start_entry.get() or None
        except ValueError:
            messagebox.showerror("Input Error", "Please enter valid numbers for length and number of sentences.")
            return

        self.generate_button.config(state=tk.DISABLED)
        self.load_button.config(state=tk.DISABLED)
        self.result_text.delete('1.0', tk.END)
        self.status_bar.config(text="Training Markov model... This may take a moment.")

        # Run in a thread to keep the GUI from freezing on large files
        thread = threading.Thread(
            target=self.run_generator_thread,
            args=(length, num_sentences, start_word),
            daemon=True
        )
        thread.start()

    def run_generator_thread(self, length, num_sentences, start_word):
        """The background task that trains the model and generates text."""
        try:
            with open(self.source_text_path, 'r', encoding='utf-8') as f:
                text = f.read()

            self.generator = MarkovGenerator(state_size=2)
            self.generator.train(text)

            self.master.after(0, lambda: self.status_bar.config(text="Generating sentences..."))

            results = []
            for _ in range(num_sentences):
                sentence = self.generator.generate(length, start_word)
                results.append(sentence)

            self.master.after(0, lambda: self.display_results(results))

        except Exception as e:
            self.master.after(0, lambda: messagebox.showerror("Error", f"An error occurred: {e}"))
        finally:
            self.master.after(0, self.generation_finished)

    def display_results(self, sentences: List[str]):
        """Displays the generated sentences in the text widget."""
        self.result_text.delete('1.0', tk.END)
        for i, sentence in enumerate(sentences, 1):
            self.result_text.insert(tk.END, f"{i}. {sentence}\n\n")

    def generation_finished(self):
        """Called when the background thread is done to re-enable buttons."""
        self.status_bar.config(text="Ready.")
        self.generate_button.config(state=tk.NORMAL)
        self.load_button.config(state=tk.NORMAL)

def main():
    root = tk.Tk()
    app = MarkovGUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()
