"""Tkinter GUI for the Radix/Base Converter.

Provides a friendly interface wrapping the core logic in `radix.py`.

Features:
    - Radio buttons for selecting source / target bases (2,8,10,16 presets)
    - Real-time validation with status label
    - Swap bases button
    - Clear inputs button (Ctrl+L shortcut)
    - Press Enter to convert
    - Graceful handling of invalid digits per base with specific feedback

Future ideas (not implemented yet):
    - Add arbitrary base entry fields
    - Batch conversion table view
    - History panel with copy to clipboard
"""

import sys
import tkinter as tk
from tkinter import ttk, messagebox

try:  # Import conversion functions dynamically
    from radix import to_decimal, from_decimal
except Exception as e:  # Show console error & abort (GUI root not yet initialized)
    print(f"Import Error: failed to import radix module: {e}", file=sys.stderr)
    sys.exit(1)


class RadixConverterGUI:
    """
    A Tkinter GUI for the Radix Base Converter tool.
    """

    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Radix Base Converter")
        master.resizable(False, False)

        self.bases = {"Binary": 2, "Octal": 8, "Decimal": 10, "Hexadecimal": 16}
        self.from_base = tk.IntVar(value=10)
        self.to_base = tk.IntVar(value=16)

        self._create_widgets()
        self._bind_events()

    def _create_widgets(self):
        """Create and layout the GUI widgets."""
        main_frame = ttk.Frame(self.master, padding="10")
        main_frame.pack(fill=tk.BOTH, expand=True)

        # --- Input and Output ---
        ttk.Label(main_frame, text="Input Number:").grid(
            row=0, column=0, columnspan=2, sticky=tk.W
        )
        self.input_entry = ttk.Entry(main_frame, width=40, font=("Arial", 12))
        self.input_entry.grid(row=1, column=0, columnspan=2, sticky=tk.EW, pady=(0, 10))

        ttk.Label(main_frame, text="Output Number:").grid(
            row=5, column=0, columnspan=2, sticky=tk.W
        )
        self.output_var = tk.StringVar()
        ttk.Entry(
            main_frame,
            textvariable=self.output_var,
            width=40,
            font=("Arial", 12),
            state="readonly",
        ).grid(row=6, column=0, columnspan=2, sticky=tk.EW)

        # --- Base Selection ---
        from_frame = ttk.LabelFrame(main_frame, text="From Base", padding="10")
        from_frame.grid(row=2, column=0, sticky=tk.NSEW, padx=(0, 5))
        for name, value in self.bases.items():
            ttk.Radiobutton(
                from_frame, text=name, variable=self.from_base, value=value
            ).pack(anchor=tk.W)

        to_frame = ttk.LabelFrame(main_frame, text="To Base", padding="10")
        to_frame.grid(row=2, column=1, sticky=tk.NSEW, padx=(5, 0))
        for name, value in self.bases.items():
            ttk.Radiobutton(
                to_frame, text=name, variable=self.to_base, value=value
            ).pack(anchor=tk.W)

        # --- Convert Button ---
        btn_frame = ttk.Frame(main_frame)
        btn_frame.grid(row=4, column=0, columnspan=2, pady=15)
        self.convert_button = ttk.Button(
            btn_frame, text="Convert", command=self.perform_conversion
        )
        self.convert_button.pack(side=tk.LEFT, padx=5)
        self.swap_button = ttk.Button(btn_frame, text="Swap", command=self.swap_bases)
        self.swap_button.pack(side=tk.LEFT, padx=5)
        self.clear_button = ttk.Button(
            btn_frame, text="Clear", command=self.clear_fields
        )
        self.clear_button.pack(side=tk.LEFT, padx=5)

        # Status label for validation / messages
        self.status_var = tk.StringVar()
        self.status_label = ttk.Label(
            main_frame, textvariable=self.status_var, foreground="grey"
        )
        self.status_label.grid(row=7, column=0, columnspan=2, sticky=tk.W, pady=(8, 0))

    def _bind_events(self):
        self.master.bind("<Return>", lambda _e: self.perform_conversion())
        self.master.bind("<Control-l>", lambda _e: self.clear_fields())

    def perform_conversion(self):
        """Validate input, perform conversion, display result, update status."""
        self.status_var.set("")
        input_number_str = self.input_entry.get().strip()
        from_b = self.from_base.get()
        to_b = self.to_base.get()

        if not input_number_str:
            self.status_var.set("Enter a value to convert.")
            self.output_var.set("")
            return

        try:
            if from_b == 10:
                decimal_value = int(input_number_str)
            else:
                decimal_value = to_decimal(input_number_str, from_b)

            if to_b == 10:
                result = str(decimal_value)
            else:
                result = from_decimal(decimal_value, to_b)

            self.output_var.set(result)
            self.status_var.set("Success")
        except ValueError as e:
            self.output_var.set("")
            self.status_var.set(str(e))
        except Exception as e:  # fallback unexpected error
            self.output_var.set("")
            self.status_var.set(f"Unexpected error: {e}")

    def swap_bases(self):
        """Swap the selected from/to bases and re-run conversion if possible."""
        f = self.from_base.get()
        t = self.to_base.get()
        self.from_base.set(t)
        self.to_base.set(f)
        if self.input_entry.get().strip():
            self.perform_conversion()

    def clear_fields(self):
        """Clear both input and output fields and reset status."""
        self.input_entry.delete(0, tk.END)
        self.output_var.set("")
        self.status_var.set("Cleared")


def main():
    """Main function to create the Tkinter window and run the app."""
    root = tk.Tk()
    app = RadixConverterGUI(root)
    root.mainloop()


if __name__ == "__main__":
    main()
