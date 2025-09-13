import tkinter as tk
from tkinter import ttk, messagebox

# Import the logic from the refactored command-line script
try:
    from radix import to_decimal, from_decimal
except ImportError:
    print("Error: This GUI requires the 'radix.py' script to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class RadixConverterGUI:
    """
    A Tkinter GUI for the Radix Base Converter tool.
    """
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Radix Base Converter")
        master.resizable(False, False)

        self.bases = {'Binary': 2, 'Octal': 8, 'Decimal': 10, 'Hexadecimal': 16}
        self.from_base = tk.IntVar(value=10)
        self.to_base = tk.IntVar(value=16)

        self._create_widgets()

    def _create_widgets(self):
        """Creates and lays out all the GUI widgets."""
        main_frame = ttk.Frame(self.master, padding="10")
        main_frame.pack(fill=tk.BOTH, expand=True)

        # --- Input and Output ---
        ttk.Label(main_frame, text="Input Number:").grid(row=0, column=0, columnspan=2, sticky=tk.W)
        self.input_entry = ttk.Entry(main_frame, width=40, font=('Arial', 12))
        self.input_entry.grid(row=1, column=0, columnspan=2, sticky=tk.EW, pady=(0, 10))

        ttk.Label(main_frame, text="Output Number:").grid(row=5, column=0, columnspan=2, sticky=tk.W)
        self.output_var = tk.StringVar()
        ttk.Entry(main_frame, textvariable=self.output_var, width=40, font=('Arial', 12), state='readonly').grid(row=6, column=0, columnspan=2, sticky=tk.EW)

        # --- Base Selection ---
        from_frame = ttk.LabelFrame(main_frame, text="From Base", padding="10")
        from_frame.grid(row=2, column=0, sticky=tk.NSEW, padx=(0, 5))
        for name, value in self.bases.items():
            ttk.Radiobutton(from_frame, text=name, variable=self.from_base, value=value).pack(anchor=tk.W)

        to_frame = ttk.LabelFrame(main_frame, text="To Base", padding="10")
        to_frame.grid(row=2, column=1, sticky=tk.NSEW, padx=(5, 0))
        for name, value in self.bases.items():
            ttk.Radiobutton(to_frame, text=name, variable=self.to_base, value=value).pack(anchor=tk.W)

        # --- Convert Button ---
        self.convert_button = ttk.Button(main_frame, text="Convert", command=self.perform_conversion)
        self.convert_button.grid(row=4, column=0, columnspan=2, pady=20)

    def perform_conversion(self):
        """
        Gets values from the UI, performs the conversion, and displays the result.
        """
        input_number_str = self.input_entry.get().strip()
        from_b = self.from_base.get()
        to_b = self.to_base.get()

        if not input_number_str:
            messagebox.showerror("Input Error", "Please enter a number to convert.")
            return

        try:
            # Step 1: Convert the input number from its base to a decimal (base 10) number.
            if from_b == 10:
                decimal_value = int(input_number_str)
            else:
                decimal_value = to_decimal(input_number_str, from_b)

            # Step 2: Convert the decimal number to the target base.
            if to_b == 10:
                result = str(decimal_value)
            else:
                result = from_decimal(decimal_value, to_b)

            self.output_var.set(result)

        except ValueError as e:
            messagebox.showerror("Conversion Error", f"Error: {e}\nPlease check if the input number is valid for the selected 'From Base'.")
        except Exception as e:
            messagebox.showerror("Error", f"An unexpected error occurred: {e}")

def main():
    """Main function to create the Tkinter window and run the app."""
    root = tk.Tk()
    app = RadixConverterGUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()
