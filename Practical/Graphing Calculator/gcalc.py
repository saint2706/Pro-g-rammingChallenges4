import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import tkinter as tk
from tkinter import ttk, messagebox
import sympy as sp

class GraphingCalculator:
    """A graphing calculator application using Tkinter, Matplotlib, and SymPy."""
    def __init__(self, master: tk.Tk):
        self.master = master
        master.title("Graphing Calculator")

        self._create_widgets()
        self._initial_plot_setup()

    def _create_widgets(self):
        """Creates and lays out all the GUI widgets."""
        # --- Main Plot Canvas ---
        self.fig, self.ax = plt.subplots(figsize=(8, 6))
        self.canvas = FigureCanvasTkAgg(self.fig, master=self.master)
        self.canvas_widget = self.canvas.get_tk_widget()
        self.canvas_widget.pack(side=tk.TOP, fill=tk.BOTH, expand=1)

        # --- Controls Frame ---
        controls_frame = ttk.Frame(self.master)
        controls_frame.pack(pady=10, padx=10, fill=tk.X)

        # Function Entry
        ttk.Label(controls_frame, text="f(x) =").pack(side=tk.LEFT, padx=(0, 5))
        self.function_entry = ttk.Entry(controls_frame, width=40)
        self.function_entry.pack(side=tk.LEFT, expand=True, fill=tk.X)
        self.function_entry.bind("<Return>", lambda event: self.plot_function())

        # Plotting Buttons
        self.plot_button = ttk.Button(controls_frame, text="Plot", command=self.plot_function)
        self.plot_button.pack(side=tk.LEFT, padx=5)
        self.clear_button = ttk.Button(controls_frame, text="Clear", command=self.clear_plot)
        self.clear_button.pack(side=tk.LEFT, padx=5)

        # Options Frame (Derivative and Range)
        options_frame = ttk.Frame(self.master)
        options_frame.pack(pady=5, padx=10, fill=tk.X)

        self.derivative_var = tk.BooleanVar()
        self.derivative_check = ttk.Checkbutton(options_frame, text="Show Derivative", variable=self.derivative_var)
        self.derivative_check.pack(side=tk.LEFT)

        ttk.Label(options_frame, text="x-min:").pack(side=tk.LEFT, padx=(20, 5))
        self.x_min_entry = ttk.Entry(options_frame, width=5)
        self.x_min_entry.insert(0, "-10")
        self.x_min_entry.pack(side=tk.LEFT)

        ttk.Label(options_frame, text="x-max:").pack(side=tk.LEFT, padx=(10, 5))
        self.x_max_entry = ttk.Entry(options_frame, width=5)
        self.x_max_entry.insert(0, "10")
        self.x_max_entry.pack(side=tk.LEFT)

    def _initial_plot_setup(self):
        """Sets up the initial state of the plot."""
        self.ax.set_xlabel("x")
        self.ax.set_ylabel("y")
        self.ax.set_title("Enter a function to plot")
        self.ax.grid(True)
        self.ax.axhline(0, color='black', linewidth=0.5)
        self.ax.axvline(0, color='black', linewidth=0.5)
        self.canvas.draw()

    def clear_plot(self):
        """Clears the plot and the function entry field."""
        self.ax.clear()
        self.function_entry.delete(0, tk.END)
        self._initial_plot_setup()

    def plot_function(self):
        """Parses the user's function, evaluates it, and plots it."""
        function_str = self.function_entry.get()
        if not function_str:
            messagebox.showwarning("Input Error", "Please enter a function.")
            return

        x = sp.Symbol("x")
        try:
            # Validate and get x-range
            x_min = float(self.x_min_entry.get())
            x_max = float(self.x_max_entry.get())
            if x_min >= x_max:
                raise ValueError("x-min must be less than x-max.")

            # Parse the string into a sympy expression
            function = sp.sympify(function_str)

            # Use sympy.lambdify for fast numerical evaluation with numpy
            f = sp.lambdify(x, function, 'numpy')

            x_vals = np.linspace(x_min, x_max, 1000)
            y_vals = f(x_vals)

            # Clear previous plot and setup new one
            self.ax.clear()
            self.ax.grid(True)
            self.ax.axhline(0, color='black', linewidth=0.5)
            self.ax.axvline(0, color='black', linewidth=0.5)

            self.ax.plot(x_vals, y_vals, label=f"f(x) = {function_str}")

            if self.derivative_var.get():
                derivative = sp.diff(function, x)
                df = sp.lambdify(x, derivative, 'numpy')
                derivative_vals = df(x_vals)
                self.ax.plot(x_vals, derivative_vals, label=f"f'(x) = {derivative}", linestyle='--')

            self.ax.set_xlabel("x")
            self.ax.set_ylabel("y")
            self.ax.set_title(f"Graph of '{function_str}'")
            self.ax.legend()
            self.canvas.draw()

        except (sp.SympifyError, TypeError):
            messagebox.showerror("Invalid Function", f"Could not parse the function: '{function_str}'")
        except ValueError as e:
            messagebox.showerror("Input Error", f"Invalid input for x-range: {e}")
        except Exception as e:
            messagebox.showerror("Error", f"An unexpected error occurred: {e}")

def main():
    """Main function to create the Tkinter window and run the app."""
    root = tk.Tk()
    app = GraphingCalculator(root)
    root.mainloop()

if __name__ == "__main__":
    main()
