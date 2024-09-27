import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import tkinter as tk
from tkinter import ttk
import sympy as sp


class GraphingCalculator:
    def __init__(self, master):
        self.master = master
        master.title("Graphing Calculator")

        self.fig, self.ax = plt.subplots(figsize=(8, 6))
        self.canvas = FigureCanvasTkAgg(self.fig, master=master)
        self.canvas_widget = self.canvas.get_tk_widget()
        self.canvas_widget.pack(side=tk.TOP, fill=tk.BOTH, expand=1)

        self.function_entry = ttk.Entry(master, width=50)
        self.function_entry.pack(pady=10)

        self.plot_button = ttk.Button(master, text="Plot", command=self.plot_function)
        self.plot_button.pack(pady=5)

        self.derivative_var = tk.BooleanVar()
        self.derivative_check = ttk.Checkbutton(
            master, text="Show Derivative", variable=self.derivative_var
        )
        self.derivative_check.pack(pady=5)

    def plot_function(self):
        self.ax.clear()
        function_str = self.function_entry.get()
        x = sp.Symbol("x")

        try:
            function = sp.sympify(function_str)
            x_vals = np.linspace(-10, 10, 1000)
            y_vals = [float(function.subs(x, val)) for val in x_vals]

            self.ax.plot(x_vals, y_vals, label="Function")

            if self.derivative_var.get():
                derivative = sp.diff(function, x)
                derivative_vals = [float(derivative.subs(x, val)) for val in x_vals]
                self.ax.plot(x_vals, derivative_vals, label="Derivative")

            self.ax.set_xlabel("x")
            self.ax.set_ylabel("y")
            self.ax.set_title(f"Graph of {function_str}")
            self.ax.legend()
            self.ax.grid(True)
            self.canvas.draw()

        except Exception as e:
            print(f"Error: {e}")


root = tk.Tk()
calculator = GraphingCalculator(root)
root.mainloop()
