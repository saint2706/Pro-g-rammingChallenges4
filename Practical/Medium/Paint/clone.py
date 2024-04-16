import tkinter as tk
from tkinter import simpledialog
from tkinter import colorchooser


class PaintApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Paint Clone")

        self.canvas = tk.Canvas(root, bg="white", width=800, height=600)
        self.canvas.pack(expand=True, fill=tk.BOTH)

        self.color = "black"
        self.shape = "line"
        self.start_x = None
        self.start_y = None

        self.canvas.bind("<Button-1>", self.start_draw)
        self.canvas.bind("<B1-Motion>", self.draw)

        self.color_btn = tk.Button(root, text="Color", command=self.choose_color)
        self.color_btn.pack(side=tk.LEFT)

        self.shape_btn = tk.Button(root, text="Shape", command=self.choose_shape)
        self.shape_btn.pack(side=tk.LEFT)

    def start_draw(self, event):
        self.start_x = event.x
        self.start_y = event.y

    def draw(self, event):
        if self.shape == "line":
            self.canvas.create_line(
                self.start_x, self.start_y, event.x, event.y, fill=self.color
            )
            self.start_x = event.x
            self.start_y = event.y
        elif self.shape == "rectangle":
            self.canvas.create_rectangle(
                self.start_x, self.start_y, event.x, event.y, outline=self.color
            )
        elif self.shape == "oval":
            self.canvas.create_oval(
                self.start_x, self.start_y, event.x, event.y, outline=self.color
            )

    def choose_color(self):
        self.color = colorchooser.askcolor()[1]

    def choose_shape(self):
        self.shape = simpledialog.askstring(
            "Shape", "Choose a shape (line, rectangle, oval):"
        )


if __name__ == "__main__":
    root = tk.Tk()
    app = PaintApp(root)
    root.mainloop()
