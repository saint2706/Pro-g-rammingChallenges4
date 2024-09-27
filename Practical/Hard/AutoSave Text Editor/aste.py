import tkinter as tk
from tkinter import filedialog
import datetime
import os


class AutosaveTextEditor:
    def __init__(self, master):
        self.master = master
        self.master.title("Autosave Text Editor")
        self.master.geometry("800x600")

        self.text_area = tk.Text(self.master, wrap="word", undo=True)
        self.text_area.pack(expand=True, fill="both")

        self.menu_bar = tk.Menu(self.master)
        self.master.config(menu=self.menu_bar)

        self.file_menu = tk.Menu(self.menu_bar, tearoff=0)
        self.menu_bar.add_cascade(label="File", menu=self.file_menu)
        self.file_menu.add_command(label="New", command=self.new_file)
        self.file_menu.add_command(label="Open", command=self.open_file)
        self.file_menu.add_command(label="Save As", command=self.save_as)
        self.file_menu.add_separator()
        self.file_menu.add_command(label="Exit", command=self.master.quit)

        self.status_bar = tk.Label(
            self.master, text="", bd=1, relief=tk.SUNKEN, anchor=tk.W
        )
        self.status_bar.pack(side=tk.BOTTOM, fill=tk.X)

        self.current_file = None
        self.last_content = ""

        self.autosave()

    def new_file(self):
        self.text_area.delete(1.0, tk.END)
        self.current_file = None

    def open_file(self):
        file_path = filedialog.askopenfilename(
            defaultextension=".txt",
            filetypes=[("Text Files", "*.txt"), ("All Files", "*.*")],
        )
        if file_path:
            with open(file_path, "r") as file:
                content = file.read()
                self.text_area.delete(1.0, tk.END)
                self.text_area.insert(tk.END, content)
            self.current_file = file_path

    def save_file(self):
        if self.current_file:
            content = self.text_area.get(1.0, tk.END)
            with open(self.current_file, "w") as file:
                file.write(content)
            self.show_autosave_indicator()
        else:
            self.save_as()

    def save_as(self):
        date_str = datetime.datetime.now().strftime("%Y-%m-%d")
        default_filename = f"document_{date_str}.txt"
        file_path = filedialog.asksaveasfilename(
            defaultextension=".txt",
            filetypes=[("Text Files", "*.txt"), ("All Files", "*.*")],
            initialfile=default_filename,
        )
        if file_path:
            content = self.text_area.get(1.0, tk.END)
            with open(file_path, "w") as file:
                file.write(content)
            self.current_file = file_path
            self.show_autosave_indicator()

    def autosave(self):
        current_content = self.text_area.get(1.0, tk.END)
        if current_content != self.last_content:
            if self.current_file:
                self.save_file()
            self.last_content = current_content
        self.master.after(20000, self.autosave)  # Autosave every 20 seconds

    def show_autosave_indicator(self):
        self.status_bar.config(text="Autosaved")
        self.master.after(2000, lambda: self.status_bar.config(text=""))


if __name__ == "__main__":
    root = tk.Tk()
    editor = AutosaveTextEditor(root)
    root.mainloop()
