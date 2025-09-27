"""Tkinter GUI for the Mastermind challenge."""
from __future__ import annotations

import tkinter as tk
from tkinter import ttk, messagebox
from typing import List, Sequence

from mastermind_logic import (
    ComputerBreaker,
    MastermindConfig,
    MastermindGame,
    Score,
    generate_palette,
)


class MastermindApp(tk.Tk):
    def __init__(self) -> None:
        super().__init__()
        self.title("Mastermind")
        self.resizable(False, False)

        self.mode_var = tk.StringVar(value="breaker")
        self.peg_var = tk.IntVar(value=4)
        self.color_var = tk.IntVar(value=6)
        self.duplicates_var = tk.BooleanVar(value=True)

        self.config_obj: MastermindConfig | None = None
        self.game: MastermindGame | None = None
        self.comboboxes: List[ttk.Combobox] = []
        self.secret_boxes: List[ttk.Combobox] = []
        self.history = tk.StringVar(value="")
        self.breaker: ComputerBreaker | None = None

        self._build_controls()
        self._build_guess_area()
        self._build_history()
        self._build_secret_controls()
        self._update_guess_inputs()

    # ------------------------------------------------------------------
    def _build_controls(self) -> None:
        frame = ttk.LabelFrame(self, text="Configuration")
        frame.grid(row=0, column=0, padx=10, pady=10, sticky="ew")

        ttk.Label(frame, text="Mode:").grid(row=0, column=0, sticky="w")
        ttk.Radiobutton(
            frame,
            text="Be the code breaker",
            variable=self.mode_var,
            value="breaker",
            command=self._update_secret_visibility,
        ).grid(row=0, column=1, sticky="w")
        ttk.Radiobutton(
            frame,
            text="Be the code maker",
            variable=self.mode_var,
            value="codemaker",
            command=self._update_secret_visibility,
        ).grid(row=0, column=2, sticky="w")

        ttk.Label(frame, text="Pegs:").grid(row=1, column=0, sticky="w")
        peg_spin = ttk.Spinbox(frame, from_=3, to=8, textvariable=self.peg_var, width=5, command=self._update_guess_inputs)
        peg_spin.grid(row=1, column=1, sticky="w")

        ttk.Label(frame, text="Colours:").grid(row=1, column=2, sticky="w")
        color_spin = ttk.Spinbox(frame, from_=4, to=12, textvariable=self.color_var, width=5, command=self._update_guess_inputs)
        color_spin.grid(row=1, column=3, sticky="w")

        ttk.Checkbutton(
            frame,
            text="Allow duplicates",
            variable=self.duplicates_var,
        ).grid(row=2, column=0, columnspan=2, sticky="w")

        ttk.Button(frame, text="Start new game", command=self.start_game).grid(row=2, column=2, columnspan=2, sticky="ew")

        frame.columnconfigure(1, weight=1)
        frame.columnconfigure(3, weight=1)

    # ------------------------------------------------------------------
    def _build_guess_area(self) -> None:
        frame = ttk.LabelFrame(self, text="Your Guess")
        frame.grid(row=1, column=0, padx=10, pady=(0, 10), sticky="ew")
        self.guess_frame = frame

        self.submit_button = ttk.Button(frame, text="Submit Guess", command=self.submit_guess, state="disabled")
        self.submit_button.grid(row=1, column=0, columnspan=8, pady=5)

    def _build_secret_controls(self) -> None:
        frame = ttk.LabelFrame(self, text="Secret Code (code maker mode)")
        frame.grid(row=2, column=0, padx=10, pady=(0, 10), sticky="ew")
        self.secret_frame = frame

        self.set_secret_button = ttk.Button(frame, text="Set Secret", command=self.set_secret, state="disabled")
        self.set_secret_button.grid(row=1, column=0, columnspan=8, pady=5)

    def _build_history(self) -> None:
        frame = ttk.LabelFrame(self, text="History")
        frame.grid(row=3, column=0, padx=10, pady=(0, 10), sticky="ew")
        self.history_list = tk.Listbox(frame, width=60, height=10)
        self.history_list.grid(row=0, column=0, sticky="nsew")
        scrollbar = ttk.Scrollbar(frame, orient="vertical", command=self.history_list.yview)
        scrollbar.grid(row=0, column=1, sticky="ns")
        self.history_list.configure(yscrollcommand=scrollbar.set)
        frame.columnconfigure(0, weight=1)

    # ------------------------------------------------------------------
    def start_game(self) -> None:
        try:
            palette = generate_palette(self.color_var.get())
            config = MastermindConfig(
                peg_count=self.peg_var.get(),
                colors=palette,
                allow_duplicates=self.duplicates_var.get(),
            )
            game = MastermindGame(config)
        except ValueError as exc:
            messagebox.showerror("Configuration error", str(exc))
            return

        self.config_obj = config
        self.game = game
        self.history_list.delete(0, tk.END)
        self.breaker = None

        if self.mode_var.get() == "breaker":
            self.submit_button.configure(state="normal")
            self.set_secret_button.configure(state="disabled")
            self._update_guess_inputs()
            self._update_secret_visibility()
            messagebox.showinfo("Mastermind", "Secret code generated! Start guessing.")
        else:
            self.submit_button.configure(state="disabled")
            self.breaker = ComputerBreaker(config)
            self._update_secret_inputs()
            self._update_secret_visibility()
            messagebox.showinfo(
                "Mastermind",
                "Select your secret combination below and press 'Set Secret'.",
            )

    def _update_guess_inputs(self) -> None:
        for widget in getattr(self, "comboboxes", []):
            widget.destroy()
        self.comboboxes = []
        palette = generate_palette(self.color_var.get())
        for idx in range(self.peg_var.get()):
            cb = ttk.Combobox(
                self.guess_frame,
                state="readonly",
                values=palette,
                width=12,
            )
            cb.grid(row=0, column=idx, padx=2, pady=5)
            if idx < len(palette):
                cb.current(idx % len(palette))
            self.comboboxes.append(cb)

    def _update_secret_inputs(self) -> None:
        for widget in getattr(self, "secret_boxes", []):
            widget.destroy()
        self.secret_boxes = []
        palette = self.config_obj.palette() if self.config_obj else generate_palette(self.color_var.get())
        for idx in range(self.peg_var.get()):
            cb = ttk.Combobox(
                self.secret_frame,
                state="readonly",
                values=palette,
                width=12,
            )
            cb.grid(row=0, column=idx, padx=2, pady=5)
            self.secret_boxes.append(cb)

    def _update_secret_visibility(self) -> None:
        is_codemaker = self.mode_var.get() == "codemaker"
        state = "normal" if is_codemaker else "disabled"
        for widget in self.secret_frame.winfo_children():
            widget.configure(state=state)
        if self.secret_boxes:
            for widget in self.secret_boxes:
                widget.configure(state=state)
        self.set_secret_button.configure(state=state)

    # ------------------------------------------------------------------
    def submit_guess(self) -> None:
        if not self.game:
            return
        guess = [box.get() for box in self.comboboxes]
        if "" in guess:
            messagebox.showwarning("Incomplete", "Select a colour for each peg.")
            return
        score = self.game.evaluate_guess(guess)
        self._append_history(len(self.history_list.get(0, tk.END)) + 1, guess, score)
        if self.game.is_win(score):
            messagebox.showinfo("Victory", "You cracked the code!")
            self.submit_button.configure(state="disabled")

    def set_secret(self) -> None:
        if not self.game or not self.secret_boxes:
            return
        secret = [box.get() for box in self.secret_boxes]
        if "" in secret:
            messagebox.showwarning("Incomplete", "Select a colour for each peg.")
            return
        try:
            self.game.set_secret(secret)
        except ValueError as exc:
            messagebox.showerror("Secret error", str(exc))
            return
        self.set_secret_button.configure(state="disabled")
        self._run_computer_turn()

    def _run_computer_turn(self) -> None:
        if not self.game or self.mode_var.get() != "codemaker":
            return
        if self.breaker is None:
            self.breaker = ComputerBreaker(self.game.config)
        try:
            guess = self.breaker.next_guess()
        except RuntimeError as exc:
            messagebox.showerror("Error", str(exc))
            return
        score = self.game.evaluate_guess(guess)
        turn = len(self.history_list.get(0, tk.END)) + 1
        self._append_history(turn, list(guess), score)
        if self.game.is_win(score):
            messagebox.showinfo("Solved", f"Computer cracked the code in {turn} turns!")
            return
        self.breaker.register_feedback(guess, score)
        # Schedule next guess so the UI remains responsive
        self.after(600, self._run_computer_turn)

    def _append_history(self, turn: int, guess: Sequence[str], score: Score) -> None:
        feedback = score.as_pegs() or "—"
        guess_text = ", ".join(guess)
        self.history_list.insert(tk.END, f"Turn {turn:02d}: {guess_text:<30} | {feedback}")
        self.history_list.see(tk.END)


def main() -> None:
    app = MastermindApp()
    app.mainloop()


if __name__ == "__main__":
    main()
