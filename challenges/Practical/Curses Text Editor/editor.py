#!/usr/bin/env python3
"""Terminal text editor built with curses.

This file glues together the TextBuffer, configuration, and curses UI
into a modal editor similar to Vim/Emacs hybrids.
"""
from __future__ import annotations

import argparse
import curses
import sys
import textwrap
import time
from pathlib import Path
from typing import Optional, Tuple

from buffer import TextBuffer
from config import EditorConfig, HUMAN_READABLE_KEYS, load_config

HELP_FILE = Path(__file__).with_name("HELP.md")


class EditorApp:
    def __init__(
        self,
        stdscr: "curses._CursesWindow",
        buffer: TextBuffer,
        config: EditorConfig,
        *,
        autosave_interval: Optional[int] = None,
        encoding: str = "utf-8",
    ) -> None:
        self.stdscr = stdscr
        self.buffer = buffer
        self.config = config
        self.mode = "command"
        self.encoding = encoding
        self.status_message = "READY"
        self.status_timer: Optional[float] = None
        self.top_line = 0
        self.key_sequence = ""
        self.search_term: Optional[str] = None
        self.search_backward = False
        self.search_position: Optional[Tuple[int, int]] = None
        self.show_help_overlay = False
        self.last_autosave = time.time()
        self.last_input_time = time.time()
        self.last_change_time = 0.0
        self.autosave_interval = (
            autosave_interval
            if autosave_interval is not None
            else int(config.options.get("autosave_interval", 10))
        )
        self.autosave_suffix = str(config.options.get("autosave_suffix", ".autosave"))
        self.tab_width = int(config.options.get("tab_width", 4))
        self.expand_tab = bool(config.options.get("expand_tab", True))
        self.clipboard: Optional[str] = None

    # ------------------------------------------------------------------
    # Main loop
    # ------------------------------------------------------------------
    def run(self) -> None:
        curses.curs_set(1)
        self.stdscr.timeout(100)
        self.check_autosave_recovery()
        while True:
            self.draw()
            self.maybe_autosave()
            try:
                key = self.stdscr.get_wch()
            except curses.error:
                continue
            self.handle_key(key)

    # ------------------------------------------------------------------
    # Rendering
    # ------------------------------------------------------------------
    def draw(self) -> None:
        self.stdscr.erase()
        height, width = self.stdscr.getmaxyx()
        text_height = height - 1
        cursor_row, cursor_col = self.buffer.position()

        # Scroll if needed
        if cursor_row < self.top_line:
            self.top_line = cursor_row
        elif cursor_row >= self.top_line + text_height:
            self.top_line = cursor_row - text_height + 1

        for idx in range(text_height):
            line_idx = self.top_line + idx
            if line_idx >= len(self.buffer.lines):
                break
            line = self.buffer.lines[line_idx]
            truncated = line[: max(width, 1) - 1]
            if self.search_term and self.search_term in truncated:
                self._add_highlighted_line(idx, truncated, width)
            else:
                self.stdscr.addstr(idx, 0, truncated)

        # Status bar
        self._draw_status_bar(width)

        # Cursor placement
        display_row = cursor_row - self.top_line
        if 0 <= display_row < text_height:
            display_col = min(cursor_col, width - 1)
            self.stdscr.move(display_row, display_col)
        else:
            self.stdscr.move(text_height - 1, 0)

        if self.show_help_overlay:
            self._draw_help_overlay(height, width)

        self.stdscr.refresh()

    def _add_highlighted_line(self, idx: int, line: str, width: int) -> None:
        term = self.search_term
        if not term:
            self.stdscr.addstr(idx, 0, line[: width - 1])
            return
        start = 0
        while True:
            pos = line.find(term, start)
            if pos == -1:
                self.stdscr.addstr(idx, start, line[start : width - 1])
                break
            self.stdscr.addstr(idx, start, line[start:pos])
            self.stdscr.addstr(idx, pos, line[pos : pos + len(term)], curses.A_REVERSE)
            start = pos + len(term)
            if start >= len(line):
                break

    def _draw_status_bar(self, width: int) -> None:
        cursor_row, cursor_col = self.buffer.position()
        filename = str(self.buffer.filename) if self.buffer.filename else "[No Name]"
        dirty = "*" if self.buffer.dirty else ""
        mode = self.mode.upper()
        stats = f"{cursor_row + 1},{cursor_col + 1}"
        msg = self.status_message
        if self.status_timer and time.time() > self.status_timer:
            msg = ""
            self.status_timer = None
        left = f" {mode} | {filename}{dirty} "
        right = f" {stats} "
        middle_space = width - len(left) - len(right)
        if middle_space < 1:
            middle_space = 1
        bar = left + msg[: middle_space - 1].ljust(middle_space - 1) + right
        self.stdscr.addstr(
            self.stdscr.getmaxyx()[0] - 1, 0, bar[: width - 1], curses.A_REVERSE
        )

    def _draw_help_overlay(self, height: int, width: int) -> None:
        if height < 5 or width < 20:
            return
        try:
            help_text = HELP_FILE.read_text()
        except OSError:
            help_text = "Help file missing."
        content_width = max(10, width - 4)
        content_width = min(content_width, width - 2)
        wrapped: list[str] = []
        for raw in help_text.splitlines():
            if not raw.strip():
                wrapped.append("")
            else:
                wrapped.extend(textwrap.wrap(raw, content_width, drop_whitespace=False))
        if not wrapped:
            wrapped = ["(no help available)"]
        overlay_height = min(len(wrapped) + 2, height - 2)
        top = max(1, (height - overlay_height) // 2)
        left = max(1, (width - (content_width + 2)) // 2)
        win = curses.newwin(overlay_height, content_width + 2, top, left)
        win.box()
        for idx in range(overlay_height - 2):
            if idx >= len(wrapped):
                break
            win.addstr(1 + idx, 1, wrapped[idx][:content_width])
        win.refresh()

    # ------------------------------------------------------------------
    # Key handling
    # ------------------------------------------------------------------
    def handle_key(self, key) -> None:  # type: ignore[override]
        self.last_input_time = time.time()
        key_name, printable = self._key_to_name(key)
        if self.show_help_overlay and key_name in {"Esc", "F1"}:
            self.show_help_overlay = False
            self.set_status("Help closed", 2)
            return

        if self.mode == "insert":
            if key_name:
                action = self.config.resolve_key(key_name, "insert")
                if action:
                    self.execute_action(action, printable)
                    return
            if printable:
                self.buffer.insert(printable)
                self.last_change_time = time.time()
            elif key_name == "Backspace":
                self.buffer.backspace()
                self.last_change_time = time.time()
            return

        # Command mode
        if key_name:
            action = self._resolve_command_mode_action(key_name)
            if action:
                self.execute_action(action, printable)
                return

        if printable and not self.key_sequence:
            # Enter insert mode automatically when typing printable character
            self.buffer.insert(printable)
            self.mode = "insert"
            self.set_status("-- INSERT --", 2)
            self.last_change_time = time.time()

    def _resolve_command_mode_action(self, key_name: str) -> Optional[str]:
        # multi-key sequences
        if len(key_name) == 1 and key_name.isprintable() and key_name.islower():
            self.key_sequence += key_name
            action = self.config.resolve_key(self.key_sequence, "command")
            if action:
                self.key_sequence = ""
                return action
            # keep waiting if any mapping begins with sequence
            for binding in self.config.mappings.get("command", {}):
                if binding.startswith(self.key_sequence) and binding != key_name:
                    return None
            self.key_sequence = ""
            action = self.config.resolve_key(key_name, "command")
            if action:
                return action
            return None
        else:
            self.key_sequence = ""
            return self.config.resolve_key(key_name, "command")

    def _key_to_name(self, key) -> Tuple[Optional[str], Optional[str]]:
        if isinstance(key, str):
            if key == "\n":
                return "Enter", "\n"
            if key == "\t":
                return "Tab", "\t"
            code = ord(key)
            if code == 27:
                return "Esc", None
            if code == 127:
                return "Backspace", None
            if 0 < code < 27:
                return f"Ctrl+{chr(code + 64)}", None
            if key.isprintable():
                if key.isupper():
                    return key, key
                return key, key
            return None, None
        if isinstance(key, int):
            if key in HUMAN_READABLE_KEYS:
                name = HUMAN_READABLE_KEYS[key]
                printable = "\n" if name == "Enter" else None
                return name, printable
            try:
                name = curses.keyname(key).decode()
            except Exception:
                name = str(key)
            return name, None
        return None, None

    # ------------------------------------------------------------------
    # Actions
    # ------------------------------------------------------------------
    def execute_action(self, action: str, printable: Optional[str] = None) -> None:
        handlers = {
            "enter_command_mode": self.action_enter_command_mode,
            "enter_insert_mode": self.action_enter_insert_mode,
            "append": self.action_append,
            "open_below": self.action_open_below,
            "open_above": self.action_open_above,
            "move_left": self.action_move_left,
            "move_right": self.action_move_right,
            "move_up": self.action_move_up,
            "move_down": self.action_move_down,
            "move_line_start": self.action_move_line_start,
            "move_line_end": self.action_move_line_end,
            "move_file_start": self.action_move_file_start,
            "move_file_end": self.action_move_file_end,
            "move_word_forward": self.action_move_word_forward,
            "move_word_backward": self.action_move_word_backward,
            "delete_char": self.action_delete_char,
            "backspace": self.action_backspace,
            "newline": self.action_newline,
            "write": self.action_write,
            "quit": self.action_quit,
            "refresh": self.action_refresh,
            "command_palette": self.action_command_palette,
            "delete_line": self.action_delete_line,
            "yank_line": self.action_yank_line,
            "paste": self.action_paste,
            "search_forward": self.action_search_forward,
            "search_backward": self.action_search_backward,
            "search_next": self.action_search_next,
            "search_prev": self.action_search_prev,
            "help": self.action_help,
            "show_stats": self.action_show_stats,
            "insert_tab": self.action_insert_tab,
        }
        handler = handlers.get(action)
        if handler:
            handler(printable)
        else:
            self.set_status(f"Unmapped action: {action}", 2)

    def action_enter_command_mode(self, *_args) -> None:
        if self.mode != "command":
            self.mode = "command"
            self.set_status("-- COMMAND --", 2)

    def action_enter_insert_mode(self, *_args) -> None:
        self.mode = "insert"
        self.set_status("-- INSERT --", 2)

    def action_append(self, *_args) -> None:
        self.buffer.move_right()
        self.action_enter_insert_mode()

    def action_open_below(self, *_args) -> None:
        self.buffer.move_to_line_end()
        self.buffer.newline()
        self.mode = "insert"
        self.last_change_time = time.time()
        self.set_status("-- INSERT --", 2)

    def action_open_above(self, *_args) -> None:
        self.buffer.move_to_line_start()
        self.buffer.cursor.row = max(0, self.buffer.cursor.row)
        self.buffer.lines.insert(self.buffer.cursor.row, "")
        self.buffer.cursor.col = 0
        self.buffer.dirty = True
        self.mode = "insert"
        self.last_change_time = time.time()
        self.set_status("-- INSERT --", 2)

    def action_move_left(self, *_args) -> None:
        self.buffer.move_left()

    def action_move_right(self, *_args) -> None:
        self.buffer.move_right()

    def action_move_up(self, *_args) -> None:
        self.buffer.move_up()

    def action_move_down(self, *_args) -> None:
        self.buffer.move_down()

    def action_move_line_start(self, *_args) -> None:
        self.buffer.move_to_line_start()

    def action_move_line_end(self, *_args) -> None:
        self.buffer.move_to_line_end()

    def action_move_file_start(self, *_args) -> None:
        self.buffer.move_to_start()
        self.top_line = 0

    def action_move_file_end(self, *_args) -> None:
        self.buffer.move_to_end()
        self.top_line = max(0, len(self.buffer.lines) - 1)

    def action_move_word_forward(self, *_args) -> None:
        self.buffer.move_word_forward()

    def action_move_word_backward(self, *_args) -> None:
        self.buffer.move_word_backward()

    def action_delete_char(self, *_args) -> None:
        self.buffer.delete()
        self.last_change_time = time.time()

    def action_backspace(self, *_args) -> None:
        self.buffer.backspace()
        self.last_change_time = time.time()

    def action_newline(self, printable: Optional[str] = None) -> None:
        self.buffer.newline()
        self.last_change_time = time.time()
        if self.mode == "command" and printable == "\n":
            self.mode = "insert"

    def action_write(self, *_args) -> None:
        if not self.buffer.filename:
            name = self.prompt("Write file: ")
            if not name:
                self.set_status("Save cancelled", 2)
                return
            self.buffer.filename = Path(name).expanduser()
        try:
            self.buffer.save_to_file(encoding=self.encoding)
        except Exception as exc:
            self.set_status(f"Write failed: {exc}", 4)
            return
        self.set_status("Written", 2)

    def action_quit(self, *_args) -> None:
        if self.buffer.dirty:
            confirm = self.prompt("Unsaved changes. Quit anyway? (y/N) ")
            if not confirm or confirm.lower() != "y":
                self.set_status("Quit aborted", 2)
                return
        curses.endwin()
        sys.exit(0)

    def action_refresh(self, *_args) -> None:
        self.stdscr.redrawwin()
        self.set_status("Screen refreshed", 1)

    def action_command_palette(self, *_args) -> None:
        command = self.prompt(": ")
        if command is None:
            return
        self.execute_command(command.strip())

    def action_delete_line(self, *_args) -> None:
        removed = self.buffer.delete_line()
        self.buffer.yank_buffer = [removed]
        self.clipboard = removed
        self.last_change_time = time.time()
        self.set_status("Line deleted", 2)

    def action_yank_line(self, *_args) -> None:
        text = self.buffer.yank_line()
        self.clipboard = text
        self.set_status("Yanked line", 1)

    def action_paste(self, *_args) -> None:
        if self.buffer.yank_buffer:
            self.buffer.paste_below()
            self.clipboard = "\n".join(self.buffer.yank_buffer)
            self.last_change_time = time.time()
            return
        if self.clipboard is None:
            self.set_status("Clipboard empty", 1)
            return
        self.buffer.lines.insert(self.buffer.cursor.row + 1, self.clipboard)
        self.buffer.cursor.row += 1
        self.buffer.cursor.col = len(self.clipboard)
        self.buffer.dirty = True
        self.last_change_time = time.time()

    def action_search_forward(self, *_args) -> None:
        term = self.prompt("/", self.search_term or "")
        if term is None:
            return
        self.search_term = term
        self.search_backward = False
        self.search_position = None
        self._perform_search()

    def action_search_backward(self, *_args) -> None:
        term = self.prompt("?", self.search_term or "")
        if term is None:
            return
        self.search_term = term
        self.search_backward = True
        self.search_position = None
        self._perform_search()

    def action_search_next(self, *_args) -> None:
        if not self.search_term:
            self.set_status("No previous search", 1)
            return
        self.search_backward = False
        self._perform_search()

    def action_search_prev(self, *_args) -> None:
        if not self.search_term:
            self.set_status("No previous search", 1)
            return
        self.search_backward = True
        self._perform_search()

    def action_help(self, *_args) -> None:
        self.show_help_overlay = not self.show_help_overlay
        self.set_status("Help toggled", 2)

    def action_show_stats(self, *_args) -> None:
        lines, words, chars = self.buffer.word_count()
        self.set_status(f"Lines: {lines} Words: {words} Chars: {chars}", 4)

    def action_insert_tab(self, *_args) -> None:
        if self.expand_tab:
            self.buffer.insert(" " * self.tab_width)
        else:
            self.buffer.insert("\t")
        self.last_change_time = time.time()

    # ------------------------------------------------------------------
    # Commands
    # ------------------------------------------------------------------
    def execute_command(self, command: str) -> None:
        if not command:
            return
        if command in {"w", "write"}:
            self.action_write()
            return
        if command.startswith("w "):
            _, _, rest = command.partition(" ")
            try:
                self.buffer.save_to_file(
                    Path(rest).expanduser(), encoding=self.encoding
                )
            except Exception as exc:
                self.set_status(f"Write failed: {exc}", 4)
                return
            self.set_status("Written", 2)
            return
        if command in {"wa"}:
            self.action_write()
            self._write_autosave(force=True)
            return
        if command in {"q", "quit"}:
            self.action_quit()
            return
        if command in {"q!", "quit!"}:
            curses.endwin()
            sys.exit(0)
        if command in {"wq", "x"}:
            self.action_write()
            curses.endwin()
            sys.exit(0)
        if command.startswith("open "):
            _, _, rest = command.partition(" ")
            self.open_file(Path(rest).expanduser())
            return
        if command.startswith("set "):
            _, _, rest = command.partition(" ")
            key, _, value = rest.partition("=")
            key = key.strip()
            value = value.strip()
            self.apply_option(key, value)
            return
        if command.lower().startswith("search "):
            _, _, term = command.partition(" ")
            self.search_term = term
            self.search_backward = False
            self.search_position = None
            self._perform_search()
            return
        if command.startswith("s/"):
            self._handle_substitution(command)
            return
        if command == "help":
            self.show_help_overlay = True
            return
        self.set_status(f"Unknown command: {command}", 2)

    def _handle_substitution(self, command: str) -> None:
        try:
            parts = command[2:].split("/")
            old = parts[0]
            new = parts[1] if len(parts) > 1 else ""
            flags = parts[2] if len(parts) > 2 else ""
        except Exception:
            self.set_status("Invalid substitution", 2)
            return
        if "g" in flags:
            replaced = self.buffer.replace(old, new)
        else:
            replaced = self.buffer.replace(old, new, count=1)
        self.set_status(f"Replaced {replaced}", 2)
        if replaced:
            self.last_change_time = time.time()

    def apply_option(self, key: str, value: str) -> None:
        if key == "autosave_interval":
            try:
                self.autosave_interval = max(1, int(value))
                self.set_status(f"Autosave every {self.autosave_interval}s", 2)
            except ValueError:
                self.set_status("Invalid autosave interval", 2)
        elif key == "autosave_suffix":
            self.autosave_suffix = value
            self.set_status("Autosave suffix updated", 2)
        elif key == "tab_width":
            try:
                self.tab_width = max(1, int(value))
                self.set_status(f"Tab width {self.tab_width}", 2)
            except ValueError:
                self.set_status("Invalid tab width", 2)
        elif key == "expand_tab":
            self.expand_tab = value.lower() in {"1", "true", "yes", "on"}
            self.set_status(f"Expand tab {self.expand_tab}", 2)
        else:
            self.set_status(f"Unknown option {key}", 2)

    def open_file(self, path: Path) -> None:
        if self.buffer.dirty:
            confirm = self.prompt("Discard changes and open new file? (y/N) ")
            if not confirm or confirm.lower() != "y":
                self.set_status("Open cancelled", 2)
                return
        if path.exists():
            self.buffer.load_from_file(path, encoding=self.encoding)
        else:
            self.buffer = TextBuffer("", path)
        self.top_line = 0
        self.key_sequence = ""
        self.search_term = None
        self.search_position = None
        self.set_status(f"Opened {path}", 2)

    # ------------------------------------------------------------------
    # Search helpers
    # ------------------------------------------------------------------
    def _perform_search(self) -> None:
        if not self.search_term:
            return
        start = self.search_position
        if start is None:
            row, col = self.buffer.position()
            if self.search_backward:
                if col > 0:
                    col -= 1
                elif row > 0:
                    row -= 1
                    col = len(self.buffer.lines[row])
            else:
                if col < len(self.buffer.lines[row]):
                    col += 1
            start = (row, col)
        result = self.buffer.search(
            self.search_term, start=start, backward=self.search_backward
        )
        if result is None:
            self.set_status("Pattern not found", 2)
            return
        row, col = result
        self.buffer.cursor.row, self.buffer.cursor.col = row, col
        self.search_position = (
            row,
            col + (0 if self.search_backward else len(self.search_term)),
        )
        self.set_status(f"Match {row + 1}:{col + 1}", 1)

    # ------------------------------------------------------------------
    # Autosave
    # ------------------------------------------------------------------
    def maybe_autosave(self) -> None:
        if not self.buffer.dirty:
            return
        now = time.time()
        if now - self.last_change_time < self.autosave_interval:
            return
        if now - self.last_autosave < self.autosave_interval:
            return
        self._write_autosave()

    def _write_autosave(self, force: bool = False) -> None:
        if not self.buffer.filename:
            temp_name = Path(f"Untitled{self.autosave_suffix}")
        else:
            temp_name = Path(str(self.buffer.filename) + self.autosave_suffix)
        try:
            temp_name.write_text(self.buffer.as_text(), encoding=self.encoding)
        except Exception:
            if force:
                self.set_status("Autosave failed", 2)
            return
        self.last_autosave = time.time()
        if force:
            self.set_status(f"Autosaved -> {temp_name}", 2)

    def check_autosave_recovery(self) -> None:
        if not self.buffer.filename:
            return
        autosave_path = Path(str(self.buffer.filename) + self.autosave_suffix)
        if not autosave_path.exists():
            return
        try:
            file_mtime = self.buffer.filename.stat().st_mtime
        except FileNotFoundError:
            file_mtime = 0
        auto_mtime = autosave_path.stat().st_mtime
        if auto_mtime <= file_mtime:
            return
        choice = self.prompt(f"Load autosave for {self.buffer.filename}? (y/N) ")
        if choice and choice.lower() == "y":
            data = autosave_path.read_text(encoding=self.encoding)
            self.buffer.set_text(data)
            self.buffer.dirty = True
            self.set_status("Autosave restored", 3)
        else:
            self.set_status("Autosave ignored", 2)

    # ------------------------------------------------------------------
    # Utilities
    # ------------------------------------------------------------------
    def set_status(self, message: str, duration: Optional[int] = None) -> None:
        self.status_message = message
        if duration:
            self.status_timer = time.time() + duration
        else:
            self.status_timer = None

    def prompt(self, message: str, initial: str = "") -> Optional[str]:
        height, width = self.stdscr.getmaxyx()
        buffer = list(initial)
        pos = len(buffer)
        while True:
            prompt_text = message + "".join(buffer)
            self.stdscr.move(height - 1, 0)
            self.stdscr.clrtoeol()
            display = prompt_text[: width - 1]
            self.stdscr.addstr(height - 1, 0, display, curses.A_REVERSE)
            cursor_x = min(len(message) + pos, width - 1)
            self.stdscr.move(height - 1, cursor_x)
            self.stdscr.refresh()
            try:
                key = self.stdscr.get_wch()
            except curses.error:
                continue
            if isinstance(key, str):
                if key in {"\n", "\r"}:
                    return "".join(buffer)
                if key == "\x1b":
                    return None
                if key in {"\x7f", "\b"}:
                    if pos > 0:
                        buffer.pop(pos - 1)
                        pos -= 1
                    continue
                if key == "\t":
                    buffer.insert(pos, "\t")
                    pos += 1
                    continue
                if key == "\x01":  # Ctrl+A
                    pos = 0
                    continue
                if key == "\x05":  # Ctrl+E
                    pos = len(buffer)
                    continue
                if key == "\x15":  # Ctrl+U
                    buffer = []
                    pos = 0
                    continue
                if key.isprintable():
                    buffer.insert(pos, key)
                    pos += 1
                    continue
            if isinstance(key, int):
                if key in {curses.KEY_LEFT}:
                    if pos > 0:
                        pos -= 1
                elif key in {curses.KEY_RIGHT}:
                    if pos < len(buffer):
                        pos += 1
                elif key in {curses.KEY_HOME}:
                    pos = 0
                elif key in {curses.KEY_END}:
                    pos = len(buffer)
                elif key in {curses.KEY_BACKSPACE, 127}:
                    if pos > 0:
                        buffer.pop(pos - 1)
                        pos -= 1
                else:
                    curses.beep()


# ----------------------------------------------------------------------
# CLI
# ----------------------------------------------------------------------


def parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Curses text editor")
    parser.add_argument(
        "--file", dest="file", type=str, help="File to open", default=None
    )
    parser.add_argument(
        "--config", dest="config", type=str, help="Path to JSON config", default=None
    )
    parser.add_argument(
        "--autosave-interval", dest="autosave_interval", type=int, default=None
    )
    parser.add_argument("--encoding", dest="encoding", type=str, default="utf-8")
    return parser.parse_args(argv)


def create_buffer(file_path: Optional[str], encoding: str) -> TextBuffer:
    if file_path:
        path = Path(file_path).expanduser()
        if path.exists():
            text = path.read_text(encoding=encoding)
            return TextBuffer(text, path)
        return TextBuffer("", path)
    return TextBuffer()


def main(argv: Optional[list[str]] = None) -> None:
    args = parse_args(argv)
    config = load_config(args.config)
    buffer = create_buffer(args.file, args.encoding)

    def _run(stdscr):
        app = EditorApp(
            stdscr,
            buffer,
            config,
            autosave_interval=args.autosave_interval,
            encoding=args.encoding,
        )
        app.run()

    curses.wrapper(_run)


if __name__ == "__main__":
    main()
