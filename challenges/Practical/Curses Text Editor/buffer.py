"""Core text buffer logic for the curses text editor.

The buffer is independent from the UI layer so it can be tested
and reused for other frontends.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple


@dataclass
class Cursor:
    row: int = 0
    col: int = 0

    def clamp(self, lines: List[str]) -> None:
        self.row = max(0, min(self.row, max(len(lines) - 1, 0)))
        line_length = len(lines[self.row]) if lines else 0
        self.col = max(0, min(self.col, line_length))


class TextBuffer:
    """Represents the text content and cursor state."""

    def __init__(self, text: str = "", filename: Optional[Path] = None) -> None:
        self.lines: List[str] = text.splitlines()
        if not self.lines:
            self.lines = [""]
        self.cursor = Cursor()
        self.filename: Optional[Path] = Path(filename) if filename else None
        self.dirty = False
        self.yank_buffer: List[str] = []

    # ------------------------------------------------------------------
    # Cursor movement helpers
    # ------------------------------------------------------------------
    def move_left(self, count: int = 1) -> None:
        for _ in range(count):
            if self.cursor.col > 0:
                self.cursor.col -= 1
            elif self.cursor.row > 0:
                self.cursor.row -= 1
                self.cursor.col = len(self.lines[self.cursor.row])

    def move_right(self, count: int = 1) -> None:
        for _ in range(count):
            line_length = len(self.lines[self.cursor.row])
            if self.cursor.col < line_length:
                self.cursor.col += 1
            elif self.cursor.row < len(self.lines) - 1:
                self.cursor.row += 1
                self.cursor.col = 0

    def move_up(self, count: int = 1) -> None:
        self.cursor.row = max(0, self.cursor.row - count)
        self.cursor.col = min(self.cursor.col, len(self.lines[self.cursor.row]))

    def move_down(self, count: int = 1) -> None:
        self.cursor.row = min(len(self.lines) - 1, self.cursor.row + count)
        self.cursor.col = min(self.cursor.col, len(self.lines[self.cursor.row]))

    def move_to_line_start(self) -> None:
        self.cursor.col = 0

    def move_to_line_end(self) -> None:
        self.cursor.col = len(self.lines[self.cursor.row])

    def move_to_start(self) -> None:
        self.cursor.row = 0
        self.cursor.col = 0

    def move_to_end(self) -> None:
        self.cursor.row = len(self.lines) - 1
        self.cursor.col = len(self.lines[self.cursor.row])

    def move_word_forward(self) -> None:
        line = self.lines[self.cursor.row]
        idx = self.cursor.col
        while idx < len(line) and line[idx].isspace():
            idx += 1
        while idx < len(line) and not line[idx].isspace():
            idx += 1
        if idx < len(line):
            self.cursor.col = idx
            return
        if self.cursor.row < len(self.lines) - 1:
            self.cursor.row += 1
            self.cursor.col = 0
            self.move_word_forward()
        else:
            self.cursor.col = len(line)

    def move_word_backward(self) -> None:
        line = self.lines[self.cursor.row]
        idx = self.cursor.col
        if idx > 0:
            idx -= 1
        while idx > 0 and line[idx].isspace():
            idx -= 1
        while idx > 0 and not line[idx - 1].isspace():
            idx -= 1
        self.cursor.col = idx

    # ------------------------------------------------------------------
    # Mutations
    # ------------------------------------------------------------------
    def insert(self, text: str) -> None:
        if text == "":
            return
        line = self.lines[self.cursor.row]
        self.lines[self.cursor.row] = (
            line[: self.cursor.col] + text + line[self.cursor.col :]
        )
        self.cursor.col += len(text)
        self.dirty = True

    def newline(self) -> None:
        line = self.lines[self.cursor.row]
        before, after = line[: self.cursor.col], line[self.cursor.col :]
        self.lines[self.cursor.row] = before
        self.lines.insert(self.cursor.row + 1, after)
        self.cursor.row += 1
        self.cursor.col = 0
        self.dirty = True

    def backspace(self) -> None:
        if self.cursor.col > 0:
            line = self.lines[self.cursor.row]
            self.lines[self.cursor.row] = (
                line[: self.cursor.col - 1] + line[self.cursor.col :]
            )
            self.cursor.col -= 1
            self.dirty = True
        elif self.cursor.row > 0:
            prev_line = self.lines[self.cursor.row - 1]
            current = self.lines.pop(self.cursor.row)
            self.cursor.row -= 1
            self.cursor.col = len(prev_line)
            self.lines[self.cursor.row] = prev_line + current
            self.dirty = True

    def delete(self) -> None:
        line = self.lines[self.cursor.row]
        if self.cursor.col < len(line):
            self.lines[self.cursor.row] = (
                line[: self.cursor.col] + line[self.cursor.col + 1 :]
            )
            self.dirty = True
        elif self.cursor.row < len(self.lines) - 1:
            next_line = self.lines.pop(self.cursor.row + 1)
            self.lines[self.cursor.row] = line + next_line
            self.dirty = True

    def delete_line(self, row: Optional[int] = None) -> str:
        if row is None:
            row = self.cursor.row
        if not self.lines:
            return ""
        removed = self.lines.pop(row)
        if not self.lines:
            self.lines = [""]
        if self.cursor.row >= len(self.lines):
            self.cursor.row = len(self.lines) - 1
        self.cursor.col = min(self.cursor.col, len(self.lines[self.cursor.row]))
        self.dirty = True
        return removed

    def yank_line(self, row: Optional[int] = None) -> str:
        if row is None:
            row = self.cursor.row
        line = self.lines[row]
        self.yank_buffer = [line]
        return line

    def paste_below(self) -> None:
        if not self.yank_buffer:
            return
        insert_row = self.cursor.row + 1
        for line in self.yank_buffer:
            self.lines.insert(insert_row, line)
            insert_row += 1
        self.cursor.row += len(self.yank_buffer)
        self.cursor.col = len(self.lines[self.cursor.row])
        self.dirty = True

    # ------------------------------------------------------------------
    # Search & replace
    # ------------------------------------------------------------------
    def search(
        self,
        needle: str,
        start: Optional[Tuple[int, int]] = None,
        *,
        backward: bool = False,
    ) -> Optional[Tuple[int, int]]:
        if not needle:
            return None
        if start is None:
            row, col = self.cursor.row, self.cursor.col
        else:
            row, col = start
        lines = self.lines
        if backward:
            for r in range(row, -1, -1):
                hay = lines[r]
                search_end = col if r == row else len(hay)
                idx = hay.rfind(needle, 0, search_end)
                if idx != -1:
                    return r, idx
            return None
        for r in range(row, len(lines)):
            hay = lines[r]
            search_start = col if r == row else 0
            idx = hay.find(needle, search_start)
            if idx != -1:
                return r, idx
        return None

    def replace(self, old: str, new: str, *, count: Optional[int] = None) -> int:
        if not old:
            return 0
        replacements = 0
        for i, line in enumerate(self.lines):
            if count is not None and replacements >= count:
                break
            if old in line:
                if count is None:
                    new_line, num = line.replace(old, new), line.count(old)
                else:
                    new_line, num = self._replace_limited(
                        line, old, new, count - replacements
                    )
                if num:
                    self.lines[i] = new_line
                    replacements += num
        if replacements:
            self.dirty = True
        return replacements

    @staticmethod
    def _replace_limited(text: str, old: str, new: str, limit: int) -> Tuple[str, int]:
        pieces = []
        start = 0
        replaced = 0
        while replaced < limit:
            idx = text.find(old, start)
            if idx == -1:
                break
            pieces.append(text[start:idx])
            pieces.append(new)
            start = idx + len(old)
            replaced += 1
        pieces.append(text[start:])
        return "".join(pieces), replaced

    # ------------------------------------------------------------------
    # File IO
    # ------------------------------------------------------------------
    def load_from_file(self, path: Path, encoding: str = "utf-8") -> None:
        data = Path(path).read_text(encoding=encoding)
        self.__init__(data, Path(path))
        self.dirty = False

    def save_to_file(
        self, path: Optional[Path] = None, encoding: str = "utf-8"
    ) -> None:
        target = Path(path) if path else self.filename
        if target is None:
            raise ValueError("No filename specified for save")
        target.write_text(self.as_text(), encoding=encoding)
        self.filename = target
        self.dirty = False

    def as_text(self) -> str:
        return "\n".join(self.lines)

    # ------------------------------------------------------------------
    # Stats
    # ------------------------------------------------------------------
    def position(self) -> Tuple[int, int]:
        return self.cursor.row, self.cursor.col

    def word_count(self) -> Tuple[int, int, int]:
        text = self.as_text()
        chars = len(text)
        words = len(text.split())
        lines = len(self.lines)
        return lines, words, chars

    def set_text(self, text: str) -> None:
        self.__init__(text, self.filename)
        self.dirty = True


__all__ = ["TextBuffer", "Cursor"]
