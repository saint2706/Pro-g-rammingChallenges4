"""Command-line To-Do list application.

Modernized Features:
- Persistent storage in plain text files (pending + done) with safe (atomic) writes.
- Rich CLI subcommands (add, show, show-done, delete, done, undo, prioritize, clear, stats, search).
- Optional priorities stored inline (e.g., (A) Buy milk) with simple validation.
- Undo last completion (moves the most recently marked-done item back to todo list).
- Colored output (auto-disabled if stdout not a TTY or --no-color flag supplied).
- Helpful error messages with exit codes (non-zero on failure).
- Type hints and clear docstrings for new developers.

Data Format:
- todo.txt: each line is a raw task string (may begin with priority token like "(A) ").
- done.txt: each line starts with: "x YYYY-MM-DD original task text".

This remains intentionally simple—no JSON or lock files—keeping compatibility with the original format.
"""

from __future__ import annotations

import argparse
import os
import sys
from dataclasses import dataclass
from datetime import date
from typing import List, Iterable, Optional

# ----------------------------- Utility & Models ----------------------------- #


@dataclass
class TodoItem:
    """Represents a single to-do item.

    Attributes:
        raw: Original line text (may include priority token "(A) ").
    """

    raw: str

    @property
    def priority(self) -> Optional[str]:
        """Return priority letter if present (e.g., (A)), else None."""
        if (
            len(self.raw) >= 4
            and self.raw[0] == "("
            and self.raw[2] == ")"
            and self.raw[1].isalpha()
        ):
            return self.raw[1].upper()
        return None

    def with_priority(self, prio: str) -> "TodoItem":
        prio = prio.upper()
        base = self.raw
        if self.priority:
            base = base[4:].lstrip()
        return TodoItem(f"({prio}) {base}")


# ----------------------------- Core Manager Class ----------------------------- #


class TodoList:
    """Manage a command-line To-Do list.

    Storage uses two plain-text files: pending (todo.txt) & completed (done.txt).
    All operations load into memory (small scale by design) then write atomically.
    """

    def __init__(
        self, todo_file: str = "todo.txt", done_file: str = "done.txt"
    ) -> None:
        self.todo_file = todo_file
        self.done_file = done_file
        self._ensure_files_exist()
        self.todos: List[TodoItem] = [
            TodoItem(line) for line in self._read_file(self.todo_file)
        ]
        self.done_items: List[str] = self._read_file(self.done_file)

    # ----------------------------- File Helpers ----------------------------- #
    def _ensure_files_exist(self) -> None:
        for path in (self.todo_file, self.done_file):
            if not os.path.exists(path):
                open(path, "w").close()

    def _read_file(self, filepath: str) -> List[str]:
        try:
            with open(filepath, "r", encoding="utf-8") as f:
                return [line.rstrip("\n") for line in f]
        except FileNotFoundError:
            return []

    def _atomic_write(self, filepath: str, lines: Iterable[str]) -> None:
        tmp = f"{filepath}.tmp"
        with open(tmp, "w", encoding="utf-8") as f:
            for line in lines:
                f.write(line + "\n")
        os.replace(tmp, filepath)

    def _flush(self) -> None:
        self._atomic_write(self.todo_file, (t.raw for t in self.todos))
        self._atomic_write(self.done_file, self.done_items)

    # ----------------------------- Public Operations ----------------------------- #
    def add(self, item: str, priority: Optional[str] = None) -> None:
        if priority:
            if len(priority) != 1 or not priority.isalpha():
                raise ValueError("Priority must be a single letter A-Z")
            todo = TodoItem(item).with_priority(priority)
        else:
            todo = TodoItem(item)
        self.todos.append(todo)
        self._flush()
        print(f'Added todo: "{todo.raw}"')

    def show(self, include_index: bool = True) -> None:
        if not self.todos:
            print("No pending todos!")
            return
        for idx, todo in reversed(list(enumerate(self.todos, 1))):
            prefix = f"[{idx}] " if include_index else ""
            print(f"{prefix}{todo.raw}")

    def show_done(self, limit: Optional[int] = None) -> None:
        if not self.done_items:
            print("No completed items yet.")
            return
        items = self.done_items[-limit:] if limit else self.done_items
        for line in reversed(items):
            print(line)

    def delete(self, item_id: int) -> None:
        if not 1 <= item_id <= len(self.todos):
            raise IndexError(f"Todo item #{item_id} does not exist")
        deleted = self.todos.pop(item_id - 1)
        self._flush()
        print(f'Deleted todo #{item_id}: "{deleted.raw}"')

    def mark_done(self, item_id: int) -> None:
        if not 1 <= item_id <= len(self.todos):
            raise IndexError(f"Todo item #{item_id} does not exist")
        done_item = self.todos.pop(item_id - 1)
        today_str = date.today().strftime("%Y-%m-%d")
        self.done_items.append(f"x {today_str} {done_item.raw}")
        self._flush()
        print(f'Marked todo #{item_id} as done: "{done_item.raw}"')

    def undo_last_done(self) -> None:
        for i in range(len(self.done_items) - 1, -1, -1):
            line = self.done_items[i]
            if line.startswith("x "):
                # Format: x YYYY-MM-DD original
                _, _, *rest = line.split(" ")
                restored = " ".join(rest)
                self.done_items.pop(i)
                self.todos.append(TodoItem(restored))
                self._flush()
                print(f'Restored: "{restored}"')
                return
        print("Nothing to undo.")

    def prioritize(self, item_id: int, prio: str) -> None:
        if not 1 <= item_id <= len(self.todos):
            raise IndexError(f"Todo item #{item_id} does not exist")
        if len(prio) != 1 or not prio.isalpha():
            raise ValueError("Priority must be a single letter A-Z")
        updated = self.todos[item_id - 1] = self.todos[item_id - 1].with_priority(prio)
        self._flush()
        print(f"Updated priority of #{item_id} -> ({prio.upper()}) {updated.raw}")

    def clear(self, confirm: bool = False) -> None:
        if not confirm:
            print("Refusing to clear without --confirm flag.")
            return
        self.todos.clear()
        self._flush()
        print("Cleared all pending todos.")

    def search(self, term: str) -> None:
        term_lower = term.lower()
        matches = [
            (i + 1, t.raw)
            for i, t in enumerate(self.todos)
            if term_lower in t.raw.lower()
        ]
        if not matches:
            print("No matches.")
            return
        for idx, raw in matches:
            print(f"[{idx}] {raw}")

    def show_stats(self) -> None:
        print("--- Statistics ---")
        print(f"Pending: {len(self.todos)}")
        print(f"Completed: {len(self.done_items)}")


# ----------------------------- CLI Parsing ----------------------------- #


class Color:
    RESET = "\033[0m"
    BOLD = "\033[1m"
    GREEN = "\033[32m"
    RED = "\033[31m"
    YELLOW = "\033[33m"


def supports_color() -> bool:
    return (
        sys.stdout.isatty()
        and os.name != "nt"
        or ("ANSICON" in os.environ or "WT_SESSION" in os.environ)
    )


def colorize(s: str, color: str, enable: bool) -> str:
    return f"{color}{s}{Color.RESET}" if enable else s


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="A modern command-line To-Do list application."
    )
    parser.add_argument(
        "--no-color",
        action="store_true",
        help="Disable colored output even if terminal supports it.",
    )

    sub = parser.add_subparsers(dest="command", required=True)

    p_add = sub.add_parser("add", help="Add a new item")
    p_add.add_argument("text", nargs="+", help="Item description")
    p_add.add_argument("-p", "--priority", help="Priority letter A-Z")

    sub.add_parser("show", help="Show pending items")
    sub.add_parser("show-done", help="Show completed items")

    p_del = sub.add_parser("delete", help="Delete item by ID")
    p_del.add_argument("id", type=int)

    p_done = sub.add_parser("done", help="Mark item done by ID")
    p_done.add_argument("id", type=int)

    sub.add_parser("undo", help="Undo last completion")

    p_prio = sub.add_parser("prioritize", help="Assign / change priority for an item")
    p_prio.add_argument("id", type=int)
    p_prio.add_argument("prio", help="Priority letter A-Z")

    p_clear = sub.add_parser(
        "clear", help="Clear all pending items (requires --confirm)"
    )
    p_clear.add_argument(
        "--confirm", action="store_true", help="Actually perform clear."
    )

    p_search = sub.add_parser("search", help="Search term in pending items")
    p_search.add_argument("term")

    sub.add_parser("stats", help="Show statistics")

    return parser


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    use_color = supports_color() and not args.no_color

    app = TodoList()

    try:
        if args.command == "add":
            app.add(" ".join(args.text), priority=args.priority)
        elif args.command == "show":
            app.show()
        elif args.command == "show-done":
            app.show_done()
        elif args.command == "delete":
            app.delete(args.id)
        elif args.command == "done":
            app.mark_done(args.id)
        elif args.command == "undo":
            app.undo_last_done()
        elif args.command == "prioritize":
            app.prioritize(args.id, args.prio)
        elif args.command == "clear":
            app.clear(confirm=args.confirm)
        elif args.command == "search":
            app.search(args.term)
        elif args.command == "stats":
            app.show_stats()
        else:
            parser.error("Unknown command")
    except (ValueError, IndexError) as e:
        print(colorize(f"Error: {e}", Color.RED, use_color), file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        print("Aborted", file=sys.stderr)
        return 130
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
