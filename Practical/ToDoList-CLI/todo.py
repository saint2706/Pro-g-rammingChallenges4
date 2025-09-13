import sys
import os
import argparse
from datetime import date
from typing import List

class TodoList:
    """
    A class to manage a command-line To-Do list application.
    Data is stored in todo.txt and done.txt.
    """
    def __init__(self, todo_file: str = "todo.txt", done_file: str = "done.txt"):
        self.todo_file = todo_file
        self.done_file = done_file
        self._ensure_files_exist()
        self.todos: List[str] = self._read_file(self.todo_file)
        self.done_items: List[str] = self._read_file(self.done_file)

    def _ensure_files_exist(self):
        """Creates the data files if they don't already exist."""
        if not os.path.exists(self.todo_file):
            open(self.todo_file, 'w').close()
        if not os.path.exists(self.done_file):
            open(self.done_file, 'w').close()

    def _read_file(self, filepath: str) -> List[str]:
        """Reads all lines from a file into a list."""
        with open(filepath, 'r') as f:
            return [line.strip() for line in f.readlines()]

    def _write_file(self, filepath: str, items: List[str]):
        """Writes a list of items to a file, one per line."""
        with open(filepath, 'w') as f:
            for item in items:
                f.write(item + '\n')

    def add(self, item: str):
        """Adds a new item to the To-Do list."""
        self.todos.append(item)
        self._write_file(self.todo_file, self.todos)
        print(f'Added todo: "{item}"')

    def show(self):
        """Displays all pending To-Do items."""
        print("\n--- Pending Todos ---")
        if not self.todos:
            print("No pending todos!")
            return
        # Print in reverse so the most recently added is at the top
        for i, item in reversed(list(enumerate(self.todos, 1))):
            print(f"[{i}] {item}")

    def delete(self, item_id: int):
        """Deletes a To-Do item by its 1-based index."""
        if not 1 <= item_id <= len(self.todos):
            print(f"Error: Todo item #{item_id} does not exist.", file=sys.stderr)
            return

        deleted_item = self.todos.pop(item_id - 1)
        self._write_file(self.todo_file, self.todos)
        print(f'Deleted todo #{item_id}: "{deleted_item}"')

    def mark_done(self, item_id: int):
        """Marks a To-Do item as done and moves it to done.txt."""
        if not 1 <= item_id <= len(self.todos):
            print(f"Error: Todo item #{item_id} does not exist.", file=sys.stderr)
            return

        done_item = self.todos.pop(item_id - 1)
        self._write_file(self.todo_file, self.todos)

        today_str = date.today().strftime("%Y-%m-%d")
        self.done_items.append(f"x {today_str} {done_item}")
        self._write_file(self.done_file, self.done_items)
        print(f'Marked todo #{item_id} as done: "{done_item}"')

    def show_stats(self):
        """Displays statistics about pending and completed todos."""
        pending_count = len(self.todos)
        done_count = len(self.done_items)
        print("\n--- Statistics ---")
        print(f"Pending To-Dos: {pending_count}")
        print(f"Completed To-Dos: {done_count}")

def main():
    """
    Main function to parse command-line arguments and run the application.
    """
    parser = argparse.ArgumentParser(description="A simple command-line To-Do list application.")
    subparsers = parser.add_subparsers(dest="command", help="Available commands", required=True)

    # --- Add command ---
    add_parser = subparsers.add_parser("add", help="Add a new item to the to-do list.")
    add_parser.add_argument("item", nargs='+', help="The content of the to-do item.")

    # --- Show command ---
    subparsers.add_parser("show", help="Show remaining to-do items.")

    # --- Delete command ---
    delete_parser = subparsers.add_parser("delete", help="Delete a to-do item.")
    delete_parser.add_argument("id", type=int, help="The 1-based ID of the item to delete.")

    # --- Done command ---
    done_parser = subparsers.add_parser("done", help="Mark a to-do item as complete.")
    done_parser.add_argument("id", type=int, help="The 1-based ID of the item to complete.")

    # --- Stats command ---
    subparsers.add_parser("stats", help="Show statistics.")

    args = parser.parse_args()

    todo_app = TodoList()

    if args.command == "add":
        todo_app.add(" ".join(args.item))
    elif args.command == "show":
        todo_app.show()
    elif args.command == "delete":
        todo_app.delete(args.id)
    elif args.command == "done":
        todo_app.mark_done(args.id)
    elif args.command == "stats":
        todo_app.show_stats()

if __name__ == "__main__":
    main()
