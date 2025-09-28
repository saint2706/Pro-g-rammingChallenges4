"""Configuration loading for the curses text editor."""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Mapping, Optional

DEFAULT_OPTIONS = {
    "autosave_interval": 10,
    "autosave_suffix": ".autosave",
    "tab_width": 4,
    "expand_tab": True,
}

DEFAULT_MAPPINGS = {
    "command": {
        "Esc": "enter_command_mode",
        "Ctrl+Q": "quit",
        "Ctrl+S": "write",
        "Ctrl+L": "refresh",
        "Ctrl+G": "show_stats",
        "h": "move_left",
        "j": "move_down",
        "k": "move_up",
        "l": "move_right",
        "KEY_LEFT": "move_left",
        "KEY_RIGHT": "move_right",
        "KEY_UP": "move_up",
        "KEY_DOWN": "move_down",
        "0": "move_line_start",
        "$": "move_line_end",
        "Ctrl+A": "move_line_start",
        "Ctrl+E": "move_line_end",
        "gg": "move_file_start",
        "G": "move_file_end",
        "Ctrl+Home": "move_file_start",
        "Ctrl+End": "move_file_end",
        "w": "move_word_forward",
        "b": "move_word_backward",
        "i": "enter_insert_mode",
        "a": "append",
        "o": "open_below",
        "O": "open_above",
        ":": "command_palette",
        "dd": "delete_line",
        "yy": "yank_line",
        "p": "paste",
        "Ctrl+Y": "paste",
        "x": "delete_char",
        "KEY_DC": "delete_char",
        "n": "search_next",
        "N": "search_prev",
        "/": "search_forward",
        "?": "search_backward",
        "F1": "help",
    },
    "insert": {
        "Esc": "enter_command_mode",
        "Ctrl+S": "write",
        "Ctrl+Q": "quit",
        "KEY_BACKSPACE": "backspace",
        "Backspace": "backspace",
        "KEY_DC": "delete_char",
        "Ctrl+D": "delete_char",
        "Enter": "newline",
        "KEY_ENTER": "newline",
        "Tab": "insert_tab",
    },
}

HUMAN_READABLE_KEYS = {
    9: "Tab",
    10: "Enter",
    13: "Enter",
    27: "Esc",
    127: "Backspace",
}


@dataclass
class EditorConfig:
    options: Dict[str, object] = field(default_factory=lambda: dict(DEFAULT_OPTIONS))
    mappings: Dict[str, Dict[str, str]] = field(
        default_factory=lambda: json.loads(json.dumps(DEFAULT_MAPPINGS))
    )
    source: Optional[Path] = None

    def merge(self, other: Mapping[str, object]) -> None:
        mappings = other.get("mappings")
        if isinstance(mappings, Mapping):
            for mode, entries in mappings.items():
                if mode not in self.mappings:
                    self.mappings[mode] = {}
                if isinstance(entries, Mapping):
                    for key, action in entries.items():
                        self.mappings[mode][str(key)] = str(action)
        options = other.get("options")
        if isinstance(options, Mapping):
            for key, value in options.items():
                self.options[str(key)] = value

    def resolve_key(self, key_name: str, mode: str) -> Optional[str]:
        return self.mappings.get(mode, {}).get(key_name)


def load_config(explicit_path: Optional[str] = None) -> EditorConfig:
    """Load editor configuration from JSON file(s)."""
    config = EditorConfig()
    paths = []
    if explicit_path:
        paths.append(Path(explicit_path).expanduser())
    else:
        cwd_path = Path("ctedit.json")
        home_path = Path.home() / ".ctedit.json"
        for candidate in (cwd_path, home_path):
            if candidate.exists():
                paths.append(candidate)
    for path in paths:
        try:
            data = json.loads(path.read_text())
        except FileNotFoundError:
            continue
        except json.JSONDecodeError as exc:
            raise ValueError(f"Failed to parse config {path}: {exc}") from exc
        config.merge(data)
        config.source = path
    return config


__all__ = [
    "EditorConfig",
    "load_config",
    "DEFAULT_OPTIONS",
    "DEFAULT_MAPPINGS",
    "HUMAN_READABLE_KEYS",
]
