"""Configuration for the lightweight tiling window manager."""
from dataclasses import dataclass
from typing import Sequence, Tuple


@dataclass(frozen=True)
class KeyBinding:
    """Declarative mapping from a key chord to an action name."""

    modifiers: Sequence[str]
    key: str
    action: str


@dataclass(frozen=True)
class WMConfig:
    """Runtime configuration knobs for the window manager."""

    terminal_command: Sequence[str] = ("xterm",)
    border_width: int = 2
    focused_border_color: str = "#4c78ff"
    unfocused_border_color: str = "#20252c"
    gap_px: int = 8
    master_ratio: float = 0.58
    keybindings: Tuple[KeyBinding, ...] = (
        KeyBinding(("Mod4",), "Return", "spawn_terminal"),
        KeyBinding(("Mod4",), "q", "close_focused"),
        KeyBinding(("Mod4",), "j", "focus_next"),
        KeyBinding(("Mod4",), "k", "focus_prev"),
        KeyBinding(("Mod4", "Shift"), "j", "shift_next"),
        KeyBinding(("Mod4", "Shift"), "k", "shift_prev"),
        KeyBinding(("Mod4",), "space", "rotate_master"),
    )


DEFAULT_CONFIG = WMConfig()
