"""Input handling for CHIP-8."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Optional


HOST_KEYMAP = {
    "1": 0x1,
    "2": 0x2,
    "3": 0x3,
    "4": 0xC,
    "q": 0x4,
    "w": 0x5,
    "e": 0x6,
    "r": 0xD,
    "a": 0x7,
    "s": 0x8,
    "d": 0x9,
    "f": 0xE,
    "z": 0xA,
    "x": 0x0,
    "c": 0xB,
    "v": 0xF,
}


@dataclass
class Keypad:
    """Tracks the 16-key CHIP-8 keypad state."""

    _state: Dict[int, bool] = field(default_factory=lambda: {index: False for index in range(16)})
    _last_pressed: Optional[int] = None

    def press(self, key: int) -> None:
        self._state[key] = True
        self._last_pressed = key

    def release(self, key: int) -> None:
        self._state[key] = False

    def is_pressed(self, key: int) -> bool:
        return self._state.get(key, False)

    def consume_press(self) -> Optional[int]:
        key = self._last_pressed
        self._last_pressed = None
        return key

    def clear(self) -> None:
        for key in self._state:
            self._state[key] = False
        self._last_pressed = None


class PygameInput:
    """Input driver backed by pygame."""

    def __init__(self, keypad: Keypad) -> None:
        try:
            import pygame
        except ImportError as exc:  # pragma: no cover - optional dependency
            raise RuntimeError("pygame is required for the pygame input backend") from exc
        self._pygame = pygame
        self.keypad = keypad
        self.quit_requested = False

    def process_events(self) -> None:  # pragma: no cover - requires pygame
        for event in self._pygame.event.get():
            if event.type == self._pygame.QUIT:
                self.quit_requested = True
            elif event.type == self._pygame.KEYDOWN:
                if event.key == self._pygame.K_ESCAPE:
                    self.quit_requested = True
                self._handle_key(event.key, True)
            elif event.type == self._pygame.KEYUP:
                self._handle_key(event.key, False)

    def _handle_key(self, keycode: int, pressed: bool) -> None:  # pragma: no cover - requires pygame
        key_name = self._pygame.key.name(keycode)
        mapping = HOST_KEYMAP.get(key_name)
        if mapping is None:
            return
        if pressed:
            self.keypad.press(mapping)
        else:
            self.keypad.release(mapping)


class CursesInput:
    """Input driver using curses nodelay polling."""

    def __init__(self, keypad: Keypad, screen) -> None:
        self.keypad = keypad
        self.screen = screen
        self._held: Dict[int, int] = {}

    def process_events(self) -> bool:  # pragma: no cover - interactive
        try:
            while True:
                keycode = self.screen.getch()
                if keycode == -1:
                    break
                if keycode in (27, ord("q")):
                    return True
                char = chr(keycode).lower()
                mapping = HOST_KEYMAP.get(char)
                if mapping is not None:
                    self.keypad.press(mapping)
                    self._held[mapping] = 2
        finally:
            # Reduce held timers to emulate key release when not repeated.
            to_release = [key for key, frames in self._held.items() if frames <= 0]
            for key in to_release:
                self.keypad.release(key)
                del self._held[key]
            for key in list(self._held):
                self._held[key] -= 1
        return False
