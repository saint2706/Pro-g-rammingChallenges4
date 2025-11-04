"""Input handling for CHIP-8.

This module provides the keypad state management and input drivers for the
Chip-8 emulator. It includes a Keypad class to track the state of the 16 keys,
as well as input drivers for Pygame and Curses.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Optional

# Maps host keyboard keys to the 16-key Chip-8 keypad.
HOST_KEYMAP = {
    "1": 0x1, "2": 0x2, "3": 0x3, "4": 0xC,
    "q": 0x4, "w": 0x5, "e": 0x6, "r": 0xD,
    "a": 0x7, "s": 0x8, "d": 0x9, "f": 0xE,
    "z": 0xA, "x": 0x0, "c": 0xB, "v": 0xF,
}


@dataclass
class Keypad:
    """Tracks the 16-key CHIP-8 keypad state.

    This class maintains the current state of each of the 16 keys on the
    Chip-8 keypad and keeps track of the last key that was pressed.
    """

    _state: Dict[int, bool] = field(
        default_factory=lambda: {index: False for index in range(16)}
    )
    _last_pressed: Optional[int] = None

    def press(self, key: int) -> None:
        """Marks a key as pressed.

        Args:
            key: The key to press (0x0-0xF).
        """
        self._state[key] = True
        self._last_pressed = key

    def release(self, key: int) -> None:
        """Marks a key as released.

        Args:
            key: The key to release (0x0-0xF).
        """
        self._state[key] = False

    def is_pressed(self, key: int) -> bool:
        """Checks if a key is currently pressed.

        Args:
            key: The key to check (0x0-0xF).

        Returns:
            True if the key is pressed, False otherwise.
        """
        return self._state.get(key, False)

    def consume_press(self) -> Optional[int]:
        """Consumes the last key press.

        This is used for the FX0A instruction, which waits for a key press.
        It returns the last pressed key and then clears it, so it's only
        returned once.

        Returns:
            The last pressed key, or None if no key has been pressed since the
            last consumption.
        """
        key = self._last_pressed
        self._last_pressed = None
        return key

    def clear(self) -> None:
        """Clears the state of all keys."""
        for key in self._state:
            self._state[key] = False
        self._last_pressed = None


class PygameInput:
    """Input driver backed by Pygame.

    This class handles keyboard input using the Pygame library.
    """

    def __init__(self, keypad: Keypad) -> None:
        """Initializes the Pygame input driver.

        Args:
            keypad: The Keypad instance to update.
        """
        try:
            import pygame
        except ImportError as exc:
            raise RuntimeError(
                "pygame is required for the pygame input backend"
            ) from exc
        self._pygame = pygame
        self.keypad = keypad
        self.quit_requested = False

    def process_events(self) -> None:
        """Processes Pygame events to update the keypad state."""
        for event in self._pygame.event.get():
            if event.type == self._pygame.QUIT:
                self.quit_requested = True
            elif event.type == self._pygame.KEYDOWN:
                if event.key == self._pygame.K_ESCAPE:
                    self.quit_requested = True
                self._handle_key(event.key, True)
            elif event.type == self._pygame.KEYUP:
                self._handle_key(event.key, False)

    def _handle_key(self, keycode: int, pressed: bool) -> None:
        """Handles a key press or release event.

        Args:
            keycode: The Pygame keycode of the key.
            pressed: True if the key was pressed, False if it was released.
        """
        key_name = self._pygame.key.name(keycode)
        mapping = HOST_KEYMAP.get(key_name)
        if mapping is not None:
            if pressed:
                self.keypad.press(mapping)
            else:
                self.keypad.release(mapping)


class CursesInput:
    """Input driver using Curses.

    This class handles keyboard input using the Curses library.
    """

    def __init__(self, keypad: Keypad, screen) -> None:
        """Initializes the Curses input driver.

        Args:
            keypad: The Keypad instance to update.
            screen: The Curses screen object.
        """
        self.keypad = keypad
        self.screen = screen
        self._held: Dict[int, int] = {}

    def process_events(self) -> bool:
        """Processes Curses events to update the keypad state.

        Returns:
            True if the user has requested to quit, False otherwise.
        """
        try:
            while True:
                keycode = self.screen.getch()
                if keycode == -1:
                    break
                if keycode in (27, ord("q")):  # Escape or 'q' to quit.
                    return True
                char = chr(keycode).lower()
                mapping = HOST_KEYMAP.get(char)
                if mapping is not None:
                    self.keypad.press(mapping)
                    self._held[mapping] = 2
        finally:
            # Emulate key releases by reducing a counter for held keys.
            to_release = [key for key, frames in self._held.items() if frames <= 0]
            for key in to_release:
                self.keypad.release(key)
                del self._held[key]
            for key in list(self._held):
                self._held[key] -= 1
        return False
