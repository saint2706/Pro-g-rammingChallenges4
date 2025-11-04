"""Keypad matrix abstraction for the TI-86 emulator.

This module provides a simple simulation of the TI-86's keypad matrix.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Tuple


@dataclass
class Keypad:
    """Tracks the state of the TI-86 keypad matrix.

    The TI-86 keypad is organized as a matrix of rows and columns. A key press
    is detected by checking which column is active for a given row.

    Attributes:
        matrix: A dictionary that stores the state of each key. The keys are
                tuples of (row, column), and the values are booleans
                indicating if the key is pressed.
    """

    matrix: Dict[Tuple[int, int], bool] = field(default_factory=dict)

    def press(self, row: int, column: int) -> None:
        """Marks a key as pressed.

        Args:
            row: The row of the key.
            column: The column of the key.
        """
        self.matrix[(row & 0x07, column & 0x07)] = True

    def release(self, row: int, column: int) -> None:
        """Marks a key as released.

        Args:
            row: The row of the key.
            column: The column of the key.
        """
        self.matrix.pop((row & 0x07, column & 0x07), None)

    def is_pressed(self, row: int, column: int) -> bool:
        """Checks if a specific key is pressed.

        Args:
            row: The row of the key.
            column: The column of the key.

        Returns:
            True if the key is pressed, False otherwise.
        """
        return self.matrix.get((row & 0x07, column & 0x07), False)

    def active_columns(self, row_mask: int) -> int:
        """Determines which columns are active for a given row mask.

        This simulates the hardware behavior of the keypad, where the CPU
        scans for key presses by checking for active columns in selected rows.

        Args:
            row_mask: A bitmask representing the rows to check.

        Returns:
            A bitmask representing the active columns.
        """
        value = 0xFF
        for (row, column), pressed in self.matrix.items():
            if pressed and (row_mask & (1 << row)):
                value &= ~(1 << column)
        return value

    def reset(self) -> None:
        """Resets the keypad by clearing all pressed keys."""
        self.matrix.clear()
