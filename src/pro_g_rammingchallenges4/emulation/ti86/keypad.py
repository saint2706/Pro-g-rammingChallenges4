"""Keypad matrix abstraction."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Iterable, Tuple


@dataclass
class Keypad:
    """Tracks key states for the TI-86 keypad matrix."""

    matrix: Dict[Tuple[int, int], bool] = field(default_factory=dict)

    def press(self, row: int, column: int) -> None:
        self.matrix[(row & 0x07, column & 0x07)] = True

    def release(self, row: int, column: int) -> None:
        self.matrix[(row & 0x07, column & 0x07)] = False

    def is_pressed(self, row: int, column: int) -> bool:
        return self.matrix.get((row & 0x07, column & 0x07), False)

    def active_columns(self, row_mask: int) -> int:
        value = 0xFF
        for (row, column), pressed in self.matrix.items():
            if pressed and (row_mask & (1 << row)):
                value &= ~(1 << column)
        return value & 0xFF

    def reset(self) -> None:
        self.matrix.clear()
