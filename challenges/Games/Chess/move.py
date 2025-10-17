"""Data structures describing chess moves."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Tuple

Square = Tuple[int, int]


@dataclass(frozen=True)
class Move:
    """Represents a single chess move.

    Attributes:
        start: (row, column) tuple in 0-indexed board coordinates.
        end: Destination square in 0-indexed board coordinates.
        promotion: Optional promoted piece character (``Q``, ``R``, ``B``, ``N``).
        is_castling: Whether the move is a castling move.
        is_en_passant: Whether the move captures en passant.
    """

    start: Square
    end: Square
    promotion: Optional[str] = None
    is_castling: bool = False
    is_en_passant: bool = False

    def to_uci(self) -> str:
        """Return the move in UCI coordinate notation."""

        return f"{self._square_to_str(self.start)}{self._square_to_str(self.end)}" + (
            (self.promotion or "").lower()
        )

    @staticmethod
    def _square_to_str(square: Square) -> str:
        file = chr(ord("a") + square[1])
        rank = str(8 - square[0])
        return f"{file}{rank}"

    @staticmethod
    def from_uci(value: str) -> "Move":
        """Parse a UCI move (e.g. ``e2e4`` or ``e7e8q``)."""

        if len(value) not in {4, 5}:
            raise ValueError(f"Invalid UCI move: {value}")
        start = (8 - int(value[1]), ord(value[0]) - ord("a"))
        end = (8 - int(value[3]), ord(value[2]) - ord("a"))
        promotion = value[4].upper() if len(value) == 5 else None
        return Move(start, end, promotion=promotion)
