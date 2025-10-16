"""Core logic for the Go board game.

The :class:`GoGame` class tracks board state, validates moves (including ko and
suicide rules), manages captures, provides undo/redo, and exports Smart Game
Format (SGF) records. The engine is intentionally UI-agnostic so it can be
covered by automated tests and reused by the Qt application.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple
import random

Color = str  # "B" for black, "W" for white
Point = Tuple[int, int]


class IllegalMove(ValueError):
    """Raised when a move violates the rules of Go."""


@dataclass(frozen=True)
class Move:
    color: Color
    point: Optional[Point]

    def to_sgf(self) -> str:
        if self.point is None:
            return ""
        row, col = self.point
        return f"{chr(ord('a') + col)}{chr(ord('a') + row)}"


def other(color: Color) -> Color:
    return "W" if color == "B" else "B"


class GoGame:
    """Game state manager for Go."""

    def __init__(self, size: int = 19):
        if size not in (9, 13, 19):
            raise ValueError(
                "Go boards must be 9x9, 13x13, or 19x19 for this implementation."
            )
        self.size = size
        self.board: List[List[Optional[Color]]] = [
            [None for _ in range(size)] for _ in range(size)
        ]
        self.to_move: Color = "B"
        self.captures: Dict[Color, int] = {"B": 0, "W": 0}
        self.moves: List[Move] = []
        # Position history used for the simple-superko rule (ko prevention)
        self._position_history: List[str] = [self._board_key(self.board)]
        self._snapshots: List[
            Tuple[List[List[Optional[Color]]], Color, Dict[Color, int]]
        ] = [(self._copy_board(self.board), self.to_move, self.captures.copy())]

    # ------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------
    def _copy_board(
        self, board: Sequence[Sequence[Optional[Color]]]
    ) -> List[List[Optional[Color]]]:
        return [list(row) for row in board]

    def _board_key(self, board: Sequence[Sequence[Optional[Color]]]) -> str:
        return "".join("." if cell is None else cell for row in board for cell in row)

    def _neighbors(self, row: int, col: int) -> Iterable[Point]:
        if row > 0:
            yield row - 1, col
        if row + 1 < self.size:
            yield row + 1, col
        if col > 0:
            yield row, col - 1
        if col + 1 < self.size:
            yield row, col + 1

    def _collect_group(
        self, board: Sequence[Sequence[Optional[Color]]], start: Point
    ) -> Tuple[Set[Point], int]:
        """Return the stones in the connected group and their liberty count."""
        stack = [start]
        seen: Set[Point] = set()
        liberties = 0
        color = board[start[0]][start[1]]
        while stack:
            point = stack.pop()
            if point in seen:
                continue
            seen.add(point)
            r, c = point
            for nr, nc in self._neighbors(r, c):
                neighbor_color = board[nr][nc]
                if neighbor_color is None:
                    liberties += 1
                elif neighbor_color == color and (nr, nc) not in seen:
                    stack.append((nr, nc))
        return seen, liberties

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------
    def legal_moves(self, color: Optional[Color] = None) -> List[Point]:
        color = color or self.to_move
        moves: List[Point] = []
        opponent = other(color)
        history = set(self._position_history)
        group_cache: Dict[Point, Tuple[Set[Point], int]] = {}

        def cached_group(point: Point) -> Tuple[Set[Point], int]:
            if point in group_cache:
                return group_cache[point]
            group, liberties = self._collect_group(self.board, point)
            for member in group:
                group_cache[member] = (group, liberties)
            return group, liberties

        for r in range(self.size):
            for c in range(self.size):
                if self.board[r][c] is not None:
                    continue

                has_adjacent_liberty = False
                friendly_safe = False
                capture_available = False

                for nr, nc in self._neighbors(r, c):
                    neighbour_color = self.board[nr][nc]
                    if neighbour_color is None:
                        has_adjacent_liberty = True
                        continue
                    group, liberties = cached_group((nr, nc))
                    if neighbour_color == opponent:
                        if liberties == 1:
                            capture_available = True
                    else:
                        if liberties > 1:
                            friendly_safe = True

                if not (has_adjacent_liberty or capture_available or friendly_safe):
                    continue

                new_board, _ = self._simulate_move(r, c, color)
                if self._board_key(new_board) in history:
                    continue

                moves.append((r, c))
        return moves

    def play(self, row: int, col: int) -> None:
        """Place a stone for the current player at *row*, *col*."""
        self._ensure_on_board(row, col)
        if self.board[row][col] is not None:
            raise IllegalMove("Point already occupied.")

        new_board, captured = self._simulate_move(row, col, self.to_move)
        key = self._board_key(new_board)
        if key in self._position_history:
            raise IllegalMove("Move violates the ko rule.")

        self.board = new_board
        if captured:
            self.captures[self.to_move] += captured
        move = Move(self.to_move, (row, col))
        self.moves.append(move)
        self.to_move = other(self.to_move)
        self._position_history.append(key)
        self._snapshots.append(
            (self._copy_board(self.board), self.to_move, self.captures.copy())
        )

    def pass_turn(self) -> None:
        """Pass without playing a stone."""
        move = Move(self.to_move, None)
        self.moves.append(move)
        self.to_move = other(self.to_move)
        # Pass does not change the board so we reuse the previous snapshot but still track turn order
        self._position_history.append(self._position_history[-1])
        self._snapshots.append(
            (self._copy_board(self.board), self.to_move, self.captures.copy())
        )

    def undo(self) -> bool:
        """Undo the last move. Returns ``True`` if a move was undone."""
        if len(self._snapshots) <= 1:
            return False
        self._snapshots.pop()
        self._position_history.pop()
        last_snapshot = self._snapshots[-1]
        self.board = self._copy_board(last_snapshot[0])
        self.to_move = last_snapshot[1]
        self.captures = last_snapshot[2].copy()
        if self.moves:
            self.moves.pop()
        return True

    def score(self) -> Dict[str, Dict[Color, int]]:
        """Return both territory and area scores for the current board."""
        territories = self._territory_map()
        territory_score = {"B": self.captures["B"], "W": self.captures["W"]}
        area_score = {"B": self.captures["B"], "W": self.captures["W"]}

        for r in range(self.size):
            for c in range(self.size):
                stone = self.board[r][c]
                if stone is None:
                    owner = territories[r][c]
                    if owner:
                        territory_score[owner] += 1
                        area_score[owner] += 1
                else:
                    area_score[stone] += 1
        return {"territory": territory_score, "area": area_score}

    def random_legal_move(self, color: Optional[Color] = None) -> Optional[Point]:
        moves = self.legal_moves(color)
        if not moves:
            return None
        return random.choice(moves)

    def to_sgf(self, komi: float = 6.5) -> str:
        """Return an SGF representation of the current game."""
        header = [
            "(;GM[1]FF[4]",
            f"SZ[{self.size}]",
            "PB[Black]",
            "PW[White]",
            f"KM[{komi}]",
        ]
        body = []
        for move in self.moves:
            color_tag = move.color
            coord = move.to_sgf()
            body.append(f";{color_tag}[{coord}]")
        return "".join(header + body) + ")"

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------
    def _ensure_on_board(self, row: int, col: int) -> None:
        if not (0 <= row < self.size and 0 <= col < self.size):
            raise IllegalMove("Point is outside the board.")

    def _simulate_move(
        self, row: int, col: int, color: Color
    ) -> Tuple[List[List[Optional[Color]]], int]:
        board = self._copy_board(self.board)
        board[row][col] = color
        opponent = other(color)
        captured = 0

        # Remove opponent groups with no liberties
        for nr, nc in list(self._neighbors(row, col)):
            if board[nr][nc] != opponent:
                continue
            group, liberties = self._collect_group(board, (nr, nc))
            if liberties == 0:
                captured += len(group)
                for r, c in group:
                    board[r][c] = None

        # Check suicide
        group, liberties = self._collect_group(board, (row, col))
        if liberties == 0:
            raise IllegalMove("Move is suicidal.")

        return board, captured

    def _territory_map(self) -> List[List[Optional[Color]]]:
        territories: List[List[Optional[Color]]] = [
            [None for _ in range(self.size)] for _ in range(self.size)
        ]
        visited: Set[Point] = set()
        for r in range(self.size):
            for c in range(self.size):
                if self.board[r][c] is not None or (r, c) in visited:
                    continue
                region, owner = self._flood_fill_territory((r, c))
                for point in region:
                    visited.add(point)
                    territories[point[0]][point[1]] = owner
        return territories

    def _flood_fill_territory(self, start: Point) -> Tuple[Set[Point], Optional[Color]]:
        queue = [start]
        region: Set[Point] = set()
        border_colors: Set[Color] = set()
        while queue:
            point = queue.pop()
            if point in region:
                continue
            region.add(point)
            r, c = point
            for nr, nc in self._neighbors(r, c):
                color = self.board[nr][nc]
                if color is None:
                    if (nr, nc) not in region:
                        queue.append((nr, nc))
                else:
                    border_colors.add(color)
        if len(border_colors) == 1:
            return region, next(iter(border_colors))
        return region, None


__all__ = ["GoGame", "IllegalMove", "Move", "other"]
