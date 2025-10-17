"""Core Tetris board mechanics (bag randomiser, scoring, and rules)."""

from __future__ import annotations

from dataclasses import dataclass
import random
from typing import Dict, List, Optional, Sequence, Tuple

from .settings import TetrisSettings

Position = Tuple[int, int]


@dataclass
class Piece:
    """Runtime representation of an active tetromino."""

    name: str
    rotations: Sequence[Sequence[Position]]
    color: Tuple[int, int, int]
    x: int
    y: int
    rotation_index: int = 0

    def cells(
        self, rotation: Optional[int] = None, offset: Optional[Position] = None
    ) -> List[Position]:
        idx = self.rotation_index if rotation is None else rotation
        ox, oy = offset if offset is not None else (self.x, self.y)
        return [(ox + px, oy + py) for px, py in self.rotations[idx]]


PIECES: Dict[str, Dict[str, Sequence]] = {
    "I": {
        "rotations": (
            ((0, 1), (1, 1), (2, 1), (3, 1)),
            ((2, 0), (2, 1), (2, 2), (2, 3)),
            ((0, 2), (1, 2), (2, 2), (3, 2)),
            ((1, 0), (1, 1), (1, 2), (1, 3)),
        ),
        "color": (0, 255, 255),
    },
    "J": {
        "rotations": (
            ((0, 0), (0, 1), (1, 1), (2, 1)),
            ((1, 0), (2, 0), (1, 1), (1, 2)),
            ((0, 1), (1, 1), (2, 1), (2, 2)),
            ((1, 0), (1, 1), (0, 2), (1, 2)),
        ),
        "color": (0, 0, 255),
    },
    "L": {
        "rotations": (
            ((2, 0), (0, 1), (1, 1), (2, 1)),
            ((1, 0), (1, 1), (1, 2), (2, 2)),
            ((0, 1), (1, 1), (2, 1), (0, 2)),
            ((0, 0), (1, 0), (1, 1), (1, 2)),
        ),
        "color": (255, 127, 0),
    },
    "O": {
        "rotations": (((1, 0), (2, 0), (1, 1), (2, 1)),),
        "color": (255, 255, 0),
    },
    "S": {
        "rotations": (
            ((1, 0), (2, 0), (0, 1), (1, 1)),
            ((1, 0), (1, 1), (2, 1), (2, 2)),
            ((1, 1), (2, 1), (0, 2), (1, 2)),
            ((0, 0), (0, 1), (1, 1), (1, 2)),
        ),
        "color": (0, 255, 0),
    },
    "T": {
        "rotations": (
            ((1, 0), (0, 1), (1, 1), (2, 1)),
            ((1, 0), (1, 1), (2, 1), (1, 2)),
            ((0, 1), (1, 1), (2, 1), (1, 2)),
            ((1, 0), (0, 1), (1, 1), (1, 2)),
        ),
        "color": (160, 0, 240),
    },
    "Z": {
        "rotations": (
            ((0, 0), (1, 0), (1, 1), (2, 1)),
            ((2, 0), (1, 1), (2, 1), (1, 2)),
            ((0, 1), (1, 1), (1, 2), (2, 2)),
            ((1, 0), (0, 1), (1, 1), (0, 2)),
        ),
        "color": (255, 0, 0),
    },
}


class TetrisBoard:
    """Maintains board state, bag randomiser, scoring, and line clears."""

    SCORE_TABLE = {0: 0, 1: 100, 2: 300, 3: 500, 4: 800}
    ROTATION_OFFSETS = ((0, 0), (1, 0), (-1, 0), (0, -1), (0, 1), (-2, 0), (2, 0))

    def __init__(self, settings: TetrisSettings) -> None:
        self.settings = settings
        self.width = settings.board_width
        self.height = settings.board_height
        self.grid: List[List[Optional[Tuple[int, int, int]]]] = [
            [None for _ in range(self.width)] for _ in range(self.height)
        ]
        self.bag: List[str] = []
        self.next_queue: List[str] = []
        self.active: Optional[Piece] = None
        self.score = 0
        self.lines_cleared = 0
        self.level = 1
        self.game_over = False
        self._refill_queue()
        self.spawn_piece()

    # --- Piece management -------------------------------------------------
    def _refill_bag(self) -> None:
        if not self.bag:
            self.bag = list(PIECES.keys())
            random.shuffle(self.bag)

    def _refill_queue(self) -> None:
        while len(self.next_queue) < self.settings.next_queue_length:
            self._refill_bag()
            self.next_queue.append(self.bag.pop())

    def spawn_piece(self) -> None:
        if not self.next_queue:
            self._refill_queue()
        name = self.next_queue.pop(0)
        data = PIECES[name]
        x = self.width // 2 - 2
        y = 0
        piece = Piece(
            name=name, rotations=data["rotations"], color=data["color"], x=x, y=y
        )
        if not self._is_valid_position(piece, piece.x, piece.y, piece.rotation_index):
            self.game_over = True
            self.active = None
        else:
            self.active = piece
        self._refill_queue()

    # --- Movement ---------------------------------------------------------
    def move(self, dx: int) -> bool:
        if not self.active or dx == 0 or self.game_over:
            return False
        if self._is_valid_position(
            self.active, self.active.x + dx, self.active.y, self.active.rotation_index
        ):
            self.active.x += dx
            return True
        return False

    def rotate(self, direction: int) -> bool:
        if not self.active or self.game_over:
            return False
        new_index = (self.active.rotation_index + direction) % len(
            self.active.rotations
        )
        for ox, oy in self.ROTATION_OFFSETS:
            nx = self.active.x + ox
            ny = self.active.y + oy
            if self._is_valid_position(self.active, nx, ny, new_index):
                self.active.rotation_index = new_index
                self.active.x = nx
                self.active.y = ny
                return True
        return False

    def soft_drop(self) -> bool:
        if not self.active or self.game_over:
            return False
        if self._is_valid_position(
            self.active, self.active.x, self.active.y + 1, self.active.rotation_index
        ):
            self.active.y += 1
            self.score += 1
            return False
        self.lock_piece()
        return True

    def hard_drop(self) -> None:
        if not self.active or self.game_over:
            return
        distance = 0
        while self._is_valid_position(
            self.active, self.active.x, self.active.y + 1, self.active.rotation_index
        ):
            self.active.y += 1
            distance += 1
        self.score += distance * 2
        self.lock_piece()

    def tick_gravity(self) -> None:
        if not self.active or self.game_over:
            return
        if not self._is_valid_position(
            self.active, self.active.x, self.active.y + 1, self.active.rotation_index
        ):
            self.lock_piece()
        else:
            self.active.y += 1

    # --- State queries ----------------------------------------------------
    def get_cells(self) -> List[Position]:
        if not self.active:
            return []
        return self.active.cells()

    def ghost_cells(self) -> List[Position]:
        if not self.active:
            return []
        drop = self._drop_distance()
        return self.active.cells(offset=(self.active.x, self.active.y + drop))

    def peek_queue(self) -> List[str]:
        return list(self.next_queue)

    def gravity_interval(self) -> float:
        return max(0.05, self.settings.gravity_seconds * (0.85 ** (self.level - 1)))

    # --- Internal helpers -------------------------------------------------
    def _is_valid_position(self, piece: Piece, x: int, y: int, rotation: int) -> bool:
        for px, py in piece.cells(rotation=rotation, offset=(x, y)):
            if px < 0 or px >= self.width or py < 0 or py >= self.height:
                return False
            if self.grid[py][px] is not None:
                return False
        return True

    def _drop_distance(self) -> int:
        if not self.active:
            return 0
        distance = 0
        while self._is_valid_position(
            self.active,
            self.active.x,
            self.active.y + distance + 1,
            self.active.rotation_index,
        ):
            distance += 1
        return distance

    def lock_piece(self) -> None:
        if not self.active:
            return
        for px, py in self.active.cells():
            if 0 <= py < self.height and 0 <= px < self.width:
                self.grid[py][px] = self.active.color
        self.active = None
        lines = self._clear_lines()
        self._apply_line_score(lines)
        self.spawn_piece()

    def _clear_lines(self) -> int:
        new_grid: List[List[Optional[Tuple[int, int, int]]]] = []
        cleared = 0
        for row in self.grid:
            if all(cell is not None for cell in row):
                cleared += 1
            else:
                new_grid.append(row)
        for _ in range(cleared):
            new_grid.insert(0, [None for _ in range(self.width)])
        self.grid = new_grid
        self.lines_cleared += cleared
        if cleared:
            self.level = 1 + self.lines_cleared // 10
        return cleared

    def _apply_line_score(self, cleared: int) -> None:
        self.score += self.SCORE_TABLE.get(cleared, 0) * self.level

    # --- Lifecycle -------------------------------------------------------
    def reset(self) -> None:
        self.grid = [[None for _ in range(self.width)] for _ in range(self.height)]
        self.bag.clear()
        self.next_queue.clear()
        self.active = None
        self.score = 0
        self.lines_cleared = 0
        self.level = 1
        self.game_over = False
        self._refill_queue()
        self.spawn_piece()
