"""Core puzzle dataclasses and helpers for Nonogram boards."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Sequence, Tuple
import json

from PIL import Image, ImageDraw, ImageFont

FILLED = 1
EMPTY = 0
UNKNOWN = -1

Cell = int
Board = List[List[Cell]]
Clues = List[List[int]]


@dataclass
class NonogramPuzzle:
    """Representation of a Nonogram puzzle."""

    row_clues: Clues
    column_clues: Clues
    name: str = "Nonogram"
    solution: Optional[Board] = None
    notes: str | None = None

    def __post_init__(self) -> None:
        if len(self.row_clues) == 0 or len(self.column_clues) == 0:
            raise ValueError("Row and column clues must not be empty")
        self.row_clues = [self._normalise_clue(row) for row in self.row_clues]
        self.column_clues = [self._normalise_clue(col) for col in self.column_clues]
        for row in self.row_clues:
            if any(num <= 0 for num in row):
                raise ValueError("Row clues must be positive integers")
        for col in self.column_clues:
            if any(num <= 0 for num in col):
                raise ValueError("Column clues must be positive integers")
        if self.solution is not None:
            height = len(self.row_clues)
            width = len(self.column_clues)
            if len(self.solution) != height or any(
                len(row) != width for row in self.solution
            ):
                raise ValueError("Solution shape does not match clues")

    @property
    def height(self) -> int:
        return len(self.row_clues)

    @property
    def width(self) -> int:
        return len(self.column_clues)

    def empty_board(self) -> Board:
        return [[UNKNOWN for _ in range(self.width)] for _ in range(self.height)]

    def copy(self) -> "NonogramPuzzle":
        return NonogramPuzzle(
            [list(row) for row in self.row_clues],
            [list(col) for col in self.column_clues],
            name=self.name,
            solution=[list(row) for row in self.solution] if self.solution else None,
            notes=self.notes,
        )

    @staticmethod
    def _normalise_clue(clue: Sequence[int]) -> List[int]:
        if len(clue) == 1 and clue[0] == 0:
            return []
        return [int(num) for num in clue]


def compute_clues(board: Board) -> Tuple[Clues, Clues]:
    """Return row and column clues for the provided solved board."""

    if not board or not board[0]:
        raise ValueError("Board must be non-empty")
    height = len(board)
    width = len(board[0])
    for row in board:
        if len(row) != width:
            raise ValueError("Board rows must be equal length")

    def line_to_clues(line: Sequence[int]) -> List[int]:
        clues: List[int] = []
        count = 0
        for cell in line:
            if cell == FILLED:
                count += 1
            else:
                if count:
                    clues.append(count)
                    count = 0
        if count:
            clues.append(count)
        return clues or [0]

    rows = [line_to_clues(row) for row in board]
    cols = [line_to_clues([board[r][c] for r in range(height)]) for c in range(width)]
    return rows, cols


def board_from_solution(board: Board) -> Board:
    return [list(row) for row in board]


def board_to_lists(board: Board) -> List[List[int]]:
    return [list(row) for row in board]


def puzzle_to_json(puzzle: NonogramPuzzle, board: Optional[Board] = None) -> str:
    payload = {
        "name": puzzle.name,
        "notes": puzzle.notes,
        "rows": puzzle.row_clues,
        "columns": puzzle.column_clues,
        "board": board_to_lists(board) if board is not None else None,
        "solution": (
            board_to_lists(puzzle.solution) if puzzle.solution is not None else None
        ),
    }
    return json.dumps(payload, indent=2)


def puzzle_from_json(data: str | Path | bytes) -> NonogramPuzzle:
    if isinstance(data, Path):
        payload = json.loads(data.read_text())
    elif isinstance(data, bytes):
        payload = json.loads(data.decode("utf-8"))
    else:
        payload = json.loads(data)
    puzzle = NonogramPuzzle(
        row_clues=[list(map(int, row)) for row in payload["rows"]],
        column_clues=[list(map(int, col)) for col in payload["columns"]],
        name=payload.get("name", "Nonogram"),
        solution=payload.get("solution"),
        notes=payload.get("notes"),
    )
    return puzzle


def board_to_json(board: Board) -> str:
    return json.dumps(board_to_lists(board))


def render_board_to_image(
    puzzle: NonogramPuzzle,
    board: Optional[Board] = None,
    cell_size: int = 28,
    clue_padding: int = 6,
    font_path: Optional[str] = None,
) -> Image.Image:
    """Render the puzzle (and optional player board) to a Pillow image."""

    board = board or (
        puzzle.solution if puzzle.solution is not None else puzzle.empty_board()
    )
    height = puzzle.height
    width = puzzle.width

    max_row_clues = max(len(clue) for clue in puzzle.row_clues)
    max_col_clues = max(len(clue) for clue in puzzle.column_clues)

    img_width = (width + max_row_clues) * cell_size + clue_padding * 2
    img_height = (height + max_col_clues) * cell_size + clue_padding * 2

    image = Image.new("RGB", (img_width, img_height), color="white")
    draw = ImageDraw.Draw(image)

    if font_path:
        try:
            font = ImageFont.truetype(font_path, int(cell_size * 0.55))
        except OSError:
            font = ImageFont.load_default()
    else:
        font = ImageFont.load_default()

    origin_x = clue_padding + max_row_clues * cell_size
    origin_y = clue_padding + max_col_clues * cell_size

    def measure(text: str) -> Tuple[int, int]:
        bbox = draw.textbbox((0, 0), text, font=font)
        return bbox[2] - bbox[0], bbox[3] - bbox[1]

    # Draw clues
    for r, clues in enumerate(puzzle.row_clues):
        x = origin_x - clue_padding
        y = origin_y + r * cell_size + cell_size // 2
        display = clues or [0]
        for clue in reversed(display):
            text = str(clue)
            text_width, text_height = measure(text)
            draw.text(
                (x - text_width, y - text_height // 2), text, fill="black", font=font
            )
            x -= cell_size

    for c, clues in enumerate(puzzle.column_clues):
        x = origin_x + c * cell_size + cell_size // 2
        y = origin_y - clue_padding
        display = clues or [0]
        for clue in reversed(display):
            text = str(clue)
            text_width, text_height = measure(text)
            draw.text(
                (x - text_width // 2, y - text_height), text, fill="black", font=font
            )
            y -= cell_size

    # Draw grid
    for r in range(height):
        for c in range(width):
            x0 = origin_x + c * cell_size
            y0 = origin_y + r * cell_size
            x1 = x0 + cell_size
            y1 = y0 + cell_size
            draw.rectangle([x0, y0, x1, y1], outline="black", width=1)
            value = board[r][c]
            if value == FILLED:
                draw.rectangle([x0 + 2, y0 + 2, x1 - 2, y1 - 2], fill="black")
            elif value == EMPTY:
                draw.line([x0 + 2, y0 + 2, x1 - 2, y1 - 2], fill="gray", width=2)
                draw.line([x0 + 2, y1 - 2, x1 - 2, y0 + 2], fill="gray", width=2)

    return image


def save_image(
    puzzle: NonogramPuzzle,
    destination: str | Path,
    board: Optional[Board] = None,
    **kwargs,
) -> Path:
    image = render_board_to_image(puzzle, board=board, **kwargs)
    path = Path(destination)
    image.save(path)
    return path
