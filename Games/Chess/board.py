"""Core board model and move generation for the chess game."""

from __future__ import annotations

from dataclasses import dataclass, replace
from typing import Dict, Iterable, List, Optional, Tuple

from .move import Move, Square

WHITE = "w"
BLACK = "b"
FILES = "abcdefgh"
RANKS = "12345678"

PIECE_VALUES = {
    "P": 100,
    "N": 320,
    "B": 330,
    "R": 500,
    "Q": 900,
    "K": 20000,
}


@dataclass(frozen=True)
class Board:
    """Immutable chess board state."""

    board: Tuple[Tuple[Optional[str], ...], ...]
    turn: str = WHITE
    castling: str = "KQkq"
    en_passant: Optional[Square] = None
    halfmove_clock: int = 0
    fullmove_number: int = 1

    @staticmethod
    def starting_position() -> "Board":
        """Return the standard chess initial setup."""

        layout = (
            tuple("rnbqkbnr"),
            tuple("pppppppp"),
            tuple([None] * 8),
            tuple([None] * 8),
            tuple([None] * 8),
            tuple([None] * 8),
            tuple("PPPPPPPP"),
            tuple("RNBQKBNR"),
        )
        return Board(layout)

    def piece_at(self, square: Square) -> Optional[str]:
        return self.board[square[0]][square[1]]

    def set_piece(self, square: Square, piece: Optional[str]) -> "Board":
        rows = [list(row) for row in self.board]
        rows[square[0]][square[1]] = piece
        return replace(self, board=tuple(tuple(row) for row in rows))

    def switch_turn(self) -> "Board":
        return replace(self, turn=WHITE if self.turn == BLACK else BLACK)

    # Utility conversions -------------------------------------------------
    @staticmethod
    def square_name(square: Square) -> str:
        return FILES[square[1]] + str(8 - square[0])

    @staticmethod
    def parse_square(name: str) -> Square:
        file = FILES.index(name[0])
        rank = 8 - int(name[1])
        return rank, file

    # Iterators -----------------------------------------------------------
    def pieces(self, color: Optional[str] = None) -> Iterable[Tuple[Square, str]]:
        for r in range(8):
            for c in range(8):
                piece = self.board[r][c]
                if piece is None:
                    continue
                if color and self.color_of(piece) != color:
                    continue
                yield (r, c), piece

    @staticmethod
    def color_of(piece: str) -> str:
        return WHITE if piece.isupper() else BLACK

    # Move generation -----------------------------------------------------
    def legal_moves(self) -> List[Move]:
        moves: List[Move] = []
        for move in self.pseudo_legal_moves(self.turn):
            if not self._leaves_king_in_check(move):
                moves.append(move)
        return moves

    def pseudo_legal_moves(self, color: str) -> Iterable[Move]:
        for square, piece in self.pieces(color):
            piece_type = piece.upper()
            if piece_type == "P":
                yield from self._pawn_moves(square, color)
            elif piece_type == "N":
                yield from self._knight_moves(square, color)
            elif piece_type == "B":
                yield from self._slider_moves(
                    square, color, [(-1, -1), (-1, 1), (1, -1), (1, 1)]
                )
            elif piece_type == "R":
                yield from self._slider_moves(
                    square, color, [(-1, 0), (1, 0), (0, -1), (0, 1)]
                )
            elif piece_type == "Q":
                yield from self._slider_moves(
                    square,
                    color,
                    [
                        (-1, -1),
                        (-1, 1),
                        (1, -1),
                        (1, 1),
                        (-1, 0),
                        (1, 0),
                        (0, -1),
                        (0, 1),
                    ],
                )
            elif piece_type == "K":
                yield from self._king_moves(square, color)

    def _pawn_moves(self, square: Square, color: str) -> Iterable[Move]:
        direction = -1 if color == WHITE else 1
        start_row = 6 if color == WHITE else 1
        promotion_row = 0 if color == WHITE else 7
        r, c = square

        one_forward = (r + direction, c)
        if self._on_board(one_forward) and self.piece_at(one_forward) is None:
            if one_forward[0] == promotion_row:
                for promo in "QRBN":
                    yield Move(square, one_forward, promotion=promo)
            else:
                yield Move(square, one_forward)
            two_forward = (r + 2 * direction, c)
            if r == start_row and self.piece_at(two_forward) is None:
                yield Move(square, two_forward)

        for dc in (-1, 1):
            target = (r + direction, c + dc)
            if not self._on_board(target):
                continue
            captured = self.piece_at(target)
            if captured and self.color_of(captured) != color:
                if target[0] == promotion_row:
                    for promo in "QRBN":
                        yield Move(square, target, promotion=promo)
                else:
                    yield Move(square, target)
            elif self.en_passant and target == self.en_passant:
                yield Move(square, target, is_en_passant=True)

    def _knight_moves(self, square: Square, color: str) -> Iterable[Move]:
        r, c = square
        for dr, dc in [
            (-2, -1),
            (-2, 1),
            (-1, -2),
            (-1, 2),
            (1, -2),
            (1, 2),
            (2, -1),
            (2, 1),
        ]:
            target = (r + dr, c + dc)
            if not self._on_board(target):
                continue
            piece = self.piece_at(target)
            if piece is None or self.color_of(piece) != color:
                yield Move(square, target)

    def _slider_moves(
        self, square: Square, color: str, directions: Iterable[Tuple[int, int]]
    ) -> Iterable[Move]:
        r, c = square
        for dr, dc in directions:
            nr, nc = r + dr, c + dc
            while self._on_board((nr, nc)):
                piece = self.piece_at((nr, nc))
                if piece is None:
                    yield Move(square, (nr, nc))
                else:
                    if self.color_of(piece) != color:
                        yield Move(square, (nr, nc))
                    break
                nr += dr
                nc += dc

    def _king_moves(self, square: Square, color: str) -> Iterable[Move]:
        r, c = square
        for dr in (-1, 0, 1):
            for dc in (-1, 0, 1):
                if dr == 0 and dc == 0:
                    continue
                target = (r + dr, c + dc)
                if not self._on_board(target):
                    continue
                piece = self.piece_at(target)
                if piece is None or self.color_of(piece) != color:
                    yield Move(square, target)

        if color == self.turn:
            yield from self._castling_moves(square, color)

    def _castling_moves(self, square: Square, color: str) -> Iterable[Move]:
        if color == WHITE:
            rights = self.castling
            back_rank = 7
            king_side = "K"
            queen_side = "Q"
        else:
            rights = self.castling
            back_rank = 0
            king_side = "k"
            queen_side = "q"

        if king_side in rights:
            if self._can_castle_through(back_rank, 4, 6, color):
                yield Move(square, (back_rank, 6), is_castling=True)
        if queen_side in rights:
            if self._can_castle_through(back_rank, 4, 2, color, queen_side=True):
                yield Move(square, (back_rank, 2), is_castling=True)

    def _can_castle_through(
        self,
        rank: int,
        king_start: int,
        king_end: int,
        color: str,
        queen_side: bool = False,
    ) -> bool:
        step = 1 if king_end > king_start else -1
        rook_col = 0 if queen_side else 7
        rook_square = (rank, rook_col)
        rook = self.piece_at(rook_square)
        if rook is None or rook.upper() != "R" or self.color_of(rook) != color:
            return False

        for col in range(min(king_start, rook_col) + 1, max(king_start, rook_col)):
            if self.piece_at((rank, col)) is not None:
                return False

        for col in range(king_start, king_end + step, step):
            if self.square_attacked((rank, col), WHITE if color == BLACK else BLACK):
                return False
        return True

    def _on_board(self, square: Square) -> bool:
        return 0 <= square[0] < 8 and 0 <= square[1] < 8

    def _leaves_king_in_check(self, move: Move) -> bool:
        new_board = self.make_move(move)
        return new_board.is_in_check(self.turn)

    def square_attacked(self, square: Square, by_color: str) -> bool:
        for move in self.pseudo_legal_moves(by_color):
            if move.end == square:
                return True
        return False

    def king_square(self, color: str) -> Square:
        for square, piece in self.pieces(color):
            if piece.upper() == "K":
                return square
        raise ValueError("King missing from board")

    def is_in_check(self, color: str) -> bool:
        king_sq = self.king_square(color)
        opponent = WHITE if color == BLACK else BLACK
        return self.square_attacked(king_sq, opponent)

    # Move application ----------------------------------------------------
    def make_move(self, move: Move) -> "Board":
        start_piece = self.piece_at(move.start)
        if start_piece is None:
            raise ValueError("No piece on the starting square")
        color = self.color_of(start_piece)
        target_piece = self.piece_at(move.end)
        rows = [list(row) for row in self.board]

        # Handle en passant capture removal
        if move.is_en_passant:
            ep_rank = move.start[0]
            rows[ep_rank][move.end[1]] = None

        rows[move.start[0]][move.start[1]] = None

        if move.is_castling:
            if move.end[1] == 6:
                # king side
                rows[move.end[0]][5] = rows[move.end[0]][7]
                rows[move.end[0]][7] = None
            else:
                rows[move.end[0]][3] = rows[move.end[0]][0]
                rows[move.end[0]][0] = None

        # Promotion or normal move
        piece_to_place = start_piece
        if move.promotion:
            piece_to_place = (
                move.promotion if color == WHITE else move.promotion.lower()
            )
        rows[move.end[0]][move.end[1]] = piece_to_place

        new_board = replace(self, board=tuple(tuple(row) for row in rows))

        # Update castling rights
        castling = new_board.castling
        if start_piece.upper() == "K":
            castling = (
                castling.replace("K", "").replace("Q", "")
                if color == WHITE
                else castling
            )
            castling = (
                castling.replace("k", "").replace("q", "")
                if color == BLACK
                else castling
            )
        if start_piece.upper() == "R":
            if move.start == (7, 0):
                castling = castling.replace("Q", "")
            elif move.start == (7, 7):
                castling = castling.replace("K", "")
            elif move.start == (0, 0):
                castling = castling.replace("q", "")
            elif move.start == (0, 7):
                castling = castling.replace("k", "")
        if move.end == (7, 0):
            castling = castling.replace("Q", "")
        elif move.end == (7, 7):
            castling = castling.replace("K", "")
        elif move.end == (0, 0):
            castling = castling.replace("q", "")
        elif move.end == (0, 7):
            castling = castling.replace("k", "")

        en_passant: Optional[Square] = None
        if start_piece.upper() == "P" and abs(move.start[0] - move.end[0]) == 2:
            en_passant = ((move.start[0] + move.end[0]) // 2, move.start[1])

        halfmove = (
            0 if start_piece.upper() == "P" or target_piece else self.halfmove_clock + 1
        )
        fullmove = self.fullmove_number + (1 if self.turn == BLACK else 0)

        new_board = replace(
            new_board,
            castling=castling,
            en_passant=en_passant,
            halfmove_clock=halfmove,
            fullmove_number=fullmove,
            turn=WHITE if self.turn == BLACK else BLACK,
        )
        return new_board

    # Evaluation helpers --------------------------------------------------
    def material_balance(self) -> int:
        score = 0
        for _, piece in self.pieces():
            value = PIECE_VALUES[piece.upper()]
            score += value if piece.isupper() else -value
        return score

    # SAN notation --------------------------------------------------------
    def san(self, move: Move) -> str:
        """Return the algebraic notation for a legal move."""

        if move.is_castling:
            return "O-O" if move.end[1] == 6 else "O-O-O"

        piece = self.piece_at(move.start)
        if piece is None:
            raise ValueError("Cannot format SAN for move without piece")
        color = self.color_of(piece)
        board_after = self.make_move(move)
        opponent = WHITE if color == BLACK else BLACK
        capture = self.piece_at(move.end) is not None or move.is_en_passant

        disambiguation = ""
        if piece.upper() != "P":
            same_piece_moves = [
                m
                for m in self.pseudo_legal_moves(color)
                if m.end == move.end
                and m != move
                and not self._leaves_king_in_check(m)
                and self.piece_at(m.start).upper() == piece.upper()
            ]
            if same_piece_moves:
                same_files = {m.start[1] for m in same_piece_moves}
                same_ranks = {m.start[0] for m in same_piece_moves}
                file_char = FILES[move.start[1]]
                rank_char = str(8 - move.start[0])
                if move.start[1] not in same_files:
                    disambiguation = file_char
                elif move.start[0] not in same_ranks:
                    disambiguation = rank_char
                else:
                    disambiguation = file_char + rank_char

        piece_char = "" if piece.upper() == "P" else piece.upper()
        capture_indicator = "x" if capture else ""
        target_square = self.square_name(move.end)
        promotion = f"={move.promotion.upper()}" if move.promotion else ""
        check = "+" if board_after.is_in_check(opponent) else ""

        if not board_after.legal_moves() and board_after.is_in_check(opponent):
            check = "#"
        elif not board_after.legal_moves():
            check = ""

        if piece.upper() == "P" and capture:
            piece_char = FILES[move.start[1]]
        return f"{piece_char}{disambiguation}{capture_indicator}{target_square}{promotion}{check}"

    # Parsing --------------------------------------------------------------
    def parse_san(self, san: str) -> Move:
        """Parse SAN notation into a Move instance."""

        san = san.strip()
        if san in {"O-O", "0-0"}:
            king_rank = 7 if self.turn == WHITE else 0
            return Move((king_rank, 4), (king_rank, 6), is_castling=True)
        if san in {"O-O-O", "0-0-0"}:
            king_rank = 7 if self.turn == WHITE else 0
            return Move((king_rank, 4), (king_rank, 2), is_castling=True)

        san = san.replace("+", "").replace("#", "")
        promotion = None
        if "=" in san:
            san, promotion = san.split("=")
            promotion = promotion.upper()

        capture = "x" in san
        san = san.replace("x", "")
        piece_char = san[0] if san[0].isupper() and san[0] in "RNBQKP" else "P"
        if piece_char != "P" and san[0].isupper():
            san = san[1:]

        file_hint = None
        rank_hint = None
        while san and san[0] in FILES:
            if len(san) == 2:
                break
            file_hint = san[0]
            san = san[1:]
        while san and san[0] in RANKS and len(san) > 2:
            rank_hint = san[0]
            san = san[1:]

        target = self.parse_square(san[-2:])

        candidates = []
        for move in self.legal_moves():
            if move.end != target:
                continue
            start_piece = self.piece_at(move.start)
            if start_piece is None or start_piece.upper() != piece_char:
                continue
            if promotion and move.promotion != promotion:
                continue
            if file_hint and FILES[move.start[1]] != file_hint:
                continue
            if rank_hint and str(8 - move.start[0]) != rank_hint:
                continue
            if capture and self.piece_at(move.end) is None and not move.is_en_passant:
                continue
            candidates.append(move)

        if len(candidates) != 1:
            raise ValueError(f"Ambiguous SAN move: {san}")
        return candidates[0]

    # FEN helpers ----------------------------------------------------------
    def to_fen(self) -> str:
        rows = []
        for r in range(8):
            empty = 0
            fen_row = ""
            for c in range(8):
                piece = self.board[r][c]
                if piece is None:
                    empty += 1
                else:
                    if empty:
                        fen_row += str(empty)
                        empty = 0
                    fen_row += piece
            if empty:
                fen_row += str(empty)
            rows.append(fen_row)
        castling = self.castling or "-"
        ep = self.square_name(self.en_passant) if self.en_passant else "-"
        return " ".join(
            [
                "/".join(rows),
                self.turn,
                castling,
                ep,
                str(self.halfmove_clock),
                str(self.fullmove_number),
            ]
        )

    @staticmethod
    def from_fen(value: str) -> "Board":
        parts = value.strip().split()
        if len(parts) != 6:
            raise ValueError("Invalid FEN")
        rows = []
        for row in parts[0].split("/"):
            expanded: List[Optional[str]] = []
            for ch in row:
                if ch.isdigit():
                    expanded.extend([None] * int(ch))
                else:
                    expanded.append(ch)
            rows.append(tuple(expanded))
        board = tuple(rows)
        en_passant = None if parts[3] == "-" else Board.parse_square(parts[3])
        return Board(
            board,
            turn=parts[1],
            castling=parts[2] if parts[2] != "-" else "",
            en_passant=en_passant,
            halfmove_clock=int(parts[4]),
            fullmove_number=int(parts[5]),
        )
