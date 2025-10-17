"""Pygame interface for the chess game."""

from __future__ import annotations

import sys
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple, Union

import pygame

from .ai import choose_move
from .board import WHITE, Board
from .move import Move

SQUARE_SIZE = 80
BOARD_SIZE = SQUARE_SIZE * 8
MARGIN = 20
WINDOW_SIZE = BOARD_SIZE + 2 * MARGIN

LIGHT = (240, 217, 181)
DARK = (181, 136, 99)
HIGHLIGHT = (246, 246, 105)
SELECTED = (186, 202, 68)
TEXT_COLOR = (20, 20, 20)
FONT_NAME = "freesansbold.ttf"

PIECE_TEXT = {
    "P": "♙",
    "N": "♘",
    "B": "♗",
    "R": "♖",
    "Q": "♕",
    "K": "♔",
    "p": "♟",
    "n": "♞",
    "b": "♝",
    "r": "♜",
    "q": "♛",
    "k": "♚",
}


@dataclass
class GameState:
    board: Board
    selected: Optional[Tuple[int, int]] = None
    legal_moves: Dict[Tuple[int, int], List[Move]] | None = None
    history: Tuple[Move, ...] = ()


def draw_board(
    screen: pygame.Surface, font: pygame.font.Font, state: GameState
) -> None:
    screen.fill((30, 30, 30))
    board = state.board
    for row in range(8):
        for col in range(8):
            color = LIGHT if (row + col) % 2 == 0 else DARK
            rect = pygame.Rect(
                MARGIN + col * SQUARE_SIZE,
                MARGIN + row * SQUARE_SIZE,
                SQUARE_SIZE,
                SQUARE_SIZE,
            )
            if state.selected == (row, col):
                pygame.draw.rect(screen, SELECTED, rect)
            elif state.legal_moves and (row, col) in state.legal_moves:
                pygame.draw.rect(screen, HIGHLIGHT, rect)
            else:
                pygame.draw.rect(screen, color, rect)
            piece = board.board[row][col]
            if piece:
                text = font.render(PIECE_TEXT[piece], True, TEXT_COLOR)
                text_rect = text.get_rect(center=rect.center)
                screen.blit(text, text_rect)

    info_font = pygame.font.Font(FONT_NAME, 20)
    turn_text = "White to move" if board.turn == WHITE else "Black to move"
    surface = info_font.render(turn_text, True, (230, 230, 230))
    screen.blit(surface, (MARGIN, WINDOW_SIZE - MARGIN + 4))


def handle_click(
    state: GameState, pos: Tuple[int, int]
) -> Union[None, Move, List[Move]]:
    board = state.board
    col = (pos[0] - MARGIN) // SQUARE_SIZE
    row = (pos[1] - MARGIN) // SQUARE_SIZE
    if not (0 <= row < 8 and 0 <= col < 8):
        state.selected = None
        state.legal_moves = None
        return None

    square = (row, col)
    if state.selected is None:
        piece = board.piece_at(square)
        if piece and board.color_of(piece) == board.turn:
            moves: Dict[Tuple[int, int], List[Move]] = {}
            for move in board.legal_moves():
                if move.start == square:
                    moves.setdefault(move.end, []).append(move)
            if moves:
                state.selected = square
                state.legal_moves = moves
        return None

    if state.legal_moves and square in state.legal_moves:
        options = state.legal_moves[square]
        state.selected = None
        state.legal_moves = None
        if len(options) == 1:
            return options[0]
        return options

    state.selected = None
    state.legal_moves = None
    return None


def ai_move(state: GameState, depth: int) -> Optional[Move]:
    try:
        return choose_move(state.board, depth)
    except ValueError:
        return None


def prompt_promotion(
    screen: pygame.Surface,
    font: pygame.font.Font,
    state: GameState,
    options: List[Move],
) -> Optional[Move]:
    option_font = pygame.font.Font(FONT_NAME, 28)
    prompt_font = pygame.font.Font(FONT_NAME, 24)
    mapping = {move.promotion or "Q": move for move in options}
    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return None
            if event.type == pygame.KEYDOWN:
                key = event.unicode.upper()
                if key in mapping:
                    return mapping[key]
            if event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                x, y = event.pos
                buttons = list(mapping.items())
                for index, (promo, move) in enumerate(buttons):
                    rect = pygame.Rect(
                        MARGIN + index * 120,
                        WINDOW_SIZE // 2 - 40,
                        100,
                        80,
                    )
                    if rect.collidepoint(x, y):
                        return move

        draw_board(screen, font, state)
        overlay = pygame.Surface((WINDOW_SIZE, WINDOW_SIZE), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 160))
        screen.blit(overlay, (0, 0))
        title = prompt_font.render(
            "Choose promotion (click or press key)", True, (255, 255, 255)
        )
        screen.blit(title, (MARGIN, WINDOW_SIZE // 2 - 80))
        for index, (promo, move) in enumerate(mapping.items()):
            rect = pygame.Rect(MARGIN + index * 120, WINDOW_SIZE // 2 - 40, 100, 80)
            pygame.draw.rect(screen, (220, 220, 220), rect)
            label = option_font.render(promo, True, (20, 20, 20))
            label_rect = label.get_rect(center=rect.center)
            screen.blit(label, label_rect)
        pygame.display.flip()


def run_pygame(ai: Optional[str] = None, depth: int = 2) -> None:
    pygame.init()
    screen = pygame.display.set_mode((WINDOW_SIZE, WINDOW_SIZE + 40))
    pygame.display.set_caption("Chess")
    font = pygame.font.Font(FONT_NAME, 48)

    state = GameState(board=Board.starting_position())
    clock = pygame.time.Clock()

    running = True
    while running:
        clock.tick(30)
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:
                if ai and state.board.turn in ai:
                    continue
                move = handle_click(state, event.pos)
                if isinstance(move, Move):
                    state.board = state.board.make_move(move)
                elif isinstance(move, list) and move:
                    choice = prompt_promotion(screen, font, state, move)
                    if choice:
                        state.board = state.board.make_move(choice)
        if ai and state.board.turn in ai:
            move = ai_move(state, depth)
            if move:
                state.board = state.board.make_move(move)

        draw_board(screen, font, state)
        pygame.display.flip()

    pygame.quit()


if __name__ == "__main__":  # pragma: no cover
    depth = 2
    ai = None
    if len(sys.argv) > 1:
        ai = sys.argv[1]
    run_pygame(ai=ai, depth=depth)
