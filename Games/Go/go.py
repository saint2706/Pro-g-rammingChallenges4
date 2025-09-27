"""PySide6 user interface for the Go challenge."""
from __future__ import annotations

import sys
from pathlib import Path
from typing import Optional

from PySide6.QtCore import QPointF, Qt
from PySide6.QtGui import QColor, QPainter, QPen, QBrush
from PySide6.QtWidgets import (
    QApplication,
    QFileDialog,
    QHBoxLayout,
    QLabel,
    QMainWindow,
    QMessageBox,
    QPushButton,
    QCheckBox,
    QComboBox,
    QVBoxLayout,
    QWidget,
)

from .logic import GoGame, IllegalMove


class GoBoardWidget(QWidget):
    """Canvas responsible for drawing the board and handling clicks."""

    def __init__(self, game: GoGame, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)
        self._game = game
        self._margin = 32
        self._last_hover: Optional[tuple[int, int]] = None
        self._move_handler: Optional[callable] = None
        self.setMinimumSize(600, 600)

    def set_game(self, game: GoGame) -> None:
        self._game = game
        self.update()

    def set_move_handler(self, handler) -> None:
        self._move_handler = handler

    # Painting ---------------------------------------------------------
    def paintEvent(self, event):  # type: ignore[override]
        painter = QPainter(self)
        painter.setRenderHint(QPainter.Antialiasing)

        board_size = min(self.width(), self.height()) - 2 * self._margin
        if board_size <= 0:
            return
        cell = board_size / (self._game.size - 1)
        left = (self.width() - board_size) / 2
        top = (self.height() - board_size) / 2

        # board background
        painter.setBrush(QBrush(QColor(210, 180, 140)))
        painter.setPen(Qt.NoPen)
        painter.drawRect(left - cell / 2, top - cell / 2, board_size + cell, board_size + cell)

        # grid lines
        pen = QPen(Qt.black)
        pen.setWidth(2)
        painter.setPen(pen)
        for i in range(self._game.size):
            offset = i * cell
            painter.drawLine(left, top + offset, left + board_size, top + offset)
            painter.drawLine(left + offset, top, left + offset, top + board_size)

        # star points (hoshi)
        painter.setBrush(Qt.black)
        hoshi = self._hoshi_points(self._game.size)
        radius = max(2, cell * 0.09)
        for r, c in hoshi:
            cx = left + c * cell
            cy = top + r * cell
            painter.drawEllipse(QPointF(cx, cy), radius, radius)

        # stones
        for r in range(self._game.size):
            for c in range(self._game.size):
                stone = self._game.board[r][c]
                if stone is None:
                    continue
                center_x = left + c * cell
                center_y = top + r * cell
                radius = cell * 0.45
                painter.setPen(Qt.black)
                color = QColor(20, 20, 20) if stone == "B" else QColor(240, 240, 240)
                painter.setBrush(QBrush(color))
                painter.drawEllipse(QPointF(center_x, center_y), radius, radius)

        # last move indicator
        if self._game.moves:
            last = self._game.moves[-1]
            if last.point:
                lr, lc = last.point
                center_x = left + lc * cell
                center_y = top + lr * cell
                indicator_color = QColor(255, 255, 0, 180)
                painter.setBrush(indicator_color)
                painter.setPen(Qt.NoPen)
                painter.drawEllipse(QPointF(center_x, center_y), cell * 0.15, cell * 0.15)

    def _hoshi_points(self, size: int) -> list[tuple[int, int]]:
        if size == 9:
            coords = [2, 4, 6]
        elif size == 13:
            coords = [3, 6, 9]
        else:  # 19
            coords = [3, 9, 15]
        points = [(r, c) for r in coords for c in coords]
        if size == 9:
            return points
        if size == 13:
            return points
        return points

    # Interaction ------------------------------------------------------
    def mouseReleaseEvent(self, event):  # type: ignore[override]
        if event.button() != Qt.LeftButton or self._move_handler is None:
            return
        board_size = min(self.width(), self.height()) - 2 * self._margin
        if board_size <= 0:
            return
        cell = board_size / (self._game.size - 1)
        left = (self.width() - board_size) / 2
        top = (self.height() - board_size) / 2
        pos = event.position()
        col = round((pos.x() - left) / cell)
        row = round((pos.y() - top) / cell)
        if 0 <= row < self._game.size and 0 <= col < self._game.size:
            self._move_handler(row, col)


class GoMainWindow(QMainWindow):
    def __init__(self) -> None:
        super().__init__()
        self.setWindowTitle("Go Challenge")
        self.game = GoGame(19)

        self.board_widget = GoBoardWidget(self.game)
        self.board_widget.set_move_handler(self.handle_move)

        self.size_selector = QComboBox()
        for size in (9, 13, 19):
            self.size_selector.addItem(f"{size} x {size}", size)
        self.size_selector.setCurrentIndex(2)

        self.ai_checkbox = QCheckBox("Enable AI for White")

        self.status_label = QLabel(self._status_text())

        new_game_btn = QPushButton("New Game")
        new_game_btn.clicked.connect(self.start_new_game)

        undo_btn = QPushButton("Undo")
        undo_btn.clicked.connect(self.handle_undo)

        pass_btn = QPushButton("Pass")
        pass_btn.clicked.connect(self.handle_pass)

        score_btn = QPushButton("Score")
        score_btn.clicked.connect(self.show_score)

        save_btn = QPushButton("Save SGF")
        save_btn.clicked.connect(self.save_sgf)

        controls = QHBoxLayout()
        controls.addWidget(self.size_selector)
        controls.addWidget(new_game_btn)
        controls.addWidget(undo_btn)
        controls.addWidget(pass_btn)
        controls.addWidget(score_btn)
        controls.addWidget(save_btn)
        controls.addWidget(self.ai_checkbox)
        controls.addStretch(1)

        layout = QVBoxLayout()
        layout.addWidget(self.board_widget, stretch=1)
        layout.addLayout(controls)
        layout.addWidget(self.status_label)

        container = QWidget()
        container.setLayout(layout)
        self.setCentralWidget(container)
        self.resize(800, 900)

    # Actions ----------------------------------------------------------
    def start_new_game(self) -> None:
        size = self.size_selector.currentData()
        self.game = GoGame(size)
        self.board_widget.set_game(self.game)
        self.board_widget.set_move_handler(self.handle_move)
        self._update_status("New game started.")

    def handle_move(self, row: int, col: int) -> None:
        try:
            self.game.play(row, col)
        except IllegalMove as exc:
            QMessageBox.warning(self, "Illegal move", str(exc))
            return
        self.board_widget.update()
        self._update_status(f"{self.game.moves[-1].color} played at ({row + 1}, {col + 1}).")
        self._maybe_run_ai()

    def handle_pass(self) -> None:
        self.game.pass_turn()
        self.board_widget.update()
        self._update_status("Player passed.")
        self._maybe_run_ai()

    def handle_undo(self) -> None:
        if not self.game.undo():
            QMessageBox.information(self, "Undo", "Nothing to undo yet.")
            return
        self.board_widget.update()
        self._update_status("Move undone.")

    def show_score(self) -> None:
        scores = self.game.score()
        territory = scores["territory"]
        area = scores["area"]
        message = (
            f"Territory\n  Black: {territory['B']}\n  White: {territory['W']}\n\n"
            f"Area\n  Black: {area['B']}\n  White: {area['W']}"
        )
        QMessageBox.information(self, "Score", message)

    def save_sgf(self) -> None:
        filename, _ = QFileDialog.getSaveFileName(self, "Save SGF", str(Path.home() / "game.sgf"), "Smart Game Format (*.sgf)")
        if not filename:
            return
        try:
            Path(filename).write_text(self.game.to_sgf(), encoding="utf-8")
        except OSError as exc:
            QMessageBox.critical(self, "Save failed", str(exc))
            return
        self._update_status(f"Saved to {filename}.")

    def _maybe_run_ai(self) -> None:
        if not self.ai_checkbox.isChecked():
            self._update_status()
            return
        if self.game.to_move != "W":
            self._update_status()
            return
        move = self.game.random_legal_move("W")
        if move is None:
            self.game.pass_turn()
            self._update_status("AI passed.")
            self.board_widget.update()
            return
        try:
            self.game.play(*move)
        except IllegalMove:
            return
        self.board_widget.update()
        self._update_status(f"AI played at ({move[0] + 1}, {move[1] + 1}).")

    def _status_text(self) -> str:
        return (
            f"Turn: {'Black' if self.game.to_move == 'B' else 'White'} | "
            f"Captures - Black: {self.game.captures['B']} White: {self.game.captures['W']}"
        )

    def _update_status(self, prefix: Optional[str] = None) -> None:
        text = self._status_text()
        if prefix:
            text = f"{prefix}  {text}"
        self.status_label.setText(text)


def main() -> None:
    app = QApplication(sys.argv)
    window = GoMainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()
