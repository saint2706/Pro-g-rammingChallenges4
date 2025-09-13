import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import sys
import argparse

"""
Knight's Tour Solver and Animator
--------------------------------
Solves and animates the Knight's Tour problem on an n x n chessboard using a backtracking algorithm
optimized with Warnsdorff's rule. Includes a CLI for board size selection and a matplotlib animation.
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import sys
import argparse
from typing import Tuple, List, Optional


class KnightTour:
    """
    Knight's Tour solver and animator using backtracking and Warnsdorff's rule.
    Attributes:
        n (int): Board size (n x n)
        board (np.ndarray): Board state, -1 for unvisited, move number otherwise
        moves_x, moves_y (List[int]): Knight move offsets
    """

    def __init__(self, board_size: int) -> None:
        if board_size < 5:
            print(
                f"Warning: A Knight's Tour on a {board_size}x{board_size} board is not always possible.",
                file=sys.stderr,
            )
        self.n: int = board_size
        self.board: np.ndarray = np.full((self.n, self.n), -1, dtype=int)
        self.moves_x: List[int] = [2, 1, -1, -2, -2, -1, 1, 2]
        self.moves_y: List[int] = [1, 2, 2, 1, -1, -2, -2, -1]

    def is_valid_move(self, x: int, y: int) -> bool:
        """Return True if (x, y) is on the board and unvisited."""
        return 0 <= x < self.n and 0 <= y < self.n and self.board[x][y] == -1

    def get_degree(self, x: int, y: int) -> int:
        """Return the number of valid onward moves from (x, y) (Warnsdorff's rule)."""
        return sum(
            self.is_valid_move(x + dx, y + dy)
            for dx, dy in zip(self.moves_x, self.moves_y)
        )

    def solve_tour(self) -> bool:
        """Attempt to find a Knight's Tour solution starting from (0,0)."""
        self.board[0][0] = 0
        return self._solve_util(0, 0, 1)

    def _solve_util(self, x: int, y: int, move_count: int) -> bool:
        """Recursive utility for Knight's Tour. Returns True if solved from (x, y)."""
        if move_count == self.n * self.n:
            return True
        # Generate valid next moves and sort by degree (Warnsdorff's rule)
        neighbors = []
        for dx, dy in zip(self.moves_x, self.moves_y):
            nx, ny = x + dx, y + dy
            if self.is_valid_move(nx, ny):
                neighbors.append(((nx, ny), self.get_degree(nx, ny)))
        neighbors.sort(key=lambda item: item[1])
        for (nx, ny), _ in neighbors:
            self.board[nx][ny] = move_count
            if self._solve_util(nx, ny, move_count + 1):
                return True
            self.board[nx][ny] = -1  # Backtrack
        return False

    def animate_tour(self) -> None:
        """Create and display an animation of the solved Knight's Tour."""
        fig, ax = plt.subplots(figsize=(8, 8))
        # Create chessboard pattern
        chessboard = np.indices((self.n, self.n)).sum(axis=0) % 2

        def update(frame):
            ax.clear()
            ax.imshow(chessboard, cmap="binary", origin="lower", alpha=0.5)
            ax.set_xticks(np.arange(-0.5, self.n, 1), minor=True)
            ax.set_yticks(np.arange(-0.5, self.n, 1), minor=True)
            ax.grid(which="minor", color="black", linestyle="-", linewidth=2)
            ax.tick_params(which="minor", size=0)
            ax.set_xticklabels([])
            ax.set_yticklabels([])
            # Draw path and move numbers
            path = [
                (j, i, self.board[i][j])
                for i in range(self.n)
                for j in range(self.n)
                if 0 <= self.board[i][j] <= frame
            ]
            artists = []
            for j, i, num in path:
                txt = ax.text(
                    j,
                    i,
                    str(num),
                    color="red",
                    ha="center",
                    va="center",
                    fontsize=12,
                    weight="bold",
                )
                artists.append(txt)
            # Draw path lines in order
            if len(path) > 1:
                path_sorted = sorted(path, key=lambda p: p[2])
                x_sorted, y_sorted, _ = zip(*path_sorted)
                (line,) = ax.plot(x_sorted, y_sorted, color="blue", linewidth=2)
                artists.append(line)
            ax.set_title(f"Knight's Tour - Move {frame+1}/{self.n*self.n}", fontsize=16)
            # Return the list of artists modified
            return artists

        anim = FuncAnimation(
            fig,
            update,
            frames=np.arange(0, self.n * self.n),
            interval=200,
            repeat=False,
        )
        plt.show()


def main() -> None:
    """Parse CLI arguments and run the Knight's Tour solver and animation."""
    parser = argparse.ArgumentParser(
        description="Solve and animate the Knight's Tour problem."
    )
    parser.add_argument(
        "size",
        type=int,
        nargs="?",
        default=8,
        help="The size of the chessboard (e.g., 8 for an 8x8 board). Default is 8.",
    )
    args = parser.parse_args()

    print(f"--- Knight's Tour on a {args.size}x{args.size} Board ---")
    tour = KnightTour(args.size)
    print("Solving the tour using backtracking with Warnsdorff's rule...")
    if tour.solve_tour():
        print("Solution found! Starting animation...")
        tour.animate_tour()
    else:
        print("No solution found for the given board size and starting position.")


if __name__ == "__main__":
    main()
