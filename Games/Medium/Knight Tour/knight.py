import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import sys
import argparse
from typing import Tuple, List, Optional

class KnightTour:
    """
    Solves and animates the Knight's Tour problem on a chessboard of size n x n
    using a backtracking algorithm with Warnsdorff's rule for optimization.
    """
    def __init__(self, board_size: int):
        if board_size < 5:
            print(f"Warning: A Knight's Tour on a {board_size}x{board_size} board is not always possible.", file=sys.stderr)
        self.n = board_size
        self.board = np.full((self.n, self.n), -1, dtype=int)
        self.moves_x = [2, 1, -1, -2, -2, -1, 1, 2]
        self.moves_y = [1, 2, 2, 1, -1, -2, -2, -1]

    def is_valid_move(self, x: int, y: int) -> bool:
        """Checks if a move to (x, y) is within the board and unvisited."""
        return 0 <= x < self.n and 0 <= y < self.n and self.board[x][y] == -1

    def get_degree(self, x: int, y: int) -> int:
        """Calculates the number of valid onward moves from a given square (Warnsdorff's rule)."""
        count = 0
        for i in range(8):
            if self.is_valid_move(x + self.moves_x[i], y + self.moves_y[i]):
                count += 1
        return count

    def solve_tour(self) -> bool:
        """
        Attempts to find a Knight's Tour solution starting from (0,0).
        Returns True if a solution is found, False otherwise.
        """
        self.board[0][0] = 0
        return self._solve_util(0, 0, 1)

    def _solve_util(self, x: int, y: int, move_count: int) -> bool:
        """Recursive utility function to solve the Knight's Tour problem."""
        if move_count == self.n * self.n:
            return True

        # Get all valid next moves and sort them by their degree (Warnsdorff's Rule)
        neighbors = []
        for i in range(8):
            next_x, next_y = x + self.moves_x[i], y + self.moves_y[i]
            if self.is_valid_move(next_x, next_y):
                degree = self.get_degree(next_x, next_y)
                neighbors.append(((next_x, next_y), degree))

        neighbors.sort(key=lambda item: item[1])

        for move, _ in neighbors:
            next_x, next_y = move
            self.board[next_x][next_y] = move_count
            if self._solve_util(next_x, next_y, move_count + 1):
                return True
            # Backtrack
            self.board[next_x][next_y] = -1
        
        return False

    def animate_tour(self):
        """Creates and displays an animation of the solved tour."""
        fig, ax = plt.subplots(figsize=(8, 8))

        # Create a chessboard pattern for the background
        chessboard = np.zeros((self.n, self.n))
        for i in range(self.n):
            for j in range(self.n):
                if (i + j) % 2 == 0:
                    chessboard[i][j] = 1

        def update(frame):
            ax.clear()
            ax.imshow(chessboard, cmap='binary', origin='lower', alpha=0.5)
            ax.set_xticks(np.arange(-0.5, self.n, 1), minor=True)
            ax.set_yticks(np.arange(-0.5, self.n, 1), minor=True)
            ax.grid(which="minor", color="black", linestyle='-', linewidth=2)
            ax.tick_params(which="minor", size=0)
            ax.set_xticklabels([])
            ax.set_yticklabels([])

            path_x, path_y = [], []
            for i in range(self.n):
                for j in range(self.n):
                    if 0 <= self.board[i][j] <= frame:
                        path_x.append(j)
                        path_y.append(i)
                        ax.text(j, i, str(self.board[i][j]), color='red', ha='center', va='center', fontsize=12, weight='bold')

            # Sort points by move number to draw the path correctly
            sorted_points = sorted(zip(path_x, path_y), key=lambda p: self.board[p[1]][p[0]])
            if len(sorted_points) > 1:
                path_x_sorted, path_y_sorted = zip(*sorted_points)
                ax.plot(path_x_sorted, path_y_sorted, color='blue', linewidth=2)

            ax.set_title(f"Knight's Tour - Move {frame+1}/{self.n*self.n}", fontsize=16)

        anim = FuncAnimation(fig, update, frames=np.arange(0, self.n * self.n), interval=200, repeat=False)
        plt.show()

def main():
    """Main function to parse arguments and run the Knight's Tour solver."""
    parser = argparse.ArgumentParser(description="Solve and animate the Knight's Tour problem.")
    parser.add_argument("size", type=int, nargs='?', default=8, help="The size of the chessboard (e.g., 8 for an 8x8 board). Default is 8.")
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
