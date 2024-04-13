import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation

def is_valid_move(board, x, y, move_x, move_y, n):
    """
    Check if the move is valid (within the board and not visited)
    """
    return 0 <= move_x < n and 0 <= move_y < n and board[move_x][move_y] == -1

def knight_tour_animation(n):
    """
    Find a Knight's Tour on an n x n chessboard and animate it
    """
    # Initialize the chessboard
    board = np.full((n, n), -1)
    
    # Possible moves for a knight
    moves_x = [2, 1, -1, -2, -2, -1, 1, 2]
    moves_y = [1, 2, 2, 1, -1, -2, -2, -1]
    
    # Knight's initial position
    board[0][0] = 0
    
    # Number of moves made
    move_count = 1
    
    # Recursive function to find a Knight's Tour
    def tour_util(x, y, move_count):
        if move_count == n * n:
            return True
        
        # Try all next moves from the current coordinate
        for i in range(8):
            next_x = x + moves_x[i]
            next_y = y + moves_y[i]
            if is_valid_move(board, x, y, next_x, next_y, n):
                board[next_x][next_y] = move_count
                if tour_util(next_x, next_y, move_count + 1):
                    return True
                # Backtrack
                board[next_x][next_y] = -1
        
        return False
    
    # Start from position (0, 0)
    tour_util(0, 0, move_count)
    
    # Create a chessboard pattern
    chessboard = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            if (i + j) % 2 == 0:
                chessboard[i][j] = 1
    
    # Define the update function for the animation
    def update(frame):
        ax.clear()
        ax.imshow(chessboard, cmap='binary', origin='lower')
        for i in range(n):
            for j in range(n):
                if board[i][j] <= frame:
                    ax.text(j, i, str(board[i][j]), color='red', ha='center', va='center')
        ax.set_title("Knight's Tour - Move {}".format(frame))
        ax.set_xlabel("Column")
        ax.set_ylabel("Row")
    
    # Create the animation
    fig, ax = plt.subplots()
    anim = FuncAnimation(fig, update, frames=np.arange(0, n*n), interval=500, repeat=False)
    plt.show()

# Example usage: Animate a Knight's Tour on a 5x5 chessboard
knight_tour_animation(5)
