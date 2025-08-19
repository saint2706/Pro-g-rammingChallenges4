import pygame
import numpy as np
from scipy.signal import convolve2d
import sys

# --- Constants ---
WIDTH, HEIGHT = 800, 600
CELL_SIZE = 10
ROWS, COLS = HEIGHT // CELL_SIZE, WIDTH // CELL_SIZE

# --- Colors ---
BG_COLOR = (20, 20, 40)  # A darker blue for the background
CELL_COLOR = (255, 255, 0) # Bright yellow for live cells
GRID_COLOR = (40, 40, 60) # A color for the grid lines

# --- Pygame Setup ---
pygame.init()
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Conway's Game of Life (Optimized)")
clock = pygame.time.Clock()


def create_grid():
    """Creates a random initial grid."""
    # Start with a lower probability of live cells for more interesting patterns
    return np.random.choice([0, 1], size=(ROWS, COLS), p=[0.8, 0.2])


def update_grid(grid):
    """
    Updates the grid based on Conway's rules using convolution for performance.
    """
    # The kernel for convolution defines the neighborhood.
    kernel = np.array([[1, 1, 1],
                       [1, 0, 1],
                       [1, 1, 1]])

    # Use convolution to efficiently count neighbors for each cell.
    # 'same' mode ensures the output is the same size as the input.
    # 'wrap' handles the boundaries by wrapping around (toroidal array).
    neighbors = convolve2d(grid, kernel, mode='same', boundary='wrap')

    # Apply Conway's rules using fast NumPy boolean logic:
    # 1. A cell is born if it's dead (0) and has exactly 3 neighbors.
    born = (grid == 0) & (neighbors == 3)
    # 2. A cell survives if it's alive (1) and has 2 or 3 neighbors.
    survives = (grid == 1) & ((neighbors == 2) | (neighbors == 3))

    # The new grid contains only the cells that were born or survived.
    new_grid = np.zeros_like(grid)
    new_grid[born | survives] = 1

    return new_grid


def draw_grid(grid):
    """
    Draws the grid efficiently. It fills the background, then only draws rectangles
    for the live cells. This is much faster than drawing every cell.
    """
    screen.fill(BG_COLOR)

    # Get the coordinates of all live cells.
    live_cells = np.argwhere(grid == 1)

    # Draw a rectangle for each live cell.
    for row, col in live_cells:
        pygame.draw.rect(
            screen,
            CELL_COLOR,
            (col * CELL_SIZE, row * CELL_SIZE, CELL_SIZE, CELL_SIZE)
        )

    pygame.display.flip()


def main():
    """Main game loop."""
    grid = create_grid()
    running = True
    paused = False

    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_r:
                    # Reset the grid with a new random configuration.
                    grid = create_grid()
                if event.key == pygame.K_SPACE:
                    # Pause or unpause the simulation.
                    paused = not paused
                if event.key == pygame.K_q or event.key == pygame.K_ESCAPE:
                    running = False

        screen.fill(BG_COLOR)

        if not paused:
            grid = update_grid(grid)

        draw_grid(grid)

        # Set the window title to show status
        status = "Paused" if paused else "Running"
        pygame.display.set_caption(f"Conway's Game of Life (Optimized) - {status}")

        # Limit the frame rate to 10 FPS
        clock.tick(10)

    pygame.quit()
    sys.exit()


if __name__ == "__main__":
    main()
