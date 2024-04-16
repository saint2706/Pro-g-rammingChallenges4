import pygame
import numpy as np
import time

# Constants
WIDTH, HEIGHT = 800, 600
CELL_SIZE = 10
ROWS, COLS = HEIGHT // CELL_SIZE, WIDTH // CELL_SIZE
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
BLUE = (0, 0, 128)

# Initialize pygame
pygame.init()
screen = pygame.display.set_mode((WIDTH, HEIGHT))
pygame.display.set_caption("Conway's Game of Life")


# Function to create the initial grid randomly
def create_grid():
    return np.random.choice([0, 1], size=(ROWS, COLS), p=[0.5, 0.5])


# Function to update the grid based on the rules of Conway's Game of Life
def update_grid(grid):
    new_grid = np.zeros_like(grid)

    for i in range(ROWS):
        for j in range(COLS):
            neighbors = np.sum(grid[i - 1 : i + 2, j - 1 : j + 2]) - grid[i, j]
            if grid[i, j] == 1 and (neighbors < 2 or neighbors > 3):
                new_grid[i, j] = 0
            elif grid[i, j] == 0 and neighbors == 3:
                new_grid[i, j] = 1
            else:
                new_grid[i, j] = grid[i, j]

    return new_grid


# Function to draw the grid on the screen
def draw_grid(grid):
    screen.fill(WHITE)

    for i in range(ROWS):
        for j in range(COLS):
            color = BLACK if grid[i, j] == 1 else BLUE
            pygame.draw.rect(
                screen, color, (j * CELL_SIZE, i * CELL_SIZE, CELL_SIZE, CELL_SIZE)
            )

    pygame.display.flip()


# Main function to run the game
def run_game():
    grid = create_grid()

    while True:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                quit()

        draw_grid(grid)
        grid = update_grid(grid)
        time.sleep(0.1)


# Run the game
run_game()
