import pygame
import numpy as np
from scipy.signal import convolve2d
import sys
from typing import Tuple

# --- Pattern Library ---
# A dictionary of classic Game of Life patterns.
PATTERNS = {
    "glider": np.array([
        [0, 1, 0],
        [0, 0, 1],
        [1, 1, 1]
    ]),
    "gosper_glider_gun": np.array([
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1],
        [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1],
        [1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [1,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,1,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    ])
}

class GameOfLife:
    """
    An object-oriented implementation of Conway's Game of Life.
    Uses Pygame for visualization and NumPy/SciPy for high-performance computation.
    """
    def __init__(self, width: int = 800, height: int = 600, cell_size: int = 10):
        pygame.init()
        self.width, self.height, self.cell_size = width, height, cell_size
        self.rows, self.cols = height // cell_size, width // cell_size

        # --- Colors ---
        self.bg_color = (20, 20, 40)
        self.cell_color = (255, 255, 0)

        self.screen = pygame.display.set_mode((self.width, self.height))
        pygame.display.set_caption("Conway's Game of Life | Press SPACE to Pause, R to Reset, G for Glider, P for Gun")
        self.clock = pygame.time.Clock()

        self.grid = self.create_grid()
        self.paused = False

    def create_grid(self, random: bool = True) -> np.ndarray:
        """Creates a new grid, either random or empty."""
        if random:
            return np.random.choice([0, 1], size=(self.rows, self.cols), p=[0.8, 0.2])
        else:
            return np.zeros((self.rows, self.cols), dtype=int)

    def place_pattern(self, pattern_name: str, offset: Tuple[int, int] = (10, 10)):
        """Places a pattern from the library onto the grid."""
        pattern = PATTERNS.get(pattern_name)
        if pattern is None:
            print(f"Pattern '{pattern_name}' not found.")
            return

        self.grid = self.create_grid(random=False)
        p_rows, p_cols = pattern.shape
        row_off, col_off = offset

        if row_off + p_rows < self.rows and col_off + p_cols < self.cols:
            self.grid[row_off:row_off + p_rows, col_off:col_off + p_cols] = pattern

    def update(self):
        """Updates the grid based on Conway's rules using convolution."""
        if self.paused:
            return

        kernel = np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]])
        neighbors = convolve2d(self.grid, kernel, mode='same', boundary='wrap')

        born = (self.grid == 0) & (neighbors == 3)
        survives = (self.grid == 1) & ((neighbors == 2) | (neighbors == 3))

        self.grid = np.zeros_like(self.grid)
        self.grid[born | survives] = 1

    def draw(self):
        """Draws the grid of cells to the screen."""
        self.screen.fill(self.bg_color)
        live_cells = np.argwhere(self.grid == 1)

        for row, col in live_cells:
            pygame.draw.rect(
                self.screen,
                self.cell_color,
                (col * self.cell_size, row * self.cell_size, self.cell_size, self.cell_size)
            )
        pygame.display.flip()

    def handle_events(self):
        """Handles user input and other events."""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_r:
                    self.grid = self.create_grid()
                elif event.key == pygame.K_SPACE:
                    self.paused = not self.paused
                elif event.key == pygame.K_g:
                    self.place_pattern("glider")
                elif event.key == pygame.K_p:
                    self.place_pattern("gosper_glider_gun")
                elif event.key == pygame.K_q or event.key == pygame.K_ESCAPE:
                    return False
        return True

    def run(self):
        """Main game loop."""
        running = True
        while running:
            running = self.handle_events()
            self.update()
            self.draw()

            status = "Paused" if self.paused else "Running"
            pygame.display.set_caption(f"Conway's Game of Life | {status} | FPS: {int(self.clock.get_fps())}")
            self.clock.tick(15) # Increased frame rate slightly

        pygame.quit()
        sys.exit()

def main():
    game = GameOfLife()
    game.run()

if __name__ == "__main__":
    main()
