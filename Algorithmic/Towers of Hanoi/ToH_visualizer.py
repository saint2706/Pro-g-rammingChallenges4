import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.animation import FuncAnimation
import argparse
import sys
from typing import Dict, List

# Import the generator function
try:
    from ToH import hanoi_state_generator
except ImportError:
    print("Error: This script requires 'ToH.py' to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class HanoiVisualizer:
    """
    Animates the solution to the Towers of Hanoi puzzle using Matplotlib.
    """
    def __init__(self, num_disks: int):
        if not 1 <= num_disks <= 10:
            raise ValueError("Number of disks must be between 1 and 10 for a clear visualization.")

        self.num_disks = num_disks
        self.peg_names = ['A', 'B', 'C']
        self.peg_coords = {'A': 1, 'B': 3, 'C': 5}

        # Get the sequence of board states from the generator
        self.steps = list(hanoi_state_generator(self.num_disks, 'A', 'C', 'B'))

        # Setup the plot
        self.fig, self.ax = plt.subplots(figsize=(10, 6))
        self._setup_plot()

        # Create a colormap for the disks
        self.disk_colors = plt.cm.viridis(np.linspace(0, 1, self.num_disks))

    def _setup_plot(self):
        """Sets up the initial state of the plot area."""
        self.ax.set_xlim(0, 6)
        self.ax.set_ylim(0, self.num_disks + 2)
        self.ax.set_aspect('equal')
        self.ax.axis('off')

        # Draw pegs
        for name, x in self.peg_coords.items():
            self.ax.plot([x, x], [0, self.num_disks + 1], 'black', lw=5)
            self.ax.text(x, self.num_disks + 1.2, name, ha='center', fontsize=16)

    def _draw_disk(self, disk_num: int, peg_name: str, position: int):
        """Draws a single disk on a given peg at a certain height."""
        width = disk_num * 0.4 + 0.5 # Width proportional to disk number
        height = 0.4
        x = self.peg_coords[peg_name] - width / 2
        y = position * height
        color = self.disk_colors[disk_num - 1]

        rect = patches.Rectangle((x, y), width, height, facecolor=color, edgecolor='black')
        self.ax.add_patch(rect)
        self.ax.text(self.peg_coords[peg_name], y + height / 2, str(disk_num), ha='center', va='center', color='white', weight='bold')

    def _update(self, frame_num: int):
        """The update function for the animation frame."""
        self.ax.clear()
        self._setup_plot()

        state = self.steps[frame_num]

        for peg_name in self.peg_names:
            disks_on_peg = state[peg_name]
            for i, disk_num in enumerate(disks_on_peg):
                self._draw_disk(disk_num, peg_name, i)

        self.ax.set_title(f"Towers of Hanoi | Disks: {self.num_disks} | Move: {frame_num}", loc='center')

    def animate(self):
        """Creates and shows the animation."""
        # The number of frames is the number of states yielded by the generator
        frames = len(self.steps)
        anim = FuncAnimation(self.fig, self._update, frames=frames, interval=500, repeat=False)
        plt.show()

def main():
    """Main function to run the visualizer."""
    parser = argparse.ArgumentParser(description="Visualize the Towers of Hanoi puzzle solution.")
    parser.add_argument("disks", type=int, nargs='?', default=4,
                        help="The number of disks to solve for (1-10 recommended). Default is 4.")

    args = parser.parse_args()

    try:
        visualizer = HanoiVisualizer(args.disks)
        visualizer.animate()
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    # Add numpy as a dependency for the colormap
    try:
        import numpy as np
    except ImportError:
        print("Error: This script requires 'numpy'. Please install it: pip install numpy", file=sys.stderr)
        sys.exit(1)
    main()
