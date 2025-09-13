import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import argparse
import sys
from typing import Tuple, List

class CubeAnimator:
    """
    Generates an animated GIF of a spinning 3D cube using manual 3D projection.
    """
    def __init__(self, size: Tuple[int, int] = (8, 6)):
        self.fig, self.ax = plt.subplots(figsize=size)

        # Define the 8 vertices of a unit cube
        self.vertices = np.array([
            [x, y, z] for x in [-1, 1] for y in [-1, 1] for z in [-1, 1]
        ])

        # Define the 12 edges connecting the vertices
        self.edges = [
            (i, j) for i in range(8) for j in range(i, 8)
            if np.sum(np.abs(self.vertices[i] - self.vertices[j])) == 2
        ]

        self.projection_distance = -5

    def _quaternion_to_matrix(self, q_vec: np.ndarray) -> np.ndarray:
        """Converts a quaternion vector into a 3x3 rotation matrix."""
        x, y, z = q_vec
        norm_sq = np.sum(q_vec**2)

        if norm_sq < 1:
            w = np.sqrt(1 - norm_sq)
        else:
            # Normalize and invert for the 'crazy' rotation effect
            w, x, y, z = 0, -x/norm_sq, -y/norm_sq, -z/norm_sq

        return np.array([
            [1-2*y*y-2*z*z, 2*x*y-2*w*z, 2*x*z+2*w*y],
            [2*x*y+2*w*z, 1-2*x*x-2*z*z, 2*y*z-2*w*x],
            [2*x*z-2*w*y, 2*y*z+2*w*x, 1-2*x*x-2*y*y]
        ])

    def _get_rotation_matrix(self, frame_index: int, total_frames: int) -> np.ndarray:
        """Calculates the rotation matrix for a given frame."""
        angle = np.pi * 2 * frame_index / total_frames
        q_vec = 0.5 * np.sin(angle * np.array([1, 2, 3]))
        return self._quaternion_to_matrix(q_vec)

    def _project(self, rotated_vertices: np.ndarray) -> np.ndarray:
        """Projects 3D vertices onto a 2D plane."""
        # Perspective projection
        return rotated_vertices[:, :2] / (rotated_vertices[:, [2]] - self.projection_distance)

    def _update_frame(self, frame_index: int, total_frames: int):
        """Updates the plot for a single frame of the animation."""
        self.ax.clear()

        rot_matrix = self._get_rotation_matrix(frame_index, total_frames)
        rotated_verts = np.dot(self.vertices, rot_matrix)
        projected_points = self._project(rotated_verts)

        # Draw edges
        for j, k in self.edges:
            self.ax.plot(projected_points[[j, k], 0], projected_points[[j, k], 1], "g-", lw=3)

        # Draw vertices
        self.ax.plot(projected_points[:, 0], projected_points[:, 1], "bo")

        self.ax.axis("equal")
        self.ax.set_xlim(-0.5, 0.5)
        self.ax.set_ylim(-0.5, 0.5)
        self.ax.set_title(f"Frame {frame_index + 1}/{total_frames}")

        return self.ax,

    def create_animation(self, total_frames: int, output_filename: str):
        """Creates and saves the cube animation as a GIF."""
        print(f"Generating {total_frames} frames for '{output_filename}'...")

        # Use FuncAnimation to create the animation in memory
        anim = FuncAnimation(
            self.fig,
            self._update_frame,
            frames=total_frames,
            fargs=(total_frames,),
            interval=40 # milliseconds per frame
        )

        # Save the animation as a GIF
        try:
            anim.save(output_filename, writer='pillow', fps=24)
            print(f"Animation successfully saved to '{output_filename}'")
        except Exception as e:
            print(f"Error saving animation: {e}", file=sys.stderr)
            print("Please ensure you have 'pillow' installed: pip install pillow", file=sys.stderr)

        plt.close(self.fig)

def main():
    """Main function to parse arguments and run the animator."""
    parser = argparse.ArgumentParser(description="Generate an animated GIF of a rotating cube.")
    parser.add_argument("-o", "--output", default="rotating_cube.gif",
                        help="Output filename for the GIF. Default is 'rotating_cube.gif'.")
    parser.add_argument("-f", "--frames", type=int, default=120,
                        help="Number of frames in the animation. Default is 120.")

    args = parser.parse_args()

    animator = CubeAnimator()
    animator.create_animation(args.frames, args.output)

if __name__ == "__main__":
    main()
