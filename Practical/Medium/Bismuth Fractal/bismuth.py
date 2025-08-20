import turtle
import math
import argparse
from typing import List

class BismuthFractal:
    """
    A class to draw a Bismuth-like crystal fractal using Python's turtle graphics.
    """
    # List of colors for each level of recursion
    COLORS: List[str] = [
        "#6A097D", "#C77DFF", "#660066", "#B6174B",
        "#F4C2C2", "#FF5733", "#FFC300", "#DAF7A6"
    ]
    # Factor to scale down each subsequent level of hexagons
    SCALE_FACTOR = 0.5
    # Factor to determine the distance of child hexagons from the parent's center
    POSITION_FACTOR = 0.75

    def __init__(self, level: int, size: int):
        """
        Initializes the BismuthFractal drawer.

        Args:
            level: The depth of recursion for the fractal.
            size: The size (radius) of the initial hexagon.
        """
        self.level = level
        self.size = size
        self.screen = turtle.Screen()
        self.screen.title("Bismuth Fractal Generator")
        self.screen.bgcolor("black")
        self.turtle = turtle.Turtle()
        self.turtle.speed(0) # Set the speed to the fastest

    def _draw_hexagon(self, x: float, y: float, size: float, color: str):
        """Draws a single filled hexagon."""
        self.turtle.penup()
        self.turtle.goto(x, y)
        self.turtle.setheading(0) # Ensure consistent orientation
        self.turtle.backward(size / 2) # Center the hexagon
        self.turtle.left(30)
        self.turtle.pendown()

        self.turtle.color(color)
        self.turtle.begin_fill()
        for _ in range(6):
            self.turtle.forward(size)
            self.turtle.left(60)
        self.turtle.end_fill()

    def _draw_recursive(self, x: float, y: float, size: float, current_level: int):
        """The main recursive function to draw the fractal."""
        if current_level == 0:
            return

        # Select color based on the level of recursion
        color = self.COLORS[current_level % len(self.COLORS)]
        self._draw_hexagon(x, y, size, color)

        # Recursively draw smaller hexagons at each vertex
        for angle in range(0, 360, 60):
            new_x = x + size * self.POSITION_FACTOR * math.cos(math.radians(angle))
            new_y = y + size * self.POSITION_FACTOR * math.sin(math.radians(angle))
            self._draw_recursive(new_x, new_y, size * self.SCALE_FACTOR, current_level - 1)

    def draw(self):
        """Starts the drawing process."""
        self.turtle.hideturtle()
        self.screen.tracer(0) # Turn off screen updates for instant drawing
        self._draw_recursive(0, 0, self.size, self.level)
        self.screen.update() # Show the final result
        print("Drawing complete. Click the window to close.")
        self.screen.exitonclick()

def main():
    """Main function to parse arguments and run the fractal generator."""
    parser = argparse.ArgumentParser(description="Generate a Bismuth-like crystal fractal.")
    parser.add_argument("-l", "--level", type=int, default=4,
                        help="The recursion level for the fractal. Default is 4.")
    parser.add_argument("-s", "--size", type=int, default=120,
                        help="The size of the initial hexagon. Default is 120.")

    args = parser.parse_args()

    if args.level < 1 or args.size < 10:
        parser.error("Level must be >= 1 and size must be >= 10 for a good visualization.")

    print(f"Generating Bismuth fractal with level={args.level} and size={args.size}...")
    fractal_drawer = BismuthFractal(level=args.level, size=args.size)
    fractal_drawer.draw()

if __name__ == "__main__":
    main()
