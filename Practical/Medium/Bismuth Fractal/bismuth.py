import turtle
import math

# List of colors for each level of recursion
colors = ["#6A097D", "#C77DFF", "#660066", "#B6174B", "#F4C2C2", "#FF5733", "#FFC300", "#DAF7A6"]

def draw_hexagon(x, y, size, color):
    turtle.penup()
    turtle.goto(x, y)
    turtle.pendown()
    turtle.begin_fill()
    turtle.color(color)
    for _ in range(6):
        turtle.forward(size)
        turtle.left(60)
    turtle.end_fill()

def draw_bismuth(x, y, size, level):
    if level == 0:
        return
    else:
        # Select color based on the level of recursion
        color_index = level % len(colors)
        color = colors[color_index]

        # Draw a hexagon at the center
        draw_hexagon(x, y, size, color)

        # Recursively draw smaller hexagons at each vertex
        for angle in range(0, 360, 60):
            new_x = x + size * 0.75 * math.cos(angle * math.pi / 180)
            new_y = y + size * 0.75 * math.sin(angle * math.pi / 180)
            draw_bismuth(new_x, new_y, size / 2, level - 1)

# Set up Turtle
turtle.speed(0)  # Set the speed of the turtle (0 is the fastest)
turtle.penup()
turtle.goto(-150, -150)
turtle.pendown()

# Draw the Bismuth fractal
draw_bismuth(-150, -150, 100, 3)

# Keep the window open
turtle.done()
