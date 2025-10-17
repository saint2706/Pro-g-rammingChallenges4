"""
Classic Snake Game (Turtle Graphics Implementation)
--------------------------------------------------
Modern, well-documented, and beginner-friendly implementation of the classic Snake game using Python's turtle module.
Features:
- Modular class-based design
- Clear comments and docstrings
- Optimized for readability and maintainability
"""

import time
import turtle
import random
from typing import List, Tuple

# --- Constants ---
SCREEN_WIDTH = 600
SCREEN_HEIGHT = 600
MOVE_DISTANCE = 20
STARTING_POSITIONS = [(0, 0), (-20, 0), (-40, 0)]
UP, DOWN, LEFT, RIGHT = 90, 270, 180, 0


class Snake:
    """
    Manages the snake's body, movement, and direction.
    """

    def __init__(self) -> None:
        self.segments: List[turtle.Turtle] = []
        self.create_snake()
        self.head: turtle.Turtle = self.segments[0]

    def create_snake(self) -> None:
        """
        Creates the initial snake body.
        """
        for position in STARTING_POSITIONS:
            self.add_segment(position)

    def add_segment(self, position: Tuple[int, int]) -> None:
        """
        Adds a new segment to the snake at the given position.
        """
        new_segment = turtle.Turtle("square")
        new_segment.color("white")
        new_segment.penup()
        new_segment.goto(position)
        self.segments.append(new_segment)

    def extend(self) -> None:
        """
        Adds a new segment to the end of the snake.
        """
        last_pos = self.segments[-1].position()
        self.add_segment((int(last_pos[0]), int(last_pos[1])))

    def move(self) -> None:
        """
        Moves the snake forward by one segment.
        """
        for seg_num in range(len(self.segments) - 1, 0, -1):
            new_x = self.segments[seg_num - 1].xcor()
            new_y = self.segments[seg_num - 1].ycor()
            self.segments[seg_num].goto(new_x, new_y)
        self.head.forward(MOVE_DISTANCE)

    def reset(self) -> None:
        """
        Resets the snake to its initial state.
        """
        for seg in self.segments:
            seg.goto(1000, 1000)  # Move segments off-screen
        self.segments.clear()
        self.create_snake()
        self.head = self.segments[0]

    # --- Direction Control ---
    def up(self) -> None:
        """Change direction to up if not currently moving down."""
        if self.head.heading() != DOWN:
            self.head.setheading(UP)

    def down(self) -> None:
        """Change direction to down if not currently moving up."""
        if self.head.heading() != UP:
            self.head.setheading(DOWN)

    def left(self) -> None:
        """Change direction to left if not currently moving right."""
        if self.head.heading() != RIGHT:
            self.head.setheading(LEFT)

    def right(self) -> None:
        """Change direction to right if not currently moving left."""
        if self.head.heading() != LEFT:
            self.head.setheading(RIGHT)


class Food(turtle.Turtle):
    """
    Manages the food item in the game.
    """

    def __init__(self) -> None:
        super().__init__()
        self.shape("circle")
        self.penup()
        self.shapesize(stretch_len=0.5, stretch_wid=0.5)
        self.color("red")
        self.speed("fastest")
        self.refresh()

    def refresh(self) -> None:
        """
        Moves the food to a new random location on the screen.
        """
        random_x = random.randint(-280, 280)
        random_y = random.randint(-280, 280)
        self.goto(random_x, random_y)


class Scoreboard(turtle.Turtle):
    """
    Manages the score display.
    """

    def __init__(self) -> None:
        super().__init__()
        self.score: int = 0
        self.high_score: int = 0
        self.color("white")
        self.penup()
        self.goto(0, SCREEN_HEIGHT / 2 - 40)
        self.hideturtle()
        self.update_scoreboard()

    def update_scoreboard(self) -> None:
        """
        Clears and redraws the scoreboard.
        """
        self.clear()
        self.write(
            f"Score: {self.score}  High Score: {self.high_score}",
            align="center",
            font=("Courier", 24, "normal"),
        )

    def increase_score(self) -> None:
        """
        Increments the score and updates the display.
        """
        self.score += 1
        self.update_scoreboard()

    def reset(self) -> None:
        """
        Resets the score and updates the high score if needed.
        """
        if self.score > self.high_score:
            self.high_score = self.score
        self.score = 0
        self.update_scoreboard()


class SnakeGame:
    """
    Manages the main game screen, objects, and game loop.
    """

    def __init__(self) -> None:
        self.screen = turtle.Screen()
        self.screen.setup(width=SCREEN_WIDTH, height=SCREEN_HEIGHT)
        self.screen.bgcolor("black")
        self.screen.title("Snake Game")
        self.screen.tracer(0)  # Turns off automatic screen updates

        self.snake = Snake()
        self.food = Food()
        self.scoreboard = Scoreboard()

        self.setup_key_listeners()
        self.game_is_on = True

    def setup_key_listeners(self) -> None:
        """
        Sets up the keyboard bindings for controlling the snake.
        """
        self.screen.listen()
        self.screen.onkey(self.snake.up, "w")
        self.screen.onkey(self.snake.down, "s")
        self.screen.onkey(self.snake.left, "a")
        self.screen.onkey(self.snake.right, "d")

    def run(self) -> None:
        """
        Starts and runs the main game loop.
        """
        while self.game_is_on:
            self.screen.update()
            time.sleep(0.1)
            self.snake.move()

            # Detect collision with food
            if self.snake.head.distance(self.food) < 15:
                self.food.refresh()
                self.snake.extend()
                self.scoreboard.increase_score()

            # Detect collision with wall
            if not (
                -SCREEN_WIDTH / 2 < self.snake.head.xcor() < SCREEN_WIDTH / 2
                and -SCREEN_HEIGHT / 2 < self.snake.head.ycor() < SCREEN_HEIGHT / 2
            ):
                self.scoreboard.reset()
                self.snake.reset()

            # Detect collision with tail
            for segment in self.snake.segments[1:]:
                if self.snake.head.distance(segment) < 10:
                    self.scoreboard.reset()
                    self.snake.reset()

        self.screen.exitonclick()


if __name__ == "__main__":
    game = SnakeGame()
    game.run()
