import time
import turtle
from typing import List

# --- Constants ---
SCREEN_WIDTH = 600
SCREEN_HEIGHT = 600
MOVE_DISTANCE = 20
STARTING_POSITIONS = [(0, 0), (-20, 0), (-40, 0)]
UP, DOWN, LEFT, RIGHT = 90, 270, 180, 0

class Snake:
    """Manages the snake's body, movement, and direction."""
    def __init__(self):
        self.segments: List[turtle.Turtle] = []
        self.create_snake()
        self.head = self.segments[0]

    def create_snake(self):
        """Creates the initial snake body."""
        for position in STARTING_POSITIONS:
            self.add_segment(position)

    def add_segment(self, position: tuple):
        """Adds a new segment to the snake."""
        new_segment = turtle.Turtle("square")
        new_segment.color("white")
        new_segment.penup()
        new_segment.goto(position)
        self.segments.append(new_segment)

    def extend(self):
        """Adds a new segment to the end of the snake."""
        self.add_segment(self.segments[-1].position())

    def move(self):
        """Moves the snake forward by one segment."""
        for seg_num in range(len(self.segments) - 1, 0, -1):
            new_x = self.segments[seg_num - 1].xcor()
            new_y = self.segments[seg_num - 1].ycor()
            self.segments[seg_num].goto(new_x, new_y)
        self.head.forward(MOVE_DISTANCE)

    def reset(self):
        """Resets the snake to its initial state."""
        for seg in self.segments:
            seg.goto(1000, 1000) # Move segments off-screen
        self.segments.clear()
        self.create_snake()
        self.head = self.segments[0]

    # --- Direction Control ---
    def up(self):
        if self.head.heading() != DOWN:
            self.head.setheading(UP)
    def down(self):
        if self.head.heading() != UP:
            self.head.setheading(DOWN)
    def left(self):
        if self.head.heading() != RIGHT:
            self.head.setheading(LEFT)
    def right(self):
        if self.head.heading() != LEFT:
            self.head.setheading(RIGHT)

class Food(turtle.Turtle):
    """Manages the food item in the game."""
    def __init__(self):
        super().__init__()
        self.shape("circle")
        self.penup()
        self.shapesize(stretch_len=0.5, stretch_wid=0.5)
        self.color("red")
        self.speed("fastest")
        self.refresh()

    def refresh(self):
        """Moves the food to a new random location on the screen."""
        random_x = turtle.randint(-280, 280)
        random_y = turtle.randint(-280, 280)
        self.goto(random_x, random_y)

class Scoreboard(turtle.Turtle):
    """Manages the score display."""
    def __init__(self):
        super().__init__()
        self.score = 0
        self.high_score = 0
        self.color("white")
        self.penup()
        self.goto(0, SCREEN_HEIGHT/2 - 40)
        self.hideturtle()
        self.update_scoreboard()

    def update_scoreboard(self):
        """Clears and redraws the scoreboard."""
        self.clear()
        self.write(f"Score: {self.score}  High Score: {self.high_score}", align="center", font=("Courier", 24, "normal"))

    def increase_score(self):
        """Increments the score."""
        self.score += 1
        self.update_scoreboard()

    def reset(self):
        """Resets the score and updates the high score if needed."""
        if self.score > self.high_score:
            self.high_score = self.score
        self.score = 0
        self.update_scoreboard()

class SnakeGame:
    """Manages the main game screen, objects, and game loop."""
    def __init__(self):
        self.screen = turtle.Screen()
        self.screen.setup(width=SCREEN_WIDTH, height=SCREEN_HEIGHT)
        self.screen.bgcolor("black")
        self.screen.title("Snake Game")
        self.screen.tracer(0) # Turns off automatic screen updates

        self.snake = Snake()
        self.food = Food()
        self.scoreboard = Scoreboard()

        self.setup_key_listeners()
        self.game_is_on = True

    def setup_key_listeners(self):
        """Sets up the keyboard bindings."""
        self.screen.listen()
        self.screen.onkey(self.snake.up, "w")
        self.screen.onkey(self.snake.down, "s")
        self.screen.onkey(self.snake.left, "a")
        self.screen.onkey(self.snake.right, "d")

    def run(self):
        """Starts and runs the main game loop."""
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
            if not (-SCREEN_WIDTH/2 < self.snake.head.xcor() < SCREEN_WIDTH/2 and
                    -SCREEN_HEIGHT/2 < self.snake.head.ycor() < SCREEN_HEIGHT/2):
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
