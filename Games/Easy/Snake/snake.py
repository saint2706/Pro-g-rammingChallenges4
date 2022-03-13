from re import S
import turtle
import time
import random

delay = 0.2
score = 0
personal_best = 0

# Creating window
wn = turtle.Screen()
wn.title("Snake Game")
wn.bgcolor("black")
wn.setup(width=600, height=600)
wn.tracer(0)

# Snake head
head = turtle.Turtle()
head.shape("square")
head.color("white")
head.penup()
head.goto(0, 0)
head.direction = "stop"

# Snake food
food = turtle.Turtle()
food.speed(0)
food.shape("circle")
food.color("red")
food.penup()
food.goto(0, 100)

# Pen
pen = turtle.Turtle()
pen.speed(0)
pen.shape("square")
pen.color("white")
pen.penup()
pen.hideturtle()
pen.goto(0, 260)
pen.write(
    "Score: 0  Personal Best: 0", align="center", font=("Courier", 24, "normal")
)

# Movement
def go_up():
    if head.direction != "down":
        head.direction = "up"


def go_down():
    if head.direction != "up":
        head.direction = "down"


def go_left():
    if head.direction != "right":
        head.direction = "left"


def go_right():
    if head.direction != "left":
        head.direction = "right"


def move():
    if head.direction == "up":
        y = head.ycor()
        head.sety(y + 20)

    if head.direction == "down":
        y = head.ycor()
        head.sety(y - 20)

    if head.direction == "left":
        x = head.xcor()
        head.setx(x - 20)

    if head.direction == "right":
        x = head.xcor()
        head.setx(x + 20)


# Keyboard bindings
wn.listen()
wn.onkeypress(go_up, "w")
wn.onkeypress(go_down, "s")
wn.onkeypress(go_left, "a")
wn.onkeypress(go_right, "d")

segs = []

# Game
while True:
    wn.update()
    if (
        head.xcor() > 290
        or head.xcor() < -290
        or head.ycor() > 290
        or head.ycor() < -290
    ):
        time.sleep(1)
        head.goto(0, 0)
        head.direction = "stop"
        for seg in segs:
            seg.goto(1000, 1000)
        segs.clear()
        score = 0
        delay = 0.2
        pen.clear()
        pen.write(
            "Score: {}  Personal Best: {}".format(score, personal_best),
            align="center",
            font=("Courier", 24, "normal"),
        )
    if head.distance(food) < 20:
        x = random.randint(-270, 270)
        y = random.randint(-270, 270)
        food.goto(x, y)

        # Add a segment
        new_seg = turtle.Turtle()
        new_seg.speed(0)
        new_seg.shape("square")
        new_seg.color("grey")
        new_seg.penup()
        segs.append(new_seg)
        delay -= 0.001
        score += 1
        if score > personal_best:
            personal_best = score
        pen.clear()
        pen.write(
            "Score: {}  Personal Best: {}".format(score, personal_best),
            align="center",
            font=("Courier", 24, "normal"),
        )

    # Collision
    for indx in range(len(segs) - 1, 0, -1):
        x = segs[indx - 1].xcor()
        y = segs[indx - 1].ycor()
        segs[indx].goto(x, y)
    if len(segs) > 0:
        x = head.xcor()
        y = head.ycor()
        segs[0].goto(x, y)
    move()
    for seg in segs:
        if seg.distance(head) < 20:
            time.sleep(1)
            head.goto(0, 0)
            head.direction = "stop"
            for seg in segs:
                seg.goto(1000, 1000)
            segs.clear()
            score = 0
            delay = 0.2
            pen.clear()
            pen.write(
                "Score: {}  Personal Best: {}".format(score, personal_best),
                align="center",
                font=("Courier", 24, "normal"),
            )
    time.sleep(delay)

wn.mainloop()
