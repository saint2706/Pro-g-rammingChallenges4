import turtle
import time

screen = turtle.Screen()
screen.setup(800, 800)
screen.setworldcoordinates(-500, -500, 500, 500)
screen.title("Connect 4")
screen.tracer(0, 0)
turtle.speed(0)
turtle.hideturtle()

score = turtle.Turtle()
score.up()
score.hideturtle()

rows = 6
cols = 7
InitX = -450
InitY = -450 * rows / cols
width = -2 * InitX
height = -2 * InitY


def symbol_a(x, y, w, h, color):
    turtle.up()
    turtle.goto(x, y)
    turtle.seth(0)
    turtle.down()
    turtle.fillcolor(color)
    turtle.begin_fill()
    turtle.fd(w)
    turtle.left(90)
    turtle.fd(h)
    turtle.left(90)
    turtle.fd(w)
    turtle.left(90)
    turtle.fd(h)
    turtle.left(90)
    turtle.end_fill()


def symbol_b(x, y, r, color):
    turtle.up()
    turtle.goto(x, y - r)
    turtle.seth(0)
    turtle.down()
    turtle.fillcolor(color)
    turtle.begin_fill()
    turtle.circle(r, 360, 150)
    turtle.end_fill()


def show_board():
    symbol_a(InitX, InitY, width, height, "white")


def make_moves():
    global board
    row_gap = height / rows
    col_gap = width / cols
    y = InitY + row_gap / 2
    for i in range(rows):
        x = InitX + col_gap / 2
        for j in range(cols):
            if board[i][j] == 0:
                symbol_b(x, y, row_gap / 3, "yellow")
            elif board[i][j] == 1:
                symbol_b(x, y, row_gap / 3, "blue")
            else:
                symbol_b(x, y, row_gap / 3, "red")
            x += col_gap
        y += row_gap


def draw():
    show_board()
    make_moves()
    screen.update()


def game_over_lastmove(brd, chance, row, col):
    counter = 1
    i = col + 1
    while i < cols and brd[row][i] == chance:
        counter, i = counter + 1, i + 1
    i = col - 1
    while i >= 0 and brd[row][i] == chance:
        counter, i = counter + 1, i - 1
    if counter >= 4:
        return chance

    if row >= 3 and brd[row - 1][col] == chance and brd[row - 2][col] == chance and brd[row - 3][col] == chance:
        return chance

    counter = 1
    i = 1
    while row + i < rows and col + i < cols and brd[row + i][col + i] == chance:
        counter, i = counter + 1, i + 1
    i = -1
    while row + i >= 0 and col + i >= 0 and brd[row + i][col + i] == chance:
        counter, i = counter + 1, i - 1
    if counter >= 4:
        return chance

    counter = 1
    i = 1
    while row + i < rows and col - i >= 0 and brd[row + i][col - i] == chance:
        counter, i = counter + 1, i + 1
    i = -1
    while row + i >= 0 and col - i < cols and brd[row + i][col - i] == chance:
        counter, i = counter + 1, i - 1
    if counter >= 4:
        return chance

    for i in range(cols):
        if brd[rows - 1][i] == 0:
            return -2
    return 0


def make_move(brd, chance, col):
    for i in range(rows):
        if brd[i][col] == 0:
            brd[i][col] = chance
            return i


def make_board():
    global board
    for _ in range(rows):
        row = []
        for _ in range(cols):
            row.append(0)
        board.append(row)


def make_move_and_draw(brd, chance, col):
    row = make_move(brd, chance, col)
    row_gap = height / rows
    col_gap = width / cols
    y = InitY + row_gap * row + row_gap / 2
    x = InitX + col_gap * col + col_gap / 2
    i = row
    j = col
    if brd[i][j] == 0:
        symbol_b(x, y, row_gap / 3, "yellow")
    elif brd[i][j] == 1:
        for _ in range(5):
            symbol_b(x, y, row_gap / 3, "yellow")
            screen.update()
            time.sleep(0.05)
            symbol_b(x, y, row_gap / 3, "blue")
            screen.update()
            time.sleep(0.05)
    else:
        for _ in range(5):
            symbol_b(x, y, row_gap / 3, "yellow")
            screen.update()
            time.sleep(0.05)
            symbol_b(x, y, row_gap / 3, "red")
            screen.update()
            time.sleep(0.05)
    return row


def play(x):
    global turn, working
    if working:
        return
    working = True
    columns = [900 / 7 * i - 450 + 900 / 14 for i in range(7)]
    for i in range(len(columns)):
        if abs(x - columns[i]) < 900 / 14 * 2 / 3 and board[rows - 1][i] == 0:
            rn = make_move_and_draw(board, turn, i)
            r = game_over_lastmove(board, turn, rn, i)
            if r == 0:
                screen.textinput("Game over", "tie")
            elif r == 1:
                screen.textinput("Game over", "player 1 won")
            elif r == -1:
                screen.textinput("Game over", "player 2 won")
            if r != -2:
                screen.bye()
            turn = -turn
    working = False


board = []
make_board()
show_board()
make_moves()
turn = 1
working = False
screen.onclick(play)
screen.mainloop()
