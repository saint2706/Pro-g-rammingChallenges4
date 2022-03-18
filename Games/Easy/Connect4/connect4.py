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


def SymbolA(x, y, w, h, color):
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


def SymbolB(x, y, r, color):
    turtle.up()
    turtle.goto(x, y - r)
    turtle.seth(0)
    turtle.down()
    turtle.fillcolor(color)
    turtle.begin_fill()
    turtle.circle(r, 360, 150)
    turtle.end_fill()


def showBoard():
    SymbolA(InitX, InitY, width, height, "white")


def makeMoves():
    global board
    row_gap = height / rows
    col_gap = width / cols
    Y = InitY + row_gap / 2
    for i in range(rows):
        X = InitX + col_gap / 2
        for j in range(cols):
            if board[i][j] == 0:
                SymbolB(X, Y, row_gap / 3, "yellow")
            elif board[i][j] == 1:
                SymbolB(X, Y, row_gap / 3, "blue")
            else:
                SymbolB(X, Y, row_gap / 3, "red")
            X += col_gap
        Y += row_gap


def draw():
    showBoard()
    makeMoves()
    screen.update()


def game_over_lastmove(board, turn, row, col):
    counter = 1
    i = col + 1
    while i < cols and board[row][i] == turn:
        counter, i = counter + 1, i + 1
    i = col - 1
    while i >= 0 and board[row][i] == turn:
        counter, i = counter + 1, i - 1
    if counter >= 4:
        return turn

    if (
        row >= 3
        and board[row - 1][col] == turn
        and board[row - 2][col] == turn
        and board[row - 3][col] == turn
    ):
        return turn

    counter = 1
    i = 1
    while row + i < rows and col + i < cols and board[row + i][col + i] == turn:
        counter, i = counter + 1, i + 1
    i = -1
    while row + i >= 0 and col + i >= 0 and board[row + i][col + i] == turn:
        counter, i = counter + 1, i - 1
    if counter >= 4:
        return turn

    counter = 1
    i = 1
    while row + i < rows and col - i >= 0 and board[row + i][col - i] == turn:
        counter, i = counter + 1, i + 1
    i = -1
    while row + i >= 0 and col - i < cols and board[row + i][col - i] == turn:
        counter, i = counter + 1, i - 1
    if counter >= 4:
        return turn

    for i in range(cols):
        if board[rows - 1][i] == 0:
            return -2
    return 0


def makeMove(board, turn, col):
    for i in range(rows):
        if board[i][col] == 0:
            board[i][col] = turn
            return i


def makeBoard():
    global board
    for i in range(rows):
        row = []
        for j in range(cols):
            row.append(0)
        board.append(row)


def makeMoveAndDraw(board, turn, col):
    row = makeMove(board, turn, col)
    row_gap = height / rows
    col_gap = width / cols
    Y = InitY + row_gap * row + row_gap / 2
    X = InitX + col_gap * col + col_gap / 2
    i = row
    j = col
    if board[i][j] == 0:
        SymbolB(X, Y, row_gap / 3, "yellow")
    elif board[i][j] == 1:
        for k in range(5):
            SymbolB(X, Y, row_gap / 3, "yellow")
            screen.update()
            time.sleep(0.05)
            SymbolB(X, Y, row_gap / 3, "blue")
            screen.update()
            time.sleep(0.05)
    else:
        for k in range(5):
            SymbolB(X, Y, row_gap / 3, "yellow")
            screen.update()
            time.sleep(0.05)
            SymbolB(X, Y, row_gap / 3, "red")
            screen.update()
            time.sleep(0.05)
    return row


def play(x, y):
    global turn, working
    if working:
        return
    working = True
    cols = [900 / 7 * i - 450 + 900 / 14 for i in range(7)]
    for i in range(len(cols)):
        if abs(x - cols[i]) < 900 / 14 * 2 / 3 and board[rows - 1][i] == 0:
            rn = makeMoveAndDraw(board, turn, i)
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
makeBoard()
showBoard()
makeMoves()
turn = 1
working = False
screen.onclick(play)
screen.mainloop()
