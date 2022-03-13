import random
from enum import IntEnum


class Move(IntEnum):
    ROCK = 0
    PAPER = 1
    SCISSORS = 2
    LIZARD = 3
    SPOCK = 4


wins = {
    Move.SCISSORS: [Move.LIZARD, Move.PAPER],
    Move.PAPER: [Move.SPOCK, Move.ROCK],
    Move.ROCK: [Move.SCISSORS, Move.LIZARD],
    Move.LIZARD: [Move.SPOCK, Move.PAPER],
    Move.SPOCK: [Move.ROCK, Move.SCISSORS],
}


def userMove():
    choices = [f"{i} - {Move(i).name}" for i in range(5)]
    print(", ".join(choices))
    sel = int(input("Choose your move: "))
    act = Move(sel)
    return act


def compMove():
    return Move(random.randint(0, 4))


def getWinner(user, comp):
    if user == comp:
        return "Tie"
    for i in wins[user]:
        if i == comp:
            return "User"
    return "Comp"


while True:
    user = userMove()
    comp = compMove()
    print(f"You chose {user.name}")
    print(f"Computer chose {comp.name}")
    print(getWinner(user, comp))
    cont = input("Continue? (y/n): ")
    if cont.lower() == "n":
        break
