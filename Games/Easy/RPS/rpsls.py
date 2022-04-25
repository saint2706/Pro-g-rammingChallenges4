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


def user_mv():
    choices = [f"{i} - {Move(i).name}" for i in range(5)]
    print(", ".join(choices))
    sel = int(input("Choose your move: "))
    act = Move(sel)
    return act


def comp_mv():
    return Move(random.randint(0, 4))


def get_winner(usr, cmp):
    if usr == cmp:
        return "Tie"
    for i in wins[usr]:
        if i == cmp:
            return "User"
    return "Comp"


while True:
    user = user_mv()
    comp = comp_mv()
    print(f"You chose {user.name}")
    print(f"Computer chose {comp.name}")
    print(get_winner(user, comp))
    cont = input("Continue? (y/n): ")
    if cont.lower() == "n":
        break
