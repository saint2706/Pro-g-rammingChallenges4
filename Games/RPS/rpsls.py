
#
# Rock Paper Scissors Lizard Spock (RPSLS) Game - Python Implementation
# Modernized, documented, and beginner-friendly code.
#
import random
from enum import IntEnum
from typing import Dict, List


class Move(IntEnum):
    """
    Enumeration for the possible moves in the game.
    """
    ROCK = 0
    PAPER = 1
    SCISSORS = 2
    LIZARD = 3
    SPOCK = 4


# A dictionary defining the winning conditions.
# Key: A move. Value: A list of moves that the key beats.
WINNING_CONDITIONS: Dict[Move, List[Move]] = {
    Move.SCISSORS: [Move.LIZARD, Move.PAPER],
    Move.PAPER: [Move.SPOCK, Move.ROCK],
    Move.ROCK: [Move.SCISSORS, Move.LIZARD],
    Move.LIZARD: [Move.SPOCK, Move.PAPER],
    Move.SPOCK: [Move.ROCK, Move.SCISSORS],
}


def get_user_move() -> Move:
    """
    Prompts the user for their move and validates the input.

    Returns:
        The valid Move selected by the user.
    """
    while True:
        print("\nChoose your move:")
        choices = [f"  {move.value} - {move.name}" for move in Move]
        print("\n".join(choices))

        try:
            selection_str = input("Your choice: ")
            selection_int = int(selection_str)
            move = Move(selection_int)
            return move
        except (ValueError, IndexError):
            print(f"\nInvalid choice. Please enter a number between 0 and {len(Move) - 1}.")


def get_computer_move() -> Move:
    """
    Selects a random move for the computer.

    Returns:
        A random Move.
    """
    return random.choice(list(Move))


def determine_winner(user_move: Move, computer_move: Move) -> str:
    """
    Determines the winner based on the moves played.

    Returns:
        A string indicating the winner: "Player", "Computer", or "It's a Tie".
    """
    if user_move == computer_move:
        return "It's a Tie"

    if computer_move in WINNING_CONDITIONS[user_move]:
        return "Player"

    return "Computer"


def main():
    """
    Main function to run the Rock, Paper, Scissors, Lizard, Spock game loop.
    """
    print("--- Welcome to Rock, Paper, Scissors, Lizard, Spock! ---")

    while True:
        user_action = get_user_move()
        computer_action = get_computer_move()

        print(f"\n> You chose:     {user_action.name}")
        print(f"> Computer chose: {computer_action.name}\n")

        winner = determine_winner(user_action, computer_action)
        print(f"Result: {winner} wins!")

        while True:
            continue_game = input("\nPlay again? (y/n): ").lower().strip()
            if continue_game in ['y', 'n']:
                break
            print("Invalid input. Please enter 'y' or 'n'.")

        if continue_game == "n":
            break

    print("\nThanks for playing!")

if __name__ == "__main__":
    main()
