import random
from collections import Counter
from typing import List, Dict


class Hand:
    """
    Represents a hand of five dice in a Yahtzee-like game.
    Provides methods for rerolling dice and evaluating hand combinations.
    """

    def __init__(self) -> None:
        # Initialize the hand with five random dice (values 1-6)
        self.dice: List[int] = [random.randint(1, 6) for _ in range(5)]

    def __str__(self) -> str:
        # Return a string representation of the dice values
        return " ".join(map(str, self.dice))

    def reroll(self, indices_to_reroll: List[int]) -> None:
        """
        Reroll the dice at the specified indices (0-based).
        Ignores invalid indices.
        """
        for index in indices_to_reroll:
            if 0 <= index < 5:
                self.dice[index] = random.randint(1, 6)

    def evaluate(self) -> str:
        """
        Evaluates the hand and returns the name of the combination.
        Returns:
            str: The name of the hand (e.g., 'Yahtzee', 'Full House', etc.)
        """
        counts = Counter(self.dice)
        vals = sorted(counts.values(), reverse=True)

        if 5 in vals:
            return "Yahtzee"
        if 4 in vals:
            return "Four of a Kind"
        if vals == [3, 2]:
            return "Full House"
        if 3 in vals:
            return "Three of a Kind"

        # Check for straights
        unique_dice = sorted(set(self.dice))
        if len(unique_dice) == 5 and unique_dice[4] - unique_dice[0] == 4:
            return "Large Straight"
        if len(unique_dice) >= 4 and (
            self._is_subsequence([1, 2, 3, 4], unique_dice)
            or self._is_subsequence([2, 3, 4, 5], unique_dice)
            or self._is_subsequence([3, 4, 5, 6], unique_dice)
        ):
            return "Small Straight"

        if vals == [2, 2, 1]:
            return "Two Pair"
        if 2 in vals:
            return "One Pair"

        return "Nothing Special"

    @staticmethod
    def _is_subsequence(sub: List[int], main: List[int]) -> bool:
        """
        Checks if all elements of 'sub' are in 'main'.
        """
        return all(elem in main for elem in sub)


class Game:
    """
    Manages the Yahtzee game flow and statistics.
    Handles user interaction, rerolls, and displays results and stats.
    """

    def __init__(self) -> None:
        self.stats: Dict[str, int] = Counter()
        self.game_count: int = 0

    def play_round(self) -> None:
        """
        Plays a single round of the game (up to 3 rolls).
        Handles user reroll input and evaluates the final hand.
        """
        self.game_count += 1
        print(f"\n--- Game {self.game_count} ---")
        hand = Hand()

        for i in range(2):  # Allow up to two rerolls
            print(f"\nRoll {i + 1}: {hand}")
            wants_to_reroll = self.get_validated_input(
                "Reroll any dice? (y/n): ", ["y", "n"]
            )
            if wants_to_reroll == "n":
                break
            indices = self.get_reroll_indices()
            hand.reroll(indices)

        print(f"\nFinal Hand: {hand}")
        result = hand.evaluate()
        print(f"Result: {result}!")
        self.stats[result] += 1

    def get_reroll_indices(self) -> List[int]:
        """
        Prompts the user for which dice to reroll and validates the input.
        Returns:
            List[int]: List of 0-based indices to reroll.
        """
        while True:
            try:
                prompt = "Enter positions of dice to reroll (e.g., '1 3 4'): "
                response = input(prompt).strip()
                if not response:
                    return []
                # Convert 1-based positions to 0-based indices
                indices = [int(pos) - 1 for pos in response.split()]
                if all(0 <= i < 5 for i in indices):
                    return indices
                else:
                    print("Invalid position. Please enter numbers between 1 and 5.")
            except ValueError:
                print("Invalid input. Please enter numbers separated by spaces.")

    @staticmethod
    def get_validated_input(prompt: str, valid_options: List[str]) -> str:
        """
        Gets user input and validates it against a list of options.
        Args:
            prompt (str): The prompt to display.
            valid_options (List[str]): List of valid input strings.
        Returns:
            str: The validated user input.
        """
        while True:
            response = input(prompt).lower().strip()
            if response in valid_options:
                return response
            print(f"Invalid input. Please enter one of: {', '.join(valid_options)}")

    def print_stats(self) -> None:
        """
        Prints the final statistics after all games are played.
        Shows the count and percentage of each hand type.
        """
        print("\n--- Final Stats ---")
        if self.game_count == 0:
            print("No games were played.")
            return

        print(f"In {self.game_count} game(s), you rolled:")
        for hand_type, count in sorted(self.stats.items()):
            percentage = (count / self.game_count) * 100
            print(f"  - {hand_type + ':':<18} {count} time(s) ({percentage:.1f}%)")

    def run(self) -> None:
        """
        Runs the main game loop, allowing the user to play multiple rounds.
        """
        print("\n" + "-" * 40)
        print("Welcome to Refactored Yahtzee!")
        print("-" * 40)

        while True:
            self.play_round()
            play_again = self.get_validated_input(
                "\nPlay another game? (y/n): ", ["y", "n"]
            )
            if play_again == "n":
                break

        self.print_stats()
        print("\nThanks for playing!")


if __name__ == "__main__":
    game = Game()
    game.run()
