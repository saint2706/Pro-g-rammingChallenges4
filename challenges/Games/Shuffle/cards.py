"""
Deck Shuffling Visualizer
------------------------
Visualizes a deck of playing cards and allows interactive shuffling using matplotlib.
Modernized for clarity, maintainability, and educational value.
"""

import random
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.widgets import Button
from typing import List, Tuple, NamedTuple


class Card(NamedTuple):
    """
    Represents a single playing card with a rank and a suit.
    Example: Card('A', '♠️')
    """

    rank: str
    suit: str


class Deck:
    """
    Represents a standard deck of 52 playing cards.
    Provides shuffling and string representation.
    """

    SUITS: List[str] = ["♠️", "♣️", "♦️", "♥️"]
    RANKS: List[str] = [
        "A",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "10",
        "J",
        "Q",
        "K",
    ]

    def __init__(self) -> None:
        # Use list comprehension for efficient deck creation
        self.cards: List[Card] = [
            Card(rank, suit) for suit in self.SUITS for rank in self.RANKS
        ]

    def shuffle(self) -> None:
        """
        Shuffles the deck in place using random.shuffle.
        """
        random.shuffle(self.cards)

    def __len__(self) -> int:
        return len(self.cards)

    def __str__(self) -> str:
        return f"Deck of {len(self.cards)} cards"


class DeckVisualizer:
    """
    Manages the matplotlib visualization of a deck of cards.
    Provides interactive shuffling and clear, educational code structure.
    """

    def __init__(self, deck: Deck) -> None:
        self.deck = deck
        self.fig, self.ax = plt.subplots(figsize=(14, 8))
        plt.subplots_adjust(bottom=0.2)  # Make space for the button

        self._setup_plot()
        self._add_shuffle_button()
        self.display_deck()

    def _setup_plot(self) -> None:
        """
        Sets up the main plot area for displaying cards.
        """
        self.ax.set_xlim(0, 13 * 4)
        self.ax.set_ylim(0, 4 * 6 + 2)
        self.ax.axis("off")

    def _draw_card(self, card: Card, pos: Tuple[float, float]) -> None:
        """
        Draws a single card at a specified position on the axis.
        Args:
            card: The Card to draw.
            pos: (x, y) position for the lower-left corner of the card.
        """
        x, y = pos
        card_color = "red" if card.suit in ["♦️", "♥️"] else "black"

        rect = patches.Rectangle(
            (x, y), 4, 6, linewidth=1, edgecolor="black", facecolor="white", zorder=2
        )
        self.ax.add_patch(rect)

        self.ax.text(
            x + 0.5,
            y + 5,
            card.rank,
            ha="left",
            va="top",
            fontsize=12,
            color=card_color,
            weight="bold",
            zorder=3,
        )
        self.ax.text(
            x + 2, y + 3, card.suit, ha="center", va="center", fontsize=20, zorder=3
        )

    def display_deck(self) -> None:
        """
        Clears the axis and displays the current state of the deck.
        Cards are arranged in 4 rows (one per suit) and 13 columns (one per rank).
        """
        self.ax.clear()
        self._setup_plot()

        for i, card in enumerate(self.deck.cards):
            col = i % 13
            row = 3 - (i // 13)  # Draw from top to bottom
            x_pos = col * 4.2
            y_pos = row * 6.5
            self._draw_card(card, (x_pos, y_pos))

        plt.draw()

    def _add_shuffle_button(self) -> None:
        """
        Adds a 'Shuffle' button to the plot for interactive shuffling.
        """
        button_ax = plt.axes(
            (0.45, 0.05, 0.1, 0.075)
        )  # [left, bottom, width, height] as tuple
        shuffle_button = Button(button_ax, "Shuffle")
        shuffle_button.on_clicked(self._shuffle_callback)
        # Keep a reference to the button so it doesn't get garbage collected
        self.shuffle_button = shuffle_button

    def _shuffle_callback(self, event) -> None:
        """
        Callback function to shuffle the deck and update the display.
        """
        print("Shuffling deck...")
        self.deck.shuffle()
        self.display_deck()


def main() -> None:
    """
    Main function to create and display the deck visualizer.
    """
    print("--- Deck Shuffling Visualizer ---")
    deck = Deck()
    visualizer = DeckVisualizer(deck)
    plt.show()


if __name__ == "__main__":
    main()
    main()
