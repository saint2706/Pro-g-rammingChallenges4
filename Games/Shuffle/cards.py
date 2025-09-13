import random
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from typing import List, Tuple, NamedTuple

class Card(NamedTuple):
    """Represents a single playing card with a rank and a suit."""
    rank: str
    suit: str

class Deck:
    """Represents a deck of 52 playing cards."""
    SUITS = ['♠️', '♣️', '♦️', '♥️']
    RANKS = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']

    def __init__(self):
        self.cards: List[Card] = [Card(rank, suit) for suit in self.SUITS for rank in self.RANKS]

    def shuffle(self):
        """Shuffles the deck in place."""
        random.shuffle(self.cards)

    def __len__(self) -> int:
        return len(self.cards)

    def __str__(self) -> str:
        return f"Deck of {len(self.cards)} cards"

class DeckVisualizer:
    """Manages the matplotlib visualization of a deck of cards."""
    def __init__(self, deck: Deck):
        self.deck = deck
        self.fig, self.ax = plt.subplots(figsize=(14, 8))
        plt.subplots_adjust(bottom=0.2) # Make space for the button

        self.setup_plot()
        self.add_shuffle_button()
        self.display_deck()

    def setup_plot(self):
        """Sets up the main plot area."""
        self.ax.set_xlim(0, 13 * 4)
        self.ax.set_ylim(0, 4 * 6 + 2)
        self.ax.axis('off')

    def draw_card(self, card: Card, pos: Tuple[float, float]):
        """Draws a single card at a specified position on the axis."""
        x, y = pos
        card_color = 'red' if card.suit in ['♦️', '♥️'] else 'black'

        rect = patches.Rectangle((x, y), 4, 6, linewidth=1, edgecolor='black', facecolor='white', zorder=2)
        self.ax.add_patch(rect)

        self.ax.text(x + 0.5, y + 5, card.rank, ha='left', va='top', fontsize=12, color=card_color, weight='bold', zorder=3)
        self.ax.text(x + 2, y + 3, card.suit, ha='center', va='center', fontsize=20, zorder=3)

    def display_deck(self):
        """Clears the axis and displays the current state of the deck."""
        self.ax.clear()
        self.setup_plot()

        for i, card in enumerate(self.deck.cards):
            col = i % 13
            row = 3 - (i // 13) # Draw from top to bottom
            x_pos = col * 4.2
            y_pos = row * 6.5
            self.draw_card(card, (x_pos, y_pos))

        plt.draw()

    def add_shuffle_button(self):
        """Adds a 'Shuffle' button to the plot."""
        button_ax = plt.axes([0.45, 0.05, 0.1, 0.075]) # [left, bottom, width, height]
        shuffle_button = plt.Button(button_ax, 'Shuffle')
        shuffle_button.on_clicked(self.shuffle_callback)
        # Keep a reference to the button so it doesn't get garbage collected
        self.shuffle_button = shuffle_button

    def shuffle_callback(self, event):
        """Callback function to shuffle the deck and update the display."""
        print("Shuffling deck...")
        self.deck.shuffle()
        self.display_deck()

def main():
    """Main function to create and display the deck visualizer."""
    print("--- Deck Shuffling Visualizer ---")
    deck = Deck()
    visualizer = DeckVisualizer(deck)
    plt.show()

if __name__ == "__main__":
    main()
