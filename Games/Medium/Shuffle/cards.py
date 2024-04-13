import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.animation import FuncAnimation
import random

# Define card suits and ranks
suits = ['♠️', '♣️', '♦️', '♥️']
ranks = ['A', '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K']

def create_deck():
    """Create a standard deck of 52 cards"""
    deck = []
    for suit in suits:
        for rank in ranks:
            deck.append((rank, suit))
    return deck

def draw_card(ax, card, pos):
    """Draw a card at the specified position"""
    x, y = pos
    rect = patches.Rectangle((x, y), 4, 6, linewidth=1, edgecolor='black', facecolor='white')
    ax.add_patch(rect)
    ax.text(x + 2, y + 5, card[0], ha='center', va='center', fontsize=12)
    ax.text(x + 2, y + 3, card[1], ha='center', va='center', fontsize=12)

def shuffle_deck(deck):
    """Shuffle the deck of cards"""
    random.shuffle(deck)

def display_deck(deck, ax):
    """Display the deck of cards"""
    ax.clear()
    ax.set_xlim(0, 56)
    ax.set_ylim(0, 24)
    ax.axis('off')
    for i, card in enumerate(deck):
        x = i % 13 * 4
        y = (i // 13) * 6
        draw_card(ax, card, (x, y))
    plt.draw()

# Create a deck of cards
deck = create_deck()

# Create the plot
fig, ax = plt.subplots(figsize=(15, 8))

# Display the initial deck
display_deck(deck, ax)

# Create a shuffle button
button_ax = plt.axes([0.85, 0.05, 0.1, 0.05])
button = plt.Button(button_ax, 'Shuffle')

def shuffle_and_display(event):
    """Callback function to shuffle the deck and update the display"""
    shuffle_deck(deck)
    display_deck(deck, ax)

# Connect the button to the callback function
button.on_clicked(shuffle_and_display)

plt.show()
