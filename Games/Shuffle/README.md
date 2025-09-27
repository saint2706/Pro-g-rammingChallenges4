# Deck Shuffle Visualiser

- **Challenge:** #113 — Shuffle a Deck of Cards (with visualisation)
- **Language:** Python

## Overview
Interactive matplotlib application that displays a deck of cards and lets you shuffle via GUI buttons. Ideal for demonstrating randomness and shuffle statistics.

## Dependencies
- `pip install -e .[games,visual]` (requires `matplotlib`).

## Run
```bash
python cards.py
```
Controls:
- **Shuffle Deck:** randomises the current order.
- **Reset Deck:** restores sorted order.

## Notes
- Requires a display server (matplotlib interactive backend).
- Modify colours, fonts, or animation speed in the constants at the top of `cards.py`.
