# Games Collection

Welcome to the **Games** folder of the Pro-g-rammingChallenges4 repository! This directory contains a diverse set of classic and algorithmic games implemented in various programming languages, including Python, Java, C++, JavaScript, and HTML/CSS. Each game is designed to be educational, beginner-friendly, and a fun way to explore programming concepts.

## Contents

- **Connect4** (`Connect4.java`, `connect4.py`): Play the classic Connect Four game in Java or Python.


- **Knight Tour** (`knight.py`): Solve the Knight's Tour puzzle using Python.
- **Minesweeper** (`mine.py`): A simple command-line Minesweeper game in Python.
- **RPS** (Rock Paper Scissors):
  - `rps.cpp`, `Rps.java`, `rps.js`, `rps.html`, `rps.css`, `rpsls.py`
  - Play Rock Paper Scissors (and RPSLS) in your favorite language, with both CLI and web versions. SVG assets included.
- **Shuffle** (`cards.py`): Simulate card shuffling and visualization in Python (uses matplotlib).
- **Simon** (`simon.py`): The classic Simon memory game with sound and graphics (Python, tkinter, pygame). Assets included.
- **Snake** (`snake.py`, `snake.js`, `snake.html`, `snake.css`): Play Snake in Python (tkinter) or in your browser (JS/HTML/CSS).
- **Sudoku** (`sudoku.py`): Sudoku puzzle generator and solver in Python (uses numpy).
- **Yahtzee** (`Yahtzee.java`, `yahtzee.py`): Play Yahtzee in Java or Python.


## Using pyproject.toml

Create a virtual environment, then install the extras for the games you want to run:

```bash
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\Activate.ps1
python -m pip install -e .[games]
# Optional helpers
python -m pip install -e .[visual]  # matplotlib-based scoreboards
python -m pip install -e .[audio]   # shared sound backends (pygame, sounddevice)
```

Editable installs keep your local changes immediately playable.

## How to Run

### Python Games

- Make sure you have Python 3.8+ installed.
- Install dependencies via pyproject extras:

  ```sh
  python -m pip install -e .[games]
  # Add .[visual] for matplotlib stats or .[audio] for richer sound
  ```

- Run the desired game:

  ```sh
  python <game>.py
  ```

- Some games (e.g., Simon, Snake) require `tkinter` and/or `pygame` for GUI and sound.

### Java Games

- Compile and run with:

  ```sh
  javac <Game>.java
  java <Game>
  ```

### C++ Games

- Compile and run with:

  ```sh
  g++ rps.cpp -o rps
  ./rps
  ```

### JavaScript/HTML/CSS Games

- Open the `.html` file in your web browser (e.g., `rps.html`, `snake.html`).

## Assets

- Some games include assets (images, audio, fonts) in their respective subfolders. No additional downloads are required.

## Educational Focus

- All code is written with clarity and learning in mind. Each file is well-documented and commented to help new developers understand the logic and structure.
- Explore different programming paradigms and languages by comparing implementations of the same game.

## Contributing

- Contributions are welcome! Feel free to add new games, improve documentation, or refactor existing code for clarity and performance.
- Please follow the style and documentation conventions used throughout the repository.

## License

This project is licensed under the MIT License. See the root `LICENSE` file for details.
