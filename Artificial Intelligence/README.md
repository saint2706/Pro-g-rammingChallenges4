# Artificial Intelligence

This folder contains a collection of classic and educational artificial intelligence projects, each implemented in Python and designed for clarity, learning, and experimentation. All scripts are self-contained and require minimal dependencies.

## Contents

- **Basic Neural Network (`BNN.py`)**  
  A simple, from-scratch implementation of a feedforward neural network. Great for learning the fundamentals of neural nets and backpropagation.

- **Connect4 (`c4.py`)**  
  A playable Connect Four game with a basic AI opponent. Demonstrates game logic, state evaluation, and simple AI strategies.

- **Sudoku (`astar.py`)**
  A modern Sudoku solver using the A* search algorithm, with an option to compare against classic recursive backtracking. Includes a command-line interface and clear code structure for educational purposes.

- **CNN Framework (`CNN_Framework/`)**
  A PyTorch-based mini-framework for MNIST digit recognition featuring automated downloads, a training CLI, checkpoint
  saving, evaluation utilities, and pytest-backed smoke tests.

## Getting Started

1. **Install dependencies** (recommended in a virtual environment):

   ```bash
   python -m venv .venv
   source .venv/bin/activate  # Windows: .venv\Scripts\Activate.ps1
   python -m pip install -e .[ai]
   # Optional helpers
   python -m pip install -e .[algorithmic]  # plotting + scipy utilities
   python -m pip install -e .[developer]   # pytest/ruff/mypy
   ```

2. **Run any script directly**. For example, to solve a Sudoku puzzle:

   ```bash
   python Sudoku/astar.py --file path/to/puzzle.txt
   ```

   For Connect4 or the neural network, simply run their respective scripts:

   ```bash
   python Connect4/c4.py
   python "Basic Neural Network/BNN.py"
   ```

## Using pyproject.toml

The AI projects share dependencies with the rest of the repository via optional extras. Install `.[ai]` for the default numpy/scikit-learn stack, then layer on `algorithmic` or `developer` as needed. Editable installs keep your working tree synced with the environment, so changes you make are immediately importable.

## Requirements

- Python 3.8 or newer
- Install extras from `pyproject.toml` such as `ai`, `algorithmic`, and `developer` depending on what you plan to run

## Contributing

Contributions, improvements, and new AI demos are welcome! Please follow the style and documentation patterns used in the rest of the repository for consistency and educational value.

---

*Explore, learn, and experiment with classic AI algorithms and games!*
