# Artificial Intelligence

This folder contains a collection of classic and educational artificial intelligence projects, each implemented in Python and designed for clarity, learning, and experimentation. All scripts are self-contained and require minimal dependencies.

## Contents

- **Basic Neural Network (`BNN.py`)**  
  A simple, from-scratch implementation of a feedforward neural network. Great for learning the fundamentals of neural nets and backpropagation.

- **Connect4 (`c4.py`)**  
  A playable Connect Four game with a basic AI opponent. Demonstrates game logic, state evaluation, and simple AI strategies.

- **Sudoku (`astar.py`)**  
  A modern Sudoku solver using the A* search algorithm, with an option to compare against classic recursive backtracking. Includes a command-line interface and clear code structure for educational purposes.

## Getting Started

1. **Install dependencies** (recommended in a virtual environment):

   ```bash
   pip install -r requirements.txt
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

## Requirements

- Python 3.8 or newer
- See `requirements.txt` for Python package dependencies

## Contributing

Contributions, improvements, and new AI demos are welcome! Please follow the style and documentation patterns used in the rest of the repository for consistency and educational value.

---

*Explore, learn, and experiment with classic AI algorithms and games!*
