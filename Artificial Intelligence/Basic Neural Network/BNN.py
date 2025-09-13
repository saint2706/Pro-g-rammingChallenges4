"""
Basic Neural Network (Single-Layer Perceptron)
------------------------------------------------
Educational, beginner-friendly neural network from scratch using NumPy.
Features:
- Single neuron, single layer (no hidden layers)
- Sigmoid activation
- Configurable epochs, learning rate, and random seed
- CLI interface for reproducibility
- Plots training error progression

Usage:
    python BNN.py --epochs 10000 --lr 0.1 --seed 42
"""

import numpy as np
import matplotlib.pyplot as plt
from typing import List, Optional
from dataclasses import dataclass
import argparse


def sigmoid(x: np.ndarray, derivative: bool = False) -> np.ndarray:
    """
    Sigmoid activation function and its derivative.

    Args:
        x: The input numpy array.
        derivative: If True, computes the derivative of the sigmoid function.

    Returns:
        The result of the sigmoid function or its derivative.
    """
    if derivative:
        # Assumes the input 'x' is already the output of a sigmoid function.
        return x * (1 - x)
    return 1 / (1 + np.exp(-x))


@dataclass
class NNConfig:
    epochs: int = 10000
    learning_rate: float = 0.1
    seed: Optional[int] = None


class NeuralNetwork:
    """
    Single-layer neural network (perceptron) with sigmoid activation.
    """

    def __init__(
        self,
        training_inputs: np.ndarray,
        training_outputs: np.ndarray,
        config: NNConfig,
    ):
        self.training_inputs = training_inputs
        self.training_outputs = training_outputs
        self.config = config
        if config.seed is not None:
            np.random.seed(config.seed)
        num_inputs = training_inputs.shape[1]
        # Small random weights, mean 0
        self.weights = 2 * np.random.random((num_inputs, 1)) - 1
        self.output = None

    def forward(self) -> np.ndarray:
        """Compute output for current weights."""
        self.output = sigmoid(np.dot(self.training_inputs, self.weights))
        return self.output

    def backward(self) -> float:
        """Update weights using mean absolute error and return error."""
        if self.output is None:
            raise ValueError("Must call forward() before backward().")
        error = self.training_outputs - self.output
        delta = error * sigmoid(self.output, derivative=True)
        # Weight update with learning rate
        self.weights += self.config.learning_rate * np.dot(
            self.training_inputs.T, delta
        )
        return float(np.mean(np.abs(error)))

    def train(self) -> List[float]:
        """Train the network and return error history."""
        print(f"Training for {self.config.epochs} epochs...")
        error_history = []
        for i in range(self.config.epochs):
            self.forward()
            error = self.backward()
            error_history.append(error)
            if (i % 1000) == 0:
                print(f"  Epoch {i}, Error: {error:.4f}")
        print("Training complete.")
        return error_history

    def predict(self, inputs: np.ndarray) -> np.ndarray:
        """Predict output for new inputs."""
        return sigmoid(np.dot(inputs, self.weights))


def main():
    """
    Set up, train, and visualize a basic neural network.
    """
    parser = argparse.ArgumentParser(
        description="Basic Neural Network (Single-Layer Perceptron)"
    )
    parser.add_argument(
        "--epochs",
        type=int,
        default=10000,
        help="Number of training epochs (default: 10000)",
    )
    parser.add_argument(
        "--lr", type=float, default=0.1, help="Learning rate (default: 0.1)"
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=None,
        help="Random seed for reproducibility (default: None)",
    )
    args = parser.parse_args()

    config = NNConfig(epochs=args.epochs, learning_rate=args.lr, seed=args.seed)

    # Example: XOR-like dataset (not linearly separable, but for demo)
    training_inputs = np.array([[0, 0, 1], [1, 1, 1], [1, 0, 1], [0, 1, 1]])
    training_outputs = np.array([[0], [1], [1], [0]])

    nn = NeuralNetwork(training_inputs, training_outputs, config)
    error_history = nn.train()

    # Predict on new data
    print("\n--- Predictions ---")
    for x in ([1, 0, 0], [0, 1, 0], [1, 1, 0], [0, 0, 0]):
        pred = nn.predict(np.array(x)).item()
        print(f"Input {x} -> Prediction: {pred:.4f}")

    # Plot error progression
    plt.figure(figsize=(10, 6))
    plt.plot(range(len(error_history)), error_history)
    plt.title("Training Error Progression", fontsize=16)
    plt.xlabel("Epoch")
    plt.ylabel("Mean Absolute Error")
    plt.grid(True)
    plt.tight_layout()
    plt.show()


if __name__ == "__main__":
    main()
