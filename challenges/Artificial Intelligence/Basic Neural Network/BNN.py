"""
Basic Neural Network (Minimal Multi-Layer Perceptron)
-----------------------------------------------------
Educational, beginner-friendly neural network from scratch using NumPy.

This script implements a simple two-layer perceptron with a configurable
hidden layer. It is designed for binary classification tasks and serves as
a learning tool for understanding the fundamentals of neural networks.

Features:
- Single hidden layer with configurable width and activation
- Sigmoid output suitable for binary classification demos
- Configurable epochs, learning rate, and random seed
- Optional progress callback instead of tight-loop ``print`` statements
- Plots training error progression

Usage:
    python BNN.py --epochs 10000 --lr 0.1 --seed 42
"""

import argparse
import logging
from dataclasses import dataclass
from typing import Callable, List, Optional

import matplotlib.pyplot as plt
import numpy as np


def sigmoid(x: np.ndarray) -> np.ndarray:
    """Sigmoid activation function.

    Args:
        x: Input array.

    Returns:
        The element-wise sigmoid of the input array.
    """

    return 1 / (1 + np.exp(-x))


def tanh(x: np.ndarray) -> np.ndarray:
    """Hyperbolic tangent activation.

    Args:
        x: Input array.

    Returns:
        The element-wise hyperbolic tangent of the input array.
    """

    return np.tanh(x)


def tanh_derivative(output: np.ndarray) -> np.ndarray:
    """Derivative of ``tanh`` with ``output`` already activated.

    Args:
        output: The output of the tanh function.

    Returns:
        The derivative of the tanh function.
    """

    return 1 - output**2


@dataclass
class NNConfig:
    """Configuration for the neural network.

    Attributes:
        epochs: The number of training epochs.
        learning_rate: The learning rate for weight updates.
        seed: An optional random seed for reproducibility.
    """
    epochs: int = 10000
    learning_rate: float = 0.1
    seed: Optional[int] = None


class NeuralNetwork:
    """Two-layer perceptron capable of solving XOR-style datasets.

    This class implements a simple neural network with one hidden layer.
    It uses the tanh activation function for the hidden layer and the
    sigmoid activation function for the output layer.
    """

    def __init__(
        self,
        training_inputs: np.ndarray,
        training_outputs: np.ndarray,
        config: NNConfig,
        hidden_units: int = 4,
        activation: Callable[[np.ndarray], np.ndarray] = tanh,
        activation_deriv: Callable[[np.ndarray], np.ndarray] = tanh_derivative,
    ):
        """Initializes the neural network.

        Args:
            training_inputs: The input data for training.
            training_outputs: The target output data for training.
            config: The configuration for the neural network.
            hidden_units: The number of units in the hidden layer.
            activation: The activation function for the hidden layer.
            activation_deriv: The derivative of the activation function.
        """
        self.training_inputs = training_inputs
        self.training_outputs = training_outputs
        self.config = config
        self.hidden_units = hidden_units
        self.activation = activation
        self.activation_deriv = activation_deriv
        if config.seed is not None:
            np.random.seed(config.seed)

        input_dim = training_inputs.shape[1]
        output_dim = training_outputs.shape[1]

        # Initialize weights and biases
        limit_hidden = 1.0 / np.sqrt(input_dim)
        self.w_ih = np.random.uniform(
            -limit_hidden, limit_hidden, size=(input_dim, hidden_units)
        )
        self.b_h = np.zeros((1, hidden_units))

        limit_out = 1.0 / np.sqrt(hidden_units)
        self.w_ho = np.random.uniform(
            -limit_out, limit_out, size=(hidden_units, output_dim)
        )
        self.b_o = np.zeros((1, output_dim))

        self.hidden_output: Optional[np.ndarray] = None
        self.output: Optional[np.ndarray] = None

    def forward(self) -> np.ndarray:
        """Compute the forward pass and cache intermediate activations.

        Returns:
            The output of the neural network.
        """

        hidden_linear = np.dot(self.training_inputs, self.w_ih) + self.b_h
        self.hidden_output = self.activation(hidden_linear)
        output_linear = np.dot(self.hidden_output, self.w_ho) + self.b_o
        self.output = sigmoid(output_linear)
        return self.output

    def backward(self) -> float:
        """Update weights using binary cross-entropy and return loss.

        Returns:
            The binary cross-entropy loss.
        """

        if self.output is None or self.hidden_output is None:
            raise ValueError("Must call forward() before backward().")

        # Calculate loss
        preds = np.clip(self.output, 1e-8, 1 - 1e-8)
        error = preds - self.training_outputs
        loss = float(
            -np.mean(
                self.training_outputs * np.log(preds)
                + (1 - self.training_outputs) * np.log(1 - preds)
            )
        )

        # Backpropagation
        grad_output = error / self.training_outputs.shape[0]

        grad_w_ho = np.dot(self.hidden_output.T, grad_output)
        grad_b_o = np.sum(grad_output, axis=0, keepdims=True)

        hidden_grad = np.dot(grad_output, self.w_ho.T) * self.activation_deriv(
            self.hidden_output
        )

        grad_w_ih = np.dot(self.training_inputs.T, hidden_grad)
        grad_b_h = np.sum(hidden_grad, axis=0, keepdims=True)

        # Update weights and biases
        lr = self.config.learning_rate
        self.w_ho -= lr * grad_w_ho
        self.b_o -= lr * grad_b_o
        self.w_ih -= lr * grad_w_ih
        self.b_h -= lr * grad_b_h

        return loss

    def train(
        self, progress_callback: Optional[Callable[[int, float], None]] = None
    ) -> List[float]:
        """Train the network and return loss history.

        Args:
            progress_callback: An optional function to call after each epoch.

        Returns:
            A list of loss values for each epoch.
        """

        logging.info("Training for %d epochs", self.config.epochs)
        loss_history: List[float] = []
        for epoch in range(1, self.config.epochs + 1):
            self.forward()
            loss = self.backward()
            loss_history.append(loss)
            if progress_callback is not None:
                progress_callback(epoch, loss)
        logging.info("Training complete.")
        return loss_history

    def predict(self, inputs: np.ndarray) -> np.ndarray:
        """Predict output for new inputs.

        Args:
            inputs: The input data to predict on.

        Returns:
            The predicted output.
        """

        hidden = self.activation(np.dot(inputs, self.w_ih) + self.b_h)
        return sigmoid(np.dot(hidden, self.w_ho) + self.b_o)


def main():
    """
    Set up, train, and visualize a basic neural network.
    """
    parser = argparse.ArgumentParser(
        description="Basic Neural Network (Minimal Multi-Layer Perceptron)"
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
    parser.add_argument(
        "--hidden-units",
        type=int,
        default=4,
        help="Width of the hidden layer (default: 4)",
    )
    parser.add_argument(
        "--log-interval",
        type=int,
        default=1000,
        help="How often (in epochs) to log progress (default: 1000)",
    )

    args = parser.parse_args()

    logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")

    config = NNConfig(epochs=args.epochs, learning_rate=args.lr, seed=args.seed)

    # Example: XOR-like dataset (not linearly separable, but for demo)
    training_inputs = np.array([[0, 0, 1], [1, 1, 1], [1, 0, 1], [0, 1, 1]])
    training_outputs = np.array([[0], [1], [1], [0]])

    nn = NeuralNetwork(
        training_inputs,
        training_outputs,
        config,
        hidden_units=args.hidden_units,
    )

    def progress(epoch: int, loss: float) -> None:
        if epoch == 1 or epoch % max(args.log_interval, 1) == 0 or epoch == config.epochs:
            logging.info("Epoch %d, loss %.4f", epoch, loss)

    error_history = nn.train(progress_callback=progress)

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
