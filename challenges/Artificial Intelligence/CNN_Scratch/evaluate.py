"""Evaluate a trained scratch CNN on the MNIST test split."""

from __future__ import annotations

import argparse
import os
import sys

import numpy as np

try:  # pragma: no cover - import shim for script execution
    from .cnn import SimpleCNN
    from .data import load_mnist
except ImportError:  # pragma: no cover - executed when run as a script
    CURRENT_DIR = os.path.dirname(__file__)
    if CURRENT_DIR not in sys.path:
        sys.path.append(CURRENT_DIR)
    from cnn import SimpleCNN
    from data import load_mnist


Array = np.ndarray


def parse_args() -> argparse.Namespace:
    """Parses command-line arguments for the evaluation script."""
    parser = argparse.ArgumentParser(description="Evaluate a saved NumPy CNN on MNIST.")
    parser.add_argument(
        "--model", type=str, required=True, help="Path to the saved weights (.npz)"
    )
    parser.add_argument(
        "--data-dir",
        type=str,
        default="./data",
        help="Directory containing the MNIST cache",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Limit the number of test samples for quicker evaluation",
    )
    return parser.parse_args()


def main() -> None:
    """Main entry point for the evaluation script."""
    args = parse_args()
    # Load the test data
    _, _, x_test, y_test = load_mnist(args.data_dir)
    if args.limit is not None:
        x_test = x_test[: args.limit]
        y_test = y_test[: args.limit]

    # Initialize the model and load the trained weights
    model = SimpleCNN()
    model.load(args.model)
    # Evaluate the model on the test set
    accuracy = model.accuracy(x_test, y_test)
    print(f"Test accuracy: {accuracy:.4f}")


if __name__ == "__main__":
    main()
