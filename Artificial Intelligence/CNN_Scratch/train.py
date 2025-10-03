"""Train the scratch CNN on the MNIST dataset."""
from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path
from typing import Tuple

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
    parser = argparse.ArgumentParser(description="Train a NumPy CNN on MNIST.")
    parser.add_argument("--epochs", type=int, default=5, help="Number of training epochs (default: 5)")
    parser.add_argument("--batch-size", type=int, default=64, help="Mini-batch size (default: 64)")
    parser.add_argument("--learning-rate", type=float, default=0.01, help="Learning rate for SGD (default: 0.01)")
    parser.add_argument("--data-dir", type=str, default="./data", help="Directory to download/cache MNIST")
    parser.add_argument(
        "--model-out",
        type=str,
        default="cnn_weights.npz",
        help="Path to save the trained weights (default: cnn_weights.npz)",
    )
    parser.add_argument(
        "--train-limit",
        type=int,
        default=None,
        help="Limit the number of training samples for quicker experiments",
    )
    parser.add_argument(
        "--validation-size",
        type=int,
        default=10000,
        help="Number of training samples reserved for validation",
    )
    return parser.parse_args()


def prepare_data(
    data_dir: str,
    validation_size: int,
    train_limit: int | None,
) -> Tuple[Array, Array, Array, Array, Array, Array]:
    x_train, y_train, x_test, y_test = load_mnist(data_dir)
    if train_limit is not None:
        x_train = x_train[:train_limit]
        y_train = y_train[:train_limit]

    if validation_size > 0:
        x_val = x_train[:validation_size]
        y_val = y_train[:validation_size]
        x_train = x_train[validation_size:]
        y_train = y_train[validation_size:]
    else:
        x_val = None
        y_val = None

    return x_train, y_train, x_val, y_val, x_test, y_test


def main() -> None:
    args = parse_args()
    x_train, y_train, x_val, y_val, x_test, y_test = prepare_data(
        args.data_dir, args.validation_size, args.train_limit
    )

    model = SimpleCNN()
    history = model.fit(
        x_train,
        y_train,
        x_val=x_val,
        y_val=y_val,
        epochs=args.epochs,
        batch_size=args.batch_size,
        learning_rate=args.learning_rate,
    )

    Path(os.path.dirname(args.model_out) or ".").mkdir(parents=True, exist_ok=True)
    model.save(args.model_out)

    val_acc = history.accuracies[-1] if x_val is not None else history.accuracies[-1]
    test_acc = model.accuracy(x_test, y_test)

    print(f"Final training loss: {history.losses[-1]:.4f}")
    print(f"Validation accuracy: {val_acc:.4f}")
    print(f"Test accuracy: {test_acc:.4f}")


if __name__ == "__main__":
    main()
