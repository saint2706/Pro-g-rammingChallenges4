"""Evaluate a trained NumPy MLP checkpoint on MNIST."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Sequence

import numpy as np

from data import load_mnist
from mlp import MLP


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    """Parses command-line arguments for the evaluation script.

    Args:
        argv: Optional list of command-line arguments.

    Returns:
        An ArgumentParser object.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("model", type=Path, help="Path to the saved .npz checkpoint")
    parser.add_argument(
        "--data-home",
        type=Path,
        default=None,
        help="Directory used to cache the dataset",
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Optional limit of test samples to evaluate",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed used to reproduce the train/test split",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> None:
    """Main entry point for the evaluation script.

    Args:
        argv: Optional list of command-line arguments.
    """
    args = parse_args(argv)

    # Load the MNIST dataset
    _, X_test, _, y_test = load_mnist(
        test_size=0.2,
        random_state=args.seed,
        data_home=str(args.data_home) if args.data_home else None,
    )

    # Load the trained model
    model = MLP.load(args.model)

    # Limit the test set if requested
    if args.limit is not None:
        X_test = X_test[: args.limit]
        y_test = y_test[: args.limit]

    # Evaluate the model
    loss, accuracy = model.evaluate(X_test, y_test)
    print(f"Loss: {loss:.4f}")
    print(f"Accuracy: {accuracy:.3f}")

    # Generate and print a confusion matrix
    preds = model.predict(X_test)
    confusion = np.zeros((model.layer_sizes[-1], model.layer_sizes[-1]), dtype=np.int64)
    for true_label, pred_label in zip(y_test, preds):
        confusion[int(true_label), int(pred_label)] += 1
    np.set_printoptions(linewidth=200)
    print("Confusion matrix (rows=true, cols=pred):")
    print(confusion)


if __name__ == "__main__":
    main()
