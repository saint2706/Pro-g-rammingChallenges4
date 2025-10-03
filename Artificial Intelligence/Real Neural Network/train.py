"""Train the NumPy MLP on MNIST or a compatible dataset."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Sequence

import numpy as np

from data import load_mnist
from mlp import MLP


def parse_args(argv: Sequence[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--hidden-layers",
        type=int,
        nargs="*",
        default=[128, 64],
        help="Sizes of hidden layers (default: 128 64)",
    )
    parser.add_argument(
        "--epochs",
        type=int,
        default=10,
        help="Number of training epochs (default: 10)",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=64,
        help="Mini-batch size (default: 64)",
    )
    parser.add_argument(
        "--learning-rate",
        type=float,
        default=0.01,
        help="Gradient descent learning rate (default: 0.01)",
    )
    parser.add_argument(
        "--l2",
        type=float,
        default=0.0,
        help="L2 regularisation strength (default: 0.0)",
    )
    parser.add_argument(
        "--model-out",
        type=Path,
        default=Path("mlp-mnist.npz"),
        help="Output path for the trained weights",
    )
    parser.add_argument(
        "--no-shuffle",
        action="store_true",
        help="Disable shuffling between epochs",
    )
    parser.add_argument(
        "--data-home",
        type=Path,
        default=None,
        help="Directory used to cache the dataset",
    )
    parser.add_argument(
        "--validation-split",
        type=float,
        default=0.1,
        help="Fraction of the training set used for validation (default: 0.1)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=42,
        help="Random seed for reproducibility",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> None:
    args = parse_args(argv)

    X_train, X_test, y_train, y_test = load_mnist(
        test_size=0.2,
        random_state=args.seed,
        data_home=str(args.data_home) if args.data_home else None,
    )

    rng = np.random.default_rng(args.seed)
    permutation = rng.permutation(X_train.shape[0])
    X_train = X_train[permutation]
    y_train = y_train[permutation]

    n_classes = len(np.unique(y_train))
    layer_sizes = [X_train.shape[1], *args.hidden_layers, n_classes]

    model = MLP(
        layer_sizes,
        hidden_activation="relu",
        learning_rate=args.learning_rate,
        l2=args.l2,
        seed=args.seed,
    )

    if not 0.0 <= args.validation_split < 1.0:
        raise ValueError("validation-split must be in the range [0, 1)")

    if args.validation_split:
        split = int((1.0 - args.validation_split) * X_train.shape[0])
        train_X, val_X = X_train[:split], X_train[split:]
        train_y, val_y = y_train[:split], y_train[split:]
        validation_data = (val_X, val_y)
    else:
        train_X, train_y = X_train, y_train
        validation_data = None

    history = model.fit(
        train_X,
        train_y,
        epochs=args.epochs,
        batch_size=args.batch_size,
        validation_data=validation_data,
        shuffle=not args.no_shuffle,
        verbose=True,
    )

    train_loss, train_acc = model.evaluate(train_X, train_y)
    test_loss, test_acc = model.evaluate(X_test, y_test)

    print("Training finished")
    print(f"Train loss: {train_loss:.4f} | Train accuracy: {train_acc:.3f}")
    print(f"Test loss:   {test_loss:.4f} | Test accuracy: {test_acc:.3f}")

    model.save(args.model_out)
    print(f"Model saved to {args.model_out.resolve()}")

    np.savez_compressed(
        args.model_out.with_suffix(".history.npz"),
        loss=np.array(history.loss),
        accuracy=np.array(history.accuracy),
        val_loss=np.array(history.val_loss),
        val_accuracy=np.array(history.val_accuracy),
    )


if __name__ == "__main__":
    main()
