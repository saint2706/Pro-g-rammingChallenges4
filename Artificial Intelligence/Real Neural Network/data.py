"""Utilities for loading datasets used by the real neural network project."""
from __future__ import annotations

from typing import Tuple

import numpy as np
from sklearn.datasets import fetch_openml
from sklearn.model_selection import train_test_split


def load_mnist(
    *,
    test_size: float = 0.2,
    random_state: int = 42,
    data_home: str | None = None,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Return train/test splits for the MNIST dataset.

    Parameters
    ----------
    test_size:
        Proportion of the dataset to include in the test split.
    random_state:
        Seed controlling the train/test split.
    data_home:
        Optional directory where the dataset is cached.
    """

    mnist = fetch_openml(
        "mnist_784",
        version=1,
        as_frame=False,
        data_home=data_home,
    )
    X = mnist.data.astype(np.float32) / 255.0
    y = mnist.target.astype(np.int64)
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=test_size, random_state=random_state, stratify=y
    )
    return X_train, X_test, y_train, y_test


__all__ = ["load_mnist"]
