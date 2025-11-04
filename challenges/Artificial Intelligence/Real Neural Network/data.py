"""Utilities for loading datasets used by the real neural network project."""

from __future__ import annotations

from pathlib import Path
from typing import Tuple

import numpy as np
from sklearn.datasets import fetch_openml
from sklearn.model_selection import train_test_split


DEFAULT_DATA_HOME = Path.home() / ".cache" / "pro_g_ai"


def load_mnist(
    *,
    test_size: float = 0.2,
    random_state: int = 42,
    data_home: str | Path | None = None,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Return train/test splits for the MNIST dataset.

    This function downloads the MNIST dataset from OpenML, caches it locally,
    and splits it into training and testing sets. The pixel values are
    normalized to the range [0, 1].

    Args:
        test_size: Proportion of the dataset to include in the test split.
        random_state: Seed controlling the train/test split.
        data_home: Optional directory where the dataset is cached.

    Returns:
        A tuple containing the training data, test data, training labels,
        and test labels.
    """

    cache_dir = (
        Path(data_home).expanduser() if data_home is not None else DEFAULT_DATA_HOME
    )
    cache_dir.mkdir(parents=True, exist_ok=True)

    # Fetch the MNIST dataset from OpenML
    mnist = fetch_openml(
        "mnist_784",
        version=1,
        as_frame=False,
        data_home=str(cache_dir),
        cache=True,
    )
    # Normalize pixel values and convert labels to integers
    X = mnist.data.astype(np.float32) / 255.0
    y = mnist.target.astype(np.int64)
    # Split the data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=test_size, random_state=random_state, stratify=y
    )
    return X_train, X_test, y_train, y_test


__all__ = ["load_mnist"]
