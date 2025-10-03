"""Utilities for downloading and loading the MNIST dataset."""
from __future__ import annotations

import gzip
import os
import struct
import urllib.request
from pathlib import Path
from typing import Tuple

import numpy as np

MNIST_URLS = {
    "train_images": "http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz",
    "train_labels": "http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz",
    "test_images": "http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",
    "test_labels": "http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",
}


def _download(url: str, dest: Path) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.exists():
        return
    with urllib.request.urlopen(url) as response, open(dest, "wb") as fh:
        fh.write(response.read())


def _load_images(path: Path) -> np.ndarray:
    with gzip.open(path, "rb") as fh:
        magic, num, rows, cols = struct.unpack(">IIII", fh.read(16))
        if magic != 2051:
            raise ValueError(f"Invalid magic number {magic} in image file {path}")
        data = np.frombuffer(fh.read(), dtype=np.uint8)
        images = data.reshape(num, rows, cols)
        return images


def _load_labels(path: Path) -> np.ndarray:
    with gzip.open(path, "rb") as fh:
        magic, num = struct.unpack(">II", fh.read(8))
        if magic != 2049:
            raise ValueError(f"Invalid magic number {magic} in label file {path}")
        labels = np.frombuffer(fh.read(), dtype=np.uint8)
        return labels


def load_mnist(data_dir: str | os.PathLike[str], normalize: bool = True) -> Tuple[np.ndarray, np.ndarray, np.ndarray, np.ndarray]:
    """Download (if necessary) and load the MNIST dataset."""
    data_path = Path(data_dir)
    data_path.mkdir(parents=True, exist_ok=True)

    for key, url in MNIST_URLS.items():
        filename = data_path / f"{key}.gz"
        _download(url, filename)

    x_train = _load_images(data_path / "train_images.gz")
    y_train = _load_labels(data_path / "train_labels.gz")
    x_test = _load_images(data_path / "test_images.gz")
    y_test = _load_labels(data_path / "test_labels.gz")

    x_train = x_train.reshape(-1, 1, 28, 28).astype(np.float32)
    x_test = x_test.reshape(-1, 1, 28, 28).astype(np.float32)

    if normalize:
        x_train /= 255.0
        x_test /= 255.0

    return x_train, y_train, x_test, y_test
