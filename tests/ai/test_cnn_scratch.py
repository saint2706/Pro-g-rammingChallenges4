"""Regression tests for the scratch CNN implementation."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import numpy as np


def _load_simple_cnn():
    module_name = "cnn_scratch_module"
    if module_name in sys.modules:
        return sys.modules[module_name].SimpleCNN
    module_path = (
        Path(__file__).resolve().parents[2]
        / "challenges"
        / "Artificial Intelligence"
        / "CNN_Scratch"
        / "cnn.py"
    )
    spec = importlib.util.spec_from_file_location(module_name, module_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    assert spec and spec.loader
    spec.loader.exec_module(module)
    return module.SimpleCNN


SimpleCNN = _load_simple_cnn()


def test_forward_shape() -> None:
    np.random.seed(0)
    model = SimpleCNN()
    batch = np.random.rand(4, 1, 28, 28).astype(np.float32)
    logits = model.forward(batch)
    assert logits.shape == (4, 10)


def synthetic_dataset(samples: int = 32) -> tuple[np.ndarray, np.ndarray]:
    x = np.zeros((samples, 1, 8, 8), dtype=np.float32)
    y = np.zeros(samples, dtype=np.int64)
    for i in range(samples):
        if i % 2 == 0:
            x[i, 0, :4, :4] = 1.0
            y[i] = 0
        else:
            x[i, 0, 4:, 4:] = 1.0
            y[i] = 1
    return x, y


def test_brief_training_reduces_loss() -> None:
    np.random.seed(1)
    x, y = synthetic_dataset()
    model = SimpleCNN(
        input_shape=(1, 8, 8), num_classes=2, conv_channels=(4, 8), hidden_features=16
    )

    initial_logits = model.forward(x)
    initial_loss = model.loss_fn.forward(initial_logits, y)

    for _ in range(15):
        model.train_epoch(x, y, batch_size=8, learning_rate=0.1)

    final_logits = model.forward(x)
    final_loss = model.loss_fn.forward(final_logits, y)

    assert final_loss < initial_loss
