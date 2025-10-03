"""Smoke tests for the CNN Framework project."""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

try:  # pragma: no cover - optional dependency guard
    import torch  # noqa: F401
    import torchvision  # noqa: F401
except ModuleNotFoundError as exc:  # pragma: no cover - skip when ai extra not installed
    pytest.skip(f"PyTorch stack not available: {exc}", allow_module_level=True)

PACKAGE_ROOT = Path(__file__).resolve().parents[1] / "Artificial Intelligence" / "CNN_Framework"
if str(PACKAGE_ROOT) not in sys.path:
    sys.path.insert(0, str(PACKAGE_ROOT))

from cnn_framework.train import TrainConfig, train_model
from cnn_framework.evaluate import evaluate_model


@pytest.mark.smoke
def test_single_step_train_and_eval(tmp_path: Path) -> None:
    data_dir = tmp_path / "data"
    checkpoint = tmp_path / "checkpoints" / "mnist_cnn.pt"

    config = TrainConfig(
        data_dir=data_dir,
        batch_size=16,
        epochs=1,
        learning_rate=1e-3,
        num_workers=0,
        checkpoint_path=checkpoint,
        prefer_gpu=False,
        train_limit=32,
        eval_limit=32,
        max_steps_per_epoch=1,
    )

    metrics = train_model(config)

    assert checkpoint.exists(), "Training should produce a checkpoint file"
    assert 0.0 <= metrics["accuracy"] <= 1.0

    eval_metrics = evaluate_model(
        checkpoint_path=checkpoint,
        data_dir=data_dir,
        batch_size=16,
        num_workers=0,
        prefer_gpu=False,
        eval_limit=16,
    )

    assert 0.0 <= eval_metrics["accuracy"] <= 1.0
