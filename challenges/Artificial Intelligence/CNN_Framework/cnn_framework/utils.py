"""Utility helpers for the CNN framework."""

from __future__ import annotations

from pathlib import Path
from typing import Dict, Optional

import torch
from torch import nn
from torch.utils.data import DataLoader
from torch.optim import Optimizer


def get_device(prefer_gpu: bool = True) -> torch.device:
    """Return an available computation device."""

    if prefer_gpu and torch.cuda.is_available():
        return torch.device("cuda")
    if prefer_gpu and torch.backends.mps.is_available():  # type: ignore[attr-defined]
        return torch.device("mps")
    return torch.device("cpu")


def accuracy(logits: torch.Tensor, labels: torch.Tensor) -> torch.Tensor:
    """Compute batch accuracy."""

    preds = logits.argmax(dim=1)
    correct = (preds == labels).float().sum()
    return correct / labels.shape[0]


def evaluate(
    model: nn.Module,
    data_loader: DataLoader,
    device: torch.device,
    loss_fn: nn.Module,
) -> Dict[str, float]:
    """Evaluate ``model`` and return loss/accuracy metrics."""

    model.eval()
    total_loss = 0.0
    total_correct = 0.0
    total_examples = 0

    with torch.no_grad():
        for inputs, targets in data_loader:
            inputs = inputs.to(device)
            targets = targets.to(device)
            outputs = model(inputs)
            loss = loss_fn(outputs, targets)

            batch_size = targets.size(0)
            total_loss += float(loss.item()) * batch_size
            total_correct += float((outputs.argmax(dim=1) == targets).sum().item())
            total_examples += batch_size

    if total_examples == 0:
        return {"loss": 0.0, "accuracy": 0.0}

    return {
        "loss": total_loss / total_examples,
        "accuracy": total_correct / total_examples,
    }


def save_checkpoint(
    model: nn.Module,
    path: str | Path,
    *,
    optimizer: Optional[Optimizer] = None,
    metadata: Optional[Dict[str, object]] = None,
) -> None:
    """Save ``model`` weights (and optional optimizer state) to ``path``."""

    Path(path).parent.mkdir(parents=True, exist_ok=True)
    checkpoint: Dict[str, object] = {"model_state": model.state_dict()}
    if optimizer is not None:
        checkpoint["optimizer_state"] = optimizer.state_dict()
    if metadata is not None:
        checkpoint["metadata"] = metadata
    torch.save(checkpoint, str(path))


def load_checkpoint(
    model: nn.Module,
    path: str | Path,
    map_location: str | torch.device | None = None,
    optimizer: Optional[Optimizer] = None,
) -> nn.Module:
    """Load weights (and optional optimizer state) from ``path`` into ``model``."""

    checkpoint = torch.load(str(path), map_location=map_location)
    if "model_state" in checkpoint:
        model.load_state_dict(checkpoint["model_state"])
        if optimizer is not None and "optimizer_state" in checkpoint:
            optimizer.load_state_dict(checkpoint["optimizer_state"])
    else:
        model.load_state_dict(checkpoint)
    return model


__all__ = ["get_device", "accuracy", "evaluate", "save_checkpoint", "load_checkpoint"]
