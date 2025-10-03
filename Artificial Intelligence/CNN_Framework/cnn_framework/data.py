"""Dataset utilities for the CNN framework."""

from __future__ import annotations

from pathlib import Path
from typing import Optional, Tuple

from torch.utils.data import DataLoader, Subset
from torchvision import datasets, transforms


def _maybe_limit(
    dataset: datasets.MNIST, limit: Optional[int]
) -> datasets.MNIST | Subset[datasets.MNIST]:
    """Return a subset of ``dataset`` limited to ``limit`` samples if provided."""

    if limit is None:
        return dataset
    return Subset(dataset, list(range(min(limit, len(dataset)))))


def create_data_loaders(
    data_dir: str | Path,
    batch_size: int = 64,
    num_workers: int = 0,
    train_limit: Optional[int] = None,
    eval_limit: Optional[int] = None,
) -> Tuple[DataLoader, DataLoader]:
    """Create MNIST train and evaluation data loaders.

    Parameters
    ----------
    data_dir:
        Directory used for storing the MNIST dataset.
    batch_size:
        Number of samples per batch.
    num_workers:
        Number of worker processes for data loading.
    train_limit:
        Optional limit on the number of training samples.
    eval_limit:
        Optional limit on the number of evaluation samples.
    """

    data_path = Path(data_dir)
    transform = transforms.Compose(
        [
            transforms.ToTensor(),
            transforms.Normalize((0.1307,), (0.3081,)),
        ]
    )

    train_dataset = datasets.MNIST(
        root=str(data_path),
        train=True,
        download=True,
        transform=transform,
    )
    eval_dataset = datasets.MNIST(
        root=str(data_path),
        train=False,
        download=True,
        transform=transform,
    )

    train_dataset = _maybe_limit(train_dataset, train_limit)
    eval_dataset = _maybe_limit(eval_dataset, eval_limit)

    train_loader = DataLoader(
        train_dataset,
        batch_size=batch_size,
        shuffle=True,
        num_workers=num_workers,
        pin_memory=False,
    )
    eval_loader = DataLoader(
        eval_dataset,
        batch_size=batch_size,
        shuffle=False,
        num_workers=num_workers,
        pin_memory=False,
    )

    return train_loader, eval_loader


__all__ = ["create_data_loaders"]
