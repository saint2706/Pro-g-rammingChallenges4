"""Training entrypoint for the CNN framework."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Optional

import torch
from torch import nn

from .data import create_data_loaders
from .model import MNISTConvNet
from .utils import evaluate, get_device, save_checkpoint


@dataclass
class TrainConfig:
    """Configuration for training the MNIST CNN."""

    data_dir: Path
    batch_size: int = 64
    epochs: int = 5
    learning_rate: float = 1e-3
    num_workers: int = 0
    checkpoint_path: Path = Path("checkpoints/mnist_cnn.pt")
    prefer_gpu: bool = True
    train_limit: Optional[int] = None
    eval_limit: Optional[int] = None
    max_steps_per_epoch: Optional[int] = None


def train_model(config: TrainConfig) -> Dict[str, float]:
    """Train the MNIST CNN according to ``config`` and return evaluation metrics."""

    device = get_device(prefer_gpu=config.prefer_gpu)
    train_loader, eval_loader = create_data_loaders(
        data_dir=config.data_dir,
        batch_size=config.batch_size,
        num_workers=config.num_workers,
        train_limit=config.train_limit,
        eval_limit=config.eval_limit,
    )

    model = MNISTConvNet().to(device)
    optimizer = torch.optim.Adam(model.parameters(), lr=config.learning_rate)
    loss_fn: nn.Module = nn.CrossEntropyLoss()

    for epoch in range(1, config.epochs + 1):
        model.train()
        running_loss = 0.0
        step = 0
        for step, (inputs, targets) in enumerate(train_loader, start=1):
            inputs = inputs.to(device)
            targets = targets.to(device)

            optimizer.zero_grad()
            outputs = model(inputs)
            loss = loss_fn(outputs, targets)
            loss.backward()
            optimizer.step()

            running_loss += float(loss.item())

            if config.max_steps_per_epoch is not None and step >= config.max_steps_per_epoch:
                break

        metrics = evaluate(model, eval_loader, device=device, loss_fn=loss_fn)
        print(
            f"Epoch {epoch}/{config.epochs}: train_loss={running_loss / max(step, 1):.4f} "
            f"eval_loss={metrics['loss']:.4f} eval_acc={metrics['accuracy']:.4f}"
        )

    save_checkpoint(model, config.checkpoint_path)
    return metrics


def _parse_args(argv: Optional[list[str]] = None) -> TrainConfig:
    parser = argparse.ArgumentParser(description="Train a convolutional network on MNIST.")
    parser.add_argument("--data-dir", type=Path, default=Path("./data"), help="Directory for MNIST data")
    parser.add_argument("--batch-size", type=int, default=64, help="Training batch size")
    parser.add_argument("--epochs", type=int, default=5, help="Number of training epochs")
    parser.add_argument("--learning-rate", type=float, default=1e-3, help="Optimizer learning rate")
    parser.add_argument("--num-workers", type=int, default=0, help="Data loader worker processes")
    parser.add_argument(
        "--checkpoint-path",
        type=Path,
        default=Path("checkpoints/mnist_cnn.pt"),
        help="Path for saving the trained weights",
    )
    parser.add_argument("--cpu", action="store_true", help="Force CPU training even if GPU is available")
    parser.add_argument("--train-limit", type=int, default=None, help="Limit number of training samples")
    parser.add_argument("--eval-limit", type=int, default=None, help="Limit number of evaluation samples")

    args = parser.parse_args(argv)

    return TrainConfig(
        data_dir=args.data_dir,
        batch_size=args.batch_size,
        epochs=args.epochs,
        learning_rate=args.learning_rate,
        num_workers=args.num_workers,
        checkpoint_path=args.checkpoint_path,
        prefer_gpu=not args.cpu,
        train_limit=args.train_limit,
        eval_limit=args.eval_limit,
    )


def main(argv: Optional[list[str]] = None) -> Dict[str, float]:
    config = _parse_args(argv)
    metrics = train_model(config)
    print("Final evaluation:", metrics)
    return metrics


if __name__ == "__main__":
    main()
