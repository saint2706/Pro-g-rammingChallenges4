"""Evaluation helpers and CLI for the CNN framework."""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Dict, Optional

from torch import nn

from .data import create_data_loaders
from .model import MNISTConvNet
from .utils import evaluate, get_device, load_checkpoint


def evaluate_model(
    checkpoint_path: Path,
    data_dir: Path,
    batch_size: int = 128,
    num_workers: int = 0,
    prefer_gpu: bool = True,
    eval_limit: Optional[int] = None,
) -> Dict[str, float]:
    """Evaluate a saved checkpoint and return metrics.

    Args:
        checkpoint_path: Path to the saved model checkpoint.
        data_dir: Directory containing the MNIST dataset.
        batch_size: Batch size for evaluation.
        num_workers: Number of worker processes for data loading.
        prefer_gpu: Whether to prefer GPU for evaluation.
        eval_limit: Optional limit on the number of evaluation samples.

    Returns:
        A dictionary containing the evaluation metrics (loss and accuracy).
    """

    device = get_device(prefer_gpu=prefer_gpu)
    # Create the evaluation data loader
    _, eval_loader = create_data_loaders(
        data_dir=data_dir,
        batch_size=batch_size,
        num_workers=num_workers,
        train_limit=None,
        eval_limit=eval_limit,
    )

    # Initialize the model and load the checkpoint
    model = MNISTConvNet().to(device)
    load_checkpoint(model, checkpoint_path, map_location=device)
    loss_fn: nn.Module = nn.CrossEntropyLoss()
    # Evaluate the model
    metrics = evaluate(model, eval_loader, device=device, loss_fn=loss_fn)
    return metrics


def _parse_args(argv: Optional[list[str]] = None) -> argparse.Namespace:
    """Parses command-line arguments for the evaluation script."""
    parser = argparse.ArgumentParser(
        description="Evaluate a trained MNIST CNN checkpoint."
    )
    parser.add_argument("checkpoint", type=Path, help="Path to the saved checkpoint")
    parser.add_argument(
        "--data-dir", type=Path, default=Path("./data"), help="Directory for MNIST data"
    )
    parser.add_argument(
        "--batch-size", type=int, default=256, help="Batch size for evaluation"
    )
    parser.add_argument(
        "--num-workers", type=int, default=0, help="Worker processes for data loading"
    )
    parser.add_argument("--cpu", action="store_true", help="Force CPU evaluation")
    parser.add_argument(
        "--eval-limit",
        type=int,
        default=None,
        help="Limit number of evaluation samples",
    )
    return parser.parse_args(argv)


def main(argv: Optional[list[str]] = None) -> Dict[str, float]:
    """Main entry point for the evaluation script.

    Args:
        argv: Optional list of command-line arguments.

    Returns:
        A dictionary containing the evaluation metrics.
    """
    args = _parse_args(argv)
    metrics = evaluate_model(
        checkpoint_path=args.checkpoint,
        data_dir=args.data_dir,
        batch_size=args.batch_size,
        num_workers=args.num_workers,
        prefer_gpu=not args.cpu,
        eval_limit=args.eval_limit,
    )
    print("Evaluation:", metrics)
    return metrics


if __name__ == "__main__":
    main()
