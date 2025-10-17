"""CNN Framework for MNIST classification using PyTorch."""

from .data import create_data_loaders
from .model import MNISTConvNet
from .train import TrainConfig, train_model
from .evaluate import evaluate_model

__all__ = [
    "create_data_loaders",
    "MNISTConvNet",
    "TrainConfig",
    "train_model",
    "evaluate_model",
]
