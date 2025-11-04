"""Model definition for the CNN framework."""

from __future__ import annotations

import torch
from torch import nn


class MNISTConvNet(nn.Module):
    """A compact convolutional neural network for MNIST.

    This model consists of a feature extractor and a classifier. The feature
    extractor is a sequence of convolutional, ReLU, and max-pooling layers.
    The classifier is a sequence of fully connected layers with dropout.
    """

    def __init__(self) -> None:
        """Initializes the MNISTConvNet."""
        super().__init__()
        # Feature extractor
        self.features = nn.Sequential(
            nn.Conv2d(1, 32, kernel_size=3, padding=1),
            nn.ReLU(inplace=True),
            nn.Conv2d(32, 64, kernel_size=3, padding=1),
            nn.ReLU(inplace=True),
            nn.MaxPool2d(2),
            nn.Dropout(0.25),
        )
        # Classifier
        self.classifier = nn.Sequential(
            nn.Flatten(),
            nn.Linear(64 * 14 * 14, 128),
            nn.ReLU(inplace=True),
            nn.Dropout(0.5),
            nn.Linear(128, 10),
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:  # type: ignore[override]
        """Defines the forward pass of the model.

        Args:
            x: The input tensor.

        Returns:
            The output tensor.
        """
        x = self.features(x)
        return self.classifier(x)


__all__ = ["MNISTConvNet"]
