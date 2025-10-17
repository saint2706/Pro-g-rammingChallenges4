"""Model definition for the CNN framework."""

from __future__ import annotations

import torch
from torch import nn


class MNISTConvNet(nn.Module):
    """A compact convolutional neural network for MNIST."""

    def __init__(self) -> None:
        super().__init__()
        self.features = nn.Sequential(
            nn.Conv2d(1, 32, kernel_size=3, padding=1),
            nn.ReLU(inplace=True),
            nn.Conv2d(32, 64, kernel_size=3, padding=1),
            nn.ReLU(inplace=True),
            nn.MaxPool2d(2),
            nn.Dropout(0.25),
        )
        self.classifier = nn.Sequential(
            nn.Flatten(),
            nn.Linear(64 * 14 * 14, 128),
            nn.ReLU(inplace=True),
            nn.Dropout(0.5),
            nn.Linear(128, 10),
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:  # type: ignore[override]
        x = self.features(x)
        return self.classifier(x)


__all__ = ["MNISTConvNet"]
