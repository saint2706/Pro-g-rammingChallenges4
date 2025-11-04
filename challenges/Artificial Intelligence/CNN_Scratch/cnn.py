"""Convolutional neural network implemented from scratch with NumPy."""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import List, Sequence, Tuple

import numpy as np
from numpy.lib.stride_tricks import sliding_window_view


Array = np.ndarray


def im2col(
    images: Array, filter_h: int, filter_w: int, stride: int, padding: int
) -> Array:
    """Convert image batches into column matrix for convolution operations."""
    n, c, h, w = images.shape
    out_h = (h + 2 * padding - filter_h) // stride + 1
    out_w = (w + 2 * padding - filter_w) // stride + 1

    padded = np.pad(
        images,
        ((0, 0), (0, 0), (padding, padding), (padding, padding)),
        mode="constant",
    )

    cols = np.empty((c * filter_h * filter_w, out_h * out_w * n), dtype=images.dtype)
    col_idx = 0
    for y in range(out_h):
        y_min = y * stride
        y_max = y_min + filter_h
        for x in range(out_w):
            x_min = x * stride
            x_max = x_min + filter_w
            patch = padded[:, :, y_min:y_max, x_min:x_max]
            cols[:, col_idx : col_idx + n] = patch.reshape(n, -1).T
            col_idx += n
    return cols


def col2im(
    cols: Array,
    input_shape: Tuple[int, int, int, int],
    filter_h: int,
    filter_w: int,
    stride: int,
    padding: int,
) -> Array:
    """Inverse of im2col to accumulate gradients back into image space."""
    n, c, h, w = input_shape
    out_h = (h + 2 * padding - filter_h) // stride + 1
    out_w = (w + 2 * padding - filter_w) // stride + 1
    padded = np.zeros((n, c, h + 2 * padding, w + 2 * padding), dtype=cols.dtype)

    col_idx = 0
    for y in range(out_h):
        y_min = y * stride
        y_max = y_min + filter_h
        for x in range(out_w):
            x_min = x * stride
            x_max = x_min + filter_w
            patch = cols[:, col_idx : col_idx + n].T.reshape(n, c, filter_h, filter_w)
            padded[:, :, y_min:y_max, x_min:x_max] += patch
            col_idx += n

    if padding == 0:
        return padded
    return padded[:, :, padding:-padding, padding:-padding]


class Layer:
    def forward(self, x: Array) -> Array:  # pragma: no cover - interface
        raise NotImplementedError

    def backward(
        self, grad: Array, learning_rate: float
    ) -> Array:  # pragma: no cover - interface
        raise NotImplementedError


class Conv2D(Layer):
    def __init__(
        self,
        in_channels: int,
        out_channels: int,
        kernel_size: int,
        stride: int = 1,
        padding: int = 0,
    ):
        self.in_channels = in_channels
        self.out_channels = out_channels
        self.kernel_size = kernel_size
        self.stride = stride
        self.padding = padding
        limit = 1.0 / math.sqrt(in_channels * kernel_size * kernel_size)
        self.weights = np.random.uniform(
            -limit, limit, size=(out_channels, in_channels, kernel_size, kernel_size)
        )
        self.bias = np.zeros(out_channels)
        self._cache: Tuple[Array, Array] | None = None

    def forward(self, x: Array) -> Array:
        cols = im2col(x, self.kernel_size, self.kernel_size, self.stride, self.padding)
        weight_matrix = self.weights.reshape(self.out_channels, -1)
        out = weight_matrix @ cols + self.bias[:, None]
        n, _, h, w = x.shape
        out_h = (h + 2 * self.padding - self.kernel_size) // self.stride + 1
        out_w = (w + 2 * self.padding - self.kernel_size) // self.stride + 1
        out = out.reshape(self.out_channels, out_h, out_w, n).transpose(3, 0, 1, 2)
        self._cache = (x, cols)
        return out

    def backward(self, grad: Array, learning_rate: float) -> Array:
        assert self._cache is not None, "forward must be called before backward"
        x, cols = self._cache
        n = grad.shape[0]
        grad_reshaped = grad.transpose(1, 2, 3, 0).reshape(self.out_channels, -1)

        d_weights = grad_reshaped @ cols.T
        d_weights = d_weights.reshape(self.weights.shape)
        d_bias = grad_reshaped.sum(axis=1)

        weight_matrix = self.weights.reshape(self.out_channels, -1)
        d_cols = weight_matrix.T @ grad_reshaped
        dx = col2im(
            d_cols,
            x.shape,
            self.kernel_size,
            self.kernel_size,
            self.stride,
            self.padding,
        )

        self.weights -= learning_rate * d_weights / n
        self.bias -= learning_rate * d_bias / n

        return dx


class ReLU(Layer):
    def __init__(self) -> None:
        self.mask: Array | None = None

    def forward(self, x: Array) -> Array:
        self.mask = x > 0
        return x * self.mask

    def backward(self, grad: Array, learning_rate: float) -> Array:  # noqa: ARG002 - interface requires parameter
        assert self.mask is not None
        return grad * self.mask


class MaxPool2D(Layer):
    def __init__(self, kernel_size: int, stride: int):
        self.kernel_size = kernel_size
        self.stride = stride
        self._mask_windows: Array | None = None
        self._input_shape: Tuple[int, int, int, int] | None = None

    def forward(self, x: Array) -> Array:
        self._input_shape = x.shape
        windows = sliding_window_view(
            x, (self.kernel_size, self.kernel_size), axis=(2, 3)
        )
        windows = windows[:, :, :: self.stride, :: self.stride, :, :]
        pooled = windows.max(axis=(-1, -2))
        self._mask_windows = windows == pooled[..., None, None]
        return pooled

    def backward(self, grad: Array, learning_rate: float) -> Array:  # noqa: ARG002 - interface requires parameter
        assert self._mask_windows is not None and self._input_shape is not None
        n, c, h, w = self._input_shape
        dx = np.zeros((n, c, h, w), dtype=grad.dtype)

        grad_expanded = grad[..., None, None] * self._mask_windows
        for i in range(self.kernel_size):
            for j in range(self.kernel_size):
                dx[:, :, i :: self.stride, j :: self.stride] += grad_expanded[..., i, j]
        return dx


class Flatten(Layer):
    def __init__(self) -> None:
        self._shape: Tuple[int, ...] | None = None

    def forward(self, x: Array) -> Array:
        self._shape = x.shape
        return x.reshape(x.shape[0], -1)

    def backward(self, grad: Array, learning_rate: float) -> Array:  # noqa: ARG002 - interface requires parameter
        assert self._shape is not None
        return grad.reshape(self._shape)


class Dense(Layer):
    def __init__(self, in_features: int, out_features: int):
        limit = 1.0 / math.sqrt(in_features)
        self.weights = np.random.uniform(
            -limit, limit, size=(in_features, out_features)
        )
        self.bias = np.zeros(out_features)
        self._input: Array | None = None

    def forward(self, x: Array) -> Array:
        self._input = x
        return x @ self.weights + self.bias

    def backward(self, grad: Array, learning_rate: float) -> Array:
        assert self._input is not None
        n = grad.shape[0]
        d_weights = self._input.T @ grad / n
        d_bias = grad.mean(axis=0)
        dx = grad @ self.weights.T
        self.weights -= learning_rate * d_weights
        self.bias -= learning_rate * d_bias
        return dx


class SoftmaxCrossEntropy:
    def __init__(self) -> None:
        self._probs: Array | None = None
        self._labels: Array | None = None

    def forward(self, logits: Array, labels: Array) -> float:
        shifted = logits - logits.max(axis=1, keepdims=True)
        exp = np.exp(shifted)
        probs = exp / exp.sum(axis=1, keepdims=True)
        n = logits.shape[0]
        self._probs = probs
        self._labels = labels
        correct_logprobs = -np.log(probs[np.arange(n), labels] + 1e-12)
        return float(correct_logprobs.mean())

    def backward(self) -> Array:
        assert self._probs is not None and self._labels is not None
        probs = self._probs.copy()
        n = probs.shape[0]
        probs[np.arange(n), self._labels] -= 1
        return probs / n


@dataclass
class TrainingHistory:
    losses: List[float]
    accuracies: List[float]


class SimpleCNN:
    """A minimal convolutional neural network for MNIST-sized images."""

    def __init__(
        self,
        input_shape: Tuple[int, int, int] = (1, 28, 28),
        num_classes: int = 10,
        conv_channels: Sequence[int] = (8, 16),
        hidden_features: int = 64,
    ) -> None:
        c, h, w = input_shape
        if len(conv_channels) != 2:
            raise ValueError("conv_channels must contain exactly two entries")
        conv1_channels, conv2_channels = conv_channels

        self.conv1 = Conv2D(c, conv1_channels, kernel_size=3, stride=1, padding=1)
        self.relu1 = ReLU()
        self.pool1 = MaxPool2D(kernel_size=2, stride=2)
        self.conv2 = Conv2D(
            conv1_channels, conv2_channels, kernel_size=3, stride=1, padding=1
        )
        self.relu2 = ReLU()
        self.pool2 = MaxPool2D(kernel_size=2, stride=2)
        self.flatten = Flatten()

        h_out = self._pool_output_dim(self._conv_output_dim(h, 3, 1, 1), 2, 2)
        h_out = self._pool_output_dim(self._conv_output_dim(h_out, 3, 1, 1), 2, 2)
        w_out = self._pool_output_dim(self._conv_output_dim(w, 3, 1, 1), 2, 2)
        w_out = self._pool_output_dim(self._conv_output_dim(w_out, 3, 1, 1), 2, 2)
        flattened = conv2_channels * h_out * w_out

        self.fc1 = Dense(flattened, hidden_features)
        self.relu3 = ReLU()
        self.fc2 = Dense(hidden_features, num_classes)
        self.loss_fn = SoftmaxCrossEntropy()

        self.layers: List[Layer] = [
            self.conv1,
            self.relu1,
            self.pool1,
            self.conv2,
            self.relu2,
            self.pool2,
            self.flatten,
            self.fc1,
            self.relu3,
            self.fc2,
        ]

    @staticmethod
    def _conv_output_dim(size: int, kernel: int, stride: int, padding: int) -> int:
        return (size + 2 * padding - kernel) // stride + 1

    @staticmethod
    def _pool_output_dim(size: int, kernel: int, stride: int) -> int:
        return (size - kernel) // stride + 1

    def forward(self, x: Array) -> Array:
        for layer in self.layers:
            x = layer.forward(x)
        return x

    def backward(self, grad: Array, learning_rate: float) -> None:
        for layer in reversed(self.layers):
            grad = layer.backward(grad, learning_rate)

    def predict(self, x: Array) -> Array:
        logits = self.forward(x)
        return np.argmax(logits, axis=1)

    def accuracy(self, x: Array, y: Array) -> float:
        preds = self.predict(x)
        return float((preds == y).mean())

    def train_epoch(
        self, x: Array, y: Array, batch_size: int, learning_rate: float
    ) -> Tuple[float, float]:
        indices = np.arange(len(x))
        np.random.shuffle(indices)
        x = x[indices]
        y = y[indices]

        losses: List[float] = []
        correct = 0
        for start in range(0, len(x), batch_size):
            end = start + batch_size
            batch_x = x[start:end]
            batch_y = y[start:end]
            logits = self.forward(batch_x)
            loss = self.loss_fn.forward(logits, batch_y)
            grad = self.loss_fn.backward()
            self.backward(grad, learning_rate)
            losses.append(loss)
            correct += (np.argmax(logits, axis=1) == batch_y).sum()
        accuracy = correct / len(x)
        return float(np.mean(losses)), float(accuracy)

    def fit(
        self,
        x_train: Array,
        y_train: Array,
        x_val: Array | None = None,
        y_val: Array | None = None,
        epochs: int = 5,
        batch_size: int = 32,
        learning_rate: float = 0.01,
    ) -> TrainingHistory:
        history = TrainingHistory([], [])
        for _ in range(epochs):
            loss, acc = self.train_epoch(x_train, y_train, batch_size, learning_rate)
            history.losses.append(loss)
            if x_val is not None and y_val is not None:
                history.accuracies.append(self.accuracy(x_val, y_val))
            else:
                history.accuracies.append(acc)
        return history

    def save(self, path: str) -> None:
        np.savez(
            path,
            conv1_weights=self.conv1.weights,
            conv1_bias=self.conv1.bias,
            conv2_weights=self.conv2.weights,
            conv2_bias=self.conv2.bias,
            fc1_weights=self.fc1.weights,
            fc1_bias=self.fc1.bias,
            fc2_weights=self.fc2.weights,
            fc2_bias=self.fc2.bias,
        )

    def load(self, path: str) -> None:
        data = np.load(path)
        self.conv1.weights = data["conv1_weights"]
        self.conv1.bias = data["conv1_bias"]
        self.conv2.weights = data["conv2_weights"]
        self.conv2.bias = data["conv2_bias"]
        self.fc1.weights = data["fc1_weights"]
        self.fc1.bias = data["fc1_bias"]
        self.fc2.weights = data["fc2_weights"]
        self.fc2.bias = data["fc2_bias"]
