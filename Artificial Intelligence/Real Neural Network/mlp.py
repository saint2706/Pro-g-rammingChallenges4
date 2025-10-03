"""Multilayer perceptron implementation using NumPy only.

This module exposes the :class:`MLP` class which performs forward and
backward propagation using matrix operations.  It supports multi-class
classification with cross-entropy loss and softmax output activation.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Tuple

import numpy as np


ActivationCache = Tuple[np.ndarray, np.ndarray]


def _relu(z: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    activated = np.maximum(0.0, z)
    derivative = (z > 0).astype(z.dtype)
    return activated, derivative


def _tanh(z: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    activated = np.tanh(z)
    derivative = 1.0 - activated**2
    return activated, derivative


def _sigmoid(z: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    activated = 1.0 / (1.0 + np.exp(-z))
    derivative = activated * (1.0 - activated)
    return activated, derivative


_ACTIVATIONS = {
    "relu": _relu,
    "tanh": _tanh,
    "sigmoid": _sigmoid,
}


@dataclass
class TrainingHistory:
    """Container holding metrics captured during :meth:`MLP.fit`."""

    loss: List[float]
    accuracy: List[float]
    val_loss: List[float]
    val_accuracy: List[float]


class MLP:
    """Configurable multilayer perceptron.

    Parameters
    ----------
    layer_sizes:
        Sequence describing the network structure including the input and
        output dimensions.  Example: ``[784, 128, 64, 10]``.
    hidden_activation:
        Activation function for hidden layers (``relu``, ``tanh`` or
        ``sigmoid``).
    learning_rate:
        Step size for gradient descent.
    l2:
        Optional L2 regularisation coefficient.
    seed:
        Optional random seed to make weight initialisation deterministic.
    """

    def __init__(
        self,
        layer_sizes: Sequence[int],
        *,
        hidden_activation: str = "relu",
        learning_rate: float = 0.01,
        l2: float = 0.0,
        seed: Optional[int] = None,
    ) -> None:
        if len(layer_sizes) < 2:
            raise ValueError("layer_sizes must contain input and output dimensions")
        if hidden_activation not in _ACTIVATIONS:
            raise ValueError(f"Unsupported activation '{hidden_activation}'")

        self.layer_sizes = list(layer_sizes)
        self.hidden_activation_name = hidden_activation
        self.hidden_activation = _ACTIVATIONS[hidden_activation]
        self.learning_rate = float(learning_rate)
        self.l2 = float(l2)
        self.rng = np.random.default_rng(seed)

        self.weights: List[np.ndarray] = []
        self.biases: List[np.ndarray] = []
        self._initialise_parameters()

    # ------------------------------------------------------------------
    # Initialisation and serialization
    # ------------------------------------------------------------------
    def _initialise_parameters(self) -> None:
        self.weights.clear()
        self.biases.clear()
        for fan_in, fan_out in zip(self.layer_sizes[:-1], self.layer_sizes[1:]):
            limit = np.sqrt(6.0 / (fan_in + fan_out))
            self.weights.append(self.rng.uniform(-limit, limit, size=(fan_in, fan_out)))
            self.biases.append(np.zeros((1, fan_out)))

    def to_dict(self) -> Dict[str, np.ndarray]:
        data: Dict[str, np.ndarray] = {
            "layer_sizes": np.array(self.layer_sizes, dtype=np.int64),
        }
        for idx, (w, b) in enumerate(zip(self.weights, self.biases)):
            data[f"W{idx}"] = w
            data[f"b{idx}"] = b
        return data

    def save(self, path: str | "os.PathLike[str]") -> None:
        np.savez_compressed(path, **self.to_dict())

    @classmethod
    def load(
        cls,
        path: str | "os.PathLike[str]",
        *,
        hidden_activation: str = "relu",
        learning_rate: float = 0.01,
        l2: float = 0.0,
    ) -> "MLP":
        data = np.load(path, allow_pickle=False)
        layer_sizes = data["layer_sizes"].tolist()
        network = cls(
            layer_sizes,
            hidden_activation=hidden_activation,
            learning_rate=learning_rate,
            l2=l2,
        )
        for idx in range(len(layer_sizes) - 1):
            network.weights[idx] = data[f"W{idx}"]
            network.biases[idx] = data[f"b{idx}"]
        return network

    # ------------------------------------------------------------------
    # Core API
    # ------------------------------------------------------------------
    def forward(self, X: np.ndarray) -> Tuple[np.ndarray, List[ActivationCache]]:
        activations: List[ActivationCache] = []
        a = X
        for idx, (w, b) in enumerate(zip(self.weights, self.biases)):
            z = a @ w + b
            if idx == len(self.weights) - 1:
                # Output layer -> softmax
                exp_z = np.exp(z - np.max(z, axis=1, keepdims=True))
                a = exp_z / np.sum(exp_z, axis=1, keepdims=True)
                derivative = a * (1.0 - a)
            else:
                a, derivative = self.hidden_activation(z)
            activations.append((a, derivative))
        return a, activations

    def predict_proba(self, X: np.ndarray) -> np.ndarray:
        probs, _ = self.forward(X)
        return probs

    def predict(self, X: np.ndarray) -> np.ndarray:
        return np.argmax(self.predict_proba(X), axis=1)

    def score(self, X: np.ndarray, y: np.ndarray) -> float:
        return float(np.mean(self.predict(X) == y))

    # ------------------------------------------------------------------
    def _one_hot(self, y: np.ndarray) -> np.ndarray:
        n_classes = self.layer_sizes[-1]
        one_hot = np.zeros((y.shape[0], n_classes), dtype=np.float64)
        one_hot[np.arange(y.shape[0]), y.astype(int)] = 1.0
        return one_hot

    def _backward(
        self,
        X: np.ndarray,
        y: np.ndarray,
        activations: List[ActivationCache],
    ) -> Tuple[List[np.ndarray], List[np.ndarray]]:
        grads_w = [np.zeros_like(w) for w in self.weights]
        grads_b = [np.zeros_like(b) for b in self.biases]

        one_hot = self._one_hot(y)
        probs = activations[-1][0]
        delta = (probs - one_hot) / X.shape[0]

        for layer in reversed(range(len(self.weights))):
            a_prev = X if layer == 0 else activations[layer - 1][0]
            grads_w[layer] = a_prev.T @ delta + self.l2 * self.weights[layer]
            grads_b[layer] = np.sum(delta, axis=0, keepdims=True)
            if layer != 0:
                w = self.weights[layer]
                derivative = activations[layer - 1][1]
                delta = (delta @ w.T) * derivative
        return grads_w, grads_b

    def _update_params(
        self,
        grads_w: Sequence[np.ndarray],
        grads_b: Sequence[np.ndarray],
    ) -> None:
        for idx, (gw, gb) in enumerate(zip(grads_w, grads_b)):
            self.weights[idx] -= self.learning_rate * gw
            self.biases[idx] -= self.learning_rate * gb

    def _loss(self, probs: np.ndarray, y: np.ndarray) -> float:
        one_hot = self._one_hot(y)
        eps = 1e-12
        loss = -np.sum(one_hot * np.log(probs + eps)) / y.shape[0]
        if self.l2:
            loss += (self.l2 / 2.0) * sum(np.sum(w**2) for w in self.weights)
        return float(loss)

    def fit(
        self,
        X: np.ndarray,
        y: np.ndarray,
        *,
        epochs: int = 10,
        batch_size: int = 32,
        validation_data: Optional[Tuple[np.ndarray, np.ndarray]] = None,
        shuffle: bool = True,
        verbose: bool = False,
    ) -> TrainingHistory:
        if X.shape[0] != y.shape[0]:
            raise ValueError("X and y must contain the same number of samples")
        n_samples = X.shape[0]
        history = TrainingHistory([], [], [], [])

        for epoch in range(epochs):
            indices = np.arange(n_samples)
            if shuffle:
                self.rng.shuffle(indices)
                X, y = X[indices], y[indices]

            for start in range(0, n_samples, batch_size):
                end = start + batch_size
                batch_X = X[start:end]
                batch_y = y[start:end]
                probs, activations = self.forward(batch_X)
                grads_w, grads_b = self._backward(batch_X, batch_y, activations)
                self._update_params(grads_w, grads_b)

            train_probs, _ = self.forward(X)
            train_loss = self._loss(train_probs, y)
            train_acc = float(np.mean(np.argmax(train_probs, axis=1) == y))

            if validation_data is not None:
                val_X, val_y = validation_data
                val_probs, _ = self.forward(val_X)
                val_loss = self._loss(val_probs, val_y)
                val_acc = float(np.mean(np.argmax(val_probs, axis=1) == val_y))
            else:
                val_loss = float("nan")
                val_acc = float("nan")

            history.loss.append(train_loss)
            history.accuracy.append(train_acc)
            history.val_loss.append(val_loss)
            history.val_accuracy.append(val_acc)

            if verbose:
                if validation_data is not None:
                    print(
                        f"Epoch {epoch + 1}/{epochs}: "
                        f"loss={train_loss:.4f}, acc={train_acc:.3f}, "
                        f"val_loss={val_loss:.4f}, val_acc={val_acc:.3f}"
                    )
                else:
                    print(
                        f"Epoch {epoch + 1}/{epochs}: loss={train_loss:.4f}, "
                        f"acc={train_acc:.3f}"
                    )

        return history

    def evaluate(self, X: np.ndarray, y: np.ndarray) -> Tuple[float, float]:
        probs, _ = self.forward(X)
        loss = self._loss(probs, y)
        acc = float(np.mean(np.argmax(probs, axis=1) == y))
        return loss, acc


__all__ = ["MLP", "TrainingHistory"]
