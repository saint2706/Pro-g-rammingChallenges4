from __future__ import annotations

import importlib.util
from pathlib import Path

import numpy as np


def load_mlp_module():
    module_path = (
        Path(__file__).resolve().parents[2]
        / "Artificial Intelligence"
        / "Real Neural Network"
        / "mlp.py"
    )
    spec = importlib.util.spec_from_file_location("mlp", module_path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    import sys

    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


mlp_module = load_mlp_module()
MLP = mlp_module.MLP


def test_forward_pass_shape():
    model = MLP([3, 5, 2], seed=0)
    X = np.random.default_rng(0).normal(size=(7, 3))
    probs, _ = model.forward(X)
    assert probs.shape == (7, 2)
    np.testing.assert_allclose(probs.sum(axis=1), 1.0, rtol=1e-6, atol=1e-6)


def test_training_improves_loss():
    X = np.array(
        [
            [0.0, 0.0],
            [0.0, 1.0],
            [1.0, 0.0],
            [1.0, 1.0],
        ],
        dtype=np.float64,
    )
    y = np.array([0, 1, 1, 0], dtype=np.int64)

    model = MLP([2, 4, 2], learning_rate=0.5, seed=1)
    initial_loss, initial_acc = model.evaluate(X, y)

    model.fit(X, y, epochs=250, batch_size=4, shuffle=False, verbose=False)
    final_loss, final_acc = model.evaluate(X, y)

    assert final_loss < initial_loss
    assert final_acc > initial_acc
    assert final_acc >= 0.75


def test_serialisation_roundtrip(tmp_path):
    X = np.array([[0.0, 1.0], [1.0, 0.0]], dtype=np.float64)
    y = np.array([0, 1], dtype=np.int64)

    model = MLP([2, 3, 2], learning_rate=0.3, seed=123)
    model.fit(X, y, epochs=50, batch_size=2, shuffle=False)

    preds_before = model.predict(X)

    save_path = tmp_path / "mlp-test.npz"
    model.save(save_path)

    loaded = MLP.load(save_path)
    preds_after = loaded.predict(X)

    np.testing.assert_array_equal(preds_before, preds_after)
