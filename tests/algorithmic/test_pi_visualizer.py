"""Tests for the Pi convergence visualizer and helper generators."""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]


def _load_module(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"Cannot load module {name} from {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_chudnovsky_convergence_structure():
    module = _load_module(
        "digitpi_module", ROOT / "challenges/Algorithmic/Digits of Pi/DigitPi.py"
    )
    steps = list(module.generate_chudnovsky_convergence(3))

    assert len(steps) == 3
    assert steps[0].iteration == 0
    assert steps[0].elapsed == 0.0
    assert steps[-1].iteration == 2
    assert steps[-1].approximation > steps[0].approximation


def test_gauss_legendre_convergence_structure():
    module = _load_module(
        "gauss_legendre_module", ROOT / "challenges/Algorithmic/1000 Digits of Pi/pi.py"
    )
    steps = list(module.generate_gauss_legendre_convergence(20))

    assert steps[0].iteration == 0
    assert steps[-1].iteration >= 1
    assert steps[-1].approximation.as_tuple().digits[0] == 3


def test_visualizer_summary_and_export(tmp_path: Path):
    visualizer = _load_module(
        "pi_visualizer",
        ROOT / "challenges/Algorithmic/Digits of Pi/pi_visualizer.py",
    )

    data = visualizer.get_convergence_data("chudnovsky", iterations=3)
    assert data and all(
        isinstance(step, visualizer.ConvergenceDatum) for step in data
    )

    if visualizer.PLOTLY_AVAILABLE:
        figure = visualizer.build_figure(data, title="Test")
        assert figure.data

    json_path = tmp_path / "summary.json"
    visualizer.write_json_summary(
        json_path,
        algorithm="chudnovsky",
        data=data,
        final_value=str(data[-1].approximation),
    )

    payload = json.loads(json_path.read_text(encoding="utf-8"))
    assert payload["algorithm"] == "chudnovsky"
    assert payload["steps"]
    assert payload["steps"][0]["iteration"] == 0


@pytest.mark.parametrize(
    "algorithm,kwargs",
    [
        ("chudnovsky", {"iterations": 2}),
        ("gauss-legendre", {"digits": 10}),
    ],
)
def test_get_convergence_data_variants(algorithm: str, kwargs: dict[str, int]):
    visualizer = _load_module(
        "pi_visualizer_param",
        ROOT / "challenges/Algorithmic/Digits of Pi/pi_visualizer.py",
    )
    data = visualizer.get_convergence_data(algorithm, **kwargs)
    assert data
    for step in data:
        assert isinstance(step.iteration, int)
        assert step.digits >= 0
        assert step.elapsed >= 0
