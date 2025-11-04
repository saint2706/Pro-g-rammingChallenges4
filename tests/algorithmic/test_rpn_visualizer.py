import importlib.util
import json
import sys
from pathlib import Path

import pytest

CHALLENGE_ROOT = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "RPN Calculator"
)


def _load_module(name: str, filename: str):
    spec = importlib.util.spec_from_file_location(name, CHALLENGE_ROOT / filename)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)  # type: ignore[call-arg]
    return module


rpn = _load_module("postifx_evaluator", "postifx_evaluator.py")
visualizer = _load_module("rpn_visualizer", "rpn_visualizer.py")


def test_stack_snapshots_follow_expected_progression():
    expression = "3 4 + 2 *"
    snapshots, result = visualizer.capture_stack_snapshots(expression)

    tokens = [snap.token for snap in snapshots]
    stacks = [snap.stack for snap in snapshots]

    assert tokens == ["3", "4", "+", "2", "*"]
    assert stacks == [
        [3.0],
        [3.0, 4.0],
        [7.0],
        [7.0, 2.0],
        [14.0],
    ]
    assert pytest.approx(result, rel=1e-12) == 14.0


def test_snapshots_respect_degree_mode_for_trig():
    cfg = rpn.EvalConfig(degrees=True)
    snapshots, result = visualizer.capture_stack_snapshots("90 sin", config=cfg)

    assert [snap.stack for snap in snapshots] == [[90.0], [1.0]]
    assert pytest.approx(result, rel=1e-12) == 1.0


def test_json_roundtrip_preserves_stack(tmp_path: Path):
    expression = "2 3 ^ 4 /"
    snapshots, result = visualizer.capture_stack_snapshots(expression)

    payload = visualizer.snapshots_to_json(
        snapshots, expression=expression, result=result
    )
    file_path = tmp_path / "snapshots.json"
    file_path.write_text(json.dumps(payload))

    loaded_expression, loaded_result, loaded_snapshots = visualizer.snapshots_from_json(
        json.loads(file_path.read_text())
    )

    assert loaded_expression == expression
    assert pytest.approx(loaded_result, rel=1e-12) == result
    assert [snap.stack for snap in loaded_snapshots] == [
        snap.stack for snap in snapshots
    ]
