import importlib.util
import subprocess
import sys
from pathlib import Path

import numpy as np
import pytest

MODULE_PATH = Path(__file__).resolve().parents[2] / "Practical" / "Matrix Arithmetic" / "matrix_arithmetic.py"
SPEC = importlib.util.spec_from_file_location("matrix_arithmetic", MODULE_PATH)
matrix_arithmetic = importlib.util.module_from_spec(SPEC)
assert SPEC is not None and SPEC.loader is not None
sys.modules["matrix_arithmetic"] = matrix_arithmetic
SPEC.loader.exec_module(matrix_arithmetic)  # type: ignore[arg-type]

add_matrices = matrix_arithmetic.add_matrices
multiply_matrices = matrix_arithmetic.multiply_matrices
determinant = matrix_arithmetic.determinant
inverse = matrix_arithmetic.inverse
SingularMatrixError = matrix_arithmetic.SingularMatrixError
visualize_transformation = matrix_arithmetic.visualize_transformation


def _to_array(result):
    assert isinstance(result.value, np.ndarray)
    return result.value


def test_addition_with_explanation():
    a = np.array([[1, 2], [3, 4]])
    b = np.array([[4, 3], [2, 1]])
    result = add_matrices(a, b, explain=True)
    np.testing.assert_array_equal(_to_array(result), np.array([[5, 5], [5, 5]]))
    assert len(result.steps) == 4
    assert "Entry (1,1)" in result.steps[0]


def test_multiplication_step_breakdown():
    a = np.array([[1, 0, -1], [2, 3, 1]])
    b = np.array([[2, 1], [0, -1], [4, 2]])
    result = multiply_matrices(a, b, explain=True)
    expected = np.array([[-2, -1], [8, 1]])
    np.testing.assert_array_equal(_to_array(result), expected)
    assert any("Entry (2,1)" in step for step in result.steps)


def test_determinant_and_inverse_agree():
    matrix = np.array([[4, 7], [2, 6]], dtype=float)
    det_result = determinant(matrix, explain=True)
    assert pytest.approx(det_result.value, rel=1e-9) == 10.0
    inv_result = inverse(matrix, explain=True)
    expected = np.array([[0.6, -0.7], [-0.2, 0.4]])
    np.testing.assert_allclose(_to_array(inv_result), expected)
    assert any("Right block" in step for step in inv_result.steps)


def test_inverse_raises_on_singular():
    singular = np.array([[1, 2], [2, 4]], dtype=float)
    with pytest.raises(SingularMatrixError):
        inverse(singular)


def test_cli_addition():
    cmd = [
        sys.executable,
        str(MODULE_PATH),
        "--precision",
        "2",
        "add",
        "--matrix-a",
        "[[1,2],[3,4]]",
        "--matrix-b",
        "[[4,3],[2,1]]",
    ]
    completed = subprocess.run(cmd, check=True, capture_output=True, text=True)
    assert "5.00" in completed.stdout


def test_visualization_output(tmp_path):
    path = tmp_path / "viz.png"
    matrix = np.array([[2, 0], [0, 1]], dtype=float)
    output_path = visualize_transformation(matrix, output=str(path))
    assert path.exists()
    assert str(path) == output_path
    assert path.stat().st_size > 0
