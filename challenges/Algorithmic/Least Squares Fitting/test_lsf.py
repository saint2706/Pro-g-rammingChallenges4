import unittest
import numpy as np
from lsf import least_squares_fit


class TestLeastSquaresFit(unittest.TestCase):
    def test_exact_line(self):
        # y = 2x + 1
        x = np.array([0, 1, 2, 3, 4], dtype=float)
        y = 2 * x + 1
        m, b = least_squares_fit(x, y)
        self.assertAlmostEqual(m, 2.0, places=9)
        self.assertAlmostEqual(b, 1.0, places=9)

    def test_noisy_line(self):
        rng = np.random.default_rng(123)
        x = np.linspace(0, 50, 100)
        true_m, true_b = 0.75, -5.0
        y = true_m * x + true_b + rng.normal(0, 2.0, x.size)
        m, b = least_squares_fit(x, y)
        self.assertAlmostEqual(m, true_m, delta=0.1)
        self.assertAlmostEqual(b, true_b, delta=0.8)

    def test_vertical_line_error(self):
        x = np.array([3, 3, 3, 3], dtype=float)
        y = np.array([0, 1, 2, 3], dtype=float)
        with self.assertRaises(ValueError):
            least_squares_fit(x, y)

    def test_mismatched_sizes(self):
        with self.assertRaises(ValueError):
            least_squares_fit(np.array([0, 1, 2]), np.array([1, 2]))

    def test_empty(self):
        with self.assertRaises(ValueError):
            least_squares_fit(np.array([]), np.array([]))


if __name__ == "__main__":
    unittest.main()
