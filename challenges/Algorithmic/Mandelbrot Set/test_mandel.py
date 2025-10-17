import unittest
import numpy as np
from mandel import MandelbrotConfig, generate_mandelbrot


class TestMandelbrot(unittest.TestCase):
    def test_basic_shape(self):
        cfg = MandelbrotConfig(width=64, height=48, max_iter=50, smooth=False)
        arr, bounds = generate_mandelbrot(cfg)
        self.assertEqual(arr.shape, (48, 64))
        self.assertEqual(len(bounds), 4)

    def test_center_in_set(self):
        # The point (0,0) is inside; using a window centered there should have many max_iter values.
        cfg = MandelbrotConfig(
            width=80, height=60, max_iter=40, center_x=0.0, center_y=0.0, scale=3.0
        )
        arr, _ = generate_mandelbrot(cfg)
        inside_ratio = np.mean(arr == cfg.max_iter)
        self.assertGreater(inside_ratio, 0.10)  # heuristic threshold

    def test_known_escape_point(self):
        # Point (2,2) is well outside; ensure it escapes quickly if window includes it.
        cfg = MandelbrotConfig(
            width=20, height=20, max_iter=80, x_min=-2, x_max=2, y_min=-2, y_max=2
        )
        arr, _ = generate_mandelbrot(cfg)
        # Find pixel corresponding to (2,2) ~ upper-right corner
        self.assertLess(arr[-1, -1], cfg.max_iter)  # should not be classified as inside

    def test_smooth_normalization(self):
        cfg = MandelbrotConfig(width=40, height=30, max_iter=60, smooth=True)
        arr, _ = generate_mandelbrot(cfg)
        self.assertTrue(arr.dtype.kind == "f")
        # Values should be within [0,1]
        self.assertGreaterEqual(arr.min(), 0.0)
        self.assertLessEqual(arr.max(), 1.0)


if __name__ == "__main__":
    unittest.main()
