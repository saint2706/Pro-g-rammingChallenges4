"""Basic behavioural checks for the Verlet cloth simulation."""

import numpy as np
import sys
from pathlib import Path

PROJECT_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(PROJECT_ROOT))

from cloth import Cloth


def test_static_cloth_remains_at_rest():
    cloth = Cloth(num_x=4, num_y=4, gravity=(0.0, 0.0), wind=(0.0, 0.0), pinned_rows=0)
    initial = cloth.positions.copy()
    cloth.step(0.01)
    np.testing.assert_allclose(cloth.positions, initial)


def test_pinned_particles_do_not_move_under_gravity():
    cloth = Cloth(num_x=4, num_y=4, pinned_rows=1)
    initial = cloth.pinned_positions().copy()
    for _ in range(10):
        cloth.step(0.016)
    np.testing.assert_allclose(cloth.pinned_positions(), initial)
