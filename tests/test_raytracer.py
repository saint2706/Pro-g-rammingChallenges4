"""Regression tests for the Emulation/RayTracer module."""
from __future__ import annotations

import hashlib
from pathlib import Path

import numpy as np
import pytest

from Emulation.RayTracer import RayTracer, load_scene_file


@pytest.mark.parametrize("resolution", [(120, 90), (160, 120)])
def test_minimal_scene_hash(resolution: tuple[int, int]) -> None:
    """Render the minimal sample scene and compare the output hash."""
    width, height = resolution
    scene_path = Path("Emulation/RayTracer/scenes/minimal.json")
    scene = load_scene_file(scene_path)
    tracer = RayTracer(width=width, height=height, scene=scene)
    image = tracer.render()
    array = np.asarray(image)
    digest = hashlib.sha256(array.tobytes()).hexdigest()
    # Hashes generated from deterministic renders to guard against regressions.
    expected = {
        (120, 90): "e951a2538d7997589310af414b32f3959ebedecd62e73102894520cd94b34039",
        (160, 120): "ef498ccda2f25e2163f3f0b770f843b8fea450663cd2756610f0e8578ce3f0ae",
    }
    assert digest == expected[resolution]
