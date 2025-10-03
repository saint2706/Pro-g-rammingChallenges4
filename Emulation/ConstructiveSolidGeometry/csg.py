"""Constructive solid geometry utilities built on signed distance fields.

This module provides primitive SDF generators, boolean operations, and helpers
for sampling an SDF into a triangular mesh using marching cubes.  The meshes can
be exported with :mod:`trimesh` or visualised with matplotlib.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Iterable, Tuple

import numpy as np
from skimage import measure
import trimesh

Bounds = Tuple[Tuple[float, float], Tuple[float, float], Tuple[float, float]]


@dataclass(frozen=True)
class SDF:
    """A signed distance field callable."""

    func: Callable[[np.ndarray, np.ndarray, np.ndarray], np.ndarray]

    def __call__(self, x: np.ndarray, y: np.ndarray, z: np.ndarray) -> np.ndarray:
        return self.func(x, y, z)

    def union(self, other: "SDF") -> "SDF":
        return SDF(lambda x, y, z: np.minimum(self(x, y, z), other(x, y, z)))

    def intersection(self, other: "SDF") -> "SDF":
        return SDF(lambda x, y, z: np.maximum(self(x, y, z), other(x, y, z)))

    def difference(self, other: "SDF") -> "SDF":
        return SDF(lambda x, y, z: np.maximum(self(x, y, z), -other(x, y, z)))

    def translate(self, offset: Tuple[float, float, float]) -> "SDF":
        ox, oy, oz = (float(v) for v in offset)
        return SDF(lambda x, y, z: self(x - ox, y - oy, z - oz))


def _ensure_tuple(value: Iterable[float], length: int) -> Tuple[float, ...]:
    seq = tuple(float(v) for v in value)
    if len(seq) != length:
        raise ValueError(f"Expected {length} values, received {len(seq)}")
    return seq


def sphere(
    *, center: Tuple[float, float, float] = (0.0, 0.0, 0.0), radius: float = 0.5
) -> SDF:
    cx, cy, cz = _ensure_tuple(center, 3)
    r = float(radius)

    def func(x: np.ndarray, y: np.ndarray, z: np.ndarray) -> np.ndarray:
        return np.sqrt((x - cx) ** 2 + (y - cy) ** 2 + (z - cz) ** 2) - r

    return SDF(func)


def box(
    *,
    center: Tuple[float, float, float] = (0.0, 0.0, 0.0),
    size: Tuple[float, float, float] = (1.0, 1.0, 1.0),
) -> SDF:
    cx, cy, cz = _ensure_tuple(center, 3)
    sx, sy, sz = _ensure_tuple(size, 3)
    hx, hy, hz = sx / 2.0, sy / 2.0, sz / 2.0

    def func(x: np.ndarray, y: np.ndarray, z: np.ndarray) -> np.ndarray:
        px = np.abs(x - cx) - hx
        py = np.abs(y - cy) - hy
        pz = np.abs(z - cz) - hz
        outside = np.sqrt(
            np.maximum(px, 0.0) ** 2
            + np.maximum(py, 0.0) ** 2
            + np.maximum(pz, 0.0) ** 2
        )
        inside = np.minimum(np.maximum(np.maximum(px, py), pz), 0.0)
        return outside + inside

    return SDF(func)


def cylinder(
    *,
    center: Tuple[float, float, float] = (0.0, 0.0, 0.0),
    radius: float = 0.4,
    height: float = 1.0,
    axis: str = "z",
) -> SDF:
    cx, cy, cz = _ensure_tuple(center, 3)
    r = float(radius)
    h = float(height) / 2.0
    axis = axis.lower()
    if axis not in {"x", "y", "z"}:
        raise ValueError("Cylinder axis must be one of 'x', 'y', or 'z'")

    def func(x: np.ndarray, y: np.ndarray, z: np.ndarray) -> np.ndarray:
        if axis == "z":
            dx = np.sqrt((x - cx) ** 2 + (y - cy) ** 2) - r
            dz = np.abs(z - cz) - h
        elif axis == "y":
            dx = np.sqrt((x - cx) ** 2 + (z - cz) ** 2) - r
            dz = np.abs(y - cy) - h
        else:  # axis == "x"
            dx = np.sqrt((y - cy) ** 2 + (z - cz) ** 2) - r
            dz = np.abs(x - cx) - h
        outside = np.sqrt(np.maximum(dx, 0.0) ** 2 + np.maximum(dz, 0.0) ** 2)
        inside = np.minimum(np.maximum(dx, dz), 0.0)
        return outside + inside

    return SDF(func)


def sample_grid(
    bounds: Bounds, resolution: int
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    if resolution < 16:
        raise ValueError("Resolution should be at least 16 for meaningful sampling")
    (xmin, xmax), (ymin, ymax), (zmin, zmax) = bounds
    xs = np.linspace(xmin, xmax, resolution)
    ys = np.linspace(ymin, ymax, resolution)
    zs = np.linspace(zmin, zmax, resolution)
    return np.meshgrid(xs, ys, zs, indexing="ij")


def mesh_from_sdf(sdf: SDF, bounds: Bounds, resolution: int = 96) -> trimesh.Trimesh:
    grid_x, grid_y, grid_z = sample_grid(bounds, resolution)
    field = sdf(grid_x, grid_y, grid_z)
    volume = np.transpose(field, (2, 1, 0))
    dx = (bounds[0][1] - bounds[0][0]) / (resolution - 1)
    dy = (bounds[1][1] - bounds[1][0]) / (resolution - 1)
    dz = (bounds[2][1] - bounds[2][0]) / (resolution - 1)
    verts, faces, _, _ = measure.marching_cubes(volume, level=0.0, spacing=(dz, dy, dx))
    verts = verts[:, [2, 1, 0]]
    origin = np.array([bounds[0][0], bounds[1][0], bounds[2][0]])
    verts += origin
    return trimesh.Trimesh(vertices=verts, faces=faces, process=False)


def estimate_volume(sdf: SDF, bounds: Bounds, resolution: int = 96) -> float:
    grid_x, grid_y, grid_z = sample_grid(bounds, resolution)
    field = sdf(grid_x, grid_y, grid_z)
    dx = (bounds[0][1] - bounds[0][0]) / (resolution - 1)
    dy = (bounds[1][1] - bounds[1][0]) / (resolution - 1)
    dz = (bounds[2][1] - bounds[2][0]) / (resolution - 1)
    cell_volume = dx * dy * dz
    return float(np.count_nonzero(field <= 0.0) * cell_volume)


def export_mesh(mesh: trimesh.Trimesh, path: str) -> None:
    mesh.export(path)


def plot_mesh(mesh: trimesh.Trimesh) -> None:
    import matplotlib.pyplot as plt

    fig = plt.figure(figsize=(6, 6))
    ax = fig.add_subplot(111, projection="3d")
    ax.plot_trisurf(
        mesh.vertices[:, 0],
        mesh.vertices[:, 1],
        mesh.vertices[:, 2],
        triangles=mesh.faces,
        color="#6699cc",
        alpha=0.9,
    )
    ax.set_box_aspect([1, 1, 1])
    ax.set_xlabel("X")
    ax.set_ylabel("Y")
    ax.set_zlabel("Z")
    plt.tight_layout()
    plt.show()
