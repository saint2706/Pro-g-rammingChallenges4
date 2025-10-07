from __future__ import annotations

import math

import pytest

# ruff: noqa: E402

pytest.importorskip("skimage")

from Emulation.ConstructiveSolidGeometry import box, mesh_from_sdf, sphere


def _volume(mesh) -> float:
    # Guard against tiny negative volumes caused by winding
    return float(abs(mesh.volume))


def test_sphere_volume_matches_analytic_value():
    radius = 0.5
    sdf = sphere(radius=radius)
    mesh = mesh_from_sdf(
        sdf, bounds=((-0.6, 0.6), (-0.6, 0.6), (-0.6, 0.6)), resolution=96
    )
    expected = 4.0 / 3.0 * math.pi * radius**3
    assert _volume(mesh) == pytest.approx(expected, rel=0.05)


def test_union_of_disjoint_boxes_adds_volumes():
    box_a = box(size=(0.4, 0.4, 0.4)).translate((-0.5, 0.0, 0.0))
    box_b = box(size=(0.4, 0.4, 0.4)).translate((0.5, 0.0, 0.0))
    mesh = mesh_from_sdf(
        box_a.union(box_b),
        bounds=((-1.0, 1.0), (-0.6, 0.6), (-0.6, 0.6)),
        resolution=96,
    )
    expected = 2 * (0.4**3)
    assert _volume(mesh) == pytest.approx(expected, rel=0.08)


def test_intersection_recovers_inner_sphere_volume():
    radius = 0.45
    mesh = mesh_from_sdf(
        box(size=(1.0, 1.0, 1.0)).intersection(sphere(radius=radius)),
        bounds=((-0.6, 0.6), (-0.6, 0.6), (-0.6, 0.6)),
        resolution=96,
    )
    expected = 4.0 / 3.0 * math.pi * radius**3
    assert _volume(mesh) == pytest.approx(expected, rel=0.06)


def test_difference_subtracts_embedded_sphere_volume():
    outer_radius, inner_radius = 0.6, 0.3
    mesh = mesh_from_sdf(
        sphere(radius=outer_radius).difference(sphere(radius=inner_radius)),
        bounds=((-0.8, 0.8), (-0.8, 0.8), (-0.8, 0.8)),
        resolution=96,
    )
    expected = 4.0 / 3.0 * math.pi * (outer_radius**3 - inner_radius**3)
    assert _volume(mesh) == pytest.approx(expected, rel=0.06)
