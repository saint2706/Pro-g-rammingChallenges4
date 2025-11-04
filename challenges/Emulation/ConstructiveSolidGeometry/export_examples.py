"""Generate sample CSG meshes and export them to STL/OBJ files.

This script provides a simple command-line interface for generating a set of
predefined CSG scenes and exporting them as 3D mesh files. It serves as a
demonstration of the capabilities of the CSG module.
"""

from __future__ import annotations

import argparse
import os
from pathlib import Path

from . import box, cylinder, export_mesh, mesh_from_sdf, sphere


def export_samples(directory: Path, resolution: int = 96) -> None:
    """Creates and exports three sample CSG meshes.

    The generated meshes are:
    - A union of a sphere and a cylinder.
    - An intersection of a box and a sphere.
    - A difference of a box and a cylinder.

    Args:
        directory: The directory to save the exported mesh files in.
        resolution: The grid resolution to use for the marching cubes algorithm.
    """
    directory.mkdir(parents=True, exist_ok=True)

    # 1. Union of a sphere and a cylinder.
    sphere_a = sphere(radius=0.5)
    cylinder_a = cylinder(radius=0.25, height=1.4).translate((0.25, 0.0, 0.0))
    union_mesh = mesh_from_sdf(
        sphere_a.union(cylinder_a),
        bounds=((-1.0, 1.2), (-1.0, 1.0), (-1.0, 1.0)),
        resolution=resolution,
    )
    export_mesh(union_mesh, str(directory / "union_sphere_cylinder.obj"))

    # 2. Intersection of a box and a sphere.
    box_a = box(size=(1.0, 1.0, 1.0))
    intersection_mesh = mesh_from_sdf(
        box_a.intersection(sphere(radius=0.6)),
        bounds=((-0.75, 0.75), (-0.75, 0.75), (-0.75, 0.75)),
        resolution=resolution,
    )
    export_mesh(intersection_mesh, str(directory / "intersection_box_sphere.stl"))

    # 3. Difference of a box and a cylinder (a box with a hole).
    difference_mesh = mesh_from_sdf(
        box(size=(1.2, 1.2, 1.2)).difference(cylinder(radius=0.35, height=1.2)),
        bounds=((-0.9, 0.9), (-0.9, 0.9), (-0.9, 0.9)),
        resolution=resolution,
    )
    export_mesh(difference_mesh, str(directory / "difference_box_cylinder.obj"))


def main(argv: list[str] | None = None) -> None:
    """The main entry point for the script.

    Args:
        argv: A list of command-line arguments.
    """
    parser = argparse.ArgumentParser(
        description="Export sample CSG meshes to STL/OBJ files"
    )
    parser.add_argument(
        "output",
        nargs="?",
        default="outputs",
        help="Directory to place exported meshes.",
    )
    parser.add_argument(
        "--resolution",
        type=int,
        default=96,
        help="Grid resolution for marching cubes.",
    )
    args = parser.parse_args(argv)

    export_samples(Path(args.output), resolution=args.resolution)
    print(f"Exported meshes to {os.path.abspath(args.output)}")


if __name__ == "__main__":
    main()
