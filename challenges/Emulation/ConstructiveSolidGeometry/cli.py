"""Command-line interface for composing simple CSG scenes.

This module provides a command-line tool for creating 3D models using
Constructive Solid Geometry (CSG). It allows users to define primitive
shapes, combine them with Boolean operations, and export the resulting
mesh to a file.
"""

from __future__ import annotations

import argparse
import os
import sys
from typing import Tuple

from . import SDF, Bounds, box, cylinder, export_mesh, mesh_from_sdf, plot_mesh, sphere


def _parse_triplet(text: str) -> Tuple[float, float, float]:
    """Parses a string of comma-separated values into a 3-tuple of floats.

    Args:
        text: The string to parse (e.g., "1,2,3" or "1.5").

    Returns:
        A tuple of three floats.

    Raises:
        ValueError: If the string is not in the correct format.
    """

    parts = [float(value) for value in text.split(",")]
    if len(parts) == 1:
        parts *= 3
    if len(parts) != 3:
        raise ValueError(f"Expected three comma-separated values, received '{text}'")
    return parts[0], parts[1], parts[2]


def parse_primitive(spec: str) -> SDF:
    """Parses a primitive shape specification from a string.

    The specification should be in the format: "name:param=value;param=value".

    Args:
        spec: The string specification of the primitive.

    Returns:
        An SDF (Signed Distance Function) object for the specified primitive.

    Raises:
        ValueError: If the primitive name or parameters are invalid.
    """
    name, _, params_text = spec.partition(":")
    name = name.strip().lower()
    params = {}
    if params_text:
        for assignment in params_text.split(";"):
            if not assignment:
                continue
            key, _, value = assignment.partition("=")
            if not _:
                raise ValueError(f"Invalid parameter assignment '{assignment}'")
            params[key.strip().lower()] = value.strip()

    if name == "sphere":
        radius = float(params.get("radius", 0.5))
        center = _parse_triplet(params.get("center", "0,0,0"))
        return sphere(center=center, radius=radius)
    if name == "box":
        size = _parse_triplet(params.get("size", "1,1,1"))
        center = _parse_triplet(params.get("center", "0,0,0"))
        return box(center=center, size=size)
    if name == "cylinder":
        radius = float(params.get("radius", 0.4))
        center = _parse_triplet(params.get("center", "0,0,0"))
        height = float(params.get("height", 1.0))
        axis = params.get("axis", "z")
        return cylinder(center=center, radius=radius, height=height, axis=axis)

    raise ValueError(f"Unsupported primitive '{name}'. Use sphere, box, or cylinder.")


def parse_bounds(text: str) -> Bounds:
    """Parses the sampling bounds from a string.

    The bounds should be in the format: "xmin,xmax;ymin,ymax;zmin,zmax".

    Args:
        text: The string representation of the bounds.

    Returns:
        A tuple of three tuples, each containing the min and max for an axis.

    Raises:
        ValueError: If the bounds string is not in the correct format.
    """
    parts = [segment.strip() for segment in text.split(";") if segment.strip()]
    if len(parts) != 3:
        raise ValueError("Bounds must be formatted as xmin,xmax;ymin,ymax;zmin,zmax")
    intervals = []
    for part in parts:
        values = [float(v) for v in part.split(",")]
        if len(values) != 2:
            raise ValueError(f"Invalid interval '{part}'")
        intervals.append((min(values), max(values)))
    return tuple(intervals)


def build_scene(operation: str, primitive_a: SDF, primitive_b: SDF) -> SDF:
    """Builds a CSG scene by combining two primitives with a Boolean operation.

    Args:
        operation: The Boolean operation to perform ("union", "intersection", or "difference").
        primitive_a: The first primitive.
        primitive_b: The second primitive.

    Returns:
        An SDF representing the combined scene.

    Raises:
        ValueError: If the operation is not one of the supported types.
    """
    if operation == "union":
        return primitive_a.union(primitive_b)
    if operation == "intersection":
        return primitive_a.intersection(primitive_b)
    if operation == "difference":
        return primitive_a.difference(primitive_b)
    raise ValueError("Operation must be union, intersection, or difference")


def create_parser() -> argparse.ArgumentParser:
    """Creates the command-line argument parser for the CSG tool.

    Returns:
        An ArgumentParser instance.
    """
    parser = argparse.ArgumentParser(
        description="Compose simple CSG scenes from primitives."
    )
    parser.add_argument(
        "operation",
        choices=["union", "intersection", "difference"],
        help="Boolean operation to combine the primitives.",
    )
    parser.add_argument(
        "primitive_a",
        help="Specification for the first primitive (e.g., 'sphere:radius=0.6').",
    )
    parser.add_argument("primitive_b", help="Specification for the second primitive.")
    parser.add_argument(
        "--bounds",
        default="-1.5,1.5;-1.5,1.5;-1.5,1.5",
        help="Sampling bounds in the format 'xmin,xmax;ymin,ymax;zmin,zmax'.",
    )
    parser.add_argument(
        "--resolution", type=int, default=96, help="Grid resolution for marching cubes."
    )
    parser.add_argument(
        "--export",
        help="Path to export the mesh (e.g., 'output.stl', 'output.obj').",
    )
    parser.add_argument(
        "--plot", action="store_true", help="Render the mesh with Matplotlib."
    )
    return parser


def main(argv: list[str] | None = None) -> None:
    """The main entry point for the command-line tool.

    Args:
        argv: A list of command-line arguments.
    """
    parser = create_parser()
    args = parser.parse_args(argv)

    try:
        primitive_a = parse_primitive(args.primitive_a)
        primitive_b = parse_primitive(args.primitive_b)
        scene = build_scene(args.operation, primitive_a, primitive_b)
        bounds = parse_bounds(args.bounds)
        mesh = mesh_from_sdf(scene, bounds=bounds, resolution=args.resolution)
    except ValueError as exc:
        print(str(exc), file=sys.stderr)
        sys.exit(1)

    print(
        f"Vertices: {len(mesh.vertices)} | Faces: {len(mesh.faces)} | Volume: {mesh.volume:.4f}"
    )

    if args.export:
        os.makedirs(os.path.dirname(args.export) or ".", exist_ok=True)
        export_mesh(mesh, args.export)
        print(f"Mesh exported to {args.export}")

    if args.plot:
        plot_mesh(mesh)


if __name__ == "__main__":
    main()
