"""Command-line interface for composing simple CSG scenes."""
from __future__ import annotations

import argparse
import os
from typing import Tuple

from . import SDF, Bounds, box, cylinder, export_mesh, mesh_from_sdf, plot_mesh, sphere


def _parse_triplet(text: str) -> Tuple[float, float, float]:
    parts = [float(value) for value in text.split(",")]
    if len(parts) == 1:
        parts = parts * 3
    if len(parts) != 3:
        raise ValueError(f"Expected three comma-separated values, received '{text}'")
    return parts[0], parts[1], parts[2]


def parse_primitive(spec: str) -> SDF:
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
    parts = [segment.strip() for segment in text.split(";") if segment.strip()]
    if len(parts) != 3:
        raise ValueError("Bounds must be formatted as xmin,xmax;ymin,ymax;zmin,zmax")
    intervals = []
    for part in parts:
        values = [float(v) for v in part.split(",")]
        if len(values) != 2:
            raise ValueError(f"Invalid interval '{part}'")
        intervals.append((min(values), max(values)))
    return tuple(intervals)  # type: ignore[return-value]


def build_scene(operation: str, primitive_a: SDF, primitive_b: SDF) -> SDF:
    if operation == "union":
        return primitive_a.union(primitive_b)
    if operation == "intersection":
        return primitive_a.intersection(primitive_b)
    if operation == "difference":
        return primitive_a.difference(primitive_b)
    raise ValueError("Operation must be union, intersection, or difference")


def create_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Compose simple CSG scenes from primitives.")
    parser.add_argument("operation", choices=["union", "intersection", "difference"], help="Boolean operation")
    parser.add_argument("primitive_a", help="First primitive specification (e.g. 'sphere:radius=0.6;center=0,0,0')")
    parser.add_argument("primitive_b", help="Second primitive specification")
    parser.add_argument(
        "--bounds",
        default="-1.5,1.5;-1.5,1.5;-1.5,1.5",
        help="Sampling bounds as xmin,xmax;ymin,ymax;zmin,zmax",
    )
    parser.add_argument("--resolution", type=int, default=96, help="Grid resolution for marching cubes")
    parser.add_argument("--export", help="Path to export mesh (extension determines format, e.g. .stl or .obj)")
    parser.add_argument("--plot", action="store_true", help="Render the generated mesh with matplotlib")
    return parser


def main(argv: list[str] | None = None) -> None:
    parser = create_parser()
    args = parser.parse_args(argv)

    primitive_a = parse_primitive(args.primitive_a)
    primitive_b = parse_primitive(args.primitive_b)
    scene = build_scene(args.operation, primitive_a, primitive_b)
    bounds = parse_bounds(args.bounds)
    mesh = mesh_from_sdf(scene, bounds=bounds, resolution=args.resolution)

    print(f"Vertices: {len(mesh.vertices)} | Faces: {len(mesh.faces)} | Volume: {mesh.volume:.4f}")

    if args.export:
        os.makedirs(os.path.dirname(args.export) or ".", exist_ok=True)
        export_mesh(mesh, args.export)
        print(f"Mesh exported to {args.export}")

    if args.plot:
        plot_mesh(mesh)


if __name__ == "__main__":
    main()
