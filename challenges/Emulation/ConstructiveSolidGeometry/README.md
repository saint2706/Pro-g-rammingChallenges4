# Constructive Solid Geometry (Challenge #93)

This mini-project builds Boolean meshes from signed distance fields (SDFs). It
provides reusable primitives, Boolean combinators, a scriptable CLI, and helper
scripts to export STL/OBJ meshes for downstream tools or printing.

## Features

- SDF primitives: sphere, box, capped cylinder
- Boolean operations: union, intersection, difference
- Grid sampling with marching cubes → watertight triangle meshes (via
  `scikit-image` and `trimesh`)
- CLI for composing scenes and exporting meshes
- Ready-made sample exporter for STL/OBJ assets
- Volume estimation helpers and automated regression tests

## Environment & Dependencies

Install the repo with the `visual` extra to pull in the required 3D stack. This
brings in `numpy`, `matplotlib`, `scikit-image`, and `trimesh`:

```bash
python -m pip install -e .[visual]
```

## Workflow

1. Pick/compose SDF primitives in `csg.py`.
2. Sample on a regular grid and convert to a triangle mesh using
   `mesh_from_sdf`.
3. Optionally inspect the mesh with `plot_mesh` (Matplotlib 3D trisurf).
4. Export to STL or OBJ with `export_mesh`.
5. Reuse the CLI or `export_examples.py` to automate scenes & exports.

All logic lives in plain Python files, so you can import the module in notebooks
or other scripts without extra scaffolding.

## CLI Usage

The CLI accepts primitive specifications of the form
`name:param=value;param=value`. Parameters requiring vectors (centre, size)
use comma-separated triples. Examples:

```bash
# Union of a centred sphere and a translated cylinder
python -m Emulation.ConstructiveSolidGeometry.cli \
    union \
    sphere:radius=0.55 \
    cylinder:radius=0.25;height=1.4;center=0.3,0,0 \
    --bounds -1.2,1.2;-1.2,1.2;-1.2,1.2 \
    --resolution 128 \
    --export outputs/union.obj

# Subtract a sphere from a cube and preview the mesh with Matplotlib
python -m Emulation.ConstructiveSolidGeometry.cli \
    difference \
    box:size=1,1,1 \
    sphere:radius=0.45 \
    --plot
```

CLI flags:

- `operation` — `union`, `intersection`, or `difference`
- `primitive_a` / `primitive_b` — primitive specs
- `--bounds` — sampling bounds `xmin,xmax;ymin,ymax;zmin,zmax`
- `--resolution` — grid resolution (default 96)
- `--export` — file path; extension chooses format (`.stl`, `.obj`, ...)
- `--plot` — render via Matplotlib (requires GUI/display)

## Batch Exports

Use the helper script to generate a trio of sample meshes (union, intersection,
difference) and store them in the `outputs/` directory:

```bash
python -m Emulation.ConstructiveSolidGeometry.export_examples outputs --resolution 96
```

The script writes a mix of STL and OBJ files that can be imported into CAD or
slicer tools.

## Automated Checks

`tests/test_constructive_solid_geometry.py` verifies the volume estimates for
classic primitives and Boolean combinations (union, intersection, difference).
This guards against regressions in the SDF definitions or marching-cubes
sampling resolution.

Run the test suite from the repo root:

```bash
pytest tests/test_constructive_solid_geometry.py
```

## Sample Outputs

- `outputs/union_sphere_cylinder.obj` — smooth blend of a sphere and cylinder
- `outputs/intersection_box_sphere.stl` — spherical cut constrained to a cube
- `outputs/difference_box_cylinder.obj` — box with cylindrical bore

Each mesh is watertight and suitable for inspection in `trimesh.Scene`, Blender,
or 3D-printing pipelines.
