# Ray Tracer (Challenge #94)

A CPU-based ray tracer that supports spheres, planes, Phong shading, shadows, and mirror-like reflections. Scenes are configured either via JSON files or directly from CLI arguments and the renderer outputs standard images via Pillow.

## Features

- Perspective camera with configurable FOV, position, and orientation.
- Geometry: spheres and infinite planes.
- Lighting: point lights with per-light color/intensity.
- Shading: Phong model (ambient + diffuse + specular) with soft reflections.
- Deterministic shadows with epsilon bias to avoid acne.
- JSON configuration loader plus ergonomic CLI builders for quick experiments.
- Sample scenes covering a Cornell box inspired layout and a minimal single-sphere setup.

## Quick Start

From the repository root:

```bash
python -m pip install -e .[visual]
python Emulation/RayTracer/raytracer.py --scene Emulation/RayTracer/scenes/mirror_room.json --width 800 --height 600 --output mirror.png
```

The CLI can also build a scene without JSON by stacking geometry options:

```bash
python Emulation/RayTracer/raytracer.py \
  --width 320 --height 240 --output cli-scene.png \
  --sphere "0,0.5,0;0.5;#ff9966;0.1;0.7;0.5;64;0.25" \
  --plane "0,0,0;0,1,0;#cccccc;0.2;0.7;0.1;16;0.05" \
  --light "5,5,-6;#ffffff;1.3" \
  --light "-3,4,-2;#ffd7a8;0.6"
```

Arguments use `;` as separators. Colors accept either hex (`#rrggbb`) or comma-separated RGB tuples.

### Key CLI Arguments

| Flag | Description |
| --- | --- |
| `--scene` | Load a JSON scene description (see samples below). |
| `--sphere` | Add a sphere using `center;radius;color;ambient;diffuse;specular;shininess;reflection`. Repeatable. |
| `--plane` | Add a plane using `point;normal;color;ambient;diffuse;specular;shininess;reflection`. Repeatable. |
| `--light` | Add a point light using `position;color;intensity`. Repeatable. |
| `--background` | Background color (hex or `r,g,b`). |
| `--max-depth` | Reflection depth (default 2). |
| `--camera-*` | Override camera position/look-at/up vectors and field of view. |

## Scene JSON Layout

```json
{
  "camera": {
    "position": [0.0, 1.0, -4.5],
    "look_at": [0.0, 0.5, 0.0],
    "up": [0.0, 1.0, 0.0],
    "fov": 45.0
  },
  "background": [30, 32, 45],
  "max_depth": 3,
  "lights": [
    {"position": [3, 5, -5], "color": [255, 255, 255], "intensity": 1.1}
  ],
  "objects": [
    {
      "type": "sphere",
      "center": [0, 0.6, 0],
      "radius": 0.6,
      "material": {
        "color": [255, 140, 120],
        "ambient": 0.1,
        "diffuse": 0.7,
        "specular": 0.5,
        "shininess": 64,
        "reflection": 0.35
      }
    },
    {
      "type": "plane",
      "point": [0, 0, 0],
      "normal": [0, 1, 0],
      "material": {
        "color": [220, 220, 220],
        "ambient": 0.2,
        "diffuse": 0.7,
        "specular": 0.1,
        "shininess": 8,
        "reflection": 0.1
      }
    }
  ]
}
```

## Sample Scenes

- `minimal.json` – compact two-object scene used by the automated tests (fast to render).
- `mirror_room.json` – reflective wall-and-floor setup with three spheres and two colored lights.

Both scenes live under `Emulation/RayTracer/scenes/` and render in well under a second at 400×300 on a modern laptop CPU.

## Benchmark Notes

| Scene | Resolution | Max Depth | Intel i7-8650U (single core) |
| --- | --- | --- | --- |
| `minimal.json` | 320×240 | 2 | ~0.35 s |
| `mirror_room.json` | 640×480 | 3 | ~1.9 s |

> Timings measured with Python 3.11 on Windows 11 using `python -m timeit -n1 -r1 -s "from Emulation.RayTracer.raytracer import load_scene_file, RayTracer; scene = load_scene_file('Emulation/RayTracer/scenes/mirror_room.json')" "RayTracer(640, 480, scene).render()"`. Results vary by CPU; use the numbers as rough guidance.

## Development & Testing

The module exposes helper functions for loading JSON scenes (`load_scene_file`) and an imperative `RayTracer.render()` that returns a Pillow `Image`. The repository's pytest suite includes a regression test that renders the minimal scene, hashes the pixel buffer, and ensures deterministic output.

## Dependencies

The renderer relies on `numpy` for vector math and `Pillow` for image creation. Both packages already ship with the repository's `visual` optional extra (`python -m pip install -e .[visual]`).
