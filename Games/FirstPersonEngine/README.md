# First Person Engine (C++ / OpenGL)

A compact OpenGL playground that renders textured walls, floor, and ceiling geometry with basic collision, jumping, and sprinting controls. The project serves as a `/g/` "First Person Engine" challenge solution using GLFW for windowing/input and GLAD for OpenGL loading.

## Features

- **Renderer pipeline** – Compiled GLSL shaders (`assets/shaders/`) power a modular renderer that builds floor, ceiling, and wall meshes from JSON level descriptions.
- **Camera + movement** – Mouse-look, configurable WASD bindings (`config/controls.json`), jump, sprint, and gravity with simple capsule collision against level geometry.
- **Level loader** – `assets/levels/default.json` defines cell size, spawn position, colours, and ASCII layout, parsed via a small built-in JSON reader that surfaces helpful error messages. Drop in new JSON maps to iterate quickly.
- **VR-ready hooks** – Optional `ENABLE_OPENXR` build flag stubs in OpenXR support so developers can bridge to a VR runtime without changing core gameplay code.

## Building

```bash
cmake -S . -B build
cmake --build build
```

The build copies `assets/` and `config/` into the CMake binary directory so the executable can find shaders and level data when launched from `build/`.

### Dependencies

| Platform | Requirements |
|----------|--------------|
| Linux    | `build-essential`, `cmake`, `pkg-config`, `libglfw3-dev`, `libopengl-dev` (Mesa) |
| macOS    | Xcode Command Line Tools (`xcode-select --install`), Homebrew `brew install glfw cmake` |
| Windows  | Visual Studio 2019+ with Desktop C++ workload, [vcpkg](https://github.com/microsoft/vcpkg) or GLFW prebuilt binaries on `%PATH%` |

GLAD is vendored inside `include/third_party/`, so no additional loader packages are required.

### Optional VR hooks

Enable OpenXR stubs by toggling the CMake option:

```bash
cmake -S . -B build -DENABLE_OPENXR=ON
cmake --build build
```

When `ENABLE_OPENXR` is enabled the executable prints which runtime it detects (stubbed as "OpenXR (stubbed integration)"). Link `openxr_loader` to start integrating a full VR path.

## Controls

Default bindings live in `config/controls.json` and can be customised without recompilation:

| Action        | Default |
|---------------|---------|
| Move forward  | `W`     |
| Move backward | `S`     |
| Strafe left   | `A`     |
| Strafe right  | `D`     |
| Jump          | `SPACE` |
| Sprint        | `Left Shift` |

Mouse-look is enabled automatically; press `Esc` to exit.

## Level authoring

A minimal level definition:

```json
{
  "cell_size": 2.0,
  "max_height": 3.0,
  "spawn": [2.5, 1.2, 2.5],
  "layout": [
    "1111",
    "1001",
    "1011",
    "1111"
  ],
  "wall": "#567d46",
  "floor": "#3b3b3b",
  "ceiling": "#7d828a"
}
```

- `layout` strings use `0`/`.` for walkable space and any other character for solid walls.
- `cell_size` controls spacing between grid columns, `max_height` defines wall height.
- Colours accept `#RRGGBB` or `#RRGGBBAA` hex values. The renderer bakes each colour into procedural checkerboard textures.

Add new level files to `assets/levels/` and adjust the loader path in `Application::initialise` to point at your map while prototyping.

## Troubleshooting

| Symptom | Fix |
|---------|-----|
| `glfw3 not found` | Install GLFW dev packages (`apt install libglfw3-dev`, `brew install glfw`, or use vcpkg) and rerun CMake. |
| Blank window | Ensure GPU drivers support OpenGL 3.3. Integrated GPUs on macOS/Windows require up-to-date vendor drivers. |
| Mouse locked on exit | On Linux window managers, alt-tab or run again; GLFW should release cursor on app shutdown. |

