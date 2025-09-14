# Emulation & Algorithmic Visualization Suite

A focused collection of small programs that emulate algorithmic processes or visualize abstract concepts (color extraction, spinning 3D shapes, Eulerian path traversal, ASCII clock rendering). Each subproject is intentionally lightweight and self-contained so you can read, experiment, and extend quickly.

This README provides:

- High-level overview of each subproject
- Quick start & environment setup
- Selective dependency guidance
- Usage examples (PowerShell examples given Windows context)
- Common design / code patterns
- Contribution and extension ideas

> New to the repo? Start with `EulerianPath/hierholzer.py` (pure stdlib, clear algorithmic flow) or `ASCII_Clock/ClockSynced.py` for a fun terminal animation.

---

## 1. Quick Start

### 1.1. Clone and enter

```pwsh
git clone https://github.com/saintwithataint/Pro-g-rammingChallenges4.git
cd Pro-g-rammingChallenges4/Emulation
```

### 1.2. (Recommended) Virtual environment

```pwsh
python -m venv .venv
. .venv/Scripts/Activate.ps1
```

### 1.3. Install everything (heavier, simplest)

```pwsh
pip install -r requirements.txt
```

### 1.4. Or install just what you need (see Section 3)

---

## 2. Project Index

| Folder | Summary | Key Tech | Optional Deps |
|--------|---------|----------|---------------|
| 5 color scheme | Extract dominant 5-color palettes & visualize swatches. | numpy, matplotlib, Pillow | scikit-learn (KMeans), opencv-python |
| ASCII_Clock | Real-time ASCII clock (synced & unsynced variants). | stdlib only | — |
| CellularTextures | (WIP) Procedural texture generation (C++ prototype). | C++ | — |
| CompColor | Color space component visualizer & composite manipulator. | numpy, Pillow | — |
| EulerianPath | Fleury vs Hierholzer algorithm demos (Java/Python/C++). | stdlib (Py) | — |
| SpinnyCube | Text-based + optional VPython 3D spinning cube. | stdlib (text mode) | vpython |

> Some folders may contain experimental or WIP code; stable scripts include `5cs.py`, `comp.py`, `hierholzer.py`, `ClockSynced.py`.

---

## 3. Selective Installs

Install only the dependencies you need:

| Feature | Minimal Install |
|---------|-----------------|
| Palette extraction (5 color scheme) | `pip install numpy Pillow matplotlib` (+ `scikit-learn` for KMeans) |
| Faster palette via OpenCV (optional) | `pip install opencv-python` |
| Component color visualizer (CompColor) | `pip install numpy Pillow` |
| SpinnyCube VPython 3D version | `pip install vpython` |

Most other scripts run with only the Python standard library.

---

## 4. Usage Examples

### 4.1. 5 Color Scheme

```pwsh
python "5 color scheme/5cs.py" input.jpg --k 5 --show --json palette.json
```

Flags:

- `--k` number of clusters (default 5)
- `--show` display matplotlib swatch panel
- `--json` export palette data (RGB + hex)

### 4.2. Component Color Visualizer

```pwsh
python "CompColor/comp.py" cat.jpg --grid 3 --out compcat.jpg
```

### 4.3. Eulerian Path (Hierholzer)

```pwsh
python "EulerianPath/hierholzer.py" graph.txt --format edge-list
```

(See script `--help` for supported formats.)

### 4.4. ASCII Clock (Synced)

```pwsh
python "ASCII_Clock/ClockSynced.py" --refresh 0.2
```

Press Ctrl+C to exit gracefully.

### 4.5. Spinny Cube (Text)

```pwsh
python "SpinnyCube/spinny.py" --mode text --speed 1.2
```

VPython mode (requires GUI environment):

```pwsh
python "SpinnyCube/spinny.py" --mode vpython --edge-color "#ff8800"
```

---

## 5. Common Patterns & Practices

- Dataclasses for configuration / parameter grouping
- Separation of algorithmic logic from I/O where feasible
- Graceful degradation when optional libs are missing (`try/except ImportError`)
- Clear `--help` outputs via `argparse` for CLI discoverability
- Pure stdlib fallbacks to keep entry barrier low
- Incremental complexity: start simple (text/CLI) → add visualization (matplotlib / VPython)

---

## 6. Dependencies Overview

See `requirements.txt` for consolidated install. Rationale:

- `numpy` foundational for pixel arrays and math
- `Pillow` robust image loading/saving (JPEG/PNG, read metadata)
- `matplotlib` quick visual diagnostics and palette swatches
- `scikit-learn` reliable KMeans (convergence, inertia reporting)
- `opencv-python` (optional) faster image ops & resizing
- `vpython` 3D interactive rendering (only for SpinnyCube in vpython mode)

You can safely omit any that a script does not import.

---

## 7. Contributing

1. Keep each script self-contained (avoid cross-folder imports unless refactoring into a shared lib).
2. Add a short module docstring explaining purpose & expected inputs.
3. For new visualizations, provide at least one static sample output (PNG/JPG) if feasible.
4. Prefer deterministic seeds in examples when randomness is involved.
5. Document optional dependencies with a brief comment in `requirements.txt`.
6. Avoid introducing heavy frameworks; simplicity is a goal here.

### Extension Ideas

- Add GIF export for SpinnyCube (text frames → animation)
- Extend Eulerian path demos with interactive step visualizer (curses or simple web UI)
- Port CellularTextures C++ prototype into a Python module with numpy acceleration
- Enhance CompColor with LAB / HSV comparative panels
- Provide benchmarking script for palette extraction strategies (sklearn vs naive)

---

## 8. Troubleshooting

| Symptom | Cause | Remedy |
|---------|-------|--------|
| `ImportError: No module named vpython` | Optional dependency missing | Install `vpython` or switch `--mode text` |
| Palette clustering very slow | Large images | Downscale first or add `--max-size` option (future) |
| Matplotlib window not opening | Running headless | Use `--no-show` and inspect exported files |
| Unicode issues in terminal | Locale mis-config | Try `chcp 65001` in PowerShell (UTF-8) |

---

## 9. License

Refer to the repository root `LICENSE` file.

---

## 10. Educational Intent

These scripts aim to be concise, readable stepping stones—not production systems. Optimize for clarity first; micro-optimizations come after correctness and pedagogy.

Happy exploring! 🧪
