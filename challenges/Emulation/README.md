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

### 1.3. Install via pyproject extras

From the repo root install the extras that match the demos you want to run:

```pwsh
python -m pip install -e .[visual]
python -m pip install -e .[algorithmic]   # optional math helpers
python -m pip install -e .[ai]            # palette clustering (scikit-learn)
python -m pip install -e .[emulation]    # pygame-backed CHIP-8 renderer/input
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
| Chip8 | CHIP-8 emulator with modular CPU/memory/display/input stacks. | Python, pygame (optional) | pygame |
| EulerianPath | Fleury vs Hierholzer algorithm demos (Java/Python/C++). | stdlib (Py) | — |
| NBodySimulator | 2D gravitational sandbox with merging collisions. | pygame | `visual` extra (pygame) |
| SpinnyCube | Text-based + optional VPython 3D spinning cube. | stdlib (text mode) | vpython |
| FFTSpectrum | Real-time FFT spectrum analyzer with mic/WAV sources. | numpy, matplotlib, sounddevice | — |

> Some folders may contain experimental or WIP code; stable scripts include `5cs.py`, `comp.py`, `hierholzer.py`, `ClockSynced.py`.

## Using pyproject.toml

Editable installs keep the code you edit inside this repo linked to your environment.

1. Create a virtual environment (recommended).

   ```pwsh
   python -m venv .venv
   . .venv/\Scripts\Activate.ps1  # Windows
   ```

2. Move to the repository root and install extras, e.g. `python -m pip install -e .[visual]`.
3. Stack extras together: `.[visual,algorithmic]` for plotting + numerical helpers, `.[visual,ai]` for VPython + scikit-learn palette clustering.

## 3. Selective Installs

Install only the extras you need:

| Feature | Extras | Notes |
|---------|--------|-------|
| Palette extraction (5 color scheme) | `visual,algorithmic` | Includes numpy, Pillow, matplotlib, scikit-learn. |
| OpenCV acceleration | `visual` | Adds `opencv-python` alongside Pillow/imageio. |
| Component color visualizer (CompColor) | `visual` | Only Pillow + numpy required. |
| SpinnyCube VPython 3D version | `visual` | VPython is part of the visual extra. |
| FFTSpectrum real-time analyzer | `visual` | Adds numpy, matplotlib, sounddevice for live plots. |
| Pure stdlib demos (Eulerian path, ASCII clock) | *(none)* | Standard library only. |

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

All third-party packages are grouped in `pyproject.toml` extras:

- `visual` &mdash; Pillow, matplotlib, imageio, opencv-python, colour-science, vpython
- `algorithmic` &mdash; numpy + scipy for numeric helpers
- `ai` &mdash; scikit-learn when you want KMeans palette clustering

Activate the extras relevant to your experiment; the rest of the suite remains pure stdlib.

---

## 7. Contributing

1. Keep each script self-contained (avoid cross-folder imports unless refactoring into a shared lib).
2. Add a short module docstring explaining purpose & expected inputs.
3. For new visualizations, provide at least one static sample output (PNG/JPG) if feasible.
4. Prefer deterministic seeds in examples when randomness is involved.
5. Document optional dependencies by extending the relevant `pyproject.toml` extras and README tables.
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
