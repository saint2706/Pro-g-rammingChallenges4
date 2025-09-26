# Practical Projects Suite

A curated collection of small-to-medium Python (and some multi-language) utilities, demos, and learning projects. Each folder under `Practical/` is intentionally self‑contained so you can explore a single topic (networking, image manipulation, GUIs, CLI patterns, simple web apps) without heavy project scaffolding.

This root README gives you:

- A high‑level map of the subprojects
- Quick setup (global vs per‑tool) guidance
- Usage examples (PowerShell oriented – you're on Windows)
- Common architectural / code patterns used across tools
- Contribution & extension tips

> New here? Skim "Quick Start" then jump to a project that interests you—each script has inline docs and many have their own README or helpful comments.

---

## 1. Quick Start

### 1.1. Clone and enter

```pwsh
git clone https://github.com/saintwithataint/Pro-g-rammingChallenges4.git
cd Pro-g-rammingChallenges4/Practical
```

### 1.2. Create a virtual environment (recommended)

```pwsh
python -m venv .venv
. .venv/Scripts/Activate.ps1   # PowerShell activation
```

### 1.3. Install consolidated dependencies

This installs everything needed for ALL tools (heavier, but simple):

```pwsh
pip install -r requirements.txt
```

Want minimal installs? See "Selective Installs" below.

### 1.4. Run something

```pwsh
python "Imageboard/imageboard.py" --help
python "Seam Carving/resize.py" --help
python "ToDoList-CLI/todo.py" add "Ship awesome README"
```

---

## 2. Project Index (Alphabetical)

Brief synopses; dive into each folder for details.

| Folder | Summary | Key Tech |
|--------|---------|----------|
| Bellman Ford Simulation | Visual Bellman–Ford walkthrough with CLI + Matplotlib GUI controls and exports. | matplotlib, argparse |
| Imageboard | Minimal Flask + SQLite anonymous imageboard (threads, replies, uploads, quoting, thumbnails). | Flask, Pillow, SQLite, Jinja filters |
| ImgToASCII | Convert images to ASCII art (CLI + Tk GUI). | Pillow, NumPy, Tkinter |
| IRC Client | Async terminal IRC client with TLS, reconnection, logging. | Python stdlib (`asyncio`, `ssl`, `argparse`, `logging`) |
| IP Tracking visualization | Fetch IP geolocation data & plot interactive map. | requests, pandas, plotly, tqdm |
| Markdown Editor | Desktop Markdown editor with live preview & exports. | Tkinter, markdown |
| Markov Chain Sentence Generator | Train simple Markov model over corpora (CLI + GUI). | Dataclasses, Tkinter |
| Matrix Arithmetic | Explainable matrix calculator (CLI + GUI) covering add/multiply/det/inverse plus 2D visualiser. | NumPy, Tkinter, matplotlib |
| Stock Market Simulator | Backtest custom strategies over Yahoo Finance data with caching + reports. | pandas, requests, matplotlib (optional) |
| MIDI Player Editor | CLI-based MIDI playback, editing, and export workflow. | mido, python-rtmidi |
| Paint (clone) | Lightweight Tk canvas paint app with palette + save. | Tkinter, Pillow (optional) |
| Pixel Editor | Layered pixel art editor with animation preview & sprite sheet IO. | Tkinter, Pillow |
| PDF Tagger | Add arbitrary JSON metadata tags to PDFs. | pypdf |
| Port Scanner | Concurrent TCP port scanning (CLI + GUI + export). | sockets, ThreadPoolExecutor, Tkinter |
| Producer Consumer | Modernized concurrency patterns (Py/Java/C/C++ examples). | threading, semaphores, queues |
| File Compression Utility | Drag-and-drop archiver with Tk GUI, reusable backend, and tests. | zipfile, tarfile, Tkinter |
| Radix Base Converter | Arbitrary base conversion (2..36) with GUI. | Pure Python, Tkinter |
| Relational DB | Educational in-memory SQL engine with parser, executor, and CLI shell. | Pure Python |
| Seam Carving | Content-aware image resizing (CLI + GUI + progress). | OpenCV, NumPy, Pillow (GUI) |
| TFTP Tool | RFC 1350-compliant UDP file transfer client/server with CLI. | sockets, logging |
| ToDoList-CLI | File‑backed todo manager (undo, prioritize, search). | Dataclasses, color output |
| Vector Product | Vector math utilities & 3D plotting. | matplotlib |

| Verlet Cloth | Cloth simulation using Verlet integration with interactive toggles. | NumPy, matplotlib |

| WAV Equalizer | Real-time multi-band audio equalizer with live spectrum GUI. | numpy, scipy, sounddevice, matplotlib, Tkinter |

| Old School cringe | Retro rotating cube + assets demo. | matplotlib.animation, NumPy |
| Graphing Calculator | Basic expression plotting GUI. | Tkinter, eval sandboxing |
| Paint / Clone | Simple drawing & export. | Tkinter |

> Some tiny folders (e.g. helper assets) may not appear above—table focuses on active code artifacts.

---

## 3. Selective Installs

If you only need a subset, install manually:

| Feature | Minimal Install |
|---------|-----------------|
| Imageboard | `pip install Flask Pillow` |
| Seam Carving | `pip install opencv-python numpy` (Pillow optional for GUI) |
| IP Map | `pip install requests pandas plotly tqdm` |
| IRC Client | no extra packages (Python 3.10+ standard library) |
| PDF Tagger | `pip install pypdf` |
| ASCII Converter | `pip install Pillow numpy` |
| Vector / Rotating Cube | `pip install matplotlib numpy` |

You can also break the monolithic `requirements.txt` later into extras (see "Future Improvements").

---

## 4. Common Patterns & Conventions

- Dataclasses: Configuration + domain objects (`TodoItem`, `CarveConfig`, etc.).
- Escape‑then‑format: For safe HTML injection (Imageboard `format_post` filter).
- GUI Separation: Logic modules importable by GUI (`resize.py` vs `resize_gui.py`).
- Atomic Writes: For todo storage to avoid corruption.
- Thread Cancellation: Event/flag based graceful shutdown (seam carving GUI, scanners).
- CLI Consistency: Most scripts provide `--help` and exit codes >0 on error.
- Optional Imports: Graceful degradation if heavy libs absent.

---

## 5. Usage Examples

A few representative workflows.

### 5.1. Imageboard (development run)

```pwsh
python "Imageboard/imageboard.py" --host 127.0.0.1 --port 5000 --data data_dev
# Open http://127.0.0.1:5000/
```

Uploads go into `data_dev/uploads/`; SQLite DB stored in `data_dev/imageboard.db`.

### 5.2. Seam Carving

```pwsh
python "Seam Carving/resize.py" input.jpg --width -200 -o resized.jpg --progress
```

Negative width means remove columns; use `--height` similarly. GUI:

```pwsh
python "Seam Carving/resize_gui.py"
```

### 5.3. Port Scanner (top 1000 default ports)

```pwsh
python "Port Scanner/scanner.py" 192.168.1.10 --top 1000 --json report.json
```

GUI:

```pwsh
python "Port Scanner/scanner_gui.py"
```

### 5.4. ToDo List

```pwsh
python "ToDoList-CLI/todo.py" add "Refactor imageboard quoting"
python "ToDoList-CLI/todo.py" list --all
python "ToDoList-CLI/todo.py" done 3
```

### 5.5. Markov Chain Sentences

```pwsh
python "Markov Chain Sentence Generator/mcsg.py" corpus.txt --order 3 --sentences 5
```

GUI:

```pwsh
python "Markov Chain Sentence Generator/mcsg_gui.py"
```

### 5.6. Relational DB (SQL shell)

```pwsh
cd "Relational DB"
python -m relational_db.cli
```

Sample session:

```sql
db> CREATE TABLE authors (id INT PRIMARY KEY, name TEXT);
db> INSERT INTO authors VALUES (1, 'Octavia Butler');
db> SELECT * FROM authors;
id | name
1  | Octavia Butler
```

### 5.6. ASCII Conversion

```pwsh
python "ImgToASCII/convert.py" cat.png --width 120 --invert --out cat.txt
```

GUI:

```pwsh
python "ImgToASCII/convert_gui.py"
```

### 5.7. TFTP Tool

```pwsh
# Terminal 1 – start the server on a high, non-privileged port
python "TFTP Tool/cli.py" --port 6969 server data_root

# Terminal 2 – download and upload files with block-size negotiation
python "TFTP Tool/cli.py" --port 6969 get 127.0.0.1 remote.txt local.txt --blksize 2048
python "TFTP Tool/cli.py" --port 6969 put 127.0.0.1 local.txt remote_copy.txt --blksize 1024
```

---

## 6. Troubleshooting

| Symptom | Fix |
|---------|-----|
| `ModuleNotFoundError` | Did you activate the venv and install deps? (`pip list`) |
| Tk windows fail to open | On WSL / headless systems Tk may not be available; run on native desktop. |
| OpenCV import crash | Try reinstall: `pip install --force-reinstall opencv-python` (ensure matching architecture). |
| Pillow fails to load WebP | Install extras: `pip install Pillow[webp]` |
| Plotly map not showing | Ensure you opened the generated HTML in a browser; disable aggressive script blockers. |

---

## 7. Contributing / Extending

1. Keep each tool self-contained (local imports, minimal global state).
2. Prefer adding a focused README inside a subfolder if usage is non-trivial.
3. Add type hints & docstrings for new public functions.
4. For new dependencies: add to root `Practical/requirements.txt` with a brief comment.
5. Avoid breaking existing CLI flags—add new ones instead of renaming where possible.
6. Run a quick lint / self-test: execute the script with `--help` (should not crash) and one typical command.

### Ideas / Future Improvements

- Split `requirements.txt` into `extras` (e.g., `imageboard`, `vision`, `gui`).
- Add lightweight pytest smoke tests per tool.
- Integrate a `pyproject.toml` for dependency groups & tooling (ruff, black, mypy).
- Provide a launcher script to list and run tools interactively.
- Add Dockerfile for the imageboard (production-like run).
- Implement backlink display in Imageboard (posts that reference an ID).

---

## 8. License

Refer to the repository root `LICENSE` file. Unless otherwise noted in a subfolder, projects follow that license.

---

## 9. Attribution & Learning Intent

These utilities are educational. Performance and security are *good enough* for learning but may need hardening for production (especially anything involving networking or file uploads). Treat them as starting points—fork, explore, and iterate.

Happy hacking! 🎯
