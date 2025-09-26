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

### 1.3. Install via pyproject extras

From the repo root install the bundles you need (editable install keeps code + env in sync). The full menu of extras lives in the [repository root README](../README.md#using-pyprojecttoml):

```pwsh
python -m pip install -e .[practical]
# Optional focused stacks
python -m pip install -e .[audio]     # Sound synthesis, WAV equalizer
python -m pip install -e .[desktop]   # Window manager, key press bot
python -m pip install -e .[web]       # Imageboard, IP tracking dashboard
```

You can mix extras (e.g., `.[practical,audio]`). See "Selective Installs" for a quick matrix.

### 1.4. Run something

```pwsh
python "Imageboard/imageboard.py" --help
python "Seam Carving/resize.py" --help
python "ToDoList-CLI/todo.py" add "Ship awesome README"
```

## Using pyproject.toml

All Practical projects share the repo-wide optional extras defined in `pyproject.toml`. Install `.[practical]` for the full toolkit or combine targeted extras (audio, web, desktop, markdown, geo, midi) depending on what you plan to explore. Editable installs (`pip install -e .[extra]`) keep your changes instantly runnable. See the table below for combinations and the [root extras table](../README.md#using-pyprojecttoml) for cross-category options.

---

## 2. Project Index (by challenge number)

Authoritative mapping of Practical folders to their original /g/ challenge numbers. Summaries reflect the current implementations; check each folder for scripts, assets, and tests.

| # | Folder | Summary | Key Tech |
|---|--------|---------|----------|
| 1 | Download Manager | Threaded HTTP downloader with resume, chunk retries, and SHA-256 verification. | requests, tqdm, concurrent.futures |
| 2 | Producer Consumer | Cross-language producer/consumer demos showcasing semaphores and bounded queues. | Python threading, C/C++, Java |
| 3 | IRC Client | Async IRC client with TLS, reconnection logic, and command scripting support. | asyncio, ssl, logging |
| 4 | Markov Chain Sentence Generator | CLI + Tk GUI for training Markov chains and sampling shitpost-friendly text. | Tkinter, argparse, dataclasses |
| 5 | Context Pointer | spaCy-powered CLI that surfaces token context, dependencies, and entity metadata. | spaCy, argparse, dataclasses |
| 6 | MIDI Player Editor | Editable MIDI playlist/transform pipeline with CLI playback/export helpers. | mido, python-rtmidi, argparse |
| 7 | Stock Market Simulator | Yahoo Finance backtester with caching, strategy hooks, and reporting suite. | pandas, requests, dataclasses |
| 8 | WAV Equalizer | Real-time multi-band equalizer with FFT visualization and device routing. | numpy, scipy, sounddevice |
| 9 | Graphing Calculator | SymPy-backed graphing GUI with derivatives, exports, and sandboxed parsing. | Tkinter, SymPy, matplotlib |
| 10 | ToDoList-CLI | File-backed todo manager supporting priorities, search, undo, and colored output. | argparse, dataclasses, ANSI colors |
| 11 | Verlet Cloth | Interactive cloth simulator with pin constraints and matplotlib visualization. | numpy, matplotlib |
| 12 | Chat Server Client | TCP/UDP chat stack with async client, retrying UDP demo, and logging. | asyncio, sockets, argparse |
| 13 | Music Streaming | LAN audio streaming server/client with playlist metadata and discovery. | Flask, mutagen, sockets |
| 14 | Shazam Clone | Audio fingerprinting pipeline using spectrogram peaks and MinHash search. | librosa, numpy, sounddevice |
| 15 | Chatbot | Rule-based chatbot with JSON rule loading, history export, and CLI tweaks. | argparse, dataclasses |
| 16 | Curses Text Editor | Modal curses editor with autosave, search/replace, and configurable keymaps. | curses, textpad |
| 17 | Paint | Tkinter paint clone with palette management, brush settings, and image export. | Tkinter, Pillow |
| 18 | ImgToASCII | Image to ASCII renderer offering CLI batch mode and Tk preview. | Pillow, numpy, Tkinter |
| 19 | Booru Imageboard Downloader | Multi-board downloader supporting bulk tags, rate limits, and metadata export. | requests, concurrent.futures, tqdm |
| 20 | Image Converter | Batch converter with resizing, metadata preservation, and Tk GUI controls. | Pillow, Tkinter |
| 21 | ID3 Reader | MP3 tag inspector exporting JSON/CSV summaries with optional Tk interface. | mutagen, Tkinter |
| 22 | Sound Synthesis | Synth toolkit for generating waveforms, envelopes, and live MIDI playback. | numpy, sounddevice, mido |
| 23 | C++ IDE Plugin | Sublime Text plugin wrapping libclang for completion, go-to, and indexing. | Sublime API, clang.cindex |
| 24 | Simple VCS | Educational file-based VCS with revision caps, locking, and CLI tooling. | pathlib, json, argparse |
| 25 | Imageboard | Flask + SQLite imageboard with threads, moderation tools, and thumbnailing. | Flask, SQLite, Pillow |
| 26 | Password Manager | CLI vault with PBKDF2-derived keys, AES-GCM, and manifest sync. | cryptography, PBKDF2 |
| 27 | Torrent Client | Educational BitTorrent client with resume, peer management, and CLI progress. | sockets, threading, tqdm |
| 28 | Booru Client | Tag-searchable booru browser with download queue and Tkinter gallery. | Tkinter, requests, Pillow |
| 29 | Key Press Bot | Cross-platform automation bot with macro recorder and hotkey scripting. | pynput, pyautogui |
| 30 | IP URL Obscurifier | Encoder/decoder for IPv4/URL disguises in mixed bases and formats. | argparse, ipaddress |
| 31 | Radix Base Converter | GUI converter handling arbitrary bases, validation, and copy-friendly output. | Tkinter |
| 32 | Chan Aggregator | Aggregates boards with caching, CLI search, and archival helpers. | requests, threading, dataclasses |
| 33 | Encrypted Upload | AES-GCM packaging with S3 upload hooks, manifests, and integrity checks. | cryptography, boto3 |
| 34 | AutoSave Text Editor | Tk text editor with autosave hashing, atomic writes, and status bar. | Tkinter, dataclasses |
| 35 | HSV color wheel | HSV visualizer rendering gradients, swatches, and conversions via Tkinter. | Tkinter, colorsys |
| 36 | Window Manager | X11 tiling window manager with master/stack layouts and configurable bindings. | python-xlib |
| 37 | Relational DB | In-memory SQL engine with parser, executor, and interactive shell. | dataclasses, Python stdlib |
| 38 | Pixel Editor | Sprite editor with layers, animation preview, and palette tools. | Tkinter, Pillow |
| 39 | TFTP Tool | RFC 1350 compliant client/server with CLI, retransmits, and logging. | sockets, argparse |
| 40 | Markdown Editor | Live preview markdown editor with export, themes, and Tk HTML widget. | Tkinter, markdown, tkhtmlview |
| 41 | IP Tracking visualization | Fetches IP geodata and plots interactive maps with caching. | requests, pandas, plotly |
| 42 | Port Scanner | Concurrent TCP scanner with CLI + Tk GUI dashboards and exports. | sockets, concurrent.futures, Tkinter |
| 43 | Old School cringe | Demoscene effects sequencer mixing plasma, scrolling text, and audio. | pygame, numpy |
| 135 | Bellman Ford Simulation | CLI + matplotlib walkthrough of Bellman-Ford with exportable plots. | argparse, matplotlib |
| 136 | Matrix Arithmetic | Matrix calculator with GUI, explanations, and visualizers for operations. | numpy, Tkinter, matplotlib |
| 137 | File Compression Utility | Tk GUI for ZIP/TAR packing with drag/drop and reusable backend. | Tkinter, zipfile, tarfile |
| 138 | PDF Tagger | CLI for attaching JSON metadata tags to PDFs and auditing contents. | pypdf |
| 139 | Nonogram Solver | Nonogram generator/solver with interactive Tk board and exports. | Tkinter, Pillow |
| 140 | Vector Product | Vector math helper computing dot/cross with 3D plotting. | numpy, matplotlib |
| 141 | Bismuth Fractal | Turtle graphics fractal renderer with animation and palette controls. | turtle, argparse |
| 142 | Seam Carving | Content-aware image resizer with CLI + GUI and progress feedback. | opencv-python, numpy, Pillow |
| 143 | Bayesian Filter | Gaussian Naive Bayes CLI using pandas datasets and rich metrics output. | pandas, scikit-learn |
| 144 | WMS Viewer | Desktop WMS client with map tiling, reprojection, and YAML presets. | Tkinter, requests, pyproj |

## 3. Selective Installs (pyproject extras)

Mix and match extras to keep installs lean. Combine them in a single command, e.g. `python -m pip install -e .[practical,audio]`.

| Focus | Extras | Notes |
|-------|--------|-------|
| All practical projects | `practical` | Pulls every dependency pinned for the Practical/ tree. |
| Audio pipelines (Shazam clone, WAV Equalizer, Sound Synthesis) | `audio` (or `practical,audio`) | Adds librosa, sounddevice, mutagen, scipy. |
| Web-facing tools (Imageboard, IP Tracker, Encrypted Upload) | `practical,web` | Flask + requests + tqdm convenience. |
| Imaging & GUI editors (Seam Carving, Pixel Editor, ImgToASCII) | `practical,visual` | Brings in Pillow, OpenCV, plotly/imageio for exports. |
| Desktop automation (Window Manager, Key Press Bot) | `practical,desktop` | Installs python-xlib, pynput, pyautogui, colorama. |
| Geospatial tooling (WMS viewer) | `practical,geo` | Provides pyproj + PyYAML configuration helpers. |
| Markdown editor suite | `practical,markdown` | Adds markdown + tkhtmlview widgets. |
| MIDI workflow | `practical,midi` | Installs mido + python-rtmidi. |

Need test tooling? Add `developer` for pytest, ruff, and mypy.

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
