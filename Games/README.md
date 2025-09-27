# Games Challenge Hub

A curated playground of `/g/` games implemented across Python, Java, C++, and web stacks. The `Games/` tree mixes fully playable titles with backlog design notes so you can explore existing builds or claim an open challenge.

This README mirrors the structure of the Practical suite guide: start with a quick environment spin-up, consult the index, and drill into per-language playbooks or contribution patterns when you are ready to extend the catalogue.

---

## 1. Quick Start

### 1.1. Clone and scope to the games

```bash
git clone https://github.com/saintwithataint/Pro-g-rammingChallenges4.git
cd Pro-g-rammingChallenges4/Games
```

> Working from the repository root? Swap the final line for `cd Pro-g-rammingChallenges4` and prefix game paths with `Games/` in the commands below.

### 1.2. Create a virtual environment (recommended)

```bash
python -m venv .venv
source .venv/bin/activate  # Windows PowerShell: .venv\Scripts\Activate.ps1
```

### 1.3. Install the `pyproject.toml` extras

The [`games` extra](../pyproject.toml) now bundles `pygame`, `numpy`, `PySide6`, and `tcod` so every solved build (Go, Roguelike, Simon, etc.) runs without manual pinning. Layer on optional stacks from the [repository README](../README.md#using-pyprojecttoml) when you need plotting or audio helpers:

```bash
python -m pip install -e .[games]
# Optional add-ons
python -m pip install -e .[visual]  # matplotlib-powered visualisers (Shuffle)
python -m pip install -e .[audio]   # pygame audio backends (Simon, Connect 4)
```

Editable extras keep Python dependencies lightweight and consistent across the repository.

### 1.4. Launch your first build

```bash
python "Snake/snake.py"          # Turtle graphics Snake
python "Breakout/breakout.py"    # pygame Breakout with JSON levels
python "Simon/simon.py"          # Pygame Simon (loads local assets)
python "OilPanic/oil_panic.py"   # Dual-screen Game & Watch tribute
python "Shuffle/cards.py"        # Matplotlib card shuffler
```

Prefer Java or C++? Check the run matrix in [Section 4](#4-run-guides-by-language).

---

## 2. Project Index (solved builds)

Authoritative map of every folder under `Games/` and its `/g/` challenge number. Each entry links to the implementation, summarises the shipped features, and lists the primary technologies in use.

| # | Folder | Status | Summary | Key Tech |
| --- | --- | --- | --- | --- |
| 104 | [Knight Tour](Knight%20Tour/) | Solved | Command-line backtracking solver with Warnsdorff ordering and configurable board size/start. | Python 3 stdlib |
| 105 | [Monster Raising](Monster%20Raising/) | Solved | Pygame stable sim with daily care loops, breeding lab, and JSON saves for rosters and events. | Python, pygame |
| 106 | [Tetris](Tetris/) | Solved | Pygame Tetris featuring seven-piece bag randomiser, DAS/ARR tuning, ghost piece toggle, and modular modules. | Python, pygame |
| 107 | [Snake](Snake/) | Solved | Classic snake delivered as both a turtle graphics script and an HTML5 canvas build with adjustable speed. | Python turtle, HTML Canvas/JavaScript |
| 108 | [Pipe Dreams](Pipe%20Dreams/) | Solved | TypeScript + Vite pipe puzzler with procedural boards, drag-and-drop editing, leak detection, and timers. | TypeScript, Vite, Canvas |
| 109 | [Pacman](Pacman/) | Solved | Pygame homage with JSON-defined mazes, classic ghost AI cycles, and configurable speed tables. | Python, pygame |
| 110 | [RPGEngine](RPGEngine/) | Solved | Data-driven RPG sandbox covering overworld exploration, dialogue/quest graphs, and menu-driven combat. | Python, pygame, JSON/YAML |
| 111 | [RPS](RPS/) | Solved | Multi-language Rock Paper Scissors suite with Python Lizard/Spock mode, C++/Java CLIs, and a themed web UI. | Python, C++, Java, HTML/JS |
| 112 | [FirstPersonEngine](FirstPersonEngine/) | Solved | C++ OpenGL playground with GLFW/GLAD, mouse-look movement, configurable controls, and JSON level loading. | C++17, OpenGL, GLFW/GLAD |
| 113 | [Shuffle](Shuffle/) | Solved | Matplotlib deck visualiser that shuffles/reset decks interactively for randomness demonstrations. | Python, matplotlib |
| 114 | [MultiAgentTag](MultiAgentTag/) | Solved | Pygame tag simulation with steering behaviours, replay capture, headless batching, and matplotlib analytics. | Python, pygame, matplotlib |
| 116 | [ScorchedEarth](ScorchedEarth/) | Solved | Artillery clone sporting destructible terrain, wind, configurable weapons, and human/AI pilots. | Python, pygame |
| 117 | [Minesweeper](Minesweeper/) | Solved | Tkinter Minesweeper with adjustable board/mine counts and dialog-driven win/loss flow. | Python, tkinter |
| 118 | [64kDemo](64kDemo/) | Solved | Procedural WebGL/Web Audio demo with a Python build script that packs the bundle under the 64 KB target. | JavaScript, WebGL, Web Audio |
| 119 | [Sudoku](Sudoku/) | Solved | Tkinter + numpy Sudoku generator/solver with difficulty presets, highlighting, and reusable logic module. | Python, tkinter, numpy |
| 120 | [DanmakuEngine](DanmakuEngine/) | Solved | PixiJS bullet hell engine supporting scripted patterns, boss phases, and tooling for new bullet behaviours. | TypeScript, PixiJS |
| 121 | [Roguelike](Roguelike/) | Solved | Terminal roguelike using tcod with procedural dungeons, combat/inventory systems, and JSON save/load. | Python, tcod |
| 122 | [UnityEngine](UnityEngine/) | Solved | Unity 2022.3 ECS-style foundation with event bus, scene catalog/loader, and JSON serialisation helpers. | Unity C#, ECS |
| 123 | [Yahtzee](Yahtzee/) | Solved | Yahtzee scoring suite provided as a Python CLI plus a matching Java console implementation. | Python stdlib, Java |
| 124 | [OilPanic](OilPanic/) | Solved | Dual-screen Game & Watch tribute with difficulty modes, scalable window, and persistent high scores. | Python, pygame |
| 125 | [Chess](Chess/) | Solved | Python chess engine offering PGN support, minimax AI, CLI shell, and Pygame board UI. | Python, pygame |
| 126 | [Go](Go/) | Solved | PySide6 Go client with capture/ko enforcement, undo, SGF export, and optional practice AI. | Python, PySide6 |
| 127 | [Connect4](Connect4/) | Solved | Connect Four delivered via pygame board with audio cues plus a Java console variant. | Python, pygame, Java |
| 128 | [Mastermind](Mastermind/) | Solved | Mastermind puzzle with CLI and tkinter GUIs, configurable difficulty, and code-maker/breaker modes. | Python, tkinter |
| 129 | [MissileCommand](MissileCommand/) | Solved | Missile Command remake with progressive waves, cooperative targeting cursor, and procedural audio. | Python, pygame, numpy |
| 130 | [Tron](Tron/) | Solved | HTML5 canvas lightcycle arena with AI riders, local/remote multiplayer, boosts, and scoring. | JavaScript, Canvas, WebRTC |
| 131 | [Breakout](Breakout/) | Solved | Feature-rich pygame Breakout with JSON levels, power-ups, particles, and a level editor helper. | Python, pygame |
| 132 | [Simon](Simon/) | Solved | Pygame Simon clone with audio/visual feedback, increasing difficulty, and bundled assets. | Python, pygame |

> When you add a new build, append it here with the challenge number, summary, and tech stack so contributors can see coverage at a glance.

---

## 3. Selective Installs (Games-focused extras)

| Focus | Extras | Notes |
|-------|--------|-------|
| All Python games | `games` | Installs numpy, pygame, PySide6, and tcod used across the solved catalog. |
| Visual analytics (Shuffle stats, future dashboards) | `games,visual` | Layers matplotlib/plotly/imageio on top of the base stack for analysis tooling. |
| Audio-heavy builds (Simon patterns, future rhythm games) | `games,audio` | Adds sounddevice/librosa alongside pygame mixers for richer sound pipelines. |
| Development + linting | `games,developer` | Extends with pytest, ruff, and mypy for automated checks. |

Need another stack? Refer to the [root extras table](../README.md#using-pyprojecttoml) to combine categories (e.g., `.[games,web]` for multiplayer dashboards).

---

## 4. Run Guides by Language

Each language links to the dedicated game README for deeper context (rules, controls, screenshots, and assets). Commands assume you are inside `Games/` with the virtual environment activated.

### 4.1. Python

```bash
python "Connect4/connect4.py"   # Requires pygame + numpy
python "Tetris/main.py"         # Pygame Tetris with DAS/ARR controls
python "Minesweeper/mine.py"    # tkinter GUI (bundled with most Python installs)
python "Simon/simon.py"         # Loads Assets/Audio and Assets/Images
python "OilPanic/oil_panic.py"  # Dual-screen Game & Watch tribute
python "Sudoku/sudoku.py"       # Needs numpy + tkinter
python "Shuffle/cards.py"       # matplotlib visualiser

python "ScorchedEarth/scorched_earth.py"  # Pygame artillery with destructible terrain

python "Pacman/main.py"        # Pygame Pac-Man with behavioural ghosts

python "Yahtzee/yahtzee.py"     # CLI edition
python "Snake/snake.py"         # Turtle graphics, ensure tkinter is available
python "Breakout/breakout.py"   # pygame-powered Breakout with JSON levels
python "Knight Tour/knight.py"  # CLI solver visualises via stdout
python "Mastermind/mastermind_cli.py"  # CLI Mastermind with difficulty presets
python "Mastermind/mastermind_gui.py"  # tkinter GUI with code-maker mode
python "RPS/rpsls.py"           # CLI (supports Lizard/Spock variant)

python -m Chess.cli              # CLI chess with optional AI
```

Per-game documentation: [Connect Four](Connect4/README.md), [Minesweeper](Minesweeper/README.md), [Simon](Simon/README.md), [Sudoku](Sudoku/README.md), [Shuffle](Shuffle/README.md), [Yahtzee](Yahtzee/README.md), [Snake](Snake/README.md), [Knight's Tour]("Knight Tour"/README.md), [Rock Paper Scissors](RPS/README.md), [Chess](Chess/README.md).


python -m Games.Roguelike.main   # Terminal roguelike (tcod)

python "Monster Raising/main.py"  # pygame ranch sim with JSON saves

```


Per-game documentation: [Connect Four](Connect4/README.md), [Minesweeper](Minesweeper/README.md), [Simon](Simon/README.md), [Sudoku](Sudoku/README.md), [Shuffle](Shuffle/README.md), [Yahtzee](Yahtzee/README.md), [Snake](Snake/README.md), [Knight's Tour]("Knight Tour"/README.md), [Rock Paper Scissors](RPS/README.md), [Breakout](Breakout/README.md).


Per-game documentation: [Connect Four](Connect4/README.md), [Minesweeper](Minesweeper/README.md), [Simon](Simon/README.md), [Sudoku](Sudoku/README.md), [Shuffle](Shuffle/README.md), [Yahtzee](Yahtzee/README.md), [Snake](Snake/README.md), [Knight's Tour]("Knight Tour"/README.md), [Rock Paper Scissors](RPS/README.md), [Pac-Man](Pacman/README.md).

Per-game documentation: [Connect Four](Connect4/README.md), [Tetris](Tetris/README.md), [Minesweeper](Minesweeper/README.md), [Simon](Simon/README.md), [Sudoku](Sudoku/README.md), [Shuffle](Shuffle/README.md), [Yahtzee](Yahtzee/README.md), [Snake](Snake/README.md), [Knight's Tour]("Knight Tour"/README.md), [Rock Paper Scissors](RPS/README.md).




Assets are co-located inside each project (e.g., `Simon/Assets/Audio`). Keep relative paths intact when running outside the repo root.

### 4.2. Java

```bash
javac Connect4/connect4.java && java -cp Connect4 connect4
javac Yahtzee/yahtzee.java && java -cp Yahtzee Yahtzee
javac RPS/rps.java && java -cp RPS rps
```

Ensure `javac`/`java` are on your `PATH`. See the linked READMEs for gameplay controls and optional CLI arguments: [Connect Four](Connect4/README.md), [Yahtzee](Yahtzee/README.md), [Rock Paper Scissors](RPS/README.md).

### 4.3. C++

```bash
g++ RPS/rps.cpp -o RPS/rps
./RPS/rps

cmake -S FirstPersonEngine -B FirstPersonEngine/build
cmake --build FirstPersonEngine/build
./FirstPersonEngine/build/first_person_engine  # Launch (Linux/macOS)
FirstPersonEngine\build\Debug\first_person_engine.exe  # Windows example
```

The `FirstPersonEngine` project ships its own [README](FirstPersonEngine/README.md) with platform-specific notes and VR hook instructions.

### 4.4. JavaScript / Web

1. Open the HTML file directly in a browser:
   - `Snake/snake.html`
   - `RPS/rps.html`

   - `64kDemo/dist/index.html` (after running `python 64kDemo/build.py`)


   - `Tron/index.html`


   - `DanmakuEngine/index.html` (or run `npm run dev` inside `DanmakuEngine/` for PixiJS build tooling)


2. For asset-backed games, keep the accompanying `.js`, `.css`, and `assets/` folders in the same directory.
3. Optional: use a static server for clean module loading (`python -m http.server` from within the game folder).

See [Snake](Snake/README.md) and [Rock Paper Scissors](RPS/README.md) for control schemes and asset notes.

2. Launch the Vite dev server for the TypeScript canvas build:
   ```bash
   cd "Pipe Dreams"
   npm install
   npm run dev
   ```
   Use `npm run build` for a static bundle (already included under `Pipe Dreams/dist/`).
3. For asset-backed games, keep the accompanying `.js`, `.css`, and `assets/` folders in the same directory.
4. Optional: use a static server for clean module loading (`python -m http.server` from within the game folder).

See [Snake](Snake/README.md), [Rock Paper Scissors](RPS/README.md), and [Pipe Dreams](Pipe%20Dreams/README.md) for control schemes and asset notes.


---

## 5. Troubleshooting

| Symptom | Fix |
|---------|-----|
| `ModuleNotFoundError: pygame` or `numpy` | Install extras with `python -m pip install -e .[games]` (virtual env recommended). |
| Python GUI fails with `No module named '_tkinter'` | Install a system package that provides Tk (macOS: `brew install python-tk`; Ubuntu: `sudo apt-get install python3-tk`). Turtle and tkinter-based games (Snake, Minesweeper, Sudoku) depend on it. |
| Pygame window opens without sound | Ensure `pygame.mixer` can access audio devices. On Linux, install SDL dependencies (`sudo apt-get install libsdl2-mixer-2.0-0`). Assets live under `Simon/Assets`. |
| Java commands fail with `class not found` | Compile within the game folder and include it on the classpath (`-cp`). Follow the exact commands in [Section 4.2](#42-java). |
| Browser games cannot load assets when served remotely | Serve via a static server that preserves folder structure (`python -m http.server`) to avoid cross-origin or relative path issues. |

If issues persist, open an issue referencing the challenge number and include OS + dependency versions.

---

## 6. Contribution Guidelines (Games-specific)

1. **Coding style**: Match existing Python conventions (type hints, docstrings, classes for game state) and follow language idioms for Java/C++ (camelCase methods, header comments). Keep logic modules importable so they can be unit-tested.
2. **Asset handling**: Place media under `<Game>/Assets/` with subfolders (`Audio/`, `Images/`, etc.). Document licensing in the game README and prefer original or CC0/GPL-compatible assets.
3. **README expectations**: Every playable game must ship a `README.md` containing: challenge number, summary, dependency checklist, run commands for each language variant, and asset notes. Use the new READMEs in this directory as templates.
4. **Extras alignment**: When introducing new dependencies, extend `pyproject.toml` extras thoughtfully and update the [root extras table](../README.md#using-pyprojecttoml) plus the table in [Section 3](#3-selective-installs-games-focused-extras).
5. **Testing & linting**: Prefer running `python -m pytest` and `ruff check` (via the `developer` extra) for shared modules. GUI-heavy games should include a CLI-friendly smoke test where feasible.

Ready to contribute? Fork the repo, branch per challenge (`feature/game-<name>`), follow the quick start above, and submit a PR summarising gameplay, assets, and controls.
