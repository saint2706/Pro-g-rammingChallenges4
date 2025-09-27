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

### 1.3. Install pyproject extras for games

Use the editable extras defined in the root [`pyproject.toml`](../pyproject.toml) and cross-referenced in the [repository README](../README.md#using-pyprojecttoml):

```bash
python -m pip install -e .[games]
# Optional add-ons
python -m pip install -e .[visual]  # matplotlib-powered visualisers (Shuffle)
python -m pip install -e .[audio]   # pygame audio backends (Simon, Connect 4)
```

Extras keep Python dependencies lightweight and consistent with the root extras table.

### 1.4. Launch your first build

```bash
python "Snake/snake.py"          # Turtle graphics Snake
python "Breakout/breakout.py"    # pygame Breakout with JSON levels
python "Simon/simon.py"          # Pygame Simon (loads local assets)
python "Shuffle/cards.py"        # Matplotlib card shuffler
```

Prefer Java or C++? Check the run matrix in [Section 4](#4-run-guides-by-language).

---

## 2. Challenge Index (solved + backlog)

Authoritative status board for challenges #104–#132. Entry points list the main script or launcher; multi-language games expose multiple commands.

| #   | Challenge | Status  | Primary Tech Stack | Entry Points |
|-----|-----------|---------|--------------------|--------------|
| 104 | Knight's Tour | Solved | Python 3 (CLI, backtracking) | `Knight Tour/knight.py` |

| 105 | Monster Raising/Breeding Simulator | Solved | Python 3 + pygame | `Monster Raising/main.py` |
| 106 | Tetris | Backlog | TBD | — |

| 105 | Monster Raising/Breeding Simulator | Backlog | TBD | — |
| 106 | Tetris | View Solution | Python 3 + pygame | `Tetris/main.py` |

| 107 | Snake | Solved | Python 3 (turtle), JavaScript + HTML Canvas | `Snake/snake.py`, `Snake/snake.html` |

| 108 | Pipe Dreams | Backlog | TBD | — |
| 109 | Pac-Man (behavioural ghosts) | Solved | Python 3 + pygame | `Pacman/main.py` |

| 108 | Pipe Dreams | Solved | TypeScript + Vite (HTML5 Canvas) | `Pipe Dreams/index.html` (Vite dev server) |
| 109 | Pac-Man (behavioural ghosts) | Backlog | TBD | — |

| 110 | Dragon Quest / Basic RPG Engine | Backlog | TBD | — |
| 111 | Rock Paper Scissors (+Lizard Spock) | Solved | Python 3 (CLI), C++, Java, JavaScript/Web | `RPS/rpsls.py`, `RPS/rps.cpp`, `RPS/rps.java`, `RPS/rps.html` |
| 112 | First-Person Engine (OpenGL) | Solved | C++17, GLFW, GLAD | `FirstPersonEngine` (see README) |
| 113 | Shuffle a Deck (with visualisation) | Solved | Python 3 + matplotlib | `Shuffle/cards.py` |
| 114 | Multi-agent Tag Simulation | Solved | Python 3 + pygame + matplotlib | `MultiAgentTag/tag_sim.py` |
| 115 | Wolfenstein Clone | Backlog | Planned raycaster | — |
| 116 | Scorched Earth Clone | Backlog | TBD | — |
| 117 | Minesweeper | Solved | Python 3 + tkinter | `Minesweeper/mine.py` |
| 118 | 64KB Audio/Visual Demo | Backlog | TBD | — |
| 119 | Sudoku | Solved | Python 3 + tkinter + numpy | `Sudoku/sudoku.py` |

| 120 | Danmaku (Bullet Hell) Engine | Backlog | TBD | — |
| 121 | Roguelike Engine / Dungeon Generator | Solved | Python 3 + tcod | `python -m Games.Roguelike.main` |
| 122 | Design a Game Engine in Unity | Backlog | Unity (planned) | — |

| 120 | Danmaku (Bullet Hell) Engine | Solved | TypeScript + PixiJS | `DanmakuEngine/` (`npm run dev`) |
| 121 | Roguelike Engine / Dungeon Generator | Backlog | TBD | — |
| 122 | Design a Game Engine in Unity | Solved | Unity 2022.3 LTS | `UnityEngine/` |

| 123 | Yahtzee | Solved | Python 3 (CLI), Java | `Yahtzee/yahtzee.py`, `Yahtzee/yahtzee.java` |
| 124 | Oil Panic | Backlog | TBD | — |
| 125 | Chess | Backlog | TBD | — |
| 126 | Go (No AI necessary) | Backlog | TBD | — |
| 127 | Connect Four | Solved | Python 3 + pygame + numpy, Java | `Connect4/connect4.py`, `Connect4/connect4.java` |
| 128 | Mastermind | Solved | Python 3 (CLI + tkinter GUI) | `Mastermind/mastermind_cli.py`, `Mastermind/mastermind_gui.py` |
| 129 | Missile Command | Backlog 
| 130 | Tron | Solved | JavaScript (HTML Canvas + WebRTC) | `Tron/index.html` |
| 131 | Breakout | Backlog | TBD | — |

| 130 | Tron | Backlog | TBD | — |
| 131 | Breakout | Solved | Python 3 + pygame | `Breakout/breakout.py` |

| 132 | Simon | Solved | Python 3 + pygame (audio assets) | `Simon/simon.py` |

> When you create a new implementation, update this table with the tech stack and primary launch command so the backlog remains actionable.

---

## 3. Selective Installs (Games-focused extras)

| Focus | Extras | Notes |
|-------|--------|-------|
| All Python games | `games` | Installs numpy, matplotlib, and pygame used throughout the solved titles. |
| Visual analytics (Shuffle stats, future dashboards) | `games,visual` | Adds plotting + rendering helpers beyond the base game set. |
| Audio-heavy builds (Simon patterns, future rhythm games) | `games,audio` | Pulls in sounddevice/librosa alongside pygame mixers. |
| Development + linting | `games,developer` | Extends with pytest, ruff, mypy for automated checks. |

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
python "Sudoku/sudoku.py"       # Needs numpy + tkinter
python "Shuffle/cards.py"       # matplotlib visualiser
python "Pacman/main.py"        # Pygame Pac-Man with behavioural ghosts
python "Yahtzee/yahtzee.py"     # CLI edition
python "Snake/snake.py"         # Turtle graphics, ensure tkinter is available
python "Breakout/breakout.py"   # pygame-powered Breakout with JSON levels
python "Knight Tour/knight.py"  # CLI solver visualises via stdout
python "Mastermind/mastermind_cli.py"  # CLI Mastermind with difficulty presets
python "Mastermind/mastermind_gui.py"  # tkinter GUI with code-maker mode
python "RPS/rpsls.py"           # CLI (supports Lizard/Spock variant)

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
