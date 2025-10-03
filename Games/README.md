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
python "WolfensteinClone/game.py"  # Pygame Wolfenstein raycaster
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
| 115 | [WolfensteinClone](WolfensteinClone/) | Solved | Pygame raycasting shooter with textured walls, sprites, minimap overlay, and configurable maps/controls. | Python, pygame |
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

Commands assume you are inside `Games/` with your virtual environment (if any) activated. Every row links to the detailed game README for controls, assets, and extended notes. Use the automated check column to quickly smoke-test builds that ship pytest coverage.

| Game | Run Command(s) | Documentation | Automated Checks |
|------|----------------|---------------|------------------|
| [64k Demo](64kDemo/README.md) | `python 64kDemo/build.py` → open `64kDemo/dist/index.html` | WebGL/Web Audio bundling guide | – |
| [Breakout](Breakout/README.md) | `python "Breakout/breakout.py"` | pygame overview + JSON level format | – |
| [Chess](Chess/README.md) | `python -m Chess.cli` | CLI + pygame UI instructions | – |
| [Connect Four](Connect4/README.md) | *Python*: `python "Connect4/connect4.py"`<br>*Java*: `javac Connect4/connect4.java && java -cp Connect4 connect4` | Rules, AI notes, language variants | – |
| [Danmaku Engine](DanmakuEngine/README.md) | `cd "DanmakuEngine" && npm install && npm run dev` | PixiJS tooling + pattern scripting | – |
| [First Person Engine](FirstPersonEngine/README.md) | `cmake -S FirstPersonEngine -B FirstPersonEngine/build`<br>`cmake --build FirstPersonEngine/build`<br>`./FirstPersonEngine/build/first_person_engine` | Platform setup + controls | – |
| [Go](Go/README.md) | `python "Go/main.py"` | PySide6 UI guide | – |
| [Knight's Tour](Knight%20Tour/README.md) | `python "Knight Tour/knight.py"` | CLI usage + solver parameters | – |
| [Mastermind](Mastermind/README.md) | `python "Mastermind/mastermind_cli.py"`<br>`python "Mastermind/mastermind_gui.py"` | CLI and tkinter UX | – |
| [Minesweeper](Minesweeper/README.md) | `python "Minesweeper/mine.py"` | tkinter board controls | – |
| [Missile Command](MissileCommand/README.md) | `python "MissileCommand/missile_command.py"` | pygame controls + wave tuning | – |
| [Monster Raising](Monster%20Raising/README.md) | `python "Monster Raising/main.py"` | Ranch loop + save data format | – |
| [Multi-Agent Tag](MultiAgentTag/README.md) | `python "MultiAgentTag/main.py"` | Steering behaviours + replay capture | – |
| [Oil Panic](OilPanic/README.md) | `python "OilPanic/oil_panic.py"` | Dual-screen layout + scaling | – |
| [Pac-Man](Pacman/README.md) | `python "Pacman/main.py"` | Maze JSON + ghost AI cycles | – |
| [Pipe Dreams](Pipe%20Dreams/README.md) | `cd "Pipe Dreams" && npm install && npm run dev` | Vite dev server + editor tooling | – |
| [RPG Engine](RPGEngine/README.md) | `python "RPGEngine/main.py"` | Data-driven overworld + battle system guide | – |
| [Roguelike](Roguelike/README.md) | `python -m Games.Roguelike.main` | tcod controls + save files | `pytest tests/test_ai_roguelike_smoke.py` |
| [Rock Paper Scissors](RPS/README.md) | *Python*: `python "RPS/rpsls.py"`<br>*C++*: `g++ RPS/rps.cpp -o RPS/rps && ./RPS/rps`<br>*Java*: `javac RPS/rps.java && java -cp RPS rps`<br>*Web*: open `RPS/rps.html` | Mode matrix + asset notes | – |
| [Scorched Earth](ScorchedEarth/README.md) | `python "ScorchedEarth/scorched_earth.py"` | Terrain settings + multiplayer tips | – |
| [Simon](Simon/README.md) | `python "Simon/simon.py"` | Asset checklist + control scheme | – |
| [Shuffle](Shuffle/README.md) | `python "Shuffle/cards.py"` | Matplotlib configuration | – |
| [Snake](Snake/README.md) | *Python*: `python "Snake/snake.py"`<br>*Web*: open `Snake/snake.html` | Controls + speed settings | – |
| [Sudoku](Sudoku/README.md) | `python "Sudoku/sudoku.py"` | Difficulty presets + solver usage | – |
| [Tetris](Tetris/README.md) | `python "Tetris/main.py"` | DAS/ARR tuning + hotkeys | – |
| [Tron](Tron/README.md) | open `Tron/index.html` or serve via `python -m http.server` | Multiplayer + WebRTC notes | – |
| [Unity Engine](UnityEngine/README.md) | Open in Unity 2022.3+ | ECS scene loader guide | – |
| [Wolfenstein Clone](WolfensteinClone/README.md) | `python "WolfensteinClone/game.py"` | Raycasting internals + assets | `pytest tests/test_wolfenstein_clone.py` |
| [Yahtzee](Yahtzee/README.md) | *Python*: `python "Yahtzee/yahtzee.py"`<br>*Java*: `javac Yahtzee/yahtzee.java && java -cp Yahtzee Yahtzee` | Scorecard reference | – |

Need another build? Browse the [Project Index](#2-project-index-solved-builds) for challenge coverage and language stacks.

---


## 5. Troubleshooting

### 5.1. Installing pygame and core dependencies
- **`ModuleNotFoundError: pygame` or `numpy`** – activate your virtual environment and reinstall the extras bundle: `python -m pip install -e .[games]`. This covers [Breakout](Breakout/README.md), [Pac-Man](Pacman/README.md), [Tetris](Tetris/README.md), [Wolfenstein Clone](WolfensteinClone/README.md), and every other pygame-powered build.
- **Tkinter/Turtle errors (`No module named '_tkinter'`)** – install the system Tk package (`brew install python-tk` on macOS or `sudo apt-get install python3-tk` on Debian/Ubuntu). Games such as [Snake](Snake/README.md), [Minesweeper](Minesweeper/README.md), and [Sudoku](Sudoku/README.md) require it.
- **Audio backends missing** – on Linux, install SDL mixer packages (`sudo apt-get install libsdl2-mixer-2.0-0`) so titles like [Simon](Simon/README.md) and [Monster Raising](Monster%20Raising/README.md) can play bundled sounds.

### 5.2. Asset paths and packaging
- Keep the repository's relative paths intact. Many builds load sprites and data from `Assets/` subdirectories (e.g., [Simon/Assets](Simon/README.md), [Pac-Man/mazes](Pacman/README.md), [WolfensteinClone/assets](WolfensteinClone/README.md)). Running from a different working directory? Prefix the script path with `Games/` or use `python -m` so the loader resolves correctly.
- For web games, serve them with `python -m http.server` or the provided dev server to avoid cross-origin errors. This is essential for [Tron](Tron/README.md), [Snake HTML](Snake/README.md), [Pipe Dreams](Pipe%20Dreams/README.md), and [Danmaku Engine](DanmakuEngine/README.md).
- Packaging for distribution? Copy the entire game folder so JSON levels, audio, and textures travel with the executable.

### 5.3. Window scaling and display issues
- **Oversized windows on high-DPI displays** – most pygame titles expose a resolution constant in their README (see [Oil Panic](OilPanic/README.md), [Scorched Earth](ScorchedEarth/README.md), and [Missile Command](MissileCommand/README.md)). Reduce the default width/height before launching or toggle fullscreen where documented.
- **Raycaster viewport cropped** – [Wolfenstein Clone](WolfensteinClone/README.md) scales the surface after rendering. Adjust the `SCREEN_WIDTH`/`SCREEN_HEIGHT` constants or run in windowed mode if your GPU driver forces scaling.
- **Dual-monitor layouts** – [Oil Panic](OilPanic/README.md) uses a stacked window. If it spawns off-screen, reset the pygame display environment (`SDL_VIDEO_WINDOW_POS=0,0`) before running the script.

#### Quick reference

| Symptom | Fix |
|---------|-----|
| pygame import errors | Install extras with `python -m pip install -e .[games]` (virtual env recommended). |
| Tkinter missing | Install a system Tk package so [Snake](Snake/README.md) and [Minesweeper](Minesweeper/README.md) can boot. |
| No audio in pygame games | Install SDL mixer libs and verify `pygame.mixer.init()` succeeds (see [Simon](Simon/README.md)). |
| Java commands fail with `class not found` | Compile within the game folder and include it on the classpath (`-cp`). Follow the exact commands in the [Run Guides](#4-run-guides-by-language). |
| Browser games cannot load assets remotely | Serve via `python -m http.server` or the framework dev server to preserve relative paths. |

If issues persist, open an issue referencing the challenge number and include OS + dependency versions.

---

## 6. Learning Resources

Level up with the official guides and community walkthroughs that underpin the projects in this catalog:

- **pygame** – [pygame.org docs](https://www.pygame.org/docs/) and the [Write Games with pygame tutorial series](https://www.pygame.org/wiki/tutorials) cover event loops, sprite groups, and audio mixing used by [Breakout](Breakout/README.md), [Pac-Man](Pacman/README.md), [Simon](Simon/README.md), and [Tetris](Tetris/README.md).
- **python-tcod (libtcod)** – The [Python tcod roguelike tutorial](https://python-tcod.readthedocs.io/en/latest/tutorials.html) mirrors the architecture in [Roguelike](Roguelike/README.md), from field-of-view to dungeon generation.
- **PixiJS** – Pixi's [official guide](https://pixijs.com/guides) and [PixiJS GitHub examples](https://github.com/pixijs/examples) map directly to the rendering patterns in [Danmaku Engine](DanmakuEngine/README.md).
- **Web build tooling** – Vite's [Getting Started](https://vitejs.dev/guide/) and Mozilla's [MDN Canvas tutorials](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API/Tutorial) support [Pipe Dreams](Pipe%20Dreams/README.md), [Tron](Tron/README.md), and the HTML variant of [Snake](Snake/README.md).
- **3D/OpenGL foundations** – Work through [LearnOpenGL](https://learnopengl.com/) and CMake's [official docs](https://cmake.org/cmake/help/latest/) before diving into [First Person Engine](FirstPersonEngine/README.md).
- **Unity ECS patterns** – Unity's [Entities docs](https://docs.unity3d.com/Packages/com.unity.entities@1.0/manual/index.html) complement the architecture in [Unity Engine](UnityEngine/README.md).

Looking for more inspiration? The `/g/` challenge threads linked inside each game README often include the original problem statements and community design discussions.

---


## 7. Contribution Guidelines (Games-specific)

1. **Coding style**: Match existing Python conventions (type hints, docstrings, classes for game state) and follow language idioms for Java/C++ (camelCase methods, header comments). Keep logic modules importable so they can be unit-tested.
2. **Asset handling**: Place media under `<Game>/Assets/` with subfolders (`Audio/`, `Images/`, etc.). Document licensing in the game README and prefer original or CC0/GPL-compatible assets.
3. **README expectations**: Every playable game must ship a `README.md` containing: challenge number, summary, dependency checklist, run commands for each language variant, and asset notes. Use the new READMEs in this directory as templates.
4. **Extras alignment**: When introducing new dependencies, extend `pyproject.toml` extras thoughtfully and update the [root extras table](../README.md#using-pyprojecttoml) plus the table in [Section 3](#3-selective-installs-games-focused-extras).
5. **Testing & linting**: Prefer running `python -m pytest` and `ruff check` (via the `developer` extra) for shared modules. GUI-heavy games should include a CLI-friendly smoke test where feasible.

Ready to contribute? Fork the repo, branch per challenge (`feature/game-<name>`), follow the quick start above, and submit a PR summarising gameplay, assets, and controls.
