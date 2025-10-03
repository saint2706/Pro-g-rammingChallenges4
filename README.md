
# Pro-g-ramming Challenges 4

[![Wakatime badge](https://wakatime.com/badge/github/saintwithataint/Pro-g-rammingChallenges4.svg)](https://wakatime.com/badge/github/saintwithataint/Pro-g-rammingChallenges4)

Welcome to the fourth iteration of my programming challenges repository! This project is a personal journey of learning and implementing various programming challenges, spanning different categories and difficulty levels.

## About This Project

This repository is a collection of solutions to a wide array of programming problems, inspired by the `/g/` programming challenges list. The goal is to learn new languages, explore different programming paradigms, and build a portfolio of work. Whether you're a beginner looking for inspiration or an experienced developer curious about different problem-solving approaches, you'll find something of interest here.

The solutions are organized by category and difficulty, making it easy to navigate and find what you're looking for. Many of the projects are implemented in multiple languages to showcase different ways of solving the same problem.

## Progress

<progress value="100" max="131"></progress>

**Overall:** 100 / 131 challenges completed (76.3%).

| Category | Completed | Total | Progress |
| --- | --- | --- | --- |
| Practical | 53 | 53 | 100% |
| Algorithmic | 27 | 27 | 100% |
| Artificial Intelligence | 4 | 8 | 50.0% |
| Emulation/Modeling | 6 | 14 | 42.9% |
| Games | 10 | 29 | 34.5% |

_Progress counts are generated from the actual solution folders in the repository (see tables below)._ 

## How to Contribute (A Guide for New Developers)

While this is a personal project, the principles behind it are universal. If you're starting your own journey of programming challenges, here's a guide to help you get the most out of it:

1. **Understand the Problem:** Before writing a single line of code, do your research. Draw diagrams, write out the logic, and break the problem down into smaller pieces. If a problem seems too easy, think about how you can add more features or complexity.
2. **Implement a Solution:** Start with a language you're comfortable with to get a working solution first. Don't worry about writing perfect code on the first try. The goal is to get it working, then you can refactor and improve it.
3. **Test Your Code:** Think about how your code might fail. Write test cases to cover different scenarios and edge cases. Find bugs, fix them, and document your process.
4. **Re-implement and Explore:** Once you have a solid solution, try implementing it in a new language. Experiment with different algorithms, data structures, and programming styles. This is a great way to deepen your understanding and expand your skills.
5. **Never Stop Learning:** Keep challenging yourself. Contribute to open source, start new projects, and keep building your portfolio.

## Using pyproject.toml

The repository now ships a `pyproject.toml` so you can install challenge stacks as editable extras.

1. **Create a virtual environment** (recommended):
   ```bash
   python -m venv .venv
   source .venv/bin/activate  # PowerShell: .venv\Scripts\Activate.ps1
   ```
2. **Install the extras you need**:
   ```bash
   python -m pip install -e .[practical]
   python -m pip install -e .[algorithmic]
   ```
   Combine extras (e.g., `.[practical,visual]`) or grab everything with `.[all]`.
3. **Run scripts/tests** directly from the repo. Editable installs keep your checkout in sync with the environment.

| Extra | Covers | Highlights |
| ----- | ------ | ---------- |
| `practical` | `Practical/` utilities, desktop apps, and web tools | Flask imageboard, Streamlit dashboards, Seam Carving, Shazam clone |
| `algorithmic` | `Algorithmic/` problem set helpers | Steganography, stock analysis, crawler tooling |
| `visual` | Visualization add-ons used across categories | Matplotlib demos, colour-science palettes, VPython spinny cube |
| `audio` | Audio processing stacks | WAV equalizer, Shazam clone, music streaming |
| `games` | Python games in `Games/` | Sudoku solver, Simon, Oil Panic tribute |
| `ai` | `Artificial Intelligence/` demos | A* Sudoku, Connect4 AI, neural network |
| `web` | HTTP and dashboard helpers | Imageboard, IP tracking, web crawlers |
| `desktop` | GUI/automation conveniences | Window manager, key press bot, Tk front-ends |
| `markdown` | Markdown Editor stack | Live preview, HTML export |
| `geo` | Geospatial helpers | WMS viewer, map projections |
| `media` | Download/encoding helpers | YouTube to MP3 workflows |
| `security` | Crypto utilities | Password manager, encrypted upload |
| `documents` | PDF tooling | PDF Tagger |
| `midi` | MIDI pipeline | MIDI player/editor, synth tools |
| `developer` | Repo test/lint helpers | pytest, ruff, mypy |

See the category READMEs for per-project suggestions; each now points back to these extras.

## Streamlit deployment keep-alive workflow

The repository includes a GitHub Actions workflow at `.github/workflows/keep-streamlit-alive.yml` that pings the deployed Streamlit app on a schedule. The job runs hourly to keep the hosted instance warm and also supports manual runs so maintainers can verify that the endpoint is still responding.

### Configure the `STREAMLIT_PING_URL` secret

1. In GitHub, open **Settings → Secrets and variables → Actions** for this repository.
2. Add a new repository secret named `STREAMLIT_PING_URL` that contains the fully qualified URL of the Streamlit deployment (e.g., `https://example.streamlit.app/`).
3. Save the secret; the workflow will automatically read the value when it runs on its hourly schedule.

### Manually test the keep-alive workflow

1. Navigate to **Actions → Keep Streamlit deployment alive** in GitHub.
2. Click **Run workflow** to launch it on demand. You may leave the `target_url` field blank to use the `STREAMLIT_PING_URL` secret, or provide a one-off URL for testing.
3. Review the workflow run logs to ensure the keep-alive request succeeded.

## Challenges

### Practical

| # | Challenge | Status |
| --- | --------- | ------ |
| 1 | Download Manager | [View Solution](./Practical/Download%20Manager/) |
| 2 | Make an elastic producer/consumer task queue. | [View Solution](./Practical/Producer%20Consumer/) |
| 3 | IRC Client | [View Solution](./Practical/IRC%20Client/) |
| 4 | Markov Chain Sentence Generator (Include Shitposting Capabilities) | [View Solution](./Practical/Markov%20Chain%20Sentence%20Generator/) |
| 5 | English Sentence Parser that Points to the Context of a Sentence | [View Solution](./Practical/Context%20Pointer/) |
| 6 | MIDI Player + Editor | [View Solution](./Practical/MIDI%20Player%20Editor/) |
| 7 | Stock Market Simulator Using Yahoo Spreadsheet Data | [View Solution](./Practical/Stock%20Market%20Simulator/) |
| 8 | Parametric/Graphic Equalizer for .wav files (Make it real-time) | [View Solution](./Practical/WAV%20Equalizer/) |
| 9 | Graphing Calculator (BONUS: Graph the Function's Derivatives) | [View Solution](./Practical/Graphing%20Calculator/) |
| 10 | To-Do List Application (Web app or CLI) | [View Solution](./Practical/ToDoList-CLI/) |
| 11 | Verlet Integration (Verlet Cloth) | [View Solution](./Practical/Verlet%20Cloth/) |
| 12 | TCP/UDP Chat Server + Client | [View Solution](./Practical/Chat%20Server%20Client/) |
| 13 | Music Streaming | [View Solution](./Practical/Music%20Streaming/) |
| 14 | Shazam | [View Solution](./Practical/Shazam%20Clone/) |
| 15 | Chatbot (with conversation retention) | [View Solution](./Practical/Chatbot/) |
| 16 | Curses Text Editor (with Emacs /Vim Keybindings) | [View Solution](./Practical/Curses%20Text%20Editor/) |
| 17 | Paint Clone | [View Solution](./Practical/Paint/) |
| 18 | Image to ASCII Art | [View Solution](./Practical/ImgToASCII/) |
| 19 | Booru (Image Board) Image Downloader | [View Solution](./Practical/Booru%20Imageboard%20Downloader/) |
| 20 | Image Converter | [View Solution](./Practical/Image%20Converter/) |
| 21 | ID3 Reader | [View Solution](./Practical/ID3%20Reader/) |
| 22 | Sound Synthesis (Sine square sawtooth etc...) ("Fuck You" mode: Realtime MIDI Playback with Custom instruments) | [View Solution](./Practical/Sound%20Synthesis/) |
| 23 | C++ IDE Plugin for Sublime/Atom (Auto-Complete Go-To Symbol Declaration and Definition using Clang's AST) | [View Solution](./Practical/C++%20IDE%20Plugin/) |
| 24 | Simple Version Control supporting checkout commit (with commit message) unlocking and per-file configuration of number of revisions kept | [View Solution](./Practical/Simple%20VCS/) |
| 25 | Imageboard (Imagine vichan) | [View Solution](./Practical/Imageboard/) |
| 26 | Password Manager | [View Solution](./Practical/Password%20Manager/) |
| 27 | Create a Torrent Client (CLI or GUI) | [View Solution](./Practical/Torrent%20Client/) |
| 28 | Booru Client | [View Solution](./Practical/Booru%20Client/) |
| 29 | Key Press Bot | [View Solution](./Practical/Key%20Press%20Bot/) |
| 30 | IP/URL Obsucrification (<http://www.pc-help.org/obscure.htm>) | [View Solution](./Practical/IP%20URL%20Obscurifier/) |
| 31 | Radix Base Converter (Given a radix base convert it to decimal) | [View Solution](./Practical/Radix%20Base%20Converter/) |
| 32 | Chan aggregator (Let's user view various boards from different 'chans') (Bonus: Add 4ChanX and Archiving Functionality) | [View Solution](./Practical/Chan%20Aggregator/) |
| 33 | Encrypt a File and Upload it online | [View Solution](./Practical/Encrypted%20Upload/) |
| 34 | Make a Text Editor that autosaves and includes the date in the filename | [View Solution](./Practical/AutoSave%20Text%20Editor/) |
| 35 | Create an HSV Color Representation | [View Solution](./Practical/HSV%20color%20wheel/) |
| 36 | Window Manager | [View Solution](./Practical/Window%20Manager/) |
| 37 | Basic Relational Database Software (SQL Support Handle Relationships Focus on Efficiency) | [View Solution](./Practical/Relational%20DB/) |
| 38 | Pixel Editor | [View Solution](./Practical/Pixel%20Editor/) |
| 39 | Trivial File Transfer Protocol (TFTP): Allow a client to put a file onto a remote host | [View Solution](./Practical/TFTP%20Tool/) |
| 40 | Markdown (HTML/XML) Editor | [View Solution](./Practical/Markdown%20Editor/) |
| 41 | IP Tracking Visualization | [View Solution](./Practical/IP%20Tracking%20visualization/) |
| 42 | Port Scanner | [View Solution](./Practical/Port%20Scanner/) |
| 43 | Old School Demo Effect (Plasma Tunnel Scrollers Zoomers etc...) | [View Solution](./Practical/Old%20School%20cringe/) |
| 135 | Bellman-Ford Simulation with at least Five Vertices | [View Solution](./Practical/Bellman%20Ford%20Simulation/) |
| 136 | Matrix Arithmetic | [View Solution](./Practical/Matrix%20Arithmetic/) |
| 137 | File Compression Utility (Make it GUI) | [View Solution](./Practical/File%20Compression%20Utility/) |
| 138 | PDF Tagger | [View Solution](./Practical/PDF%20Tagger/) |
| 139 | Nonogram Generator and Solver | [View Solution](./Practical/Nonogram%20Solver/) |
| 140 | Calculate Dot and Cross Product of Two Vectors | [View Solution](./Practical/Vector%20Product/) |
| 141 | Bismuth Fractal | [View Solution](./Practical/Bismuth%20Fractal/) |
| 142 | Seam Carving | [View Solution](./Practical/Seam%20Carving/) |
| 143 | Bayesian Filter | [View Solution](./Practical/Bayesian%20Filter/) |
| 144 | WMS viewer that isn't web based | [View Solution](./Practical/WMS%20Viewer/) |

> The IRC client depends only on Python 3.10+ standard-library modules (`asyncio`, `ssl`, `argparse`, `logging`).

> **Security notice:** The encrypted upload utility in this repository is educational. Review the [project documentation](./Practical/Encrypted%20Upload/README.md) before using it with sensitive information and ensure keys, manifests, and remote endpoints are secured.

### Algorithmic

| # | Challenge | Status |
| --- | --------- | ------ |
| 44 | Fizzbuzz (BONUS: In Assembly) | [View Solution](./Algorithmic/FizzBuzz/) |
| 45 | RPN Calculator | [View Solution](./Algorithmic/RPN%20Calculator/) |
| 46 | Counts occurrences of characters in a Given String (include support for unicode characters) | [View Solution](./Algorithmic/Character%20Counter/) |
| 47 | Towers of Hanoi | [View Solution](./Algorithmic/Towers%20of%20Hanoi/) |
| 48 | Music Visualizer | [View Solution](./Algorithmic/Music%20Visualizer/) |
| 49 | Unicode Converter (Support for UTF-8 16LE 32LE and 32BE) | [View Solution](./Algorithmic/basic%20text%20encoding/) |
| 50 | Calculate the first (n) digits of pi | [View Solution](./Algorithmic/Digits%20of%20Pi/) |
| 51 | Least Squares Fitting Algorithm | [View Solution](./Algorithmic/Least%20Squares%20Fitting/) |
| 52 | Given an Array of Stocks's values over time find the period of time where the stocks could have made the most money | [View Solution](./Algorithmic/Stock%20Prices/) |
| 53 | Highest Prime Factor Calculator | [View Solution](./Algorithmic/Highest%20prime%20factor/) |
| 54 | Hide and Extract Data in images (Basic Steganography) (Bonus: Include .gif support) | [View Solution](./Algorithmic/Steganography/) |
| 55 | Web Page Crawler | [View Solution](./Algorithmic/Web%20Page%20Crawler/) |
| 56 | Password Generator (Let User Choose Options) | [View Solution](./Algorithmic/PassGen/) |
| 57 | Vigenère cipher encryption/decryption | [View Solution](./Algorithmic/Vigniere%20Cipher/) |
| 58 | Game Of Life | [View Solution](./Algorithmic/Game%20of%20life/) |
| 59 | Caesar Cipher Cracker | [View Solution](./Algorithmic/Caesar%20Cipher/) |
| 60 | Dijkstra's Algorithm | [View Solution](./Algorithmic/Djikstra/) |
| 61 | ROT 13 | [View Solution](./Algorithmic/ROT%2013/) |
| 62 | Program that displays MBR Contents | [View Solution](./Algorithmic/MBR/) |
| 63 | Random Name Picker | [View Solution](./Algorithmic/Random%20Name%20Picker/) |
| 64 | Encrypt/Decrypt Text: Implement at least one from <http://rumkin.com/tools/cipher/collection> | [View Solution](./Algorithmic/Rumkin%20Ciphers/) |
| 65 | Youtube To MP3 | [View Solution](./Algorithmic/ytmp3/) |
| 66 | Text to Hexadecimal/Binary | [View Solution](./Algorithmic/basic%20text%20encoding/) |
| 67 | Calculate the first 1 000 digits of pi iteratively | [View Solution](./Algorithmic/1000%20Digits%20of%20Pi/) |
| 68 | Sierpinski Triangle | [View Solution](./Algorithmic/Sierpinski/) |
| 69 | Mandelbrot Set | [View Solution](./Algorithmic/Mandelbrot%20Set/) |
| 134 | Ulam Spiral | [View Solution](./Algorithmic/Ulam%20Spiral/) |

### Artificial Intelligence

| # | Challenge | Status |
| --- | --------- | ------ |
| 70 | OpenAI Gym Project | Not Yet |
| 71 | AI for Roguelikes | Not Yet |
| 72 | Sudoku/n-Puzzle Solver using A* algorithm | [View Solution](./Artificial%20Intelligence/Sudoku/) |
| 73 | Connect-4 AI Player using Alpha-Beta Pruning | [View Solution](./Artificial%20Intelligence/Connect4/) |
| 74 | Basic Neural Network - Simulate individual neurons and their connections | [View Solution](./Artificial%20Intelligence/Basic%20Neural%20Network/) |
| 75 | Real Neural Network - Implement a basic feed-forward neural network using matrices for entire layers along with matrix operations for computations. | [View Solution](./Artificial%20Intelligence/Real%20Neural%20Network/) |
| 76 | Convolutional Neural Network: Implement a convolutional N.N. for a handwritten digit recognition test on MNIST dataset (Use TensorFlow Theano etc...) | Not Yet |
| 77 | Convolutional Neural Network: Implement your own convolutional neural network for handwritten digit recognition test on MNIST Dataset (Without TensorFlow Theano etc...) | Not Yet |

### Emulation/Modeling

| # | Challenge | Status |
| --- | --------- | ------ |
| 91 | Chip - 8 Emulator (Hard Mode: Cached Interpreter. Fuck You: Dynamic Recompiler use dynarec/jit library) | Not Yet |
| 92 | Double Pendulum Simulation | Not Yet |
| 93 | Constructive Solid Geometry | Not Yet |
| 94 | Ray Tracer | Not Yet |
| 95 | Real-Time Fast Fourier Transform Spectrum Visualizer | Not Yet |
| 96 | Generate a Complimentary Color from any input color | [View Solution](./Emulation/CompColor/) |
| 97 | Generate a 5-Color Scheme from the most dominant tones in any image | [View Solution](./Emulation/5%20color%20scheme/) |
| 98 | General Lambert's-Problem Solver (At least it's not rocket science... Oh wait it actually is) | Not Yet |
| 99 | TI-86 Emulator (Bonus: Include the Option to Create Programs) | Not Yet |
| 100 | N-Body Simulator with particles having a certain mass and radius depending on the mass that merge if they collide (Bonus: Include a GUI where you can place particles) | Not Yet |
| 101 | Eulerian Path | [View Solution](./Emulation/EulerianPath/) |
| 102 | Draw a spinning 3D Cube | [View Solution](./Emulation/SpinnyCube/) |
| 103 | Cellular Textures | [View Solution](./Emulation/CellularTextures/) |
| 145 | ASCII Digital Clock | [View Solution](./Emulation/ASCII_Clock/) |

### Games

| # | Challenge | Status |
| --- | --------- | ------ |
| 104 | Knight's Tour | [View Solution](./Games/Knight%20Tour/) |

| 105 | Monster Raising/Breeding Simulator | [View Solution](./Games/Monster%20Raising/) |
| 106 | Tetris | Not Yet |

| 105 | Monster Raising/Breeding Simulator | Not Yet |
| 106 | Tetris | [View Solution](./Games/Tetris/) |

| 107 | Snake | [View Solution](./Games/Snake/) |

| 108 | Pipe Dreams | Not Yet |
| 109 | Pac Man With Different Behaviors for each ghost | [View Solution](./Games/Pacman/) |

| 108 | Pipe Dreams | [View Solution](./Games/Pipe%20Dreams/) |
| 109 | Pac Man With Different Behaviors for each ghost | Not Yet |

| 110 | Dragon Quest / Basic RPG Engine | Not Yet |
| 111 | Rock Paper Scissors | [View Solution](./Games/RPS/) |
| 112 | First-Person Engine in OpenGL (Walking Looking Around Jumping on Obstacles) (BONUS: VR Compatibility) | Not Yet |
| 113 | Shuffle a Deck of Cards (Include a Visualization) | [View Solution](./Games/Shuffle/) |
| 114 | Simulate a game of Tag using a multi-agent system | Not Yet |
| 115 | Wolfenstein Clone (FPS two-dimensional map that appears to be 3-D) (If you need a starting point search for bisquit's video about DOOM-like Engines) | Not Yet |
| 116 | Scorched Earth Clone | [View Solution](./Games/ScorchedEarth/) |
| 117 | Minesweeper | [View Solution](./Games/Minesweeper/) |
| 118 | An Audio/Visual 64KB Demonstration | [View Solution](./Games/64kDemo/) |
| 119 | Sudoku | [View Solution](./Games/Sudoku/) |
| 120 | Danmaku (Bullet Hell) Engine | Not Yet |

| 121 | Roguelike Engine/Dungeon Generator | [View Solution](./Games/Roguelike/) |
| 122 | Design a Game Engine in Unity | Not Yet |

| 121 | Roguelike Engine/Dungeon Generator | Not Yet |
| 122 | Design a Game Engine in Unity | [View Solution](./Games/UnityEngine/) |

| 123 | Yahtzee | [View Solution](./Games/Yahtzee/) |


| 124 | Oil Panic | [View Solution](./Games/OilPanic/) |

| 124 | Oil Panic | Not Yet |


| 125 | Chess | Not Yet |
| 126 | Go (No AI Necessary) | [View Solution](./Games/Go/) |

| 125 | Chess | [View Solution](./Games/Chess/) |
| 126 | Go (No AI Necessary) | Not Yet |

| 127 | Connect Four | [View Solution](./Games/Connect4/) |
| 128 | Mastermind | Not Yet |

| 129 | Missile Command Game | [View Solution](./Games/MissileCommand/) |
| 130 | Tron | Not Yet |

| 129 | Missile Command Game | Not Yet |

| 130 | Tron | [View Solution](./Games/Tron/) |





| 131 | Breakout | Not Yet |

| 130 | Tron | Not Yet |
| 131 | Breakout | [View Solution](./Games/Breakout/) |

| 132 | Simon | [View Solution](./Games/Simon/) |

## Highlights

This repository includes several scripts with graphical user interfaces (GUIs) and algorithm visualizations. Here are a few highlights:

### GUI Applications

| Tool                              | Command                                                                 |
| --------------------------------- | ----------------------------------------------------------------------- |
| PDF Metadata Tagger               | `python "Practical/PDF Tagger/pdftag_gui.py"`                           |
| Image to ASCII Art Converter      | `python "Practical/ImgToASCII/convert_gui.py"`                          |
| Multi-threaded Port Scanner       | `python "Practical/Port Scanner/scanner_gui.py"`                        |
| Radix Base Converter              | `python "Practical/Radix Base Converter/radix_gui.py"`                  |
| Markov Chain Sentence Generator   | `python "Practical/Markov Chain Sentence Generator/mcsg_gui.py"`        |
| Seam Carving Image Resizer        | `python "Practical/Seam Carving/resize_gui.py"`                         |

### Algorithm Visualizations

| Visualization                   | Command                                                                 |
| ------------------------------- | ----------------------------------------------------------------------- |
| Dijkstra's Shortest Path        | `python "Algorithmic/Djikstra/dijkstra_visualizer.py" --start A`        |
| Towers of Hanoi                 | `python "Algorithmic/Towers of Hanoi/ToH_visualizer.py" 4`              |
| Character Frequency             | `python "Algorithmic/Character Counter/charcount_visualizer.py" -t "hello world"` |

## Further Learning

For those hungry for more, here are some excellent resources to continue your journey.

### Recommended Reading

* Knuth: *The Art of Computer Programming*
* Skiena: *The Algorithm Design Manual*
* Cormen et al: *Introduction to Algorithms*
* Russel: *Artificial Intelligence: A Modern Approach*
* Abelson: *Structure and Interpretation of Computer Programs*

### More Challenges

* [HackerRank](https://www.hackerrank.com)
* [CodeChef](https://www.codechef.com)
* [CodeFights](https://www.codefights.com)
* [Project Euler](https://www.projecteuler.net)
* [Rosetta Code](https://www.rosettacode.org)
* [CodeAbbey](https://www.codeabbey.com)
* [CodingBat](https://www.codingbat.com)
* [Programming Praxis](https://www.programmingpraxis.com)

---

## Repository Usage Guide

This section provides a practical, execution-focused overview so new contributors (or you on a fresh machine) can get from clone → running examples quickly.

### 1. Environment Setup

PowerShell (Windows):

```pwsh
git clone https://github.com/saintwithataint/Pro-g-rammingChallenges4.git
cd Pro-g-rammingChallenges4
python -m venv .venv
. .venv/Scripts/Activate.ps1
pip install -r requirements.txt
```

If you only want a subset (e.g., just run the imageboard or seam carving), install that folder's own `requirements.txt` instead of the root.

### 2. Dependency Strategy

* Root `requirements.txt` = superset, categorized (web, imaging, analysis, visualization).
* Folder-level `requirements.txt` files (e.g. `Practical/`, `Emulation/`) are leaner.
* Heavy/optional libs (plotly, vpython, scikit-learn, colour-science) can be skipped unless you need those features.
* Future improvement: adopt `pyproject.toml` with extras (e.g. `pip install .[imageboard]`).

### 3. At-a-Glance Tool Categories

| Domain | Example Scripts | Key Deps | Typical Command |
|--------|-----------------|----------|-----------------|
| Web App | Imageboard | Flask, Pillow | `python "Practical/Imageboard/imageboard.py" --help` |
| Image Processing | Seam Carving | opencv-python, numpy | `python "Practical/Seam Carving/resize.py" in.jpg --width -100 -o out.jpg` |
| Visualization | 5 Color Scheme | numpy, matplotlib, scikit-learn | `python "Emulation/5 color scheme/5cs.py" img.jpg --k 5 --show` |
| Data / Geo | IP Tracking Visualization | requests, pandas, plotly | `python "Practical/IP Tracking visualization/trackip.py" ips.txt --html map.html` |
| CLI Utility | Radix Converter | (stdlib) | `python "Practical/Radix Base Converter/radix.py" 1a --from 16 --to 2` |
| Text / ASCII | ASCII Clock | (stdlib) | `python "Emulation/ASCII_Clock/ClockSynced.py" --refresh 0.2` |
| Math / Vectors | Vector Product | matplotlib | `python "Practical/Vector Product/vector.py" cross 1,0,0 0,1,0` |
| PDF Metadata | PDF Tagger | pypdf | `python "Practical/PDF Tagger/pdftag.py" doc.pdf --add key=value` |
| Networking Scan | Port Scanner | (stdlib) | `python "Practical/Port Scanner/scanner.py" 192.168.1.10 --top 100` |
| Markov Text | Markov Chain | (stdlib) | `python "Practical/Markov Chain Sentence Generator/mcsg.py" corpus.txt --sentences 3` |

### 4. Common Options & Patterns

* Most Python scripts expose `--help` for argument details.
* Image tools usually accept `--out` or `-o` for output file specification.
* Long-running tasks often support interruption with Ctrl+C (graceful cleanup implemented where applicable).
* Some GUIs are mirrored by a CLI variant (e.g. `resize.py` vs `resize_gui.py`).
* Optional dependencies are imported inside `try/except` blocks; absence results in reduced functionality, not crashes.

### 5. Recommended Minimal Installs Per Interest

| Interest | Install Command |
|----------|-----------------|
| Web + Images | `pip install Flask Pillow` |
| Algorithm Visualizers | (typically stdlib) |
| Image Resizing / Seam Carving | `pip install opencv-python numpy Pillow` |
| Palette & Color Tools | `pip install numpy Pillow matplotlib scikit-learn` |
| PDF Tagging | `pip install pypdf` |
| Geo/IP Visualization | `pip install requests pandas plotly tqdm` |
| 3D Cube (VPython) | `pip install vpython` |

### 6. Quick Smoke Test Script (Optional)

You can verify key imports with a short one-liner:

```pwsh
python - <<'PY'
import importlib, sys
mods = ["flask","PIL","numpy","cv2","matplotlib","plotly","pandas","pypdf"]
for m in mods:
    try:
        importlib.import_module(m)
        print(f"[OK] {m}")
    except Exception as e:
        print(f"[MISS] {m}: {e.__class__.__name__}: {e}")
PY
```

Run the repository's automated CLI smoke tests when you want to ensure
`--help` continues to execute successfully across the command-line tools:

```bash
pytest tests/test_cli_help.py
```

If you add a new GUI-only program (or a script that needs network access or
large optional dependencies), include its relative path in the
`SKIP_PATTERNS` dictionary in `tests/test_cli_help.py` along with a short
reason. The test module treats each entry as a prefix, so adding
`"Games"` skips the entire games tree, while a specific file path affects
just that script.

### 7. Troubleshooting Quick Reference

| Issue | Cause | Fix |
|-------|-------|-----|
| `ImportError: cv2` | OpenCV not installed | `pip install opencv-python` |
| `No module named plotly` | Plotly omitted | `pip install plotly` |
| Pillow missing WEBP | Extra codecs absent | `pip install Pillow[webp]` |
| GUI window invisible | Running headless | Use CLI version or run locally |
| `OSError: [Errno ...] image file truncated` | Corrupt input image | Re-download / validate file |

### 8. Running Tests Locally

To mirror the automated checks:

1. Install the developer and practical extras in editable mode so intra-repo imports work the same way as CI:

   ```bash
   python -m pip install --upgrade pip
   python -m pip install -e .[developer,practical]
   ```

2. From the repository root, run the full test suite:

   ```bash
   pytest
   ```

   The included `pytest.ini` sets `PYTHONPATH=.` so tests in `Practical/Imageboard/tests` and `Algorithmic/MBR` resolve local modules without extra flags.

3. If you need to replicate the imageboard tests manually, export the environment variables they expect before invoking the app or running individual tests:

   ```bash
   export IMAGEBOARD_DATA_DIR=$(mktemp -d)
   export IMAGEBOARD_SECRET_KEY=testing-secret
   export IMAGEBOARD_ADMIN_PASSWORD=letmein
   ```

### 9. Next Steps / Improvements

* Introduce `pyproject.toml` with extras: `imageboard`, `visual`, `ml`, `geo`.
* Add `pytest` smoke tests (import + `--help` execution) to CI.
* Provide a unified launcher (`python tools.py list` / `run <tool>`).
* Add Dockerfile for the imageboard deployment scenario.
* Generate HTML index page summarizing runnable tools.

---

If you’d like help implementing any of these improvements next, open an issue or continue the session here.
