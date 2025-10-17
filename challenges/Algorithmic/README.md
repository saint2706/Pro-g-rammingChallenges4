# Algorithmic Solutions

This directory contains a curated collection of algorithmic programming challenges and their solutions. Each subfolder addresses a classic or interesting problem, with a focus on clarity, learning, and practical implementation.

## Structure

- Each challenge is organized in its own folder, often with both the main solution and a corresponding test file (e.g., `test_*.py`).
- Many problems include both Python and other language implementations (e.g., C, JavaScript) for comparison.
- Some solutions provide visualizations or GUI tools to help you understand the algorithm in action.

## Getting Started

1. **Explore**: Browse the folders to find a topic that interests you (e.g., ciphers, number theory, recursion, data visualization).
2. **Run**: Most Python scripts can be run directly. Use `python script.py --help` to see available options.
3. **Test**: Run `pytest` in this directory to check your understanding and verify correctness.
4. **Learn**: Read the code and comments—many scripts are written to be beginner-friendly and include explanations.

## Using pyproject.toml

Create a virtual environment, then install the extras that match the problems you want to explore:

```bash
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\Activate.ps1
python -m pip install -e .[algorithmic]      # Core numerics + utilities
python -m pip install -e .[visual]           # Plotting / colour science add-ons
python -m pip install -e .[media]            # YouTube downloader helper for ytmp3
```

Need linting or tests? Add `.[developer]` for pytest, ruff, and mypy.

## Example Topics

- **Ciphers**: Caesar, Atbash, Affine, Vigenère, ROT13
- **Math & Patterns**: Sierpinski Triangle, Ulam Spiral, Mandelbrot Set, Digits of Pi
- **Data Processing**: Character Counter, RPN Calculator, Stock Price Analysis
- **Algorithms**: Dijkstra's Shortest Path, Towers of Hanoi (recursive & iterative)
- **Steganography**: Hide and extract data in images
- **Web & Media**: Web Page Crawler, YouTube to MP3 downloader

## Project Guide

| Project | Highlights | Further Reading |
| --- | --- | --- |
| [1000 Digits of Pi](./1000%20Digits%20of%20Pi/README.md) | Gauss–Legendre iteration for arbitrary-precision \(\pi\). | Gauss–Legendre algorithm overview; Borwein & Borwein's *Pi and the AGM*. |
| [Affine & Atbash (Rumkin Ciphers)](./Rumkin%20Ciphers/README.md) | Classic substitution ciphers with brute-force helpers. | Wikipedia articles on affine and Atbash ciphers; Encyclopedia of Cryptography and Security. |
| [Caesar Cipher](./Caesar%20Cipher/README.md) | CLI for shifting, cracking, and analyzing monoalphabetic ciphers. | Caesar cipher history; *Introduction to Modern Cryptography*. |
| [Character Counter](./Character%20Counter/README.md) | Unicode frequency stats, entropy metrics, and visual dashboards. | Unicode UAX #44; Shannon's "Mathematical Theory of Communication". |
| [Digits of Pi](./Digits%20of%20Pi/README.md) | Chudnovsky-series engine with verification against reference digits. | Chudnovsky algorithm (1988); Bailey–Borwein–Plouffe research. |
| [Dijkstra's Algorithm](./Djikstra/README.md) | Shortest-path solver with visualizer and generator-based tracing. | Dijkstra's 1959 paper; CLRS Section 20.3. |
| [FizzBuzz](./FizzBuzz/README.md) | Multi-language implementations and tests for the classic kata. | Jeff Atwood's blog on FizzBuzz; Princeton COS introductory materials. |
| [Game of Life](./Game%20of%20life/README.md) | Pygame-based cellular automaton with pattern library and headless mode. | Gardner's 1970 *Scientific American* column; *Winning Ways* Volume 2. |
| [Highest Prime Factor](./Highest%20prime%20factor/README.md) | Optimized trial division with CLI batching and JSON output. | Project Euler Problem 3; Crandall & Pomerance's *Prime Numbers*. |
| [Least Squares Fitting](./Least%20Squares%20Fitting/README.md) | OLS regression in Python/C/MATLAB with plotting and tests. | Montgomery, Peck & Vining; SIAM Review history of least squares. |
| [MBR Parser](./MBR/README.md) | Inspect legacy Master Boot Records with dataclass-backed parsing. | Intel BIOS Enhanced Disk Drive spec; Microsoft partitioning guidance. |
| [Mandelbrot Set](./Mandelbrot%20Set/README.md) | NumPy-based fractal renderer with smooth coloring options. | Mandelbrot's 1982 paper; *The Science of Fractal Images*. |
| [Music Visualizer](./Music%20Visualizer/README.md) | STFT/mel spectrogram plots with optional synthetic tone generation. | Müller's *Fundamentals of Music Processing*; Librosa documentation. |
| [Password Generator](./PassGen/README.md) | `secrets`-powered password creation with entropy reporting. | NIST SP 800-63B; "The Quest to Replace Passwords" (IEEE S&P). |
| [ROT13](./ROT%2013/README.md) | Involutory substitution cipher with multi-source IO and JSON output. | ROT13 reference; Schneier's *Applied Cryptography*. |
| [RPN Calculator](./RPN%20Calculator/README.md) | Postfix evaluator supporting trig, factorial, JSON errors, and REPL. | Burks–Goldstine–von Neumann report; Knuth TAOCP Vol. 1. |
| [Random Name Picker](./Random%20Name%20Picker/README.md) | Weighted sampling with replacement controls and deterministic seeding. | Ross's *Probability Models for Computer Science*; Knuth TAOCP Vol. 2. |
| [Sierpinski Triangle](./Sierpinski/README.md) | Bitmask-driven ASCII fractal with JSON metadata export. | Wolfram MathWorld on Sierpinski Sieve; Mandelbrot's *Fractals and Chaos*. |
| [Steganography](./Steganography/README.md) | LSB text embedding/extraction with capacity analysis. | Fridrich's palette steganography paper; Johnson & Jajodia (1998). |
| [Stock Prices](./Stock%20Prices/README.md) | Plotly/Pandas visualization with SMA/EMA overlays and JSON summaries. | Pandas time-series guide; Plotly financial chart docs. |
| [Text Encoding Converter](./basic%20text%20encoding/README.md) | Hex/binary converters with encoding controls and logging. | Unicode Standard Chapter 2; RFC 3629 on UTF-8. |
| [Towers of Hanoi](./Towers%20of%20Hanoi/README.md) | Recursive and iterative solvers plus visualization helpers. | Lucas's *Récréations Mathématiques*; Sedgewick & Wayne's *Algorithms*. |
| [Ulam Spiral](./Ulam%20Spiral/README.md) | Prime-plotting spiral with NumPy sieve and matplotlib export. | Ulam's 1964 SIAM Review article; Chebotarev density discussions. |
| [Vigenère Cipher](./Vigniere%20Cipher/README.md) | Flexible Vigenère encoder/decoder with stream-friendly CLI. | Kahn's *The Codebreakers*; Wikipedia on Vigenère cipher. |
| [Web Page Crawler](./Web%20Page%20Crawler/README.md) | BFS web crawler with robots, rate limiting, and edge export. | Russell & Norvig's AIMA; Olston & Najork's *Web Crawling*. |
| [YouTube Audio Downloader](./ytmp3/README.md) | `yt-dlp`/`youtube_dl` wrapper for audio extraction with JSON summaries. | yt-dlp documentation; FFmpeg manuals. |

## Tips for New Developers

- Start with simpler scripts (FizzBuzz, ROT13, Character Counter) to build confidence.
- Experiment by changing parameters or adding new features.
- Use the test files to check your changes and learn about edge cases.
- Don't hesitate to read the code—it's meant to be educational!

## Requirements

Dependencies now live in optional extras defined in the root `pyproject.toml`. For most scripts `python -m pip install -e .[algorithmic]` is sufficient; add `visual` or `media` as shown above for plotting/downloading helpers.

---

Happy coding and exploring algorithms!
