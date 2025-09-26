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

## Tips for New Developers

- Start with simpler scripts (FizzBuzz, ROT13, Character Counter) to build confidence.
- Experiment by changing parameters or adding new features.
- Use the test files to check your changes and learn about edge cases.
- Don't hesitate to read the code—it's meant to be educational!

## Requirements

Dependencies now live in optional extras defined in the root `pyproject.toml`. For most scripts `python -m pip install -e .[algorithmic]` is sufficient; add `visual` or `media` as shown above for plotting/downloading helpers.

---

Happy coding and exploring algorithms!
