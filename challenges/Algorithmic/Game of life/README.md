# Conway's Game of Life

## Problem Statement
Simulate Conway's Game of Life on a 2D grid where cells live, die, or are born based on the number of live neighbours. This implementation adds modern controls, pattern injection, optional acceleration, and headless automation hooks.

## Usage
- Launch an interactive window with the default configuration:
  ```bash
  python conway.py
  ```
- Customize resolution and pattern placement:
  ```bash
  python conway.py --width 1000 --height 800 --cell-size 8 --pattern gosper_glider_gun
  ```
- Benchmark in headless mode without opening a Pygame window:
  ```bash
  python conway.py --headless --max-generations 2000 --fps 120
  ```

### Haskell headless simulator
- Compile the CLI simulator (requires GHC 8.10+):
  ```bash
  cd "challenges/Algorithmic/Game of life"
  ghc -O2 Conway.hs -o conway-hs
  ```
  Alternatively, run it without producing a binary:
  ```bash
  runghc Conway.hs --headless --iterations 250
  ```
- Render ASCII snapshots for a preset pattern:
  ```bash
  ./conway-hs --width 60 --height 40 --iterations 120 --pattern gosper_glider_gun
  ```
- Gather statistics without printing the grid (headless mode mirrors `conway.py --headless`):
  ```bash
  ./conway-hs --headless --iterations 500 --pattern glider
  ```
- Reuse existing pattern assets by saving their ASCII art (e.g. the strings embedded in
  `conway.py`'s `PATTERNS` table) to a text file and loading them via:
  ```bash
  ./conway-hs --pattern-file assets/gosper.txt --width 80 --height 50
  ```
  The loader treats `#`, `O`, `o`, or `X` as live cells so you can copy/paste the bundled
  patterns verbatim or experiment with your own `.lif` style designs.

## Debugging Tips
- Press `SPACE` to pause and `N` to step one generation at a time; this makes it easy to confirm neighbour counts manually.
- Use small grids (e.g., `--width 100 --height 100 --cell-size 5`) to make counting by hand feasible.
- If evolution seems incorrect, toggle the overlay (`O`) to display FPS and generation counters and verify SciPy/NumPy backend selection logged on startup.

## How It Works
The Python implementation (`conway.py`) uses `pygame` for rendering and user interaction. The game's state is stored in a NumPy array, where each cell is either alive (1) or dead (0). The core logic is in the `step` method, which calculates the number of neighbors for each cell and applies the rules of the Game of Life to determine the next generation.

To speed up the neighbor calculation, the script uses `scipy.signal.convolve2d` if available, which is significantly faster than manually iterating through each cell. If SciPy is not installed, it falls back to a NumPy-based implementation.

The `Conway.hs` file provides a Haskell implementation of the Game of Life, which can be run in headless mode to simulate the game without a graphical interface.

## Implementation Notes
- Relies on NumPy arrays for grid state; optionally leverages SciPy's `convolve2d` for fast neighbour computation.
- Uses dataclasses to capture configuration and Pygame for rendering and input handling.
- Bundled pattern library safely injects gliders and Gosper glider guns relative to window dimensions.

## Further Reading
- [John H. Conway's Game of Life (Martin Gardner, *Scientific American*, 1970)](https://www.scientificamerican.com/article/mathematical-games-1970-10/)
- [Berlekamp, Conway & Guy, *Winning Ways for Your Mathematical Plays*, Vol. 2, Chapter on Life](https://press.princeton.edu/books/paperback/9781568811444/winning-ways-for-your-mathematical-plays)
