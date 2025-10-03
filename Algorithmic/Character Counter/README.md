# Character Counter

## Problem Statement
Analyze the distribution of characters in a Unicode text sample, reporting frequency counts, entropy, and other diversity metrics. The module offers both scriptable functions and a visualization helper for exploratory analysis.

## Usage
- Count characters from the command line by piping text:
  ```bash
  python charcount.py < input.txt
  ```
- Use the module interactively:
  ```python
  >>> from charcount import get_char_counts, analyze_text_statistics
  >>> analyze_text_statistics("Hello World!\n")
  ```
- Launch the Plotly-based dashboard for an interactive bar chart:
  ```bash
  python charcount_visualizer.py sample.txt
  ```

## Debugging Tips
- Short inputs such as `AAAaa!!` are helpful sanity checks—case-insensitive mode should treat `A` and `a` as the same symbol.
- Enable debug logging to inspect intermediate category tallies:
  ```bash
  python charcount.py --log DEBUG "Hello"
  ```
- Although this folder has no bundled tests, you can draft quick assertions with `pytest` by importing `get_char_counts` and verifying expected Counters.

## Implementation Notes
- Built on top of Python's `collections.Counter` and `unicodedata` for robust Unicode handling.
- Provides dataclass-backed statistics summaries and JSON/CSV serialization helpers.
- The visualization script uses Plotly to render counts interactively.

## Further Reading
- [Unicode Standard Annex #44: Unicode Character Database](https://www.unicode.org/reports/tr44/)
- [Shannon, "A Mathematical Theory of Communication" (Bell System Technical Journal, 1948)](https://doi.org/10.1002/j.1538-7305.1948.tb01338.x)
