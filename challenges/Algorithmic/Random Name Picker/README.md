# Random Name Picker

## Problem Statement
Select one or more names uniformly or with optional weights from a list, supporting reproducibility, replacement rules, and JSON output for integrations.

## Usage
- Pick three unique names from the default list:
  ```bash
  python rnp.py --count 3
  ```
- Allow repeats (sampling with replacement):
  ```bash
  python rnp.py --count 5 --with-replacement
  ```
- Produce deterministic selections and JSON output:
  ```bash
  python rnp.py --count 4 --seed 42 --json
  ```
- Compare theoretical weights against simulated draws and render charts:
  ```bash
  python rnp_visualizer.py --count 2 --trials 200 --seed 7 --json --no-show --save wheel.png
  ```
  The visualizer reports normalized probabilities, runs deterministic Monte Carlo
  draws when seeded, and can save matplotlib pie/bar charts for quick sanity
  checks.

### Haskell CLI

- Draw weighted samples with deterministic seeding:
  ```bash
  runghc RNP.hs --file names.txt --count 4 --with-replacement --seed 99 --json --stats
  ```
  The Haskell script mirrors the Python defaults, emits the same JSON payload, and
  optionally embeds a `statistics` object containing the names, raw weights, and
  normalized probabilities for downstream visualizers.

## Debugging Tips
- If no `names.txt` exists, the script can create one automatically—verify contents before running weighted draws.
- Test suite command:
  ```bash
  pytest test_rnp.py
  ```
  It covers weight parsing, replacement rules, and deterministic seeding.
- Pass `--log DEBUG` to print parsed weights and confirm totals when debugging bias issues.

## Implementation Notes
- Dataclass configuration validates requested counts against the number of available names when sampling without replacement.
- Supports `name,weight` lines for weighted random selection (fallback to weight 1 when omitted).
- Uses Python's `random.Random` seeded instance to keep deterministic runs isolated from global state.
- The Haskell implementation (`RNP.hs`) consumes the exact same data files as the
  Python version. Mixed-weight inputs (some names with explicit weights and others
  without) are normalized identically, ensuring compatible statistics for
  `rnp_visualizer.py` or any JSON-based tooling.

## Further Reading
- [Ross, *Probability Models for Computer Science*, Chapter 3: Discrete Distributions](https://www.pearson.com/en-us/subject-catalog/p/probability-models-for-computer-science/P200000004495)
- [Knuth, *The Art of Computer Programming*, Vol. 2, Section 3.4.1 (Random Sampling)](https://www-cs-faculty.stanford.edu/~knuth/taocp.html)
