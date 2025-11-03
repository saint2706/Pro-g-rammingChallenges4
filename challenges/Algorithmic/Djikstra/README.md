# Dijkstra's Algorithm

## Problem Statement
Given a weighted graph with non-negative edge costs, compute the shortest-path distance from a source node to every other reachable node using Dijkstra's algorithm. This implementation includes both standard and step-wise generator variants plus a visualization helper.

## Usage
- Run the module directly with the built-in example graph:
  ```bash
  python dijkstra.py --source A
  ```
- Supply your own graph description (JSON or adjacency list) and request verbose logging:
  ```bash
  python dijkstra.py --graph graph.json --source S --log INFO
  ```
- Explore the step-by-step visualizer:
  ```bash
  python dijkstra_visualizer.py --source A
  ```

### Haskell command-line solver

The repository now includes a standalone Haskell CLI (`Dijkstra.hs`) that mirrors the Python implementation's behaviour. You can run it with `runghc` (ships with GHC) without compiling, or build an executable with `ghc Dijkstra.hs`.

```bash
# Run against the bundled example graph
runghc Dijkstra.hs --source A

# Load a custom graph (JSON or adjacency list) and emit a trace
runghc Dijkstra.hs --graph my_graph.txt --source S --trace

# Turn on verbose logging and export a JSON summary of every step
runghc Dijkstra.hs --graph weighted.json --source Start --verbose --json summary.json
```

Supported flags:

| Flag | Description |
| --- | --- |
| `--graph FILE` | Parse a custom graph. Supports compact JSON (`{"A": {"B": 2}}`) or adjacency text (`A: B(2), C(4)`). |
| `--source NODE` | Choose one or more starting nodes. Omit to use the first node found in the graph. |
| `--trace` | Print each generator-style step (initialisation, relaxations, completion). |
| `--verbose` | Emit progress information and file handling details to stderr. |
| `--json FILE` | Persist a machine-readable summary containing distances, predecessors, frontier snapshots, and the visit order. |

The JSON file produced by `--json` pairs naturally with the Python visualiser—load the same graph file in `dijkstra_visualizer.py` and the animation will match the recorded Haskell trace exactly. The summary also includes the reconstructed shortest paths, making it handy for regression tests or comparing two different implementations.

## Debugging Tips
- Start with the bundled `example_graph`—the shortest distance from `A` to `E` should be `7`.
- Use the generator mode (`--step`) to inspect queue operations if results look off; each yielded step describes relaxations and frontier changes.
- Run the automated tests:
  ```bash
  pytest test_dijkstra.py
  ```
  The suite covers disconnected graphs, invalid inputs, and tie-breaking behaviour.

## Implementation Notes
- Uses a binary heap (`heapq`) priority queue and explicit predecessor tracking for path reconstruction.
- Includes dataclass-backed configuration toggles for validation, logging, and timeout handling.
- The visualizer streams generator output to animate relaxations for educational purposes.

## Further Reading
- [E. W. Dijkstra, "A Note on Two Problems in Connexion with Graphs" (Numerische Mathematik, 1959)](https://doi.org/10.1007/BF01386390)
- [CLRS, *Introduction to Algorithms*, 4th ed., Section 20.3: Dijkstra's Algorithm](https://mitpress.mit.edu/9780262046305/introduction-to-algorithms/)
