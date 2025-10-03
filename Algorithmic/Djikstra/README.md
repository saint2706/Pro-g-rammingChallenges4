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
