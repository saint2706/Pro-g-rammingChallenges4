# Bellman-Ford Simulation

This project provides an interactive visual walkthrough of the Bellman–Ford shortest path algorithm on directed, weighted graphs. It was built to satisfy the Practical challenge requirement of simulating the algorithm on graphs with **at least five vertices** while highlighting how relaxations propagate through the network and how negative cycles are detected.

## Goals

- Visualize Bellman–Ford iterations on graphs with five or more vertices.
- Show each edge relaxation, including whether it improves a distance estimate.
- Expose the final negative-cycle detection pass with clear visual feedback.
- Allow loading graphs from JSON or CSV so you can experiment with your own data.
- Provide interactive controls to step, rewind, auto-play, and reset the simulation.
- Offer a CLI fallback that prints each relaxation step for headless environments.
- Export the final distance/predecessor tables (JSON or CSV) for reporting or testing.

## Project Layout

```
challenges/Practical/Bellman Ford Simulation/
├── README.md                # You are here.
├── bellman_ford_simulation.py
├── sample_graph.json        # Default graph with 6 vertices and a mild negative edge.
└── sample_graph.csv         # Equivalent CSV edge list.
```

## Usage

> Activate your virtual environment and install dependencies first (see `challenges/Practical/requirements.txt`). Matplotlib is required for the GUI, pandas is only needed if you want to post-process CSV exports.

### Launch the Visual Simulation

```bash
python "challenges/Practical/Bellman Ford Simulation/bellman_ford_simulation.py" \
    --input "challenges/Practical/Bellman Ford Simulation/sample_graph.json" \
    --start A
```

Controls (displayed below the plot):

- **Prev / Next** – Step backward or forward through individual relaxations.
- **Play/Pause** – Auto-advance once per second; pauses at the end or on toggle.
- **Reset** – Return to the initial state.
- Keyboard shortcuts: Left/Right arrows map to Prev/Next, Space toggles play.

Each step highlights the currently processed edge, updates node labels with the latest distance estimates (∞ means unreachable), and shows console-style commentary for the relaxation.

### CLI (Headless) Mode

```bash
python "challenges/Practical/Bellman Ford Simulation/bellman_ford_simulation.py" \
    --input my_graph.csv --start S --no-gui
```

You will be prompted to press `Enter` to advance, `b` to go back, or `q` to quit. This mode mirrors the GUI state machine, making it convenient for remote shells or testing scripts.

### Export Results

```bash
python "challenges/Practical/Bellman Ford Simulation/bellman_ford_simulation.py" \
    --input custom_graph.json --start source --export results.json
```

Exports include:

- Source vertex
- Final distance estimates (with `null` representing unreachable nodes in JSON, or empty strings in CSV)
- Predecessor table
- Whether a negative cycle is reachable and which edges triggered it

The export happens immediately after the algorithm finishes preparing the step log, so it does not depend on opening the GUI.

## Graph File Formats

### JSON

```json
{
  "nodes": ["A", "B", "C", "D", "E", "F"],
  "edges": [
    {"source": "A", "target": "B", "weight": 6},
    {"source": "B", "target": "C", "weight": 5},
    {"source": "C", "target": "D", "weight": -4}
  ]
}
```

- `nodes` is optional; it will be inferred from the edges if omitted.
- Self-loops are ignored; parallel edges are supported.

### CSV

CSV files must contain `source,target,weight` columns (header required). Example:

```csv
source,target,weight
A,B,6
B,C,5
C,D,-4
```

## Extending the Simulation

- Add alternative layouts (spring embedding, manual coordinates) by editing `generate_layout()`.
- Integrate with `networkx` if you want built-in graph algorithms or layout helpers.
- Hook up logging or screenshot capture to create teaching materials.

Have fun experimenting with Bellman–Ford and negative cycles!
