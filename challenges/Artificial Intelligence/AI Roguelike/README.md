# AI Roguelike Agent

This package provides an automated agent for the [Roguelike challenge](../../challenges/Games/Roguelike/).
It drives the existing `challenges/Games/Roguelike` engine headlessly and chooses actions with a
Monte Carlo Tree Search (MCTS) planner.  The agent is intentionally lightweight so it can
be embedded into tutorials, smoke tests, or benchmarking scripts without requiring a full
rendering stack.

## Features

- Deterministic, headless wrapper around the roguelike engine (`challenges/Games/Roguelike`).
- Monte Carlo Tree Search agent with configurable rollout depth and exploration constant.
- ASCII renderer for logging/visualising the agent's perception of the dungeon.
- Command-line interface for running the agent through sample dungeons.
- Smoke tests that ensure the agent can survive several automated turns without crashing.

## Installation

The agent depends on the same libraries as the base game, most notably
[`tcod`](https://github.com/libtcod/libtcod) for dungeon utilities and `numpy` for
map manipulation.  Install the extras declared in the repository's `pyproject.toml`:

```bash
pip install -e .[ai_roguelike]
```

This pulls in:

- `tcod` for field-of-view and tile helpers used by `challenges/Games/Roguelike`.
- `numpy` for map representations and deterministic dungeon generation.
- `scipy` for compatibility with other AI tooling in the repository (it is used by
  some roguelike combat utilities).

## Command-line usage

The CLI runs the Monte Carlo agent against a compact sample dungeon.  From the repository
root run:

```bash
PYTHONPATH="challenges/Artificial Intelligence/AI Roguelike" \
    python -m ai_roguelike.cli --turns 30 --iterations 128 --visualise
```

Behind the scenes the `ai_roguelike` package lives in `challenges/Artificial Intelligence/AI Roguelike/`.
If you prefer to invoke it from that directory you can also run:

```bash
cd "challenges/Artificial Intelligence/AI Roguelike"
python -m ai_roguelike.cli --turns 20 --seed 42 --log-file run.log
```

Useful flags:

- `--turns`: maximum number of turns to simulate.
- `--iterations`: number of MCTS iterations per turn.
- `--rollout-depth`: depth of random playouts during search.
- `--seed`: seed for reproducible dungeon layouts.
- `--visualise`: print an ASCII snapshot of the dungeon after each decision.
- `--log-file`: capture detailed logs in addition to console output.

Example output (truncated for brevity):

```
2024-09-01 12:00:00 | ai_roguelike | INFO | Starting automated run for 20 turns
2024-09-01 12:00:01 | ai_roguelike | INFO | Chosen action: north
2024-09-01 12:00:01 | ai_roguelike | INFO | Turn 1 | HP=30 | hostiles=5 | hostile HP=54
```

## Development

### Automated Tests
Automated smoke tests ensure the planner can advance several turns without triggering
errors:

```bash
pytest tests/test_ai_roguelike_smoke.py
```

The tests load a small dungeon via `create_sample_environment`, run the agent for a few
turns, and assert that the player remains alive or the run terminates cleanly.

### Code Structure
- **`cli.py`**: The command-line interface for running the agent. It handles argument parsing, logging configuration, and episode execution.
- **`environment.py`**: A headless wrapper around the roguelike engine. It provides a simplified API for agent interaction, including action space definition, state evaluation, and ASCII rendering.
- **`mcts.py`**: The Monte Carlo Tree Search planner. It contains the logic for the MCTS algorithm, including tree node representation, selection, expansion, simulation, and backpropagation.

### Customization and Extension
Feel free to tune the heuristics in `environment.py` or swap in a different planner.  The
`RoguelikeEnvironment` exposes a small action space and deterministic seeding helpers that
make it easy to prototype reinforcement learning agents or search-based controllers. The main components to modify are:
- **`RoguelikeEnvironment.evaluate()`**: The heuristic function that guides the MCTS agent. Modifying this function will change the agent's behavior.
- **`MCTSAgent`**: The MCTS planner. You can replace this with your own planner, as long as it conforms to the same `choose_action` interface.
