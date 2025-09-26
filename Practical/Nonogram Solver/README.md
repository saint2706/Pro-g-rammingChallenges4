# Nonogram Solver & Generator

This toolkit tackles Practical Challenge #139: create a Nonogram puzzle generator, solver, and playable interface. The project is split into three pillars so you can study or extend each topic independently:

| Pillar | Modules | Highlights |
| --- | --- | --- |
| **Puzzle modelling** | `nonogram_solver/puzzle.py` | Dataclasses for puzzles and boards, JSON import/export helpers, and Pillow-powered rendering. |
| **Logic engine** | `nonogram_solver/solver.py` | Constraint propagation plus heuristic backtracking for fast, unique solutions. |
| **User experience** | `nonogram_solver/gui.py`, `ui.py` | Tkinter desktop app with keyboard/mouse play, hinting, solver integration, and export shortcuts. |

## Puzzle Generation

The generator (`nonogram_solver/generator.py`) produces random boards with a configurable density. Each candidate board is analysed by the solver to guarantee *at least one* solution. To keep puzzles interesting we also enforce uniqueness by requesting two solutions and discarding boards that admit more than one. You can tune:

- `width` & `height`: board size.
- `density`: approximate proportion of filled tiles (the generator retries if it drifts too sparse or too dense).
- `max_attempts`: safety valve so you never loop forever; hit the ceiling and you'll get a helpful exception.
- `rng`: seedable `random.Random` so your puzzles become reproducible in tutorials or tests.

The module exposes higher level convenience helpers:

```python
from nonogram_solver.generator import generate_puzzle
puzzle = generate_puzzle(width=10, height=10, density=0.45)
```

`puzzle.solution` always contains the canonical solved grid which the GUI, solver, and exporters reuse.

## Solving Strategy

Our solver was designed for clarity first and raw performance second; most 15×15 puzzles resolve instantly. The pipeline is:

1. **Constraint propagation** &mdash; For each line (row or column) we list all valid permutations that match the current partial board. An intersection step pins down cells that must be filled or empty.
2. **Heuristic search** &mdash; If propagation stalls we branch on the line with the fewest remaining permutations (a "minimum remaining values" heuristic). Each branch applies its candidate pattern and recurses.
3. **Early exit** &mdash; Callers can cap the number of solutions; the generator sets `max_solutions=2` so it can detect ambiguity quickly.

The solver API looks like this:

```python
from nonogram_solver.solver import NonogramSolver
solver = NonogramSolver(puzzle)
solutions = solver.solve(max_solutions=1)
assert solutions[0] == puzzle.solution
```

Progressively solved boards are accessible via `solver.iter_solutions()` if you want to animate the search.

## Interface Goals

The Tkinter GUI (`ui.py` entry point) is intentionally ergonomic and hackable. Key features include:

- **Play mode**: left-click cycles Unknown → Filled → Empty; right-click jumps straight to Empty. Keyboard shortcuts (`S` to solve, `N` for a fresh puzzle, `Ctrl+E/J` to export to PNG/JSON) mirror the toolbar buttons.
- **Hints**: Run the solver against your current state and highlight deterministic deductions without spoiling the whole grid.
- **Exports**: Save a PNG board snapshot or a JSON puzzle description (clues + optional user progress) so you can share or import later.
- **Status bar**: Immediate feedback about conflicts, completion, and exported file names.

Launch it with:

```bash
python "Practical/Nonogram Solver/ui.py"
```

If you prefer scripting, you can bypass the GUI entirely and operate on `nonogram_solver` from any Python module.

## Example Output

Render any puzzle to an image on demand instead of tracking binary assets in git. The snippet below saves a PNG preview next to your working script:

```python
from pathlib import Path

from nonogram_solver import NonogramPuzzle, render_board_to_image

rows = [[3], [1, 2], [2], [1], [1, 1]]
cols = [[1, 2], [1], [3], [2], [1, 1]]
puzzle = NonogramPuzzle(rows, cols)

output = Path("sample_nonogram.png")
render_board_to_image(puzzle).save(output)
print(f"Saved preview to {output}")
```

Open the generated file to inspect the default styling, or tweak the parameters (`cell_size`, `clue_padding`, etc.) to match your needs.

## Next Steps

- Add smarter line-solving heuristics (e.g., advanced overlap reasoning) for larger boards.
- Provide SVG exports alongside PNG/JSON for print-ready puzzles.
- Extend the GUI with undo/redo and custom puzzle loading dialogs.

Pull requests that focus on accessibility (e.g., keyboard navigation, high-contrast themes) are especially welcome.
