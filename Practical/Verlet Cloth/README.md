# Verlet Cloth Simulation

This mini-project demonstrates a two-dimensional cloth simulated with Verlet integration and distance constraints. It is designed as an educational reference: the implementation keeps the math explicit, documents the physical model, and includes an interactive visualizer with realtime toggles for gravity and wind forces.

## Physical Model

The cloth is represented as a rectangular grid of point masses connected by distance constraints:

- **Particles** carry position, previous position, and accumulated acceleration. They have unit mass and integrate motion using the classic position-based Verlet update.
- **Constraints** enforce a fixed rest length between neighbouring particles. The solver iteratively corrects positions to satisfy these constraints, distributing the correction based on whether particles are pinned.
- **Forces** (gravity, wind gusts, user-applied impulses) act as accelerations. Gravity pulls every unpinned particle downward. Wind acts horizontally with a mild sinusoidal variation to create a fluttering effect.
- **Damping** is applied implicitly through the Verlet step by scaling the preserved velocity component, preventing the cloth from gaining energy indefinitely.

The default configuration pins the entire top row of particles so the fabric hangs while remaining anchored.

## Controls & Usage

Two entry points are provided:

1. `cloth.py` exposes the simulation core. Import `Cloth` and call `step(dt)` to advance time. Helper methods expose particle positions and constraint segments for rendering.
2. `visualize.py` renders the cloth with `matplotlib`. It animates the grid and responds to keyboard input:
   - Press **`g`** to toggle gravity.
   - Press **`w`** to toggle wind gusts.
   - Press **`r`** to reset the cloth to its initial, perfectly flat state.
   - Press **`q`** or close the window to exit.

Run the demo with:

```bash
python visualize.py
```

Optional arguments let you change grid resolution or constraint iterations; run `python visualize.py --help` for details.

## Tests & Further Experiments

The `tests/test_cloth.py` module checks two simple invariants:

- When no external forces are active the cloth remains perfectly still after a simulation step.
- Pinned particles stay anchored even when gravity is active.

Use these as a foundation for your own experiments—try different pinning patterns, add shear/bend constraints, or plug the simulation into an OpenGL renderer.
