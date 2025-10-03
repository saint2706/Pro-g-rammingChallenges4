# Double Pendulum Simulator

A chaotic two-link pendulum visualiser with a fourth-order Runge–Kutta
integrator. The script exposes a CLI for sweeping initial conditions,
rendering trajectories with matplotlib, and exporting data as CSV or
animated GIFs.

## Physics Overview

The model assumes two point masses connected by massless rods of lengths
`l1` and `l2`. The state vector is `[θ₁, ω₁, θ₂, ω₂]` with `θ` measured
from the vertical. Equations of motion are integrated with RK4. An
optional linear damping coefficient applies to both joints by subtracting
`damping * ω` from the angular accelerations each step. Energy
calculations combine kinetic energy of both bobs and potential energy
relative to the pivot height.

## CLI Usage

```bash
python "Emulation/DoublePendulum/simulate.py" --help
```

Key options:

- `--theta1`, `--theta2` – initial angles in degrees
- `--omega1`, `--omega2` – initial angular velocities (rad/s)
- `--duration` / `--dt` – simulation span and integrator time step
- `--damping` – linear damping coefficient (0 for conservative system)
- `--csv PATH` – export full trace with optional energy column
- `--gif PATH` – render an animated GIF of the motion
- `--no-show` – skip live matplotlib window (useful on headless hosts)
- `--trail` – number of points to keep in the tail history overlay

Example: simulate a lightly damped system and export results:

```bash
python "Emulation/DoublePendulum/simulate.py" \
    --theta1 135 --theta2 -45 --damping 0.02 --duration 20 \
    --csv runs/dp.csv --gif runs/dp.gif --trail 240
```

## Export Utilities

- **CSV:** The trace is written with columns `time, θ₁, θ₂, ω₁, ω₂` and
  (optionally) `energy`.
- **GIF:** Frames are generated with matplotlib; install the `visual`
  extra (`pip install -e .[visual]`) to pull in `matplotlib`, `imageio`
  and `numpy`.

## Installation

From the repository root install the required extras:

```bash
python -m pip install -e .[visual]
```

The developer extra is handy if you want to run the automated tests:

```bash
python -m pip install -e .[developer]
pytest tests/test_double_pendulum.py
```

## Tests

`tests/test_double_pendulum.py` verifies that energy remains within a
small tolerance over short conservative runs, providing a regression
check for the integrator.
