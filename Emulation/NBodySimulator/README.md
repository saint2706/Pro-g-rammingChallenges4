# N-Body Simulator

A 2D gravitational sandbox for the `/g/` challenge #100. Bodies attract
one another, collide, and merge while conserving momentum. The project
uses pygame for the interactive front-end and a pure-python physics core
that is shared with automated tests.

## Features

- Symplectic Euler integration with configurable timestep
- Softened Newtonian gravity to stabilise close encounters
- Radius scaling with the cube root of mass (constant density assumption)
- Elastic merging that conserves total mass and linear momentum
- Optional particle trails and velocity preview during placement
- Mouse/keyboard controls for quickly iterating on scenarios

## Controls

| Action | Effect |
| ------ | ------ |
| Left click & drag | Create a new body. Drag vector sets the initial velocity. |
| Mouse wheel / +/- | Increase or decrease the spawn mass. |
| `[` / `]` | Decrease or increase the simulation timestep. |
| `T` | Toggle trails on/off. |
| `Space` | Pause or resume integration. |
| `C` | Clear all bodies. |
| `Esc` | Exit the simulator. |

## Running the simulator

```bash
python "Emulation/NBodySimulator/simulator.py"
```

The pygame window targets 60 FPS and sub-steps the physics to match the
requested timestep. Merging behaviour is triggered whenever body centres
come within the sum of their radii, which are derived from mass assuming
uniform density.

## Physics notes

- **Gravity**: uses Newton's law with a configurable gravitational
  constant (`1000` in the GUI) and Plummer softening to avoid singular
  forces at zero separation.
- **Integration**: symplectic Euler (semi-implicit) provides better
  energy stability than naive Euler with minimal overhead.
- **Collisions**: perfectly inelastic. Masses add together and the new
  velocity is the momentum-weighted average of the participants.
- **Radius model**: cube-root scaling maintains constant density and
  prevents merged bodies from collapsing to a point.

## Dependencies

The simulator relies on `pygame`, which is already part of the repo's
`visual` extra. Install from the repository root:

```bash
python -m pip install -e .[visual]
```

Automated tests cover the gravitational force calculations and verify
that mass is conserved when bodies merge.
