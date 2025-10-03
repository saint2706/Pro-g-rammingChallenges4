# Lambert's Orbital Boundary Value Problem

This module implements a universal-variable Lambert solver for astrodynamics
experiments. The code follows the formulation from Bate, Mueller, and White
(1971) and the refinements discussed in Vallado (2013), delivering the
short- and long-way zero-revolution solutions that mission designers use to
link two position vectors with a specified transfer time.

The package ships two entry points:

- `lambert.py` &mdash; reusable solver functions and data structures.
- `cli.py` &mdash; command-line utility for quick calculations, JSON export, and
  optional trajectory plots.

The solver and examples rely on `numpy` and `scipy`. Optional trajectory
visualisation (`--plot`) requires `matplotlib`.

## Installing dependencies

From the repository root install the "algorithmic" extra which already lists
`numpy` and `scipy`:

```bash
python -m pip install -e .[algorithmic]
```

Add the "visual" extra if you want to enable plotting support:

```bash
python -m pip install -e .[algorithmic,visual]
```

## Usage

Solve for the transfer velocities between two position vectors with a
3600-second Earth-orbit transfer (example adapted from Vallado):

```bash
python -m Emulation.LambertsProblem.cli \
    --mu 398600.4418 \
    solve 5000 10000 2100 -14600 2500 7000 3600
```

Example output:

```
Lambert solution (universal variable method)
  r1 = [ 5000. 10000.  2100.]
  r2 = [-14600.   2500.   7000.]
  μ  = 398600.441800 km^3/s^2
  Transfer time = 3600.000000 s
  Iterations: 4
  Universal variable z = 1.53985790
  v1 = [-5.99249502  1.92536671  3.24563805] km/s
  v2 = [-3.3124585  -4.19661901 -0.38528906] km/s
```

The departure and arrival velocities match the benchmark tabulated in
Vallado (Example 5-4) to better than 1 mm/s.

Obtain the minimum (parabolic) transfer time for the same geometry:

```bash
python -m Emulation.LambertsProblem.cli \
    analyze 5000 10000 2100 -14600 2500 7000
```

Output:

```
Parabolic (minimum) transfer time
  r1 = [ 5000. 10000.  2100.]
  r2 = [-14600.   2500.   7000.]
  μ  = 398600.441800 km^3/s^2
  Minimum time-of-flight ≈ 2761.371855 s
```

Add `--plot` to `solve` to integrate the two-body equations of motion and
render the transfer arc in 3D (requires matplotlib):

```bash
python -m Emulation.LambertsProblem.cli \
    --plot --samples 300 \
    solve 5000 10000 2100 -14600 2500 7000 3600
```

JSON export is available through `--json path/to/output.json`.

## References

- Bate, R. R., Mueller, D. D., & White, J. E. (1971). *Fundamentals of
  Astrodynamics*. Dover Publications.
- Vallado, D. A. (2013). *Fundamentals of Astrodynamics and Applications*
  (4th ed.). Microcosm Press.

## Tests

Unit tests compare the solver output against Vallado's benchmark solution and
validate the parabolic time-of-flight calculation. Run them from the repository
root:

```bash
pytest tests/emulation/test_lambert.py
```
