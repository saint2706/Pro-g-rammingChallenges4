# Matrix Arithmetic Toolkit

This mini-project provides a reusable matrix manipulation library, a CLI utility, and optional visualisation helpers. It focuses on the four core linear-algebra operations requested by the challenge:

- **Addition** – element-wise sum of equally-sized matrices, with per-entry explanations of the arithmetic.
- **Multiplication** – row-by-column products with detailed breakdowns of each dot-product that forms the resulting matrix entries.
- **Determinants** – Gaussian-elimination based determinant calculation that documents pivot choices, row swaps, and elimination steps.
- **Inversion** – Gauss–Jordan elimination with full logging of row operations. Singular (non-invertible) matrices are detected early with a descriptive error.

## Quick Start

1. (Optional) create and activate the shared virtual environment from `Practical/` and install dependencies via `pip install -r ../requirements.txt`.
2. Run `python matrix_arithmetic.py --help` for an overview of the CLI commands.

### CLI examples

```bash
# Matrix addition with explanations
python matrix_arithmetic.py add --matrix-a "[[1,2],[3,4]]" --matrix-b "[[5,6],[7,8]]" --explain

# Matrix multiplication and save a 2×2 transformation plot
python matrix_arithmetic.py multiply --matrix-a matrices/a.json --matrix-b matrices/b.json --explain
python matrix_arithmetic.py visualize --matrix "[[0,-1],[1,0]]" --output rotation.png

# Determinant & inverse of the same matrix
python matrix_arithmetic.py determinant --matrix matrices/a.json --explain
python matrix_arithmetic.py inverse --matrix matrices/a.json --explain
```

Matrices can be passed inline (as shown above) or via a path to a JSON/`ast.literal_eval` compatible file. Explanations list each elimination or dot-product step so you can follow the arithmetic.

### GUI mode

If you prefer a graphical front end run `python matrix_arithmetic.py --gui`. A lightweight Tk window lets you paste matrices, pick an operation, and view both the result and the logged row operations.

### Optional 2D visualisation

Any 2×2 matrix can be visualised as a transformation of the unit square using `python matrix_arithmetic.py visualize --matrix ...`. The generated PNG is convenient for demonstrations of scaling, shear, or rotation.

The library powers both the CLI and the accompanying unit tests.

See the root repository README and the project index in `Practical/README.md` for environment setup instructions.
