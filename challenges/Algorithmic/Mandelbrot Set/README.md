# Mandelbrot Set

## Problem Statement
Render the Mandelbrot set by iterating \(z_{n+1} = z_n^2 + c\) for each complex point \(c\) on a grid and coloring pixels based on escape time. The script supports smooth coloring, custom bounds, and image export.

## Usage
- Display a default render:
  ```bash
  python mandel.py
  ```
- Target a specific region and save the result:
  ```bash
  python mandel.py --bounds -2 1 -1.5 1.5 --max-iter 1000 --save mandel.png
  ```
- Use smooth coloring with a different colormap in headless mode:
  ```bash
  python mandel.py --smooth --cmap plasma --headless --save smooth.png
  ```

## Debugging Tips
- Begin with low resolution/iteration settings (`--width 400 --height 300 --max-iter 100`) to confirm shapes before scaling up.
- Run the regression tests:
  ```bash
  pytest test_mandel.py
  ```
  They check boundary calculations and array dimensions.
- Enable verbose logging (`--log-level DEBUG`) to print derived bounds and confirm CLI parsing.

## Implementation Notes
- Configuration is managed via a dataclass that derives bounds from either center/scale or explicit min/max values.
- Uses NumPy for vectorized complex iteration and matplotlib for rendering/saving figures.
- Smooth coloring computes continuous escape values for gradient-friendly palettes.

## Further Reading
- [Mandelbrot, "Fractal Aspects of the Iteration of \(z \mapsto z^2 + c\)" (Annals of the New York Academy of Sciences, 1982)](https://doi.org/10.1111/j.1749-6632.1982.tb39505.x)
- [Heinz-Otto Peitgen et al., *The Science of Fractal Images*](https://link.springer.com/book/10.1007/978-1-4612-4650-5)
