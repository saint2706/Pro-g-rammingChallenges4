# Python CLI Plotter

This solution offers a minimal command-line interface for plotting mathematical expressions. It uses SymPy for parsing and Matplotlib for rendering.

## Running the script

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
python main.py "sin(x)" --xmin -3.14 --xmax 3.14 --outfile plot.png
```

The generated `plot.png` file will contain the rendered graph for the supplied expression.
