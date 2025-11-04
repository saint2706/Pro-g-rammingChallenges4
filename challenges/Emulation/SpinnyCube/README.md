# Spinny Cube

This solution provides two different implementations of a spinning 3D cube visualization:
- A 3D version using the VPython library (`spinny.py`).
- A web-based version using HTML, CSS, and JavaScript (`spinny.html`, `spinny.css`, `spinny.js`).

## VPython Version (`spinny.py`)

This script uses the VPython library to create an interactive 3D rendering of a spinning cube. It provides a simple and effective way to visualize 3D transformations in real-time.

### Dependencies
- Python 3
- VPython (`vpython`)

You can install the required dependency using pip:
```bash
pip install vpython
```

### Usage
You can run the script from the command line with various options:

**Basic Example:**
```bash
python "challenges/Emulation/SpinnyCube/spinny.py"
```

**Advanced Example:**
To run the cube at a different speed and for a limited duration:
```bash
python "challenges/Emulation/SpinnyCube/spinny.py" --speed 1.5 --duration 10
```

### Command-Line Arguments
- `--speed`: The rotation speed multiplier.
- `--fps`: The target frames per second.
- `--duration`: An optional duration in seconds to run the simulation for.
- `--reduced`: Start in a reduced-motion mode (slower rotation).

## Web Version (`spinny.html`)

This version uses CSS 3D transforms and animations to create a spinning cube in the browser. It's a lightweight and visually appealing demonstration of 3D graphics on the web.

### Usage
To run the web version, simply open the `spinny.html` file in a modern web browser.
