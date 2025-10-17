# Snake

- **Challenge:** #107 — Snake
- **Languages:** Python, JavaScript/Web

## Overview
Classic Snake implemented twice: a Python turtle graphics version and a browser-based canvas edition. Both support adjustable speed and track the current score.

## Dependencies
- **Python:** Standard library `turtle` (requires `tkinter`). Install `pip install -e .[games]` to stay aligned with repo tooling.
- **Web:** Modern browser (Chrome, Firefox, Edge). Optional CSS/JS customisation via the included files.

## Run
### Python (turtle)
```bash
python snake.py
```
Controls: Arrow keys to move. Game resets after collision.

### Web (Canvas)
Open `snake.html` in a browser, or serve locally:
```bash
python -m http.server 8000
# Visit http://localhost:8000/challenges/Games/Snake/snake.html
```
Controls: Arrow keys or WASD.

## Notes
- Ensure tkinter is installed on Linux (`sudo apt-get install python3-tk`).
- Canvas assets are pure CSS/JS—no external images required.
