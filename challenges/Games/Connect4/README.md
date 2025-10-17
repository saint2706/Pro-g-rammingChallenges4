# Connect Four

- **Challenge:** #127 — Connect Four
- **Languages:** Python, Java

## Overview
Two-player Connect Four with both a desktop GUI (pygame) and a Java console mode. The Python version renders a 7×6 grid with mouse-driven piece drops; the Java build mirrors the classic rules in a text-based interface.

## Dependencies
- **Python:** `pip install -e .[games]` (pulls `pygame` and `numpy`).
- **Java:** JDK 11+ (uses standard library only).

## Run
### Python (pygame board)
```bash
python connect4.py
```
Controls: move the mouse to select a column, click to drop your checker.

### Java (console mode)
```bash
javac connect4.java
java -cp . connect4
```
Controls: follow the on-screen prompts to choose columns (0-indexed).

## Assets
No external assets; colours and layout are generated procedurally.

## Notes
- The Python game depends on `pygame.mixer` for sound cues; ensure audio devices are available.
- To tweak board size or winning length, adjust the constants near the top of `connect4.py`.
