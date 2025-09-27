# Yahtzee

- **Challenge:** #123 — Yahtzee
- **Languages:** Python, Java

## Overview
Dice game implementation with score evaluation, rerolls, and combination detection. The Python version is CLI-based; the Java edition mirrors gameplay for JVM users.

## Dependencies
- **Python:** Standard library only (`random`, `collections`). Install `pip install -e .[games]` to stay consistent with repo tooling.
- **Java:** JDK 11+ (standard library).

## Run
### Python
```bash
python yahtzee.py
```
Follow the prompts to reroll dice and record scores.

### Java
```bash
javac yahtzee.java
java -cp . yahtzee
```

## Notes
- Python version exposes helper classes (`Hand`, `ScoreCard`) for testing; import them if you want to script simulations.
- Consider adding a GUI by building on these core logic classes.
