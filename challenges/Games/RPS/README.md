# Rock Paper Scissors Suite

- **Challenge:** #111 — Rock Paper Scissors (with Lizard & Spock variant)
- **Languages:** Python, C++, Java, JavaScript/Web

## Overview
Multi-language implementations of classic Rock Paper Scissors plus a Python bonus mode supporting the Lizard/Spock expansion. Includes a browser UI with theming assets.

## Dependencies
- **Python:** `pip install -e .[games]` (standard library only, but keeps environments consistent).
- **C++:** GCC/Clang with C++17 support.
- **Java:** JDK 11+.
- **Web:** Any modern browser; assets under `assets/` for icons and sounds.

## Run
### Python (CLI + Lizard/Spock)
```bash
python rpsls.py --mode classic
python rpsls.py --mode lizardspock
```

### C++
```bash
g++ rps.cpp -std=c++17 -o rps
./rps
```

### Java
```bash
javac rps.java
java -cp . rps
```

### Web
Open `rps.html` in a browser. For local development you can launch a static server:
```bash
python -m http.server 8000
# Navigate to http://localhost:8000/challenges/Games/RPS/rps.html
```

## Assets
SVG icons and CSS live in `assets/`, `rps.css`, and `rps.js`. Keep the folder structure intact when serving the page.

## Notes
- Python CLI supports score tracking and an autoplay demo (`--autoplay`).
- Update move sets or translations in `rps.js` / `rpsls.py` to localise.
