"""Smoke-test CLI entry points by asserting their ``--help`` output works."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path
from typing import Iterable

import pytest

REPO_ROOT = Path(__file__).resolve().parent.parent
CHALLENGES_ROOT = REPO_ROOT / "challenges"
SEARCH_ROOTS: list[Path] = [
    CHALLENGES_ROOT / "Algorithmic",
    CHALLENGES_ROOT / "Practical",
    CHALLENGES_ROOT / "Games",
    CHALLENGES_ROOT / "Artificial Intelligence",
    CHALLENGES_ROOT / "Emulation",
]

# Map of path prefixes (relative to the repo root, using POSIX separators)
# to skip reasons. Prefixes allow skipping an entire directory of GUI-only
# tools without having to list each file individually.
SKIP_PATTERNS: dict[str, str] = {
    "challenges/Algorithmic/Character Counter/charcount_visualizer.py": "Requires optional visualization dependencies.",
    "challenges/Algorithmic/Djikstra/dijkstra_visualizer.py": "Visualizer depends on GUI/plotting stacks not available in CI.",
    "challenges/Algorithmic/Web Page Crawler": "Crawler needs third-party services and network access.",
    "challenges/Algorithmic/ytmp3": "YouTube tooling depends on external services/ffmpeg.",
    "challenges/Artificial Intelligence/AI Roguelike": "CLI depends on package context and tcod resources not bundled in CI.",
    "challenges/Artificial Intelligence/CNN_Framework": "Training scripts require the PyTorch stack and datasets not installed in CI.",
    "challenges/Artificial Intelligence/OpenAI Gym": "Gym training tools depend on heavy RL libraries not present in CI.",
    "challenges/Artificial Intelligence/Real Neural Network": "Neural network demos require deep-learning frameworks and data assets.",
    "challenges/Artificial Intelligence/Connect4": "Connect 4 demo expects a human or GUI session.",
    "challenges/Emulation": "Terminal animation demos require interactive consoles or external deps.",
    "challenges/Games": "Games expect graphical output or user interaction.",
    "challenges/Practical/Bayesian Filter": "Scikit-learn dataset dependencies are not bundled.",
    "challenges/Practical/Booru Client": "Booru client GUI depends on desktop frameworks.",
    "challenges/Practical/Chan Aggregator": "Aggregator uses GUI/web tooling that is not available in CI.",
    "challenges/Practical/Chat Server Client": "Chat client/server requires network peers for --help to exit cleanly.",
    "challenges/Practical/Context Pointer": "Context pointer integrates platform-specific accessibility APIs.",
    "challenges/Practical/Download Manager": "Download manager performs network operations when invoked.",
    "challenges/Practical/Encrypted Upload": "Encrypted upload utility requires external credentials/services.",
    "challenges/Practical/File Compression Utility": "Compression GUI needs desktop toolkits.",
    "challenges/Practical/Graphing Calculator": "Graphing calculator relies on GUI and plotting frameworks.",
    "challenges/Practical/ID3 Reader": "ID3 reader GUI depends on desktop libraries.",
    "challenges/Practical/Imageboard": "Imageboard launcher requires external services and credentials.",
    "challenges/Practical/ImgToASCII/convert_gui.py": "GUI front-end requires tkinter/desktop backends.",
    "challenges/Practical/MIDI Player Editor": "MIDI tooling needs audio devices and external dependencies.",
    "challenges/Practical/Markdown Editor": "Markdown editor uses GUI components not suited for CI.",
    "challenges/Practical/Markov Chain Sentence Generator/mcsg_gui.py": "GUI front-end requires graphical environment.",
    "challenges/Practical/Music Streaming": "Streaming server/client expect network/audio backends.",
    "challenges/Practical/Nonogram Solver/ui.py": "GUI front-end requires user interaction.",
    "challenges/Practical/PDF Tagger": "PDF tagger depends on desktop frameworks that are unavailable in CI.",
    "challenges/Practical/Paint": "Paint clone is a GUI-only program.",
    "challenges/Practical/Password Manager": "Password manager relies on secure storage and GUI prompts.",
    "challenges/Practical/Pixel Editor": "Pixel editor requires a GUI environment.",
    "challenges/Practical/Port Scanner/scanner_gui.py": "GUI wrapper for the port scanner needs a display server.",
    "challenges/Practical/Radix Base Converter/radix_gui.py": "GUI wrapper requires desktop support.",
    "challenges/Practical/Relational DB": "Relational DB CLI expects database files and optional deps.",
    "challenges/Practical/Seam Carving": "Seam carving tools depend on heavy imaging stacks not installed in CI.",
    "challenges/Practical/Shazam Clone": "Audio fingerprinting suite requires data files and external deps.",
    "challenges/Practical/Simple VCS": "VCS prototype expects repository state and interactive input.",
    "challenges/Practical/Sound Synthesis": "Sound synthesis scripts need audio hardware and libraries.",
    "challenges/Practical/Torrent Client": "Torrent client needs network connectivity and peer discovery.",
    "challenges/Practical/Verlet Cloth": "Visualization requires graphical dependencies.",
    "challenges/Practical/WAV Equalizer": "Audio equalizer depends on optional DSP libraries.",
    "challenges/Practical/WMS Viewer": "WMS viewer relies on GIS/Qt dependencies unavailable in CI.",
    "challenges/Practical/Window Manager": "Window manager manipulates desktop sessions and cannot run headlessly.",
}


def discover_entry_points() -> Iterable[Path]:
    """Yield scripts that expose a ``__main__`` block under the target roots."""
    for root in SEARCH_ROOTS:
        if not root.exists():
            continue
        for script in sorted(root.rglob("*.py")):
            if script.name == "__init__.py":
                continue
            try:
                contents = script.read_text(encoding="utf-8")
            except UnicodeDecodeError:
                contents = script.read_text(encoding="utf-8", errors="ignore")
            if 'if __name__ == "__main__"' in contents:
                yield script


def skip_reason(rel_path: Path) -> str | None:
    """Return a skip reason for *rel_path* if it matches any configured prefix."""
    rel_str = rel_path.as_posix()
    for prefix, reason in SKIP_PATTERNS.items():
        if rel_str == prefix or rel_str.startswith(f"{prefix}/"):
            return reason
    return None


def build_params() -> list:
    params: list = []
    for script in discover_entry_points():
        rel_path = script.relative_to(REPO_ROOT)
        reason = skip_reason(rel_path)
        param = pytest.param(
            script,
            id=rel_path.as_posix(),
            marks=pytest.mark.skip(reason=reason) if reason else (),
        )
        params.append(param)
    return params


@pytest.mark.parametrize("script_path", build_params())
def test_cli_help(script_path: Path) -> None:
    """Each CLI should exit successfully when invoked with ``--help``."""
    subprocess.run(
        [sys.executable, str(script_path), "--help"],
        check=True,
        capture_output=True,
        text=True,
        timeout=5,
    )
