"""Smoke-test CLI entry points by asserting their ``--help`` output works."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path
from typing import Iterable

import pytest

REPO_ROOT = Path(__file__).resolve().parent.parent
SEARCH_ROOTS: list[Path] = [
    REPO_ROOT / "Algorithmic",
    REPO_ROOT / "Practical",
    REPO_ROOT / "Games",
    REPO_ROOT / "Artificial Intelligence",
    REPO_ROOT / "Emulation",
]

# Map of path prefixes (relative to the repo root, using POSIX separators)
# to skip reasons. Prefixes allow skipping an entire directory of GUI-only
# tools without having to list each file individually.
SKIP_PATTERNS: dict[str, str] = {
    "Algorithmic/Character Counter/charcount_visualizer.py": "Requires optional visualization dependencies.",
    "Algorithmic/Djikstra/dijkstra_visualizer.py": "Visualizer depends on GUI/plotting stacks not available in CI.",
    "Algorithmic/Web Page Crawler": "Crawler needs third-party services and network access.",
    "Algorithmic/ytmp3": "YouTube tooling depends on external services/ffmpeg.",
    "Artificial Intelligence/AI Roguelike": "CLI depends on package context and tcod resources not bundled in CI.",
    "Artificial Intelligence/CNN_Framework": "Training scripts require the PyTorch stack and datasets not installed in CI.",
    "Artificial Intelligence/OpenAI Gym": "Gym training tools depend on heavy RL libraries not present in CI.",
    "Artificial Intelligence/Real Neural Network": "Neural network demos require deep-learning frameworks and data assets.",
    "Artificial Intelligence/Connect4": "Connect 4 demo expects a human or GUI session.",
    "Emulation": "Terminal animation demos require interactive consoles or external deps.",
    "Games": "Games expect graphical output or user interaction.",
    "Practical/Bayesian Filter": "Scikit-learn dataset dependencies are not bundled.",
    "Practical/Booru Client": "Booru client GUI depends on desktop frameworks.",
    "Practical/Chan Aggregator": "Aggregator uses GUI/web tooling that is not available in CI.",
    "Practical/Chat Server Client": "Chat client/server requires network peers for --help to exit cleanly.",
    "Practical/Context Pointer": "Context pointer integrates platform-specific accessibility APIs.",
    "Practical/Download Manager": "Download manager performs network operations when invoked.",
    "Practical/Encrypted Upload": "Encrypted upload utility requires external credentials/services.",
    "Practical/File Compression Utility": "Compression GUI needs desktop toolkits.",
    "Practical/Graphing Calculator": "Graphing calculator relies on GUI and plotting frameworks.",
    "Practical/ID3 Reader": "ID3 reader GUI depends on desktop libraries.",
    "Practical/Imageboard": "Imageboard launcher requires external services and credentials.",
    "Practical/ImgToASCII/convert_gui.py": "GUI front-end requires tkinter/desktop backends.",
    "Practical/MIDI Player Editor": "MIDI tooling needs audio devices and external dependencies.",
    "Practical/Markdown Editor": "Markdown editor uses GUI components not suited for CI.",
    "Practical/Markov Chain Sentence Generator/mcsg_gui.py": "GUI front-end requires graphical environment.",
    "Practical/Music Streaming": "Streaming server/client expect network/audio backends.",
    "Practical/Nonogram Solver/ui.py": "GUI front-end requires user interaction.",
    "Practical/PDF Tagger": "PDF tagger depends on desktop frameworks that are unavailable in CI.",
    "Practical/Paint": "Paint clone is a GUI-only program.",
    "Practical/Password Manager": "Password manager relies on secure storage and GUI prompts.",
    "Practical/Pixel Editor": "Pixel editor requires a GUI environment.",
    "Practical/Port Scanner/scanner_gui.py": "GUI wrapper for the port scanner needs a display server.",
    "Practical/Radix Base Converter/radix_gui.py": "GUI wrapper requires desktop support.",
    "Practical/Relational DB": "Relational DB CLI expects database files and optional deps.",
    "Practical/Seam Carving": "Seam carving tools depend on heavy imaging stacks not installed in CI.",
    "Practical/Shazam Clone": "Audio fingerprinting suite requires data files and external deps.",
    "Practical/Simple VCS": "VCS prototype expects repository state and interactive input.",
    "Practical/Sound Synthesis": "Sound synthesis scripts need audio hardware and libraries.",
    "Practical/Torrent Client": "Torrent client needs network connectivity and peer discovery.",
    "Practical/Verlet Cloth": "Visualization requires graphical dependencies.",
    "Practical/WAV Equalizer": "Audio equalizer depends on optional DSP libraries.",
    "Practical/WMS Viewer": "WMS viewer relies on GIS/Qt dependencies unavailable in CI.",
    "Practical/Window Manager": "Window manager manipulates desktop sessions and cannot run headlessly.",
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
