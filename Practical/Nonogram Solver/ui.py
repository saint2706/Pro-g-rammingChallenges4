"""Entry point for the Nonogram GUI."""
from __future__ import annotations

import sys
from pathlib import Path

PACKAGE_ROOT = Path(__file__).resolve().parent
if str(PACKAGE_ROOT) not in sys.path:
    sys.path.insert(0, str(PACKAGE_ROOT))

from nonogram_solver.gui import launch_app  # noqa: E402


if __name__ == "__main__":
    launch_app()
