"""Test helpers for the stock simulator."""

from __future__ import annotations

import sys
from pathlib import Path

# Ensure the package root (``Practical/Stock Market Simulator``) is importable
PACKAGE_ROOT = Path(__file__).resolve().parents[1]
if str(PACKAGE_ROOT) not in sys.path:
    sys.path.insert(0, str(PACKAGE_ROOT))
