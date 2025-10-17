"""Convenience wrapper for the CNN framework evaluation CLI."""

from __future__ import annotations

import sys
from pathlib import Path

PACKAGE_ROOT = Path(__file__).resolve().parent
if str(PACKAGE_ROOT) not in sys.path:
    sys.path.insert(0, str(PACKAGE_ROOT))

from cnn_framework.evaluate import main


if __name__ == "__main__":
    main()
