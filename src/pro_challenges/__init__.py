"""Utility namespace for packaging the Pro-g-rammingChallenges4 examples.

The individual challenge folders in this repository are intentionally kept
self-contained.  This lightweight package simply exposes repository metadata
so that `pip install -e .` provides an importable module.
"""

from importlib import metadata as _metadata

try:  # pragma: no cover - convenience helper for interactive use
    __version__ = _metadata.version("pro-g-rammingchallenges4")
except _metadata.PackageNotFoundError:  # pragma: no cover
    __version__ = "0.0.dev0"

__all__ = ["__version__"]
