"""Aggregated challenge categories for Pro-g-ramming Challenges."""

from __future__ import annotations

from importlib import import_module
from types import ModuleType

__all__ = [
    "load_category",
]


def load_category(name: str) -> ModuleType:
    """Return the module representing *name* under :mod:`challenges`.

    This helper keeps backwards compatibility for tooling that dynamically
    imports categories by their legacy names (e.g. ``Practical``).
    """
    return import_module(f"{__name__}.{name}")
