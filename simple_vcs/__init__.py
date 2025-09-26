"""Proxy package to expose the Simple VCS implementation at repository root."""

from __future__ import annotations

from pathlib import Path

_impl_dir = Path(__file__).resolve().parent.parent / "Practical" / "Simple VCS" / "simple_vcs"
if not _impl_dir.exists():  # pragma: no cover - defensive
    raise ImportError("simple_vcs implementation directory is missing")

__path__ = [str(_impl_dir)]

from .repository import Repository  # noqa: E402  # isort:skip

__all__ = ["Repository"]
