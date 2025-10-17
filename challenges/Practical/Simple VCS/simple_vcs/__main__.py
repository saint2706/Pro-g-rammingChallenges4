"""Module entry-point for ``python -m simple_vcs``."""

from .cli import main


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
