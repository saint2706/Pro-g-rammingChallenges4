"""Root package entry point delegating to the implementation."""

from .cli import main


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
