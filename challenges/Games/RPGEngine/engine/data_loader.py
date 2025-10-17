"""Helpers for loading JSON and YAML data files."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import yaml

from .constants import DATA_PATH


def _load_file(path: Path) -> Any:
    if not path.exists():
        raise FileNotFoundError(f"Data file not found: {path}")
    if path.suffix in {".yaml", ".yml"}:
        return yaml.safe_load(path.read_text())
    if path.suffix == ".json":
        return json.loads(path.read_text())
    raise ValueError(f"Unsupported data format: {path.suffix}")


def load_data(file_name: str) -> Any:
    """Load a data file relative to the engine data directory."""

    path = Path(DATA_PATH) / file_name
    return _load_file(path)


def ensure_directories() -> None:
    """Create any directories required at runtime."""

    Path(DATA_PATH).mkdir(parents=True, exist_ok=True)
