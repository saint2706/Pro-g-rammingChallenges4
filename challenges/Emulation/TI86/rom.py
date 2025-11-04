"""ROM loading helpers."""

from __future__ import annotations

from pathlib import Path
from typing import BinaryIO

from .memory import Memory


def load_rom_image(memory: Memory, path: str | Path) -> None:
    """Load a TI-86 ROM dump from *path* into *memory*."""

    rom_path = Path(path)
    with rom_path.open("rb") as handle:
        data = handle.read()
    memory.load_rom(data)


def load_rom_stream(memory: Memory, stream: BinaryIO) -> None:
    data = stream.read()
    memory.load_rom(data)
