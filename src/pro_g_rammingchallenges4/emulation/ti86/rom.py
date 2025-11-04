"""ROM loading helpers for the TI-86 emulator.

This module provides utility functions for loading a TI-86 ROM image into the
emulator's memory from a file or a binary stream.
"""

from __future__ import annotations

from pathlib import Path
from typing import BinaryIO

from .memory import Memory


def load_rom_image(memory: Memory, path: str | Path) -> None:
    """Loads a TI-86 ROM image from a file into memory.

    Args:
        memory: The Memory object to load the ROM into.
        path: The path to the ROM file.
    """
    rom_path = Path(path)
    with rom_path.open("rb") as handle:
        data = handle.read()
    memory.load_rom(data)


def load_rom_stream(memory: Memory, stream: BinaryIO) -> None:
    """Loads a TI-86 ROM image from a binary stream into memory.

    Args:
        memory: The Memory object to load the ROM into.
        stream: The binary stream to read the ROM from.
    """
    data = stream.read()
    memory.load_rom(data)
