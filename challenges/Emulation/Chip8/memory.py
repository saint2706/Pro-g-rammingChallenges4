"""Memory management for the CHIP-8 emulator.

This module provides the Memory class, which represents the 4K memory space
of the Chip-8. It handles loading the font set, loading ROMs, and provides
methods for reading and writing to memory.
"""

from __future__ import annotations

from dataclasses import dataclass, field

# The built-in font set for characters 0-F.
# Each character is 5 bytes long.
FONTSET = (
    0xF0, 0x90, 0x90, 0x90, 0xF0,  # 0
    0x20, 0x60, 0x20, 0x20, 0x70,  # 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0,  # 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0,  # 3
    0x90, 0x90, 0xF0, 0x10, 0x10,  # 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0,  # 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0,  # 6
    0xF0, 0x10, 0x20, 0x40, 0x40,  # 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0,  # 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0,  # 9
    0xF0, 0x90, 0xF0, 0x90, 0x90,  # A
    0xE0, 0x90, 0xE0, 0x90, 0xE0,  # B
    0xF0, 0x80, 0x80, 0x80, 0xF0,  # C
    0xE0, 0x90, 0x90, 0x90, 0xE0,  # D
    0xF0, 0x80, 0xF0, 0x80, 0xF0,  # E
    0xF0, 0x80, 0xF0, 0x80, 0x80,  # F
)


@dataclass
class Memory:
    """Represents the 4K memory layout of the CHIP-8 architecture.

    The Chip-8 has 4096 bytes of memory. The first 512 bytes (0x000-0x1FF)
    are reserved for the interpreter, and this is where the font set is loaded.
    ROMs are loaded starting at address 0x200.

    Attributes:
        size: The total size of the memory in bytes.
        start_address: The starting address for loading ROMs.
        data: A bytearray representing the memory.
    """

    size: int = 4096
    start_address: int = 0x200
    data: bytearray = field(default_factory=lambda: bytearray(4096))

    def __post_init__(self) -> None:
        """Initializes the memory by loading the font set."""
        self._load_fontset()

    def _load_fontset(self) -> None:
        """Loads the built-in font set into the reserved memory area."""
        for offset, value in enumerate(FONTSET):
            self.data[offset] = value

    def clear(self) -> None:
        """Clears the memory and reloads the font set."""
        self.data = bytearray(self.size)
        self._load_fontset()

    def load_rom(self, rom_bytes: bytes) -> None:
        """Loads a ROM into memory.

        Args:
            rom_bytes: The binary content of the ROM.

        Raises:
            ValueError: If the ROM is too large to fit in memory.
        """
        if len(rom_bytes) + self.start_address > self.size:
            raise ValueError("ROM is too large to fit into memory")
        self.data[self.start_address : self.start_address + len(rom_bytes)] = rom_bytes

    def load_rom_file(self, path: str) -> None:
        """Loads a ROM from a file.

        Args:
            path: The path to the ROM file.
        """
        with open(path, "rb") as handle:
            self.load_rom(handle.read())

    def read_byte(self, address: int) -> int:
        """Reads a single byte from memory.

        Args:
            address: The address to read from.

        Returns:
            The byte at the specified address.
        """
        return self.data[address]

    def read_word(self, address: int) -> int:
        """Reads a 2-byte word from memory.

        Args:
            address: The address to read from.

        Returns:
            The word at the specified address.
        """
        high = self.data[address]
        low = self.data[address + 1]
        return (high << 8) | low

    def write_byte(self, address: int, value: int) -> None:
        """Writes a single byte to memory.

        Args:
            address: The address to write to.
            value: The value to write.
        """
        self.data[address] = value & 0xFF
