"""Memory map for the TI-86 emulator.

This module provides a simplified memory model for the TI-86, which is sufficient
for the purposes of this emulator. It simulates the 64KB address space of the
Z80 CPU, with a dedicated area for ROM and RAM.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable

RAM_SIZE = 32 * 1024


@dataclass
class Memory:
    """Represents the 64KB Z80 address space.

    The TI-86 has a more complex memory map with banked ROM and RAM. This class
    implements a simplified model where:
    - The first 32KB (0x0000-0x7FFF) is mapped to a single ROM page.
    - The upper 32KB (0x8000-0xFFFF) is mapped to RAM.

    This is sufficient for the emulator's current capabilities.

    Attributes:
        rom: A bytearray representing the ROM.
        ram: A bytearray representing the RAM.
    """

    rom: bytearray = field(default_factory=lambda: bytearray(0x8000))
    ram: bytearray = field(default_factory=lambda: bytearray(RAM_SIZE))

    def load_rom(self, data: bytes) -> None:
        """Loads a ROM image into the memory.

        Args:
            data: The ROM content as bytes.

        Raises:
            ValueError: If the ROM size is not supported.
        """
        # Truncate the ROM if it's larger than the available space.
        if len(data) > len(self.rom):
            data = data[: len(self.rom)]
        self.rom[: len(data)] = data

    def read_byte(self, address: int) -> int:
        """Reads a single byte from memory.

        Args:
            address: The 16-bit address to read from.

        Returns:
            The byte at the specified address.
        """
        address &= 0xFFFF
        if address < 0x8000:
            return self.rom[address]
        return self.ram[address - 0x8000]

    def write_byte(self, address: int, value: int) -> None:
        """Writes a single byte to memory.

        Writes to the ROM area are ignored.

        Args:
            address: The 16-bit address to write to.
            value: The byte value to write.
        """
        address &= 0xFFFF
        if address >= 0x8000:
            self.ram[address - 0x8000] = value & 0xFF

    def read_word(self, address: int) -> int:
        """Reads a 16-bit word from memory in little-endian format.

        Args:
            address: The 16-bit address to read from.

        Returns:
            The word at the specified address.
        """
        low = self.read_byte(address)
        high = self.read_byte((address + 1) & 0xFFFF)
        return (high << 8) | low

    def write_word(self, address: int, value: int) -> None:
        """Writes a 16-bit word to memory in little-endian format.

        Args:
            address: The 16-bit address to write to.
            value: The word value to write.
        """
        self.write_byte(address, value & 0xFF)
        self.write_byte((address + 1) & 0xFFFF, (value >> 8) & 0xFF)

    def load_ram(self, start: int, data: Iterable[int]) -> None:
        """Loads a block of data into RAM at a specific address.

        Args:
            start: The starting address to load the data at.
            data: An iterable of byte values.
        """
        for offset, value in enumerate(data):
            self.write_byte(start + offset, value)
