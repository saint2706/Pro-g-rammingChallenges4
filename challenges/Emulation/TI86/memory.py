"""Memory map for the TI-86 emulator."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable

ROM_SIZE = 128 * 1024
RAM_SIZE = 32 * 1024
ADDRESS_SPACE = 0x10000


@dataclass
class Memory:
    """Represents the 64KiB Z80 address space.

    The TI-86 uses banked ROM/RAM in hardware.  The emulator implements a
    simplified model that maps the first 32KiB of RAM to 0x8000-0xFFFF and a
    single 128KiB ROM image to 0x0000-0x7FFF.  This is sufficient for unit tests
    and interactive experimentation.  The layout is intentionally isolated from
    the CPU so a more faithful implementation can be swapped in later.
    """

    rom: bytearray = field(default_factory=lambda: bytearray(0x8000))
    ram: bytearray = field(default_factory=lambda: bytearray(RAM_SIZE))

    def load_rom(self, data: bytes) -> None:
        if len(data) not in {0x20000, 0x40000, 0x80000} and len(data) != 0x8000:
            raise ValueError("Unsupported ROM size for TI-86 image")
        if len(data) < len(self.rom):
            self.rom[: len(data)] = data
        else:
            self.rom = bytearray(data[: len(self.rom)])

    def read_byte(self, address: int) -> int:
        address &= 0xFFFF
        if address < len(self.rom):
            return self.rom[address]
        ram_addr = address - 0x8000
        return self.ram[ram_addr % len(self.ram)]

    def write_byte(self, address: int, value: int) -> None:
        address &= 0xFFFF
        if address < 0x8000:
            return
        ram_addr = address - 0x8000
        self.ram[ram_addr % len(self.ram)] = value & 0xFF

    def read_word(self, address: int) -> int:
        low = self.read_byte(address)
        high = self.read_byte(address + 1)
        return (high << 8) | low

    def write_word(self, address: int, value: int) -> None:
        self.write_byte(address, value & 0xFF)
        self.write_byte(address + 1, (value >> 8) & 0xFF)

    def load_ram(self, start: int, data: Iterable[int]) -> None:
        for offset, value in enumerate(data):
            self.write_byte(start + offset, value)
