"""Memory management for the CHIP-8 emulator."""

from __future__ import annotations

from dataclasses import dataclass, field

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
    """Represents the 4K memory layout of the CHIP-8 architecture."""

    size: int = 4096
    start_address: int = 0x200
    data: bytearray = field(default_factory=lambda: bytearray(4096))

    def __post_init__(self) -> None:
        self._load_fontset()

    def _load_fontset(self) -> None:
        for offset, value in enumerate(FONTSET):
            self.data[offset] = value

    def clear(self) -> None:
        self.data = bytearray(self.size)
        self._load_fontset()

    def load_rom(self, rom_bytes: bytes) -> None:
        if len(rom_bytes) + self.start_address > self.size:
            raise ValueError("ROM is too large to fit into memory")
        self.data[self.start_address : self.start_address + len(rom_bytes)] = rom_bytes

    def load_rom_file(self, path: str) -> None:
        with open(path, "rb") as handle:
            self.load_rom(handle.read())

    def read_byte(self, address: int) -> int:
        return self.data[address]

    def read_word(self, address: int) -> int:
        high = self.data[address]
        low = self.data[address + 1]
        return (high << 8) | low

    def write_byte(self, address: int, value: int) -> None:
        self.data[address] = value & 0xFF
