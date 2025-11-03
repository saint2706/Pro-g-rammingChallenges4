"""High level TI-86 emulator façade."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from .cpu import ExecutionTrace, Z80CPU
from .keypad import Keypad
from .lcd import LCD
from .memory import Memory


@dataclass
class TI86:
    """Bundles CPU, memory, LCD and keypad."""

    memory: Memory
    cpu: Z80CPU
    lcd: LCD
    keypad: Keypad

    @classmethod
    def create(cls) -> "TI86":
        memory = Memory()
        lcd = LCD()
        keypad = Keypad()
        cpu = Z80CPU(memory)
        return cls(memory=memory, cpu=cpu, lcd=lcd, keypad=keypad)

    def reset(self) -> None:
        self.cpu.reset()
        self.memory.ram[:] = b"\x00" * len(self.memory.ram)
        self.lcd.clear()
        self.keypad.reset()

    def step(self) -> ExecutionTrace:
        return self.cpu.step()

    def run_until_break(self, max_instructions: Optional[int] = None) -> None:
        executed = 0
        while True:
            self.step()
            executed += 1
            if self.cpu.halted:
                break
            if max_instructions is not None and executed >= max_instructions:
                break

    def load_rom(self, data: bytes) -> None:
        self.memory.load_rom(data)
