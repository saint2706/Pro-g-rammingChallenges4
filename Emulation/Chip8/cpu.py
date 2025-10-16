"""CPU core implementing the CHIP-8 instruction set."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional
import random

from .display import DisplayBuffer
from .input import Keypad
from .memory import Memory


@dataclass
class CPU:
    """Emulates the CHIP-8 CPU."""

    memory: Memory = field(default_factory=Memory)
    display: DisplayBuffer = field(default_factory=DisplayBuffer)
    keypad: Keypad = field(default_factory=Keypad)
    clock_hz: int = 500
    rng: random.Random = field(default_factory=random.Random)

    def __post_init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        self.V = [0] * 16
        self.I = 0
        self.pc = self.memory.start_address
        self.stack = [0] * 16
        self.sp = 0
        self.delay_timer = 0
        self.sound_timer = 0
        self.waiting_register: Optional[int] = None
        self.halted = False

    def load_rom(self, rom_bytes: bytes) -> None:
        self.memory.clear()
        self.reset()
        self.memory.load_rom(rom_bytes)

    def fetch_opcode(self) -> int:
        opcode = self.memory.read_word(self.pc)
        self.pc = (self.pc + 2) & 0xFFFF
        return opcode

    def step(self) -> int:
        if self.halted:
            return 0x0000

        if self.waiting_register is not None:
            key = self.keypad.consume_press()
            if key is None:
                return 0x0000
            self.V[self.waiting_register] = key
            self.waiting_register = None

        opcode = self.fetch_opcode()
        self.execute(opcode)
        return opcode

    # pylint: disable=too-many-branches,too-many-statements
    def execute(self, opcode: int) -> None:
        nnn = opcode & 0x0FFF
        nn = opcode & 0x00FF
        n = opcode & 0x000F
        x = (opcode >> 8) & 0x000F
        y = (opcode >> 4) & 0x000F

        if opcode == 0x00E0:
            self.display.clear()
        elif opcode == 0x00EE:
            self.sp -= 1
            self.pc = self.stack[self.sp]
        elif opcode == 0x0000:
            # Treat 0x0000 as NOP to keep handcrafted ROMs simple.
            pass
        else:
            prefix = opcode & 0xF000
            if prefix == 0x1000:
                self.pc = nnn
            elif prefix == 0x2000:
                self.stack[self.sp] = self.pc
                self.sp = (self.sp + 1) & 0xF
                self.pc = nnn
            elif prefix == 0x3000:
                if self.V[x] == nn:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0x4000:
                if self.V[x] != nn:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0x5000:
                if n == 0 and self.V[x] == self.V[y]:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0x6000:
                self.V[x] = nn
            elif prefix == 0x7000:
                self.V[x] = (self.V[x] + nn) & 0xFF
            elif prefix == 0x8000:
                self._execute_math(opcode, x, y, n)
            elif prefix == 0x9000:
                if n == 0 and self.V[x] != self.V[y]:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0xA000:
                self.I = nnn
            elif prefix == 0xB000:
                self.pc = (nnn + self.V[0]) & 0xFFFF
            elif prefix == 0xC000:
                rand_val = self.rng.randint(0, 255)
                self.V[x] = rand_val & nn
            elif prefix == 0xD000:
                sprite = [self.memory.read_byte(self.I + offset) for offset in range(n)]
                collision = self.display.draw_sprite(self.V[x], self.V[y], sprite)
                self.V[0xF] = 1 if collision else 0
            elif prefix == 0xE000:
                if nn == 0x9E:
                    if self.keypad.is_pressed(self.V[x]):
                        self.pc = (self.pc + 2) & 0xFFFF
                elif nn == 0xA1:
                    if not self.keypad.is_pressed(self.V[x]):
                        self.pc = (self.pc + 2) & 0xFFFF
                else:
                    raise NotImplementedError(f"Unknown EX opcode: {opcode:#06x}")
            elif prefix == 0xF000:
                self._execute_extended(x, nn)
            else:
                raise NotImplementedError(f"Unknown opcode: {opcode:#06x}")

    def _execute_math(self, opcode: int, x: int, y: int, n: int) -> None:
        if n == 0x0:
            self.V[x] = self.V[y]
        elif n == 0x1:
            self.V[x] |= self.V[y]
        elif n == 0x2:
            self.V[x] &= self.V[y]
        elif n == 0x3:
            self.V[x] ^= self.V[y]
        elif n == 0x4:
            result = self.V[x] + self.V[y]
            self.V[0xF] = 1 if result > 0xFF else 0
            self.V[x] = result & 0xFF
        elif n == 0x5:
            self.V[0xF] = 1 if self.V[x] > self.V[y] else 0
            self.V[x] = (self.V[x] - self.V[y]) & 0xFF
        elif n == 0x6:
            self.V[0xF] = self.V[x] & 0x1
            self.V[x] = (self.V[x] >> 1) & 0xFF
        elif n == 0x7:
            self.V[0xF] = 1 if self.V[y] > self.V[x] else 0
            self.V[x] = (self.V[y] - self.V[x]) & 0xFF
        elif n == 0xE:
            self.V[0xF] = (self.V[x] >> 7) & 0x1
            self.V[x] = (self.V[x] << 1) & 0xFF
        else:
            raise NotImplementedError(f"Unknown math opcode: {opcode:#06x}")

    def _execute_extended(self, x: int, nn: int) -> None:
        if nn == 0x07:
            self.V[x] = self.delay_timer
        elif nn == 0x0A:
            key = self.keypad.consume_press()
            if key is None:
                self.waiting_register = x
                self.pc = (self.pc - 2) & 0xFFFF
            else:
                self.V[x] = key
        elif nn == 0x15:
            self.delay_timer = self.V[x]
        elif nn == 0x18:
            self.sound_timer = self.V[x]
        elif nn == 0x1E:
            value = self.I + self.V[x]
            self.V[0xF] = 1 if value > 0xFFF else 0
            self.I = value & 0xFFFF
        elif nn == 0x29:
            self.I = self.V[x] * 5
        elif nn == 0x33:
            value = self.V[x]
            hundreds = value // 100
            tens = (value // 10) % 10
            ones = value % 10
            self.memory.write_byte(self.I, hundreds)
            self.memory.write_byte(self.I + 1, tens)
            self.memory.write_byte(self.I + 2, ones)
        elif nn == 0x55:
            for index in range(x + 1):
                self.memory.write_byte(self.I + index, self.V[index])
            self.I = (self.I + x + 1) & 0xFFFF
        elif nn == 0x65:
            for index in range(x + 1):
                self.V[index] = self.memory.read_byte(self.I + index)
            self.I = (self.I + x + 1) & 0xFFFF
        else:
            raise NotImplementedError(f"Unknown FX opcode: 0xF{ x:X}{nn:02X}")

    def tick_timers(self) -> None:
        if self.delay_timer > 0:
            self.delay_timer -= 1
        if self.sound_timer > 0:
            self.sound_timer -= 1
