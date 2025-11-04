"""CPU core implementing the CHIP-8 instruction set.

This module contains the CPU class, which is the heart of the emulator. It
manages the CPU state, including registers, timers, and the program counter,
and it is responsible for the fetch-decode-execute cycle of the Chip-8
instruction set.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional
import random

from .display import DisplayBuffer
from .input import Keypad
from .memory import Memory


@dataclass
class CPU:
    """Emulates the CHIP-8 CPU.

    This class encapsulates the state and functionality of the Chip-8 CPU.

    Attributes:
        memory: The memory module for the emulator.
        display: The display buffer for rendering.
        keypad: The keypad for handling input.
        clock_hz: The clock speed of the CPU in cycles per second.
        rng: A random number generator.
    """

    memory: Memory = field(default_factory=Memory)
    display: DisplayBuffer = field(default_factory=DisplayBuffer)
    keypad: Keypad = field(default_factory=Keypad)
    clock_hz: int = 500
    rng: random.Random = field(default_factory=random.Random)

    def __post_init__(self) -> None:
        """Initializes the CPU state after the object is created."""
        self.reset()

    def reset(self) -> None:
        """Resets the CPU to its initial state."""
        self.V = [0] * 16  # General-purpose registers V0-VF.
        self.I = 0  # Index register.
        self.pc = self.memory.start_address  # Program counter.
        self.stack = [0] * 16  # Stack for subroutines.
        self.sp = 0  # Stack pointer.
        self.delay_timer = 0
        self.sound_timer = 0
        self.waiting_register: Optional[int] = None  # For FX0A instruction.
        self.halted = False

    def load_rom(self, rom_bytes: bytes) -> None:
        """Loads a ROM into memory and resets the CPU.

        Args:
            rom_bytes: The binary content of the ROM.
        """
        self.memory.clear()
        self.reset()
        self.memory.load_rom(rom_bytes)

    def fetch_opcode(self) -> int:
        """Fetches the next opcode from memory and advances the program counter.

        Returns:
            The fetched opcode.
        """
        opcode = self.memory.read_word(self.pc)
        self.pc = (self.pc + 2) & 0xFFFF
        return opcode

    def step(self) -> int:
        """Performs a single CPU cycle.

        This involves fetching, decoding, and executing one opcode.

        Returns:
            The executed opcode.
        """
        if self.halted:
            return 0x0000

        if self.waiting_register is not None:
            key = self.keypad.consume_press()
            if key is None:
                return 0x0000  # Still waiting for a key press.
            self.V[self.waiting_register] = key
            self.waiting_register = None

        opcode = self.fetch_opcode()
        self.execute(opcode)
        return opcode

    def execute(self, opcode: int) -> None:
        """Executes a single Chip-8 opcode.

        Args:
            opcode: The opcode to execute.

        Raises:
            NotImplementedError: If an unknown opcode is encountered.
        """
        # Decode the opcode into its components.
        nnn = opcode & 0x0FFF
        nn = opcode & 0x00FF
        n = opcode & 0x000F
        x = (opcode >> 8) & 0x000F
        y = (opcode >> 4) & 0x000F

        if opcode == 0x00E0:  # 00E0: Clears the screen.
            self.display.clear()
        elif opcode == 0x00EE:  # 00EE: Returns from a subroutine.
            self.sp -= 1
            self.pc = self.stack[self.sp]
        elif opcode == 0x0000:
            # 0000: No operation, used to simplify handcrafted ROMs.
            pass
        else:
            prefix = opcode & 0xF000
            if prefix == 0x1000:  # 1NNN: Jumps to address NNN.
                self.pc = nnn
            elif prefix == 0x2000:  # 2NNN: Calls subroutine at NNN.
                self.stack[self.sp] = self.pc
                self.sp = (self.sp + 1) & 0xF
                self.pc = nnn
            elif prefix == 0x3000:  # 3XNN: Skips the next instruction if VX equals NN.
                if self.V[x] == nn:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0x4000:  # 4XNN: Skips the next instruction if VX does not equal NN.
                if self.V[x] != nn:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0x5000:  # 5XY0: Skips the next instruction if VX equals VY.
                if n == 0 and self.V[x] == self.V[y]:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0x6000:  # 6XNN: Sets VX to NN.
                self.V[x] = nn
            elif prefix == 0x7000:  # 7XNN: Adds NN to VX.
                self.V[x] = (self.V[x] + nn) & 0xFF
            elif prefix == 0x8000:  # 8XYN: Bitwise and math operations.
                self._execute_math(opcode, x, y, n)
            elif prefix == 0x9000:  # 9XY0: Skips the next instruction if VX does not equal VY.
                if n == 0 and self.V[x] != self.V[y]:
                    self.pc = (self.pc + 2) & 0xFFFF
            elif prefix == 0xA000:  # ANNN: Sets I to the address NNN.
                self.I = nnn
            elif prefix == 0xB000:  # BNNN: Jumps to the address NNN plus V0.
                self.pc = (nnn + self.V[0]) & 0xFFFF
            elif prefix == 0xC000:  # CXNN: Sets VX to a random number and NN.
                rand_val = self.rng.randint(0, 255)
                self.V[x] = rand_val & nn
            elif prefix == 0xD000:  # DXYN: Draws a sprite.
                sprite = [self.memory.read_byte(self.I + offset) for offset in range(n)]
                collision = self.display.draw_sprite(self.V[x], self.V[y], sprite)
                self.V[0xF] = 1 if collision else 0
            elif prefix == 0xE000:  # EXNN: Keypad-related instructions.
                if nn == 0x9E:  # EX9E: Skips the next instruction if the key in VX is pressed.
                    if self.keypad.is_pressed(self.V[x]):
                        self.pc = (self.pc + 2) & 0xFFFF
                elif nn == 0xA1:  # EXA1: Skips the next instruction if the key in VX is not pressed.
                    if not self.keypad.is_pressed(self.V[x]):
                        self.pc = (self.pc + 2) & 0xFFFF
                else:
                    raise NotImplementedError(f"Unknown EX opcode: {opcode:#06x}")
            elif prefix == 0xF000:  # FXNN: Miscellaneous instructions.
                self._execute_extended(x, nn)
            else:
                raise NotImplementedError(f"Unknown opcode: {opcode:#06x}")

    def _execute_math(self, opcode: int, x: int, y: int, n: int) -> None:
        """Executes the 8XYN family of opcodes."""
        if n == 0x0:  # 8XY0: Sets VX to the value of VY.
            self.V[x] = self.V[y]
        elif n == 0x1:  # 8XY1: Sets VX to VX or VY.
            self.V[x] |= self.V[y]
        elif n == 0x2:  # 8XY2: Sets VX to VX and VY.
            self.V[x] &= self.V[y]
        elif n == 0x3:  # 8XY3: Sets VX to VX xor VY.
            self.V[x] ^= self.V[y]
        elif n == 0x4:  # 8XY4: Adds VY to VX. VF is set to 1 when there's a carry.
            result = self.V[x] + self.V[y]
            self.V[0xF] = 1 if result > 0xFF else 0
            self.V[x] = result & 0xFF
        elif n == 0x5:  # 8XY5: VY is subtracted from VX. VF is set to 0 when there's a borrow.
            self.V[0xF] = 1 if self.V[x] > self.V[y] else 0
            self.V[x] = (self.V[x] - self.V[y]) & 0xFF
        elif n == 0x6:  # 8XY6: Shifts VX right by one. VF is set to the value of the LSB of VX before the shift.
            self.V[0xF] = self.V[x] & 0x1
            self.V[x] = (self.V[x] >> 1) & 0xFF
        elif n == 0x7:  # 8XY7: Sets VX to VY minus VX. VF is set to 0 when there's a borrow.
            self.V[0xF] = 1 if self.V[y] > self.V[x] else 0
            self.V[x] = (self.V[y] - self.V[x]) & 0xFF
        elif n == 0xE:  # 8XYE: Shifts VX left by one. VF is set to the value of the MSB of VX before the shift.
            self.V[0xF] = (self.V[x] >> 7) & 0x1
            self.V[x] = (self.V[x] << 1) & 0xFF
        else:
            raise NotImplementedError(f"Unknown math opcode: {opcode:#06x}")

    def _execute_extended(self, x: int, nn: int) -> None:
        """Executes the FXNN family of opcodes."""
        if nn == 0x07:  # FX07: Sets VX to the value of the delay timer.
            self.V[x] = self.delay_timer
        elif nn == 0x0A:  # FX0A: A key press is awaited, and then stored in VX.
            key = self.keypad.consume_press()
            if key is None:
                self.waiting_register = x
                self.pc = (self.pc - 2) & 0xFFFF  # Decrement PC to re-run this instruction.
            else:
                self.V[x] = key
        elif nn == 0x15:  # FX15: Sets the delay timer to VX.
            self.delay_timer = self.V[x]
        elif nn == 0x18:  # FX18: Sets the sound timer to VX.
            self.sound_timer = self.V[x]
        elif nn == 0x1E:  # FX1E: Adds VX to I.
            value = self.I + self.V[x]
            self.V[0xF] = 1 if value > 0xFFF else 0
            self.I = value & 0xFFFF
        elif nn == 0x29:  # FX29: Sets I to the location of the sprite for the character in VX.
            self.I = self.V[x] * 5
        elif nn == 0x33:  # FX33: Stores the BCD representation of VX in memory.
            value = self.V[x]
            self.memory.write_byte(self.I, value // 100)
            self.memory.write_byte(self.I + 1, (value // 10) % 10)
            self.memory.write_byte(self.I + 2, value % 10)
        elif nn == 0x55:  # FX55: Stores V0 to VX in memory starting at address I.
            for index in range(x + 1):
                self.memory.write_byte(self.I + index, self.V[index])
            self.I = (self.I + x + 1) & 0xFFFF
        elif nn == 0x65:  # FX65: Fills V0 to VX with values from memory starting at address I.
            for index in range(x + 1):
                self.V[index] = self.memory.read_byte(self.I + index)
            self.I = (self.I + x + 1) & 0xFFFF
        else:
            raise NotImplementedError(f"Unknown FX opcode: 0xF{ x:X}{nn:02X}")

    def tick_timers(self) -> None:
        """Decrements the delay and sound timers if they are active."""
        if self.delay_timer > 0:
            self.delay_timer -= 1
        if self.sound_timer > 0:
            self.sound_timer -= 1
