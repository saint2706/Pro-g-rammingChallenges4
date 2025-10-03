"""Unit tests for the CHIP-8 CPU implementation."""

from __future__ import annotations

import random
from typing import Callable

import pytest

from Emulation.Chip8.cpu import CPU
from Emulation.Chip8.display import DisplayBuffer
from Emulation.Chip8.input import Keypad
from Emulation.Chip8.memory import Memory

RNG_SEED = 1234


@pytest.fixture()
def cpu() -> CPU:
    memory = Memory()
    display = DisplayBuffer()
    keypad = Keypad()
    rng = random.Random(RNG_SEED)
    return CPU(memory=memory, display=display, keypad=keypad, rng=rng)


def run_program(
    cpu: CPU,
    program: bytes,
    steps: int | None = None,
    after_reset: Callable[[CPU], None] | None = None,
) -> None:
    cpu.memory.clear()
    cpu.memory.load_rom(program)
    cpu.reset()
    if after_reset is not None:
        after_reset(cpu)
    executed = 0
    while steps is None or executed < steps:
        if cpu.pc >= cpu.memory.start_address + len(program):
            break
        cpu.step()
        executed += 1


def test_load_store_and_memory(cpu: CPU) -> None:
    program = bytes(
        [
            0x60,
            0x0A,  # LD V0, 0x0A
            0x61,
            0x05,  # LD V1, 0x05
            0x80,
            0x14,  # ADD V0, V1 -> V0 = 0x0F
            0xA3,
            0x00,  # LD I, 0x300
            0xF1,
            0x55,  # LD [I], V0..V1
            0x60,
            0x00,  # LD V0, 0
            0x61,
            0x00,  # LD V1, 0
            0xF1,
            0x65,  # LD V0..V1, [I]
        ]
    )
    run_program(cpu, program)
    assert cpu.V[0] == 0x0F
    assert cpu.V[1] == 0x05
    assert cpu.memory.read_byte(0x300) == 0x0F
    assert cpu.memory.read_byte(0x301) == 0x05


def test_sprite_render_and_collision(cpu: CPU) -> None:
    sprite = [0xF0, 0x90, 0x90, 0x90, 0xF0]
    program = bytes(
        [
            0x60,
            0x00,  # LD V0, 0
            0x61,
            0x00,  # LD V1, 0
            0xA3,
            0x00,  # LD I, 0x300
            0xD0,
            0x15,  # DRW V0, V1, 5
            0xD0,
            0x15,  # DRW V0, V1, 5 -> collision on identical draw
        ]
    )

    def load_sprite(state: CPU) -> None:
        for index, value in enumerate(sprite):
            state.memory.write_byte(0x300 + index, value)

    run_program(cpu, program, after_reset=load_sprite)
    pixels = cpu.display.pixels()
    assert pixels[0][:4] == (0, 0, 0, 0)
    assert cpu.V[0xF] == 1  # collision flag set on second draw


def test_flow_control_and_random(cpu: CPU) -> None:
    mirror_rng = random.Random(RNG_SEED)
    expected_random = mirror_rng.randint(0, 255) & 0x0F
    program = bytes(
        [
            0x60,
            0x05,  # LD V0, 5
            0x30,
            0x05,  # SE V0, 5 -> skip next
            0x60,
            0x00,  # (skipped)
            0x70,
            0x01,  # ADD V0, 1 -> V0 = 6
            0xC1,
            0x0F,  # RND V1, 0x0F -> deterministic via seeded RNG
            0x22,
            0x12,  # CALL 0x212
            0x12,
            0x14,  # JP 0x214 -> terminate
            0x00,
            0xE0,  # padding
            0x00,
            0x00,  # padding
            0x72,
            0x02,  # @0x212: ADD V2, 2
            0x00,
            0xEE,  # RET
        ]
    )
    run_program(cpu, program, steps=8)
    assert cpu.V[0] == 0x06
    assert cpu.V[1] == expected_random
    assert cpu.V[2] == 0x02
    assert cpu.pc == 0x214


def test_bcd_encoding(cpu: CPU) -> None:
    program = bytes(
        [
            0x60,
            0xFF,  # LD V0, 255
            0xA3,
            0x10,  # LD I, 0x310
            0xF0,
            0x33,  # BCD V0
        ]
    )
    run_program(cpu, program)
    assert cpu.memory.read_byte(0x310) == 2
    assert cpu.memory.read_byte(0x311) == 5
    assert cpu.memory.read_byte(0x312) == 5
