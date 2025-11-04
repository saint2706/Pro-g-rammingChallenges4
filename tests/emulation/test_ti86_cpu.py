"""Regression tests for the TI-86 CPU core."""

# ruff: noqa: E402

import json
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "src"))

import pro_g_rammingchallenges4.emulation.ti86 as ti86_pkg
from pro_g_rammingchallenges4.emulation.ti86 import Debugger, Memory, TI86, Z80CPU

DATA_PATH = Path(ti86_pkg.__file__).with_name("opcode_truth.json")


def load_opcode_cases():
    with DATA_PATH.open("r", encoding="utf-8") as handle:
        return json.load(handle)


@pytest.mark.parametrize("case", load_opcode_cases(), ids=lambda c: c["name"])
def test_cpu_against_known_good_cases(case):
    memory = Memory()
    program = bytes(case["program"])
    memory.rom[: len(program)] = program

    if "pre_memory" in case:
        for addr_str, value in case["pre_memory"].items():
            memory.write_byte(int(addr_str), value)

    cpu = Z80CPU(memory)
    cpu.reset()
    cpu.load_state(case.get("initial", {}))

    steps = case.get("steps", 1)
    for _ in range(steps):
        cpu.step()

    state = cpu.export_state()
    for key, value in case["expected"].items():
        assert state[key] == value, f"{case['name']} -> {key}"

    if "post_memory" in case:
        for addr_str, value in case["post_memory"].items():
            addr = int(addr_str)
            assert memory.read_byte(addr) == value, (
                f"{case['name']} memory[{addr:#04x}]"
            )


def test_memory_map_distinguishes_rom_and_ram():
    memory = Memory()
    memory.rom[0] = 0xAA
    memory.write_byte(0x0000, 0x55)
    assert memory.read_byte(0x0000) == 0xAA

    memory.write_byte(0x8000, 0x12)
    assert memory.read_byte(0x8000) == 0x12


def test_debugger_breakpoint_triggers_runtime_error():
    memory = Memory()
    memory.rom[0] = 0x00
    cpu = Z80CPU(memory)
    dbg = Debugger(cpu)
    dbg.add_breakpoint(0x0000)

    with pytest.raises(RuntimeError):
        dbg.step()


def test_lcd_and_keypad_integration():
    calc = TI86.create()
    calc.lcd.write_pixel(10, 10, 1)
    assert calc.lcd.get_pixels()[10 * calc.lcd.width + 10] == 1

    calc.keypad.press(1, 2)
    assert calc.keypad.is_pressed(1, 2)
    assert calc.keypad.active_columns(0b00000010) == 0xFB

    calc.keypad.release(1, 2)
    assert not calc.keypad.is_pressed(1, 2)
