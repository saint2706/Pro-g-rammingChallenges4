"""Minimal command-line runner for the TI-86 emulator core.

This script provides a command-line interface for the TI-86 emulator. It can be
used to load and run a ROM for a specified number of instructions, as well as to
verify the correctness of the Z80 CPU core against a set of predefined test cases.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Iterable

# Add the source directory to the Python path to allow importing the emulator modules.
ROOT = Path(__file__).resolve().parents[2]
SRC_PATH = ROOT / "src"
if str(SRC_PATH) not in sys.path:
    sys.path.insert(0, str(SRC_PATH))

from pro_g_rammingchallenges4.emulation import ti86 as ti86_pkg
from pro_g_rammingchallenges4.emulation.ti86 import TI86, Memory, Z80CPU

TRUTH_PATH = Path(ti86_pkg.__file__).with_name("opcode_truth.json")


def _load_rom_bytes(path: Path) -> bytes:
    """Loads a ROM file from the specified path.

    Args:
        path: The path to the ROM file.

    Returns:
        The content of the ROM file as bytes.
    """
    if not path.exists():
        raise FileNotFoundError(f"ROM file not found: {path}")
    return path.read_bytes()


def _run_truth_cases(cases: Iterable[dict]) -> None:
    """Runs a series of test cases to verify the CPU's opcode implementations.

    Args:
        cases: An iterable of test case dictionaries.

    Raises:
        AssertionError: If any of the test cases fail.
    """
    for case in cases:
        memory = Memory()
        program = bytes(case["program"])
        memory.rom[: len(program)] = program

        if "pre_memory" in case:
            for addr, value in case["pre_memory"].items():
                memory.write_byte(int(addr), value)

        cpu = Z80CPU(memory)
        cpu.reset()
        cpu.load_state(case.get("initial", {}))

        for _ in range(case.get("steps", 1)):
            cpu.step()

        state = cpu.export_state()
        for key, value in case["expected"].items():
            actual = state[key]
            if actual != value:
                raise AssertionError(
                    f"{case['name']}: Expected {key}={value:#04x}, but got {actual:#04x}"
                )

        if "post_memory" in case:
            for addr, value in case["post_memory"].items():
                actual = memory.read_byte(int(addr))
                if actual != value:
                    raise AssertionError(
                        f"{case['name']}: Memory at {addr} should be {value:#04x}, but was {actual:#04x}"
                    )


def _run_rom(calc: TI86, rom: bytes, max_instructions: int, trace: bool) -> None:
    """Loads and runs a ROM in the emulator.

    Args:
        calc: The TI86 emulator instance.
        rom: The ROM content to load.
        max_instructions: The maximum number of instructions to execute.
        trace: Whether to print a trace of each executed instruction.
    """
    calc.reset()
    calc.load_rom(rom)

    for executed in range(max_instructions):
        trace_info = calc.step()
        if trace:
            print(
                f"{trace_info.pc:04X}: {trace_info.mnemonic:<10} "
                f"(opcode=0x{trace_info.opcode:02X}, cycles={trace_info.cycles})"
            )
        if calc.cpu.halted:
            print("CPU halted.")
            break


def build_parser() -> argparse.ArgumentParser:
    """Builds the command-line argument parser."""
    parser = argparse.ArgumentParser(description="Minimal TI-86 emulator runner.")
    parser.add_argument("--rom", type=Path, help="Path to a TI-86 ROM image.")
    parser.add_argument(
        "--verify-truth",
        action="store_true",
        help="Run opcode truth-table cases to verify the CPU core.",
    )
    parser.add_argument(
        "--max-instructions",
        type=int,
        default=200,
        help="Maximum number of instructions to execute.",
    )
    parser.add_argument(
        "--trace",
        action="store_true",
        help="Print a trace of each executed instruction.",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    """The main entry point for the script."""
    parser = build_parser()
    args = parser.parse_args(argv)

    if args.verify_truth:
        with TRUTH_PATH.open("r", encoding="utf-8") as handle:
            cases = json.load(handle)
        try:
            _run_truth_cases(cases)
        except AssertionError as exc:
            parser.error(str(exc))
        print(f"Successfully verified {len(cases)} opcode test cases.")

    if args.rom:
        try:
            rom = _load_rom_bytes(args.rom)
        except FileNotFoundError as exc:
            parser.error(str(exc))
        calc = TI86.create()
        _run_rom(calc, rom, args.max_instructions, args.trace)
    elif not args.verify_truth:
        parser.error("No action specified. Use --rom or --verify-truth.")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
