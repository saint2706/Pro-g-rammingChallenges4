"""Minimal command-line runner for the TI-86 emulator core.

This module bridges the research artefacts in :mod:`challenges/Emulation/TI86` with the
fully functional CPU, memory and peripheral skeleton implemented under
``src/pro_g_rammingchallenges4/emulation/ti86``.  It supports loading ROMs,
single-stepping through instructions, and validating the opcode truth table
documented alongside the research notes.
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Iterable

ROOT = Path(__file__).resolve().parents[2]
SRC_PATH = ROOT / "src"
if str(SRC_PATH) not in sys.path:
    sys.path.insert(0, str(SRC_PATH))

from pro_g_rammingchallenges4.emulation.ti86 import TI86, Memory, Z80CPU


def _load_rom_bytes(path: Path) -> bytes:
    if not path.exists():
        raise FileNotFoundError(f"ROM '{path}' does not exist")
    return path.read_bytes()


def _run_truth_cases(cases: Iterable[dict]) -> None:
    """Execute opcode truth-table cases to sanity check the CPU core."""

    for case in cases:
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
            actual = state[key]
            if actual != value:
                raise AssertionError(
                    f"{case['name']}: expected {key}={value:#04x}, got {actual:#04x}"
                )

        if "post_memory" in case:
            for addr_str, value in case["post_memory"].items():
                addr = int(addr_str)
                actual = memory.read_byte(addr)
                if actual != value:
                    raise AssertionError(
                        f"{case['name']} memory[{addr:#04x}]={actual:#04x}, expected {value:#04x}"
                    )


def _run_rom(calc: TI86, rom: bytes, max_instructions: int, trace: bool) -> None:
    calc.reset()
    calc.load_rom(rom)

    executed = 0
    while executed < max_instructions:
        trace_info = calc.step()
        executed += 1
        if trace:
            print(
                f"{trace_info.pc:04X}: {trace_info.mnemonic:<10}"
                f" opcode=0x{trace_info.opcode:02X} cycles={trace_info.cycles}"
            )
        if calc.cpu.halted:
            break


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Minimal TI-86 emulator runner")
    parser.add_argument("--rom", type=Path, help="Path to a TI-86 ROM image")
    parser.add_argument(
        "--verify-truth",
        action="store_true",
        help="Run opcode truth-table cases from opcode_truth.json",
    )
    parser.add_argument(
        "--max-instructions",
        type=int,
        default=200,
        help="Maximum instructions to execute when running a ROM",
    )
    parser.add_argument(
        "--trace",
        action="store_true",
        help="Print each executed instruction while running a ROM",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    if args.verify_truth:
        truth_path = ROOT / "challenges" / "Emulation" / "TI86" / "opcode_truth.json"
        with truth_path.open("r", encoding="utf-8") as handle:
            cases = json.load(handle)
        try:
            _run_truth_cases(cases)
        except AssertionError as exc:  # pragma: no cover - CLI guardrail
            parser.error(str(exc))
        print(f"Verified {len(cases)} opcode cases.")

    if args.rom:
        try:
            rom = _load_rom_bytes(args.rom)
        except FileNotFoundError as exc:  # pragma: no cover - CLI guardrail
            parser.error(str(exc))
        calc = TI86.create()
        _run_rom(calc, rom, args.max_instructions, args.trace)
        if calc.cpu.halted:
            print("Execution halted.")
    elif not args.verify_truth:
        parser.error("No action requested. Specify --rom or --verify-truth.")

    return 0


if __name__ == "__main__":  # pragma: no cover - manual entry point
    raise SystemExit(main())
