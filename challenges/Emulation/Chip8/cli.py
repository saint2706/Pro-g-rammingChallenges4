"""Command line interface for the CHIP-8 emulator.

This module provides the main entry point for running the Chip-8 emulator.
It handles command-line argument parsing, ROM selection, and the main
emulator loop.
"""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path
from typing import Iterable, Optional

from .cpu import CPU
from .display import CursesDisplay, DisplayBuffer, PygameDisplay, create_display
from .input import CursesInput, Keypad, PygameInput
from .memory import Memory

ROM_DIRECTORY = Path(__file__).resolve().parent / "roms"


def iter_roms(directory: Path) -> Iterable[Path]:
    """Iterates over the ROM files in a given directory.

    Args:
        directory: The directory to search for ROM files.

    Returns:
        An iterable of Path objects for the found ROMs.
    """
    if not directory.exists():
        return []
    return sorted(path for path in directory.iterdir() if path.is_file())


def choose_rom(directory: Path) -> Optional[Path]:
    """Prompts the user to choose a ROM from a directory.

    Args:
        directory: The directory containing the ROMs.

    Returns:
        The Path object for the selected ROM, or None if no selection is made.
    """
    roms = list(iter_roms(directory))
    if not roms:
        print("No ROMs found in", directory)
        return None
    print("Available ROMs:")
    for index, rom in enumerate(roms, start=1):
        print(f"  {index}. {rom.name}")
    try:
        selection = int(input("Select ROM number: "))
    except (ValueError, EOFError):
        return None
    if not 1 <= selection <= len(roms):
        print("Invalid selection")
        return None
    return roms[selection - 1]


def build_parser() -> argparse.ArgumentParser:
    """Builds the command-line argument parser.

    Returns:
        An ArgumentParser instance for the emulator.
    """
    parser = argparse.ArgumentParser(description="Minimal CHIP-8 emulator")
    parser.add_argument(
        "rom", nargs="?", help="ROM path or name inside the roms directory"
    )
    parser.add_argument(
        "--rom-dir",
        type=Path,
        default=ROM_DIRECTORY,
        help="Directory containing ROM files",
    )
    parser.add_argument(
        "--backend",
        choices=["pygame", "curses", "headless"],
        default="pygame",
        help="Display backend",
    )
    parser.add_argument(
        "--scale", type=int, default=12, help="Pixel scale for the pygame backend"
    )
    parser.add_argument("--speed", type=int, default=500, help="CPU cycles per second")
    parser.add_argument(
        "--headless-frames",
        type=int,
        default=0,
        help="Run a limited number of frames in headless mode",
    )
    return parser


def resolve_rom(path_or_name: Optional[str], directory: Path) -> Optional[Path]:
    """Resolves the path to a ROM file.

    If a path is provided, it will be used. If a name is provided, it will be
    searched for in the specified ROM directory. If no path or name is given,
    the user will be prompted to choose a ROM.

    Args:
        path_or_name: The path or name of the ROM.
        directory: The directory to search for the ROM in.

    Returns:
        The Path object for the resolved ROM, or None if it cannot be found.
    """
    if path_or_name is None:
        return choose_rom(directory)
    candidate = Path(path_or_name)
    if candidate.is_file():
        return candidate
    candidate = directory / path_or_name
    if candidate.is_file():
        return candidate
    print(f"Unable to locate ROM '{path_or_name}'", file=sys.stderr)
    return None


def create_input_and_display(
    backend: str, scale: int
) -> tuple[DisplayBuffer, Optional[object], Keypad]:
    """Creates the display and input driver for the specified backend.

    Args:
        backend: The name of the backend to use ("pygame", "curses", or "headless").
        scale: The display scale factor for the Pygame backend.

    Returns:
        A tuple containing the display buffer, input driver, and keypad.
    """
    keypad = Keypad()
    if backend == "pygame":
        display = create_display("pygame", scale=scale)
        input_driver = PygameInput(keypad)
    elif backend == "curses":
        display = create_display("curses")
        assert isinstance(display, CursesDisplay)
        input_driver = CursesInput(keypad, display._screen)
    else:
        display = create_display("headless")
        input_driver = None
    return display, input_driver, keypad


def main(argv: Optional[list[str]] = None) -> int:
    """The main function for the emulator.

    This function initializes the emulator components, loads the ROM, and runs
    the main execution loop.

    Args:
        argv: A list of command-line arguments.

    Returns:
        An integer exit code (0 for success, 1 for failure).
    """
    parser = build_parser()
    args = parser.parse_args(argv)

    rom_path = resolve_rom(args.rom, args.rom_dir)
    if rom_path is None:
        return 1

    display, input_driver, keypad = create_input_and_display(args.backend, args.scale)
    memory = Memory()
    cpu = CPU(memory=memory, display=display, keypad=keypad)

    try:
        memory.load_rom_file(str(rom_path))
    except (OSError, ValueError) as exc:
        print(f"Failed to load ROM '{rom_path}': {exc}", file=sys.stderr)
        return 1

    cpu.reset()
    last_timer_tick = time.perf_counter()
    cycle_delay = 1.0 / args.speed if args.speed > 0 else 0
    frame_limit = args.headless_frames if args.backend == "headless" else None
    executed_frames = 0

    try:
        # Main emulator loop.
        while True:
            start = time.perf_counter()

            opcode = cpu.step()
            if opcode == 0x0000 and cpu.waiting_register is not None:
                # If the CPU is waiting for a key press, sleep to avoid a busy loop.
                time.sleep(0.005)

            if input_driver is not None:
                if isinstance(input_driver, PygameInput):
                    input_driver.process_events()
                    if input_driver.quit_requested:
                        break
                elif isinstance(input_driver, CursesInput):
                    if input_driver.process_events():
                        break

            display.present()

            now = time.perf_counter()
            if now - last_timer_tick >= 1 / 60:
                cpu.tick_timers()
                last_timer_tick = now
                executed_frames += 1
                if frame_limit is not None and executed_frames >= frame_limit:
                    break

            if cycle_delay:
                elapsed = time.perf_counter() - start
                sleep_time = max(0.0, cycle_delay - elapsed)
                if sleep_time > 0:
                    time.sleep(sleep_time)
    finally:
        display.close()

    return 0


if __name__ == "__main__":
    sys.exit(main())
