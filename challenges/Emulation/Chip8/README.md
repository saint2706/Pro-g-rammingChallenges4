# CHIP-8 Emulator

This is a complete and well-structured CHIP-8 emulator written in Python. It is designed with a modular architecture, separating the core components of the emulator into distinct, focused modules.

## Project Structure

The emulator is organized into the following files and directories:

- `roms/`: A directory containing CHIP-8 ROMs.
- `cli.py`: The command-line interface for running the emulator.
- `cpu.py`: The core of the emulator, handling the instruction cycle.
- `display.py`: Manages the 64x32 monochrome display.
- `input.py`: Handles keypad input.
- `memory.py`: Manages the 4K memory map and font set.

## Modules

### `memory.py`
This module represents the 4K memory map of the Chip-8. It includes the built-in font set and provides methods for reading and writing to memory.

### `cpu.py`
The heart of the emulator, this module contains the CPU class, which is responsible for the fetch-decode-execute cycle. It manages the registers, program counter, and stack, and it executes the Chip-8 opcodes.

### `display.py`
This module handles the 64x32 monochrome framebuffer. It provides different rendering backends, including:
- **Pygame**: A graphical backend that displays the emulator in a window.
- **Curses**: A terminal-based backend for running the emulator in a text-based interface.
- **Headless**: A backend that does not render anything, useful for testing and automation.

### `input.py`
This module abstracts the 16-key hexadecimal keypad of the Chip-8. It provides drivers for both Pygame and Curses to handle user input.

### `cli.py`
The command-line front-end for the emulator. It allows you to select a ROM, choose a rendering backend, and configure the CPU speed.

## Requirements

- Python 3.10+
- **Optional**: `pygame>=2.6` for the Pygame rendering and input backend.
- **Optional**: A Curses-capable terminal for the Curses backend.

You can install the optional dependencies using the repository extras:
```bash
python -m pip install -e .[emulation]
```

## Running the Emulator

You can run the emulator from the command line with various options.

```bash
python -m Emulation.Chip8.cli hello.ch8 --speed 600 --backend pygame --scale 16
```

### Arguments:

- `rom`: The filename of the ROM to load.
- `--backend`: The rendering backend to use (`pygame`, `curses`, or `headless`).
- `--speed`: The CPU cycles per second (default is 500).
- `--scale`: The pixel scale factor for the Pygame renderer.
- `--headless-frames`: The number of frames to run in headless mode before exiting.

## Key Bindings

The Chip-8's 16-key keypad is mapped to a standard keyboard layout:

```
┌────┬────┬────┬────┐
│ 1  │ 2  │ 3  │ 4  │ -> 1 2 3 C
├────┼────┼────┼────┤
│ Q  │ W  │ E  │ R  │ -> 4 5 6 D
├────┼────┼────┼────┤
│ A  │ S  │ D  │ F  │ -> 7 8 9 E
├────┼────┼────┼────┤
│ Z  │ X  │ C  │ V  │ -> A 0 B F
└────┴────┴────┴────┘
```
Press `Esc` (in Pygame) or `q`/`Esc` (in Curses) to exit the emulator.

## Testing

The project includes unit tests for the CPU's opcode implementations. You can run them from the root of the repository:

```bash
pytest tests/test_chip8_cpu.py
```

## Implementation Notes

- Timers tick at 60 Hz, derived from the system's wall-clock time.
- The `FX0A` opcode will wait for a key press without advancing the program counter.
- The `FX55` and `FX65` opcodes update the index register (`I`) to match the behavior of the original Chip-8 interpreter.
- The headless backend is ideal for automated testing, as it reuses the framebuffer logic without creating a window.
