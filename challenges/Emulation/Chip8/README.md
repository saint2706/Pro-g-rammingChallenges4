# CHIP-8 Emulator

A small but complete CHIP-8 emulator written in Python. The implementation splits the core components into focused modules:

- `memory.py` – 4K memory map with built-in font set loader.
- `cpu.py` – Instruction fetch/decode/execute loop with timer support.
- `display.py` – 64×32 monochrome framebuffer with pygame and curses renderers.
- `input.py` – CHIP-8 keypad abstraction plus pygame/curses drivers.
- `cli.py` – Command line front-end for selecting ROMs and setting CPU speed.

Bundled with the emulator is a handcrafted `hello.ch8` ROM that renders the classic “0” sprite as a sanity check. Drop your own ROMs into `challenges/Emulation/Chip8/roms/` or pass an explicit path to the CLI.

## Requirements

- Python 3.10+
- Optional: `pygame>=2.6` when using the pygame renderer/input backend.
- Optional: a curses-capable terminal for the curses backend (ships with the stdlib).

Install via the repository extras once the `pyproject.toml` entry is added:

```bash
python -m pip install -e .[emulation]
```

## Running the Emulator

```bash
python -m Emulation.Chip8.cli hello.ch8 --speed 600 --backend pygame --scale 16
```

Arguments:

- `rom` – ROM filename or full path. If omitted, the CLI lists `roms/` and prompts.
- `--backend` – `pygame`, `curses`, or `headless`. Headless is useful for tests/CI.
- `--speed` – CPU cycles per second (default 500). Lower for slow motion, higher for faster execution.
- `--scale` – Pixel scale factor for pygame rendering.
- `--headless-frames` – When using `--backend headless`, stop after *N* 60 Hz frames.

### Key Bindings

The keypad maps to the standard CHIP-8 layout:

```
┌────┬────┬────┬────┐
│ 1  │ 2  │ 3  │ 4  │ → 1 2 3 C
├────┼────┼────┼────┤
│ Q  │ W  │ E  │ R  │ → 4 5 6 D
├────┼────┼────┼────┤
│ A  │ S  │ D  │ F  │ → 7 8 9 E
├────┼────┼────┼────┤
│ Z  │ X  │ C  │ V  │ → A 0 B F
└────┴────┴────┴────┘
```

Press <kbd>Esc</kbd> (pygame) or <kbd>q</kbd>/<kbd>Esc</kbd> (curses) to exit.

## Testing

Unit tests cover a spread of opcodes using handcrafted programs. Run them from the repo root:

```bash
pytest tests/test_chip8_cpu.py
```

## Implementation Notes

- Timers tick at 60 Hz; the CLI derives this from wall-clock time.
- `FX0A` waits for a keypad press without advancing the program counter.
- `FX55`/`FX65` update the index register (`I`) to match the original interpreter behaviour.
- The headless backend reuses the framebuffer logic without opening a window – ideal for automated testing.
