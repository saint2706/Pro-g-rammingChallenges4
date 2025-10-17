# TI-86 Emulator Core

This directory houses a clean-room TI-86 emulator skeleton written in Python. The
focus is on clarity, deterministic behaviour, and unit-test-driven development
rather than cycle-perfect reproduction of the original hardware. The code ships
with a Z80 CPU core, a simplified memory map, LCD frame buffer, keypad matrix, a
ROM loader, and hooks that make single-step debugging straightforward.

## Usage

```python
from pathlib import Path

from pro_g_rammingchallenges4.emulation.ti86 import TI86, load_rom_image

calc = TI86.create()
load_rom_image(calc.memory, Path("/path/to/ti86.rom"))
calc.reset()

# Step through the boot ROM
for _ in range(10):
    trace = calc.step()
    print(trace)
```

### Command-line runner

For quick smoke tests, the repository now ships a minimal CLI wrapper around the
emulator core. It can execute a ROM for a bounded number of instructions or
validate the opcode truth table used by the unit tests:

```bash
python challenges/Emulation/TI86/emulator.py --verify-truth
python challenges/Emulation/TI86/emulator.py --rom path/to/ti86.rom --trace --max-instructions 50
```

### Debugging helpers

```python
from pro_g_rammingchallenges4.emulation.ti86 import Debugger

dbg = Debugger(calc.cpu)
dbg.add_breakpoint(0x0045)

def on_step(trace):
    print(f"{trace.pc:04X}: {trace.mnemonic} ({trace.cycles} cycles)")

dbg.on_step = on_step

# Run until the breakpoint triggers
try:
    while True:
        dbg.step()
except RuntimeError:
    print("Reached breakpoint")
```

## Dependencies

* Python 3.10+
* `pytest` (only required for running the regression tests)

No external graphics or audio libraries are required; the LCD panel is stored as
a plain list of pixels so you can integrate your preferred rendering solution.

## Legal notice

The emulator requires a TI-86 ROM image to boot. Texas Instruments retains the
copyright for the operating system and firmware. Do **not** distribute ROM dumps
with this repository. Extract the ROM from hardware you own or follow the
procedures allowed in your jurisdiction.

## Running the tests

The emulator is covered by regression tests that execute representative opcodes
against a reference table, validate the memory map, and exercise the debugger,
LCD, and keypad plumbing.

```bash
python -m pip install -e .[developer]
pytest tests/emulation/test_ti86_cpu.py
```

Refer to `DEVELOPMENT.md` for internals and extension guidelines.
