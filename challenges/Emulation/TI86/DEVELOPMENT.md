# TI-86 emulator architecture notes

## Overview

The emulator is organised as small, composable modules:

| Module | Responsibility |
| ------ | -------------- |
| `memory.py` | 64 KiB Z80 address space backed by a ROM page and a flat 32 KiB RAM window. |
| `cpu.py` | Z80 execution core with single-step tracing and structured register access. |
| `lcd.py` | 128×64 monochrome framebuffer stored as a 1-D list for easy inspection. |
| `keypad.py` | Simple row/column matrix that mimics the TI-86 keypad scanning model. |
| `emulator.py` | High level façade that bundles the CPU, memory, LCD and keypad. |
| `debugger.py` | Thin breakpoint manager that raises on breakpoints and emits step traces. |
| `rom.py` | Utility helpers for loading ROM images from files or streams. |

`TI86.create()` wires the components together.  The CPU exposes `step()`,
`load_state()` and `export_state()` which the debugger and unit tests use to
control execution deterministically.

## Opcode coverage

The CPU currently implements the commonly used core of the Z80 instruction set:

* 8-bit loads (`LD r,r`, `LD r,n`, `LD (HL),n`, `LD (rr),A`, `LD A,(rr)`).
* 16-bit loads and stack ops (`LD rr,nn`, `LD (nn),HL`, `LD HL,(nn)`, `PUSH`, `POP`, `LD SP,HL`).
* Arithmetic and logical instructions (`ADD/ADC/SUB/SBC/AND/OR/XOR/CP` on registers and immediates, `INC/DEC` on registers and `(HL)`, `DAA`, `CPL`, `SCF`, `CCF`).
* 16-bit arithmetic (`ADD HL,rr`).
* Control flow (`JR`, conditional `JR`, `JP`, conditional `JP`, `CALL`, `RET`, conditional `RET`, `HALT`).
* Bit-test prefix `CB` (`BIT b,r`).
* Rotation helpers (`RLCA`, `RRCA`, `RLA`, `RRA`).
* Interrupt toggles (`DI`, `EI`) and a stubbed `OUT` instruction for memory-mapped I/O.

Extending the coverage only requires adding a handler to `_build_decoder()` and
implementing the opcode as a method. The helpers `_read_reg_by_index`,
`_write_reg_by_index`, `_set_flags_add`, and `_set_flags_sub` encapsulate the
trickier flag logic.

## Testing strategy

Unit tests in `tests/emulation/test_ti86_cpu.py` load scenarios from
`src/pro_g_rammingchallenges4/emulation/ti86/opcode_truth.json`. Each scenario records the initial register file, the program
bytes to execute, and the expected register/memory state after one or more
instructions. The values mirror the canonical tables from Sean Young's “The Z80
Instruction Set” documentation and have been simplified for brevity.

Additional tests validate the memory map (ROM writes are ignored, RAM writes
persist), the breakpoint behaviour, and the integration of the LCD and keypad
helpers. Run the full suite with:

```bash
pytest tests/emulation/test_ti86_cpu.py
```

To instrument new instructions, add a scenario to the JSON table and write a
focused unit test if hardware integration is required.
