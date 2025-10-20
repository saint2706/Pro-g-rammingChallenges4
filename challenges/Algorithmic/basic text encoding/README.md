# Text Encoding Converter

## Problem Statement
Convert text between human-readable strings and binary/hexadecimal byte representations while supporting multiple character encodings and reverse transformations.

## Usage
- Convert input to hexadecimal:
  ```bash
  python txtToHexAndBin.py "Hello" hex
  ```
- Generate binary output using UTF-16 encoding:
  ```bash
  python txtToHexAndBin.py "Hello" bin --encoding utf-16
  ```
- Launch the interactive prompt:
  ```bash
  python txtToHexAndBin.py --interactive
  ```

### Visualising encodings
- Produce JSON describing byte/bit structure (written to stdout with `--json -`):
  ```bash
  python encoding_visualizer.py --text "Hello" --json - --pretty
  ```
- Read text from a file, render plots comparing UTF-16 output with ASCII, and save
  the figure:
  ```bash
  python encoding_visualizer.py --file message.txt --encoding utf-16 --save-plot report.png
  ```
- The JSON includes `byte_values`, `bit_matrix`, round-trip verification flags,
  and ASCII comparison metadata that can be consumed by unit tests or other
  tooling.

## Debugging Tips
- Verify reversible conversions by piping the output back into the script's reverse mode (if available) or using Python's built-in `bytes.fromhex` to confirm byte values.
- Enable verbose logging to inspect encoding steps:
  ```bash
  python txtToHexAndBin.py "Hello" hex --log DEBUG
  ```
- While no formal tests are bundled, quick `pytest` snippets can assert that `text_to_hex` and the inverse conversions round-trip sample strings.

## Implementation Notes
- Defines `ConversionMode` and `Encoding` enums to constrain valid CLI arguments.
- Provides helper functions for hex/binary conversion plus utilities for handling separators and custom encodings.
- Logging configuration centralizes diagnostics for both CLI and import usage.

## Further Reading
- [Unicode Standard, Chapter 2: General Structure](https://www.unicode.org/versions/latest/ch02.pdf)
- [RFC 3629: UTF-8, a transformation format of ISO 10646](https://www.rfc-editor.org/rfc/rfc3629)
