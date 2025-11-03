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

### Haskell CLI parity

The repository also ships a Haskell reimplementation of the converter with the
same focus on reversible conversions and visualisation-friendly output.

1. Build the executable using `ghc` (installed with the Glasgow Haskell Compiler):
   ```bash
   cd "challenges/Algorithmic/basic text encoding"
   ghc -O2 Encoding.hs -o encoding
   ```
   Alternatively, run it without compiling:
   ```bash
   runghc "challenges/Algorithmic/basic text encoding/Encoding.hs" --help
   ```
2. Run commands mirroring the Python toolset:
   ```bash
   ./encoding --mode hex --text "Hello" --encoding utf-8
   ./encoding --mode bin --text "Hello" --encoding utf-16
   ./encoding --mode hex2text --text "48 65 6c 6c 6f" --encoding ascii
   ./encoding --interactive
   ```
3. Produce structured summaries (matching the JSON schema produced by
   `encoding_visualizer.py`) or terminal visualisations:
   ```bash
   ./encoding --mode hex --text "Hello" --json -
   ./encoding --mode bin --text "Hello" --visualize
   ```

The Haskell CLI exposes the same conversion modes (`hex`, `bin`, `hex2text`,
`bin2text`), respects the encoding options (`utf-8`, `utf-16`, `utf-16le`,
`utf-16be`, `ascii`), supports interactive prompts, and emits JSON summaries with
ASCII comparisons to stay feature-aligned with the Python utilities.

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
