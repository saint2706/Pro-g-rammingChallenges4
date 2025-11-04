# ROT13 Cipher

## Problem Statement
Apply the ROT13 substitution cipher—a Caesar cipher rotating letters by 13 positions—to encode or decode text from various input sources.

## Usage
### Python
- Transform inline text:
  ```bash
  python rot13.py --text "Hello World!"
  ```
- Process a file and save the output:
  ```bash
  python rot13.py --file message.txt --save encoded.txt
  ```
- Use stdin/stdout for pipelines:
  ```bash
  echo "uryyb" | python rot13.py --stdin
  ```
- Emit a JSON report:
  ```bash
  python rot13.py --text "Hello" --json
  ```

### Haskell
- Transform inline text with the matching CLI:
  ```bash
  runghc Rot13.hs --text "Hello World!"
  ```
- Process a file and save the output:
  ```bash
  runghc Rot13.hs --file message.txt --save encoded.txt
  ```
- Use stdin/stdout for pipelines:
  ```bash
  echo "uryyb" | runghc Rot13.hs --stdin
  ```
- Emit the structured JSON summary:
  ```bash
  runghc Rot13.hs --text "Hello" --json
  ```

### Visual exploration
- Compare ROT13's involutive mapping to other substitutions with the shared helper:
  ```bash
  python ../Caesar\ Cipher/cipher_visualizer.py --cipher rot13 --text "uryyb" --output-json rot13.json --pretty
  ```
- Add `--show` to launch the Plotly heatmap or `--output-svg rot13.svg` for static documentation assets.

## Debugging Tips
- ROT13 is involutory; running the script twice on the same text should return the original string.
- Execute the unit tests:
  ```bash
  pytest test_rot13.py
  ```
  They cover ASCII handling, idempotence, and command-line parsing.
- Enable `--json` to compare structured results when debugging text encoding issues.

## How It Works
The Python script `rot13.py` implements the ROT13 cipher by pre-computing a translation table that maps each letter of the alphabet to its rotated counterpart. This is a very efficient approach, as the table is created only once. The `rot13` function then uses this table to transform the input text.

The script provides a command-line interface that can read from a string, a file, or standard input. It can also save the output to a file and supports JSON output for scripting purposes. The `test_rot13.py` file contains unit tests to ensure the implementation is correct.

## Implementation Notes
- Precomputes a translation table for fast transformations using `str.translate`.
- Supports multiple input modes (direct string, file, stdin, interactive prompt) and optional JSON output.
- Includes a dataclass-backed configuration object to validate CLI combinations.

## Further Reading
- [Wikipedia: ROT13](https://en.wikipedia.org/wiki/ROT13)
- [Bruce Schneier, *Applied Cryptography*, Appendix on Classical Ciphers](https://www.schneier.com/books/applied_cryptography/)
