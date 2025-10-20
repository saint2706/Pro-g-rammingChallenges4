# ROT13 Cipher

## Problem Statement
Apply the ROT13 substitution cipher—a Caesar cipher rotating letters by 13 positions—to encode or decode text from various input sources.

## Usage
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

## Implementation Notes
- Precomputes a translation table for fast transformations using `str.translate`.
- Supports multiple input modes (direct string, file, stdin, interactive prompt) and optional JSON output.
- Includes a dataclass-backed configuration object to validate CLI combinations.

## Further Reading
- [Wikipedia: ROT13](https://en.wikipedia.org/wiki/ROT13)
- [Bruce Schneier, *Applied Cryptography*, Appendix on Classical Ciphers](https://www.schneier.com/books/applied_cryptography/)
