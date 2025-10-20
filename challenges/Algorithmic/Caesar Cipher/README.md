# Caesar Cipher

## Problem Statement
Encrypt, decrypt, and analyze substitution ciphers where each alphabetic character is shifted by a fixed offset. The tool supports classical Caesar cipher experimentation, brute-force cracking, and basic frequency analysis.

## Usage
- Encrypt or decrypt from the CLI:
  ```bash
  python caesar.py encrypt "Hello World" 3
  python caesar.py decrypt "Khoor Zruog" 3
  ```
- Attempt to crack an unknown shift via frequency analysis:
  ```bash
  python caesar.py crack "Wklv lv d vhfuhw"
  ```
- Launch the interactive helper:
  ```bash
  python caesar.py --interactive
  ```

### Visual exploration
- Generate substitution maps, frequency charts, and optional Plotly outputs with the shared visualizer:
  ```bash
  python cipher_visualizer.py --cipher caesar --shift 5 --text "Meet me at the park" --output-json caesar.json --output-html caesar.html
  ```
- Add `--show` for an interactive window, or `--no-plot --pretty` to print just the JSON data when running headless (e.g., in CI).

## Debugging Tips
- Known plaintext pairs are great smoke tests. For example, encrypting `abc` with shift `1` should produce `bcd`.
- Run the script with `--verbose` (logging set to `INFO`) to see alphabet normalization and scoring decisions.
- No automated tests ship with this module, but you can add `pytest`-style cases by asserting round trips: `decrypt(encrypt(text, k), k) == text`.

## Implementation Notes
- Supports multiple alphabet presets (letters, alphanumeric, printable) and normalizes case internally.
- Includes brute-force enumeration and Chi-squared scoring to rank candidate plaintexts.
- Designed for both command-line usage and import as a library module.

## Further Reading
- [Wikipedia: Caesar cipher](https://en.wikipedia.org/wiki/Caesar_cipher)
- [Katz & Lindell, *Introduction to Modern Cryptography* – Chapter on Classical Ciphers](https://doi.org/10.1201/9781315311140)
