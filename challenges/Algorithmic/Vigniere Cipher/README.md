# Vigenère Cipher

## Problem Statement
Encrypt or decrypt alphabetic text using the classical Vigenère cipher, where each plaintext letter is shifted by the alphabetic position of a repeating key.

## Usage
- Encrypt a message with key `LEMON`:
  ```bash
  python vig.py encrypt LEMON -t "Attack at dawn"
  ```
- Decrypt from a file and write plaintext to another:
  ```bash
  python vig.py decrypt secret --in ciphertext.txt --out plain.txt
  ```
- Use stdin/stdout pipelines with uppercase normalization:
  ```bash
  echo "Hello World" | python vig.py encrypt key --upper
  ```

## Debugging Tips
- Sanitized keys must contain alphabetic characters only; keys like `abc123` will be reduced to `abc`.
- ROT13 is the special case `key = N`; test with `--text "hello" --key N` to verify round-trip consistency.
- Automated tests not included in this folder—consider creating quick assertions by importing `vigenere_cipher` and checking `decrypt(encrypt(text, key), key) == text`.

## Implementation Notes
- CLI builds on dataclass-configured options and supports files, stdin, or direct `-t/--text` input.
- Non-letter characters are passed through unchanged and do not consume key characters, matching the classic autokeyless convention.
- Optional JSON output captures mode, key, and transformed text for integration with other tools.

## Further Reading
- [Kahn, *The Codebreakers*, Chapter on Polyalphabetic Ciphers](https://www.simonandschuster.com/books/The-Codebreakers/David-Kahn/9780684831305)
- [Wikipedia: Vigenère Cipher](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher)
