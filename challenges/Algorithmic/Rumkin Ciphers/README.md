# Rumkin Ciphers

## Problem Statement
Experiment with classical substitution ciphers popularized on rumkin.com, including the affine cipher and Atbash substitution. Tools support encryption, decryption, brute-force analysis, and JSON export.

## Usage
- Encrypt with the affine cipher:
  ```bash
  python affine.py --mode encrypt --text "hello" --a 5 --b 8
  ```
- Decrypt using Atbash (involution):
  ```bash
  python atbash.py --text "svool"
  ```
- Enumerate all affine decryptions when the key is unknown:
  ```bash
  python affine.py --mode brute-force --text "ZOLSS"
  ```

### Haskell command-line tools

Compile the new Haskell executables with `ghc` (installed with the Glasgow Haskell Compiler):

```bash
cd challenges/Algorithmic/Rumkin\ Ciphers
ghc -o Affine Affine.hs
ghc -o Atbash Atbash.hs
```

- Encrypt using the affine implementation:
  ```bash
  ./Affine --mode encrypt --text "HELLO" --a 5 --b 8
  ```
- Decrypt (or encrypt—the mapping is symmetric) with Atbash:
  ```bash
  ./Atbash --mode decrypt --text "SVOOL"
  ```
- Enumerate every possible affine decryption and emit JSON for tooling:
  ```bash
  ./Affine --mode brute-force --text "ZOLSS" --json > affine_bruteforce.json
  ```
- JSON summaries are also available for single-key runs:
  ```bash
  ./Affine --mode decrypt --text "ZOLSS" --a 5 --b 8 --json
  ./Atbash --mode encrypt --text "HELLO" --json
  ```

The affine executable validates the `--a` multiplier against the standard coprime set `{1,3,5,7,9,11,15,17,19,21,23,25}` and refuses `--mode brute-force` when a key is supplied. Both programs accept either `--text` or `--stdin` for input.

### Visual exploration
- Inspect affine or Atbash mappings alongside frequency analysis:
  ```bash
  python ../Caesar\ Cipher/cipher_visualizer.py --cipher affine --mode decrypt --text "ZOLSS" --a 5 --b 8 --bruteforce --max-bruteforce 5 --output-json affine.json
  ```
- Swap `--cipher atbash` for the symmetric mapping, or add `--output-html affine.html` to review the generated Plotly heatmap offline.

To pair the Haskell binaries with the existing visualizer, direct their JSON to a file (or pipe) and point the tool's `--input-json` flag at the result. For example:

```bash
./Affine --mode brute-force --text "ZOLSS" --json > affine.json
python ../Caesar\ Cipher/cipher_visualizer.py --cipher affine --input-json affine.json
```

Likewise, Atbash output can be piped to the visualizer for quick comparison of the mirrored alphabet:

```bash
./Atbash --mode encrypt --text "cipher" --json > atbash.json
python ../Caesar\ Cipher/cipher_visualizer.py --cipher atbash --input-json atbash.json
```

## Debugging Tips
- Affine cipher requires `a` to be coprime with 26; values such as 2 or 13 should trigger validation errors.
- Run the tests to validate functionality:
  ```bash
  pytest test_affine.py test_atbash.py
  ```
- Use `--json` output to inspect transformed text and metadata when diagnosing encoding issues.

## Implementation Notes
- Both scripts rely on dataclass-backed configurations and shared helper routines for I/O normalisation.
  - `affine.py` computes modular inverses and provides brute-force key search.
  - `atbash.py` precomputes translation tables for fast symmetric substitution.
- Designed to work with stdin, files, or inline text for flexible workflows.

## Further Reading
- [Affine Cipher (Wikipedia)](https://en.wikipedia.org/wiki/Affine_cipher)
- [Atbash Cipher (Encyclopedia of Cryptography and Security)](https://doi.org/10.1007/978-1-4419-5906-5_20)
