# Password Generator

## Problem Statement
Produce cryptographically strong passwords from configurable character pools, ensuring entropy estimates and policy enforcement (length, required categories, ambiguous character filtering).

## Usage
- Generate a single 16-character password with letters and digits:
  ```bash
  python passgen.py --length 16 --letters --digits
  ```
- Request multiple passwords with symbols and no ambiguous characters:
  ```bash
  python passgen.py --length 24 --count 5 --letters --digits --symbols --no-ambiguous
  ```
- Emit machine-readable output:
  ```bash
  python passgen.py --length 20 --letters --digits --symbols --json
  ```

## Haskell implementation

The repository also ships a standalone Haskell CLI (`PassGen.hs`) that mirrors the
Python feature set while using the `crypto-random` DRG for entropy.

### Build prerequisites

Install the required packages (tested with GHC 9.2+):

```bash
cabal update
cabal install --lib aeson bytestring containers text crypto-random
```

You can now compile once with `ghc` or run ad-hoc via `runghc`:

```bash
ghc -O2 -threaded PassGen.hs
# or
runghc PassGen.hs --help
```

### Running the generator

*Text output*

```bash
./PassGen --length 16 --letters --digits --count 3
```

Example output:

```
Generated Password(s):
  1: h4HjMSWdL8r1nDoF
  2: B6jdYZHoOQwC8x7n
  3: vm1xH3XsUf2EL5sD

Pool size: 62 | Estimated entropy: 95.16 bits
```

*JSON mode*

```bash
runghc PassGen.hs --length 20 --letters --digits --symbols --json
```

Which prints payloads shaped like:

```json
{
  "passwords": ["m$h%..."],
  "length": 20,
  "count": 1,
  "letters": true,
  "digits": true,
  "symbols": true,
  "excluded_ambiguous": false,
  "pool_size": 94,
  "estimated_entropy_bits": 131.0
}
```

### Analytics summary for visualisation

Leverage the built-in summary collector to produce visualiser-friendly data with
consistent CLI flags. The summary honours the `--batches` option (default 1).

```bash
runghc PassGen.hs --length 12 --letters --digits --count 5 --batches 10 --summary --json
```

This emits the aggregated frequencies, probabilities, and entropy metrics used
by `passgen_visualizer.py`:

```json
{
  "spec": {
    "length": 12,
    "count": 5,
    "letters": true,
    "digits": true,
    "symbols": false,
    "exclude_ambiguous": false,
    "min_categories": 2
  },
  "batches": 10,
  "total_passwords": 50,
  "total_characters": 600,
  "pool_size": 62,
  "estimated_entropy_bits": 71.54,
  "baseline_entropy_bits": 78.97,
  "character_frequencies": {"0": 11, "1": 7, "A": 9, ...},
  "character_probabilities": {"0": 0.018333, "1": 0.011667, ...},
  "category_frequencies": {"letters": 468, "digits": 132, "symbols": 0}
}
```

Use `--summary` without `--json` for a formatted textual report of the same
statistics.

### Visual analytics

- Inspect character frequencies and entropy trends across batches while keeping the same CLI flags:
  ```bash
  python passgen_visualizer.py --length 16 --letters --digits --count 5 --batches 10
  ```
- Skip plotting and export just the summary JSON for pipelines or CI checks:
  ```bash
  python passgen_visualizer.py --length 20 --letters --digits --symbols --json --no-plot
  ```

## Debugging Tips
- Entropy values are printed alongside passwords; a 16-character letters+digits password should report roughly 95 bits.
- Run the regression tests:
  ```bash
  pytest test_passgen.py
  ```
  They verify pool construction, entropy calculations, and validation errors.
- When troubleshooting policy failures, adjust `--min-categories` or explicitly pass `--letters/--digits/--symbols` to inspect how the pool changes.

## Implementation Notes
- Backed by the standard-library `secrets` module to avoid predictable randomness.
- Dataclass (`PasswordSpec`) captures CLI options and ensures minimum policy requirements.
- Provides helper functions for entropy calculation and pool assembly, aiding testability.

## Further Reading
- [NIST SP 800-63B: Digital Identity Guidelines – Memorized Secrets](https://pages.nist.gov/800-63-3/sp800-63b.html)
- [Bonneau, "The Quest to Replace Passwords" (IEEE Security & Privacy, 2012)](https://doi.org/10.1109/MSP.2012.85)
