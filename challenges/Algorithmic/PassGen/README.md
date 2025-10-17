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
