# Image Steganography

## Problem Statement
Hide and retrieve UTF-8 text inside the least significant bits of RGB images. The script can embed messages, extract them, or report available capacity while handling escaping for sentinel markers.

## Usage
- Report embedding capacity of an image:
  ```bash
  python steg.py capacity cover.png
  ```
- Hide a message and save the stego image:
  ```bash
  python steg.py hide cover.png secret.png --message "Hello" --json
  ```
- Extract a hidden message:
  ```bash
  python steg.py extract secret.png
  ```

## Debugging Tips
- Capacity is roughly `(width * height * 3) // 8` characters; ensure the payload fits before calling `hide`.
- Run automated tests:
  ```bash
  pytest test_steg.py
  ```
  They cover escaping, end-of-message detection, and capacity calculations.
- Use `--verbose` to log byte counts and confirm the escape marker is appended and removed properly.

## Implementation Notes
- Uses Pillow (`PIL.Image`) for pixel manipulation and stores one bit per color channel for clarity.
- Dataclass configurations encapsulate hide/extract settings and perform validation prior to modifying images.
- Escaping ensures the chosen end-of-message marker does not collide with user data.

## Further Reading
- [Fridrich, "A New Steganographic Method for Palette-Based Images" (IS&T/SPIE, 1998)](https://doi.org/10.1117/12.306948)
- [Johnson & Jajodia, "Exploring Steganography: Seeing the Unseen" (IEEE Computer, 1998)](https://doi.org/10.1109/2.668972)
