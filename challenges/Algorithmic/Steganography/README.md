# Image Steganography

## Problem Statement
Hide and retrieve UTF-8 text inside the least significant bits of RGB images. The script can embed messages, extract them, or report available capacity while handling escaping for sentinel markers.

## Python CLI Usage
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

## Haskell Companion CLI (`Steg.hs`)
`Steg.hs` mirrors the Python tooling using [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) for image IO and
`aeson` for JSON output. Install the dependencies with Cabal or Stack (for example,
`cabal install --lib JuicyPixels aeson text vector`), then run the script with `runghc`/`stack runghc`:

- Report capacity (text or JSON):
  ```bash
  stack runghc --package JuicyPixels --package aeson --package text --package vector \
    challenges/Algorithmic/Steganography/Steg.hs capacity cover.png --json
  ```
- Hide text from `--message`, `--message-file`, or `--stdin`:
  ```bash
  runghc challenges/Algorithmic/Steganography/Steg.hs hide cover.png secret.png \
    --message "Hello" --json
  ```
- Extract a hidden payload:
  ```bash
  runghc challenges/Algorithmic/Steganography/Steg.hs extract secret.png
  ```
- Analyse cover vs stego pairs (JSON metrics + optional exports used by the Python visualiser):
  ```bash
  runghc challenges/Algorithmic/Steganography/Steg.hs analyse cover.png secret.png \
    --json --metrics-out diff.json --export-mask mask.png --export-overlay overlay.png
  ```

The JSON produced by `analyse` shares the same structure as `steg_visualizer.py`, so the Python visualiser can ingest the
generated metrics, masks, or overlays without additional conversion.

## Visualising Stego Differences
Use `steg_visualizer.py` to inspect how embedding altered the cover image:

- Export JSON metrics describing pixel and bit-plane changes:
  ```bash
  python steg_visualizer.py cover.png secret.png --json diff.json
  ```
- Generate overlay and heatmap assets (runs without requiring a display):
  ```bash
  python steg_visualizer.py cover.png secret.png \
    --export-overlay overlay.png \
    --save-heatmap heatmap.png \
    --save-histogram histogram.png
  ```
- Show the heatmap and histogram interactively (falls back gracefully if
  Matplotlib cannot initialise a GUI backend):
  ```bash
  python steg_visualizer.py cover.png secret.png --show
  ```

The JSON output includes counts of modified pixels, per-channel bit-plane
statistics, and the cover image's embedding capacity (via
`image_capacity_chars`).

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
