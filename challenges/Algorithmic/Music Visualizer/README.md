# Music Visualizer

## Problem Statement
Generate short-time Fourier transform (STFT) or mel-spectrogram visualizations for audio files or synthetic tones. The tool loads audio, computes spectral features, and renders them via matplotlib with optional JSON metadata.

## Usage
### Python reference implementation (`mv.py`)
- Visualize a WAV file with default settings:
  ```bash
  python mv.py -f path/to/audio.wav
  ```
- Save a mel-spectrogram without opening a window:
  ```bash
  python mv.py -f song.mp3 --mel --headless --save mel.png
  ```
- Generate a synthetic tone for quick demos:
  ```bash
  python mv.py --synthetic --freq 440 --duration 2.5 --save tone.png
  ```

### Haskell CLI (`MusicViz.hs`)
- Install dependencies (example with `stack`):
  ```bash
  stack install aeson JuicyPixels numeric-fft optparse-applicative text vector
  ```
  Ensure [SoX](http://sox.sourceforge.net/) with libsox is available on your PATH for decoding non-WAV files.
- Run the visualizer directly with `runghc`:
  ```bash
  runghc MusicViz.hs --file path/to/audio.mp3 --mel --save output.png --json stats.json
  ```
- Generate a synthetic tone without reading disk input:
  ```bash
  runghc MusicViz.hs --synthetic --freq 330 --duration 1.5 --save tone.png --json /tmp/tone.json
  ```
- Preview interactively (opens the rendered PNG with `xdg-open`):
  ```bash
  runghc MusicViz.hs --file fur\ elise.mp3 --save elise.png
  ```

Additional `MusicViz.hs` flags include window/hop sizing, mel band count, and headless rendering for CI-friendly workflows. Omit `--headless` to open a preview window, provide `--headless --save <file>` for batch mode, and pass `--json -` to stream metadata to stdout.

## Debugging Tips
- Install the optional dependency before running: `pip install librosa matplotlib`.
- Install Haskell dependencies (`stack install aeson JuicyPixels numeric-fft optparse-applicative text vector`) and ensure the `sox` executable is installed for audio decoding.
- When diagnosing numeric issues, start with `--synthetic` to ensure the pipeline works without external files.
- Execute the regression tests:
  ```bash
  pytest test_mv.py
  ```
  They confirm configuration defaults and JSON payloads without requiring real audio.

## Implementation Notes
- Uses a dataclass (`VisualizerConfig`) to capture CLI options and manage optional mel scaling.
- Gracefully degrades when `librosa` is unavailable, providing informative errors while still allowing JSON-only runs.
- Supports headless rendering, dynamic range controls, and color map customization.

## Further Reading
- [Müller, *Fundamentals of Music Processing*, Chapter 2: Fourier Analysis of Audio Signals](https://link.springer.com/book/10.1007/978-3-319-21945-5)
- [Librosa Documentation: Spectrograms and Mel-scaled Representations](https://librosa.org/doc/latest/feature.html#spectrograms)
