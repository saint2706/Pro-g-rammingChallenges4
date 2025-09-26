# WAV Equalizer

This tool experiments with **real-time equalization** of WAV audio files. The goals are:

* Stream audio from disk with predictable latency while leaving headroom for live adjustments.
* Provide an accessible interface with per-band gain sliders and spectrum feedback so users can hear and see the effect of their changes in real time.
* Ship with a handful of presets that demonstrate common use cases (e.g., speech clarity, bass boost) to serve as starting points for experimentation.

## Features

* Multi-band FFT-based equalization with 10 ISO-style bands.
* Live playback using [`sounddevice`](https://python-sounddevice.readthedocs.io/) so filter changes are audible immediately.
* Tkinter GUI with sliders for each band, preset selector, start/stop controls, and a matplotlib-powered live magnitude spectrum plot.
* Latency safeguards: the application warns when the selected block size would introduce more than 80 ms of buffering so you can adjust settings before playback.

## Getting Started

1. Install dependencies (see repository root `requirements.txt` or the Practical subset) and add `scipy` + `sounddevice` if they are not already present.
2. Run `python equalizer.py path/to/file.wav` from this directory.
3. Adjust the gain sliders or choose a preset while the audio is playing. The spectrum plot updates with the most recent audio block.

## Notes

* WAV input is decoded entirely in memory for simplicity. Large files may use significant RAM.
* The player stops automatically once the end of the file is reached. Press **Start** again to replay.
* Spectrum drawing is throttled to keep UI responsive; expect ~15 frames per second.
* On Linux you may need to install the PortAudio development headers before installing `sounddevice` (`sudo apt install libportaudio2`).
