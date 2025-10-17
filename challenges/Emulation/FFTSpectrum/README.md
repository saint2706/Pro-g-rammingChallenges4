# Real-Time FFT Spectrum Visualizer

A lightweight spectrum analyzer that listens to a microphone (via [sounddevice](https://python-sounddevice.readthedocs.io/en/latest/)) or streams audio blocks from a WAV file and renders a real-time FFT plot in matplotlib. Window size, sample rate, refresh cadence, and scaling are configurable at runtime so you can tune fidelity vs latency.

## Hardware Requirements

- **Microphone input**: Any ALSA/CoreAudio/WASAPI-compatible device. USB condensers and laptop microphones work out of the box; multi-channel interfaces are mixed down to mono.
- **CPU**: Modern dual-core processor recommended. FFTs on 2048-sample windows execute comfortably on low-voltage CPUs, but extremely small windows at high refresh rates increase CPU load.
- **GPU/Display**: Matplotlib updates run on the CPU but require an active display server (X11/Wayland/macOS/Windows). Headless servers should use the WAV replay mode or set a non-interactive backend.

## Latency Considerations

- **Window size** controls temporal resolution. Smaller windows (~512 samples) lower latency but smear frequency resolution. Larger windows (>4096) stabilize the spectrum but add up to ~100 ms delay at 44.1 kHz.
- **Refresh rate** dictates how often the plot redraws. High FPS (>60) increases CPU/GPU usage; 20–30 FPS provides smooth motion without spikes.
- **Audio backend buffers** (sounddevice/PortAudio) add their own latency. Use the `--device` flag to select low-latency hardware and experiment with `--window-size` to balance responsiveness.

## Installation

Install from the repository root using the optional extras that bundle visualization and audio tooling:

```bash
python -m pip install -e .[visual]
```

The `visual` extra now pulls in `numpy`, `matplotlib`, `imageio`, `colour-science`, `opencv-python`, and `sounddevice`, covering this demo. If you prefer a lean install for CLI testing only, `numpy` and `matplotlib` are the hard requirements.

## Usage

```bash
python challenges/Emulation/FFTSpectrum/fft_spectrum.py \
    --source mic \
    --samplerate 48000 \
    --window-size 2048 \
    --scale log
```

Switch to a WAV file input:

```bash
python challenges/Emulation/FFTSpectrum/fft_spectrum.py \
    --source wav \
    --wav-path challenges/Emulation/FFTSpectrum/test_tone.wav \
    --window-size 4096 \
    --scale linear \
    --loop
```

### Useful Flags

- `--window` selects Hann (default), Hamming, or rectangular windows.
- `--refresh` adjusts redraw rate (frames per second).
- `--device` passes an explicit PortAudio device string/index when multiple microphones are connected.
- `--loop` replays a WAV file continuously; omit to stop at EOF.

## Testing Without a Microphone

The repository ships a `test_tone.wav` sine wave sample. Automated tests consume this file to verify FFT shape and magnitude, ensuring the analyzer works even in headless CI environments.

## Known Limitations

- 24-bit WAV files are not supported yet. Convert to 16-bit PCM if necessary.
- The matplotlib window must stay in the foreground on some window managers to continue refreshing.
- Microphone mode depends on the `sounddevice` package; if it's missing, the script fails with a clear error and you can still analyze WAV files.

## Challenge Status

This implementation completes `/g/` Challenge **#95 – Real-Time Fast Fourier Transform Spectrum Visualizer**.
