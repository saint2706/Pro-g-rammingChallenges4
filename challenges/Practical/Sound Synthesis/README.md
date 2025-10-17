# Sound Synthesis Challenge

This solution provides a modular software synthesizer focused on live performance and scripting flexibility.
It includes several oscillator shapes, programmable envelopes, filtering, and optional real-time audio + MIDI
playback.

## Features

- **Oscillators:** Sine, square, sawtooth, and white noise oscillators with sample-accurate rendering.
- **ADSR envelope:** Per-voice amplitude shaping with attack, decay, sustain, and release stages.
- **Filters:** One-pole low-pass and high-pass filters for tone control.
- **Realtime audio:** Callback-based streaming using [`sounddevice`](https://python-sounddevice.readthedocs.io/)
  when the dependency and audio hardware are available.
- **MIDI input:** Live performance using [`mido`](https://mido.readthedocs.io/) with `python-rtmidi` backend.
- **Instrument presets:** JSON-formatted patches describing oscillator, envelope, and filter settings.
- **Waveform visualization:** Generate PNG waveforms from any preset to preview tone envelopes.
- **Offline rendering:** Render preset performances to WAV buffers for environments without realtime output.

## Repository Layout

```
challenges/Practical/Sound Synthesis/
├── README.md              ← this guide
├── presets/
│   ├── ambient_pad.json   ← sample patch with slow evolving envelope
│   └── bass_lead.json     ← punchy bass/lead hybrid
├── scripts/
│   ├── visualize.py       ← render waveform PNGs for a preset + note pattern
│   └── perform.py         ← realtime playback & MIDI bridge
├── sound_synth.py         ← core synthesis engine (oscillators, envelopes, filters, voices)
└── tests/
    └── test_sound_synth.py← automated coverage for oscillators/envelopes/filters/presets
```

## Goals and Design Notes

1. **Oscillator coverage:** Provide classic subtractive synthesis waveforms that can be layered freely.
2. **MIDI-first workflow:** The real-time engine is designed to respond to `NoteOn`/`NoteOff` and control changes
   with minimal latency. MIDI is optional so the engine remains functional without hardware.
3. **Scriptable presets:** Patches are JSON documents so they can be generated/edited from any language or tool.
4. **Visualization:** Visual previews of amplitude over time make it easy to iterate on envelopes and filters.

## Getting Started

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
```

### Running the Realtime Performer

```bash
python challenges/Practical/Sound\ Synthesis/scripts/perform.py --preset ambient_pad --midi "Your MIDI Device"
```

- Omit `--midi` to play a scripted demo phrase through the default output.
- The performer degrades gracefully when optional dependencies (`sounddevice`, `mido`, `python-rtmidi`) are missing.

### Rendering and Visualising Waveforms

```bash
python challenges/Practical/Sound\ Synthesis/scripts/visualize.py --preset bass_lead --output bass_lead.png
```

This saves the amplitude envelope + raw waveform plot to the provided PNG path.

## Tests

```bash
pytest challenges/Practical/Sound\ Synthesis/tests
```

Tests cover oscillator waveforms, ADSR envelope stage handling, filter frequency response sanity checks,
JSON preset parsing, and offline rendering heuristics.

## Dependencies

Core functionality relies on the Python standard library and `numpy`. Optional features require:

- `sounddevice` for low-latency audio streaming.
- `mido` with `python-rtmidi` backend for MIDI input.
- `matplotlib` for waveform visualization.

All dependencies (mandatory + optional) are listed in the repository `requirements.txt` file.

## Challenge Status

The Sound Synthesis challenge is **solved** with realtime MIDI playback, customizable presets,
and visualization tooling.
