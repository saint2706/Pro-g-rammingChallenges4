# MIDI Player + Editor

A combined command-line MIDI playback and editing toolkit. Load a `.mid` file, audition tracks in real time, tweak the arrangement, and export a fresh file for your DAW or hardware sequencer.

## Project Goals

### Playback
- ✅ Load Standard MIDI Files (type 0 or 1) via [`mido`](https://mido.readthedocs.io/).
- ✅ Stream events to any available RtMidi-compatible output port (hardware synth, loopMIDI, DAW).
- ✅ Provide transport-style controls: play, pause/resume, stop, and time seeking.
- ✅ Respect tempo map changes and honor per-track mute toggles during playback.

### Editing
- ✅ Toggle track mute state and optionally bounce muted tracks to silence on export.
- ✅ Apply global tempo scaling (double-time/half-time style) by rewriting tempo meta events.
- ✅ Insert new notes (configurable pitch, velocity, duration, channel) at specific tick offsets.
- ✅ Remove notes by pitch (first matching instance after a tick offset) without disturbing surrounding timing.

### Export & Workflow
- ✅ Export the in-memory arrangement to a new `.mid` file, preserving metadata and updated edits.
- ✅ Offer a lightweight CLI workflow so you can script batch edits or combine with other tooling.
- ✅ Ship unit tests that verify structural edits (tempo changes, note insertion/removal, mute rendering).

## Quick Start

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r ../../requirements.txt  # or Practical/requirements.txt
python "midi_tool.py" --help
```

Common invocations:

```bash
# Audition a file on the default MIDI out
python "midi_tool.py" demo.mid play

# Mute track 1, double the tempo, insert a pickup note, then export
python "midi_tool.py" demo.mid mute 1 \
    tempo --scale 0.5 \
    insert-note 0 --note 76 --tick 120 --duration 240 --velocity 80 \
    export remixed.mid
```

> Tip: Chain multiple subcommands in one invocation; the CLI applies them sequentially against an in-memory project.

## Module Overview

- `midi_tool.py` &mdash; Core module containing:
  - `MIDIProject`: editing façade around `mido.MidiFile` with helpers for muting, tempo scaling, and note operations.
  - `MIDIPlayer`: threaded transport built on RtMidi output for real-time playback with pause/resume/seek.
  - CLI entry point wiring subcommands to the above classes.
- `tests/test_midi_tool.py` &mdash; Pytest suite covering editing behaviors and export integrity.

## Usage Examples

### Inspecting Tempo and Tracks
```bash
python "midi_tool.py" demo.mid info
```
Outputs track names, lengths, and detected tempo markers.

### Rehearsal Loop
```bash
python "midi_tool.py" demo.mid play --seek 30 --duration 8
```
Seeks to 30 seconds, plays 8 seconds, then stops.

### Batch Silence of Support Tracks
```bash
python "midi_tool.py" backing.mid mute 2 mute 3 mute 4 export backing_sparse.mid
```
Mutes three accompaniment tracks and exports a practice version.

## Testing

```bash
pytest Practical/"MIDI Player Editor"/tests -q
```

The tests synthesize miniature MIDI files and ensure exports reflect tempo scaling, note insertion/removal, and mute rendering.

## Future Ideas

- Piano-roll style Tkinter editor
- Quantization helpers
- Key/scale aware note insertion
- CLI session presets (JSON/YAML)

