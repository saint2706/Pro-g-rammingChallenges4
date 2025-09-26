from __future__ import annotations

"""Realtime performer for the modular synth engine."""

import argparse
import pathlib
import wave

import numpy as np

from ..sound_synth import (
    DEFAULT_SAMPLE_RATE,
    InstrumentPreset,
    RealtimePerformer,
    create_demo_sequence,
    list_presets,
)

PRESETS_DIR = pathlib.Path(__file__).resolve().parents[1] / "presets"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--preset", default="ambient_pad", help="Preset name (defaults to ambient_pad)")
    parser.add_argument("--midi", help="Optional MIDI input device name", default=None)
    parser.add_argument("--duration", type=float, default=6.0, help="Demo duration for offline fallback")
    parser.add_argument("--output", type=pathlib.Path, default=pathlib.Path("demo_output.wav"))
    parser.add_argument("--sample-rate", type=int, default=DEFAULT_SAMPLE_RATE)
    return parser.parse_args()


def load_preset(name: str) -> InstrumentPreset:
    presets = list_presets(PRESETS_DIR)
    if name not in presets:
        available = ", ".join(sorted(presets)) or "none"
        raise SystemExit(f"Preset '{name}' not found. Available presets: {available}")
    return presets[name]


def offline_render(preset: InstrumentPreset, duration: float, output: pathlib.Path, sample_rate: int) -> None:
    from ..sound_synth import SynthEngine

    engine = SynthEngine(preset, sample_rate)
    sequence = create_demo_sequence()
    audio = engine.render_sequence(sequence, duration)
    pcm = np.clip(audio * 32767.0, -32768, 32767).astype(np.int16)
    with wave.open(str(output), "wb") as wav:
        wav.setnchannels(1)
        wav.setsampwidth(2)
        wav.setframerate(sample_rate)
        wav.writeframes(pcm.tobytes())
    print(
        "Realtime playback unavailable. Rendered offline demo sequence to",
        output,
    )


def main() -> None:
    args = parse_args()
    preset = load_preset(args.preset)
    try:
        with RealtimePerformer(preset, sample_rate=args.sample_rate) as performer:
            midi_port = None
            if args.midi:
                midi_port = performer.open_midi(args.midi)
                print(f"Listening to MIDI input '{args.midi}'...")
            else:
                print("No MIDI device specified. Playing demo pattern (Ctrl+C to quit)...")
            import time

            step = 0.5
            idx = 0
            while True:
                if not args.midi:
                    if idx % 2 == 0:
                        performer.synth.note_on(60 + (idx % 8) * 2, 100)
                    else:
                        performer.synth.note_off(60 + ((idx - 1) % 8) * 2)
                    idx += 1
                    time.sleep(step)
                else:
                    time.sleep(0.1)
    except KeyboardInterrupt:  # pragma: no cover - realtime loop
        print("\nExiting performer")
    except Exception as exc:
        offline_render(preset, args.duration, args.output, args.sample_rate)
        print(f"Reason: {exc}")


if __name__ == "__main__":
    main()
