"""Waveform visualisation utility for the sound synthesis engine."""
from __future__ import annotations

import argparse
import pathlib

import numpy as np

from ..sound_synth import (
    DEFAULT_SAMPLE_RATE,
    InstrumentPreset,
    create_demo_sequence,
    list_presets,
    render_waveform_preview,
)

try:
    import matplotlib.pyplot as plt  # type: ignore
except Exception as exc:  # pragma: no cover - optional dependency
    raise SystemExit(
        "matplotlib is required for waveform visualisation. Install it via 'pip install matplotlib'."
    ) from exc


PRESETS_DIR = pathlib.Path(__file__).resolve().parents[1] / "presets"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--preset", default="ambient_pad", help="Preset name (defaults to ambient_pad)")
    parser.add_argument(
        "--output", type=pathlib.Path, default=pathlib.Path("waveform.png"), help="Output PNG path"
    )
    parser.add_argument("--duration", type=float, default=4.0, help="Duration in seconds")
    parser.add_argument("--root-note", type=int, default=60, help="Root MIDI note for demo sequence")
    parser.add_argument(
        "--sample-rate", type=int, default=DEFAULT_SAMPLE_RATE, help="Sample rate for rendering"
    )
    return parser.parse_args()


def load_preset(name: str) -> InstrumentPreset:
    presets = list_presets(PRESETS_DIR)
    if name not in presets:
        available = ", ".join(sorted(presets)) or "none"
        raise SystemExit(f"Preset '{name}' not found. Available presets: {available}")
    return presets[name]


def main() -> None:
    args = parse_args()
    preset = load_preset(args.preset)
    sequence = create_demo_sequence(args.root_note)
    audio, amplitude = render_waveform_preview(preset, sequence, args.duration, args.sample_rate)
    t = np.linspace(0, args.duration, audio.size)

    fig, ax = plt.subplots(2, 1, figsize=(10, 6), sharex=True)
    ax[0].plot(t, audio, linewidth=0.8)
    ax[0].set_title(f"Waveform – {preset.name}")
    ax[0].set_ylabel("Amplitude")

    ax[1].plot(t, amplitude, color="darkorange", linewidth=0.8)
    ax[1].set_title("Envelope Magnitude")
    ax[1].set_ylabel("|Amplitude|")
    ax[1].set_xlabel("Time (s)")

    fig.tight_layout()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.output)
    print(f"Waveform saved to {args.output}")


if __name__ == "__main__":
    main()
