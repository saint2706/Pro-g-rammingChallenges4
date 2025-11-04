"""Modular sound synthesis engine with MIDI-aware realtime playback support."""

from __future__ import annotations

import json
import math
import pathlib
from dataclasses import dataclass, field
from typing import Dict, Iterable, List, Optional, Tuple

import numpy as np

DEFAULT_SAMPLE_RATE = 44100
TWOPI = 2.0 * math.pi


def midi_to_frequency(note: float) -> float:
    """Convert a MIDI note number to frequency in Hz."""
    return 440.0 * (2.0 ** ((note - 69.0) / 12.0))


@dataclass
class OscillatorConfig:
    type: str
    detune_cents: float = 0.0
    gain: float = 1.0


@dataclass
class EnvelopeConfig:
    attack: float = 0.01
    decay: float = 0.1
    sustain: float = 0.8
    release: float = 0.2


@dataclass
class FilterConfig:
    mode: str = "lowpass"
    cutoff: float = 1000.0
    resonance: float = 0.0


@dataclass
class InstrumentPreset:
    name: str
    oscillators: List[OscillatorConfig] = field(default_factory=list)
    envelope: EnvelopeConfig = field(default_factory=EnvelopeConfig)
    filter: FilterConfig = field(default_factory=FilterConfig)

    @classmethod
    def from_dict(cls, data: Dict) -> "InstrumentPreset":
        return cls(
            name=data.get("name", "Untitled"),
            oscillators=[
                OscillatorConfig(**osc) for osc in data.get("oscillators", [])
            ],
            envelope=EnvelopeConfig(**data.get("envelope", {})),
            filter=FilterConfig(**data.get("filter", {})),
        )

    @classmethod
    def load(cls, path: pathlib.Path) -> "InstrumentPreset":
        return cls.from_dict(json.loads(path.read_text()))


class ADSREnvelope:
    """Linear ADSR envelope generator."""

    def __init__(
        self, config: EnvelopeConfig, sample_rate: int = DEFAULT_SAMPLE_RATE
    ) -> None:
        self.config = config
        self.sample_rate = sample_rate
        self.state = "idle"
        self._level = 0.0
        self._samples_in_state = 0

    def _seconds_to_samples(self, seconds: float) -> int:
        return max(1, int(seconds * self.sample_rate))

    def note_on(self) -> None:
        self.state = "attack"
        self._samples_in_state = 0

    def note_off(self) -> None:
        if self.state != "idle":
            self.state = "release"
            self._samples_in_state = 0

    def is_finished(self) -> bool:
        return self.state == "idle"

    def render(self, num_samples: int) -> np.ndarray:
        envelope = np.zeros(num_samples, dtype=np.float32)
        for i in range(num_samples):
            if self.state == "attack":
                attack_samples = self._seconds_to_samples(self.config.attack)
                self._level += 1.0 / attack_samples
                if self._level >= 1.0 or self._samples_in_state >= attack_samples:
                    self.state = "decay"
                    self._samples_in_state = 0
                    self._level = 1.0
            elif self.state == "decay":
                decay_samples = self._seconds_to_samples(self.config.decay)
                target = self.config.sustain
                self._level += (target - self._level) / decay_samples
                if self._samples_in_state >= decay_samples:
                    self.state = "sustain"
                    self._samples_in_state = 0
                    self._level = target
            elif self.state == "sustain":
                self._level = self.config.sustain
            elif self.state == "release":
                release_samples = self._seconds_to_samples(self.config.release)
                self._level += (0.0 - self._level) / release_samples
                if self._samples_in_state >= release_samples or self._level <= 0.0001:
                    self.state = "idle"
                    self._level = 0.0
                    self._samples_in_state = 0
            envelope[i] = max(0.0, min(self._level, 1.0))
            if self.state != "idle":
                self._samples_in_state += 1
        return envelope


class Oscillator:
    """Oscillator supporting multiple waveform types."""

    def __init__(
        self, config: OscillatorConfig, sample_rate: int = DEFAULT_SAMPLE_RATE
    ) -> None:
        self.config = config
        self.sample_rate = sample_rate
        self.phase = 0.0

    def _increment(self, frequency: float) -> float:
        detuned = frequency * (2.0 ** (self.config.detune_cents / 1200.0))
        return TWOPI * detuned / self.sample_rate

    def render(self, frequency: float, num_samples: int) -> np.ndarray:
        phase_increment = self._increment(frequency)
        phases = self.phase + phase_increment * np.arange(num_samples, dtype=np.float32)
        self.phase = float((phases[-1] + phase_increment) % TWOPI)
        osc_type = self.config.type.lower()
        if osc_type == "sine":
            waveform = np.sin(phases)
        elif osc_type == "square":
            waveform = np.sign(np.sin(phases))
        elif osc_type in {"saw", "sawtooth"}:
            waveform = 2.0 * ((phases / TWOPI) % 1.0) - 1.0
        elif osc_type == "noise":
            waveform = np.random.uniform(-1.0, 1.0, size=num_samples)
        else:
            raise ValueError(f"Unsupported oscillator type: {self.config.type}")
        return waveform.astype(np.float32) * float(self.config.gain)


class OnePoleFilter:
    """Simple one-pole filter for tone shaping."""

    def __init__(
        self, config: FilterConfig, sample_rate: int = DEFAULT_SAMPLE_RATE
    ) -> None:
        self.config = config
        self.sample_rate = sample_rate
        self._prev = 0.0

    def _compute_coefficient(self, cutoff: float) -> float:
        cutoff = max(5.0, min(cutoff, self.sample_rate / 2.0 - 100.0))
        x = math.exp(-TWOPI * cutoff / self.sample_rate)
        return x

    def process(self, signal: np.ndarray) -> np.ndarray:
        coeff = self._compute_coefficient(self.config.cutoff)
        output = np.zeros_like(signal)
        mode = self.config.mode.lower()
        for i, sample in enumerate(signal):
            self._prev = (1 - coeff) * sample + coeff * self._prev
            if mode == "lowpass":
                output[i] = self._prev
            elif mode == "highpass":
                output[i] = sample - self._prev
            else:
                output[i] = sample
        return output.astype(np.float32)


class SynthVoice:
    def __init__(
        self,
        preset: InstrumentPreset,
        note: int,
        velocity: int,
        sample_rate: int = DEFAULT_SAMPLE_RATE,
    ):
        self.preset = preset
        self.note = note
        self.velocity = velocity
        self.sample_rate = sample_rate
        self.oscillators = [Oscillator(osc, sample_rate) for osc in preset.oscillators]
        self.envelope = ADSREnvelope(preset.envelope, sample_rate)
        self.envelope.note_on()
        self.filter = OnePoleFilter(preset.filter, sample_rate)

    def render(self, num_samples: int) -> np.ndarray:
        if not self.oscillators:
            return np.zeros(num_samples, dtype=np.float32)
        freq = midi_to_frequency(self.note)
        samples = sum(osc.render(freq, num_samples) for osc in self.oscillators)
        samples /= max(len(self.oscillators), 1)
        envelope = self.envelope.render(num_samples)
        signal = samples * envelope * (self.velocity / 127.0)
        return self.filter.process(signal)

    def note_off(self) -> None:
        self.envelope.note_off()

    def is_finished(self) -> bool:
        return self.envelope.is_finished()


class SynthEngine:
    """Polyphonic synth engine that supports realtime streaming and offline rendering."""

    def __init__(
        self, preset: InstrumentPreset, sample_rate: int = DEFAULT_SAMPLE_RATE
    ) -> None:
        self.sample_rate = sample_rate
        self.preset = preset
        self.active_voices: Dict[int, SynthVoice] = {}

    def note_on(self, note: int, velocity: int = 100) -> None:
        self.active_voices[note] = SynthVoice(
            self.preset, note, velocity, self.sample_rate
        )

    def note_off(self, note: int) -> None:
        voice = self.active_voices.get(note)
        if voice:
            voice.note_off()

    def render(self, num_samples: int) -> np.ndarray:
        buffer = np.zeros(num_samples, dtype=np.float32)
        finished_notes = []
        for note, voice in self.active_voices.items():
            buffer += voice.render(num_samples)
            if voice.is_finished():
                finished_notes.append(note)
        for note in finished_notes:
            del self.active_voices[note]
        buffer = np.clip(buffer, -1.0, 1.0)
        return buffer

    def render_sequence(
        self,
        events: Iterable[Tuple[float, str, int, int]],
        duration: float,
    ) -> np.ndarray:
        """Render an event list into an audio buffer.

        Args:
            events: Iterable of (time_seconds, event_type, note, velocity)
            duration: total output duration in seconds
        """
        buffer = np.zeros(int(duration * self.sample_rate), dtype=np.float32)
        events_by_sample: Dict[int, List[Tuple[str, int, int]]] = {}
        for time_s, event_type, note, velocity in events:
            index = min(len(buffer) - 1, max(0, int(time_s * self.sample_rate)))
            events_by_sample.setdefault(index, []).append((event_type, note, velocity))
        cursor = 0
        event_positions = sorted(events_by_sample)
        while cursor < len(buffer):
            next_event = event_positions[0] if event_positions else len(buffer)
            chunk_end = min(len(buffer), next_event)
            if chunk_end > cursor:
                chunk = self.render(chunk_end - cursor)
                buffer[cursor:chunk_end] = chunk
                cursor = chunk_end
            else:
                cursor = next_event
            if event_positions and cursor == event_positions[0]:
                index = event_positions.pop(0)
                for event_type, note, velocity in events_by_sample.get(index, []):
                    if event_type.lower() == "note_on" and velocity > 0:
                        self.note_on(note, velocity)
                    else:
                        self.note_off(note)
        return buffer


def list_presets(directory: pathlib.Path) -> Dict[str, InstrumentPreset]:
    presets = {}
    for path in directory.glob("*.json"):
        preset = InstrumentPreset.load(path)
        key = path.stem
        presets[key] = preset
    return presets


class OptionalImports:
    """Lazy import helpers for optional dependencies."""

    def __init__(self) -> None:
        self._sounddevice = None
        self._mido = None

    @property
    def sounddevice(self):
        if self._sounddevice is None:
            try:
                import sounddevice  # type: ignore

                self._sounddevice = sounddevice
            except Exception:  # pragma: no cover - optional
                self._sounddevice = False
        return self._sounddevice

    @property
    def mido(self):
        if self._mido is None:
            try:
                import mido  # type: ignore

                self._mido = mido
            except Exception:  # pragma: no cover - optional
                self._mido = False
        return self._mido


OPTIONAL_IMPORTS = OptionalImports()


class RealtimePerformer:
    """Realtime synth performer bridging MIDI and audio output."""

    def __init__(
        self,
        preset: InstrumentPreset,
        sample_rate: int = DEFAULT_SAMPLE_RATE,
        block_size: int = 256,
    ) -> None:
        self.synth = SynthEngine(preset, sample_rate)
        self.sample_rate = sample_rate
        self.block_size = block_size
        self.sd = OPTIONAL_IMPORTS.sounddevice
        self.mido = OPTIONAL_IMPORTS.mido
        if not self.sd:
            raise RuntimeError("sounddevice is required for realtime playback")
        self.stream = self.sd.OutputStream(
            channels=1,
            samplerate=sample_rate,
            blocksize=block_size,
            dtype="float32",
            callback=self._callback,
        )

    def _callback(
        self, outdata, frames, _time, _status
    ) -> None:  # pragma: no cover - realtime
        chunk = self.synth.render(frames)
        outdata[:, 0] = chunk

    def open_midi(
        self, device_name: Optional[str] = None
    ):  # pragma: no cover - realtime
        if not self.mido:
            raise RuntimeError("mido + python-rtmidi are required for MIDI input")
        if device_name:
            port = self.mido.open_input(device_name)
        else:
            port_names = self.mido.get_input_names()
            if not port_names:
                raise RuntimeError("No MIDI input devices detected")
            port = self.mido.open_input(port_names[0])
        port.callback = self._on_midi_message
        return port

    def _on_midi_message(self, message):  # pragma: no cover - realtime
        if message.type == "note_on":
            if message.velocity > 0:
                self.synth.note_on(message.note, message.velocity)
            else:
                self.synth.note_off(message.note)
        elif message.type == "note_off":
            self.synth.note_off(message.note)

    def __enter__(self):  # pragma: no cover - realtime
        self.stream.start()
        return self

    def __exit__(self, exc_type, exc, tb):  # pragma: no cover - realtime
        self.stream.stop()
        self.stream.close()


def render_waveform_preview(
    preset: InstrumentPreset,
    sequence: Iterable[Tuple[float, str, int, int]],
    duration: float,
    sample_rate: int = DEFAULT_SAMPLE_RATE,
) -> Tuple[np.ndarray, np.ndarray]:
    """Render a waveform and amplitude envelope for visualisation purposes."""
    synth = SynthEngine(preset, sample_rate)
    audio = synth.render_sequence(sequence, duration)
    amplitude = np.abs(audio)
    return audio, amplitude


def create_demo_sequence(root_note: int = 60) -> List[Tuple[float, str, int, int]]:
    """Generate a simple arpeggiated demo useful for tests and screenshots."""
    pattern = [0, 4, 7, 12]
    events: List[Tuple[float, str, int, int]] = []
    step = 0.5
    time = 0.0
    for idx, interval in enumerate(pattern * 2):
        note = root_note + interval
        events.append((time, "note_on", note, 100))
        events.append((time + step * 0.8, "note_off", note, 0))
        time += step
    return events
