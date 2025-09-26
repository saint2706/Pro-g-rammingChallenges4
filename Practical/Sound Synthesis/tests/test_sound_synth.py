import importlib.util
import pathlib

import numpy as np

MODULE_PATH = pathlib.Path(__file__).resolve().parents[1] / "sound_synth.py"
spec = importlib.util.spec_from_file_location("sound_synth", MODULE_PATH)
synth = importlib.util.module_from_spec(spec)
import sys
sys.modules.setdefault('sound_synth', synth)
assert spec.loader is not None
spec.loader.exec_module(synth)  # type: ignore[attr-defined]

PRESET_DIR = pathlib.Path(__file__).resolve().parents[1] / "presets"


def test_midi_to_frequency_a440():
    assert np.isclose(synth.midi_to_frequency(69), 440.0)


def test_oscillator_shapes():
    sine = synth.Oscillator(synth.OscillatorConfig(type="sine"))
    square = synth.Oscillator(synth.OscillatorConfig(type="square"))
    saw = synth.Oscillator(synth.OscillatorConfig(type="saw"))

    freq = 440
    num_samples = 1024
    sine_wave = sine.render(freq, num_samples)
    square_wave = square.render(freq, num_samples)
    saw_wave = saw.render(freq, num_samples)

    assert np.all(np.abs(sine_wave) <= 1.0)
    assert set(np.unique(square_wave)).issuperset({-1.0, 1.0})
    assert np.isclose(np.mean(saw_wave), 0.0, atol=1e-1)


def test_noise_oscillator_statistics():
    noise = synth.Oscillator(synth.OscillatorConfig(type="noise"))
    samples = noise.render(0, 10_000)
    assert np.isclose(np.mean(samples), 0.0, atol=0.1)
    assert samples.std() > 0.3


def test_adsr_envelope_stages():
    env = synth.ADSREnvelope(synth.EnvelopeConfig(attack=0.01, decay=0.01, sustain=0.5, release=0.01))
    env.note_on()
    attack = env.render(100)
    assert attack.max() <= 1.0
    env.note_off()
    release = env.render(500)
    assert release[-1] < 0.05


def test_filter_lowpass_reduces_high_freq():
    config = synth.FilterConfig(mode="lowpass", cutoff=500.0)
    filt = synth.OnePoleFilter(config)
    t = np.arange(0, 1.0, 1 / synth.DEFAULT_SAMPLE_RATE)
    high = np.sin(2 * np.pi * 4000 * t[:2000]).astype(np.float32)
    filtered = filt.process(high)
    assert filtered.std() < high.std()


def test_render_sequence_uses_events():
    preset = synth.InstrumentPreset(
        name="Test",
        oscillators=[synth.OscillatorConfig(type="sine")],
        envelope=synth.EnvelopeConfig(attack=0.01, decay=0.01, sustain=0.9, release=0.01),
    )
    engine = synth.SynthEngine(preset)
    events = [(0.0, "note_on", 60, 100), (0.5, "note_off", 60, 0)]
    audio = engine.render_sequence(events, 1.0)
    assert len(audio) == synth.DEFAULT_SAMPLE_RATE
    assert audio.max() > 0.1


def test_preset_loading_and_listing():
    presets = synth.list_presets(PRESET_DIR)
    assert "ambient_pad" in presets
    preset = presets["ambient_pad"]
    assert preset.envelope.attack > 1.0


def test_waveform_preview_matches_duration():
    preset = synth.list_presets(PRESET_DIR)["bass_lead"]
    duration = 2.0
    sequence = synth.create_demo_sequence()
    audio, amplitude = synth.render_waveform_preview(preset, sequence, duration)
    assert len(audio) == len(amplitude) == int(duration * synth.DEFAULT_SAMPLE_RATE)
    assert amplitude.max() <= 1.0
