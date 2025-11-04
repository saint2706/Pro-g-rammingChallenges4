from pathlib import Path
import wave

import numpy as np
import pytest

from challenges.Emulation.FFTSpectrum.fft_spectrum import SpectrumConfig, chunk_to_fft


def load_samples(window_size: int) -> np.ndarray:
    wav_path = Path("challenges/Emulation/FFTSpectrum/test_tone.wav")
    if not wav_path.exists() or wav_path.stat().st_size < 256:
        pytest.skip(
            "FFT sample tone is unavailable; fetch Git LFS assets before running."
        )
    with wave.open(str(wav_path), "rb") as wav_file:
        frames = wav_file.readframes(window_size)
    data = np.frombuffer(frames, dtype=np.int16)
    return data


def test_fft_shape_and_peak_frequency():
    window_size = 4096
    config = SpectrumConfig(sample_rate=44100, window_size=window_size)
    samples = load_samples(window_size)
    freqs, magnitude = chunk_to_fft(samples, config)

    assert freqs.shape[0] == window_size // 2 + 1
    assert magnitude.shape == freqs.shape

    dominant_frequency = float(freqs[int(np.argmax(magnitude))])
    assert abs(dominant_frequency - 440.0) < 5.0
    assert float(np.max(magnitude)) > 1e-3
