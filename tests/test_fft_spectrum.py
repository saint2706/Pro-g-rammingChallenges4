from pathlib import Path
from pathlib import Path
import wave

import numpy as np

from Emulation.FFTSpectrum.fft_spectrum import SpectrumConfig, chunk_to_fft


def load_samples(window_size: int) -> np.ndarray:
    wav_path = Path("Emulation/FFTSpectrum/test_tone.wav")
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
