"""Tests for mv.py (music visualizer).

We only test synthetic tone + spectrogram logic to avoid external file
dependencies and to keep tests fast and deterministic.
If librosa is not installed the test will be skipped.
"""

from __future__ import annotations

import unittest

import mv


@unittest.skipUnless(
    mv._LIBROSA_AVAILABLE, "librosa not installed; skipping spectrogram tests"
)
class TestMusicVisualizer(unittest.TestCase):
    def test_synthetic_linear_spectrogram(self):
        cfg = mv.VisualizerConfig(
            synthetic=True,
            duration=0.25,
            freq=440.0,
            sample_rate=8000,
            n_fft=512,
            hop_length=128,
        )
        y, sr = mv.load_audio(cfg)
        self.assertAlmostEqual(len(y) / sr, cfg.duration, places=2)
        S_db, times, freqs = mv.compute_spectrogram(y, sr, cfg)
        self.assertEqual(S_db.ndim, 2)
        self.assertTrue(S_db.shape[0] > 0 and S_db.shape[1] > 0)
        # Frequency resolution check ~ sr/2 present
        self.assertAlmostEqual(freqs[-1], sr / 2, delta=sr * 0.01)

    def test_synthetic_mel_spectrogram(self):
        cfg = mv.VisualizerConfig(
            synthetic=True,
            mel=True,
            duration=0.25,
            freq=523.25,
            sample_rate=8000,
            n_fft=512,
            hop_length=128,
            n_mels=40,
        )
        y, sr = mv.load_audio(cfg)
        S_db, times, mel_freqs = mv.compute_mel_spectrogram(y, sr, cfg)
        self.assertEqual(S_db.shape[0], cfg.n_mels)
        self.assertEqual(len(mel_freqs), cfg.n_mels)
        # Ensure dynamic range clamping worked
        self.assertLessEqual(S_db.max() - S_db.min(), cfg.dynamic_range + 1e-6)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
