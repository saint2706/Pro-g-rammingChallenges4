"""Audio fingerprint generation utilities."""

from __future__ import annotations

import gzip
import hashlib
import io
from dataclasses import dataclass
from typing import Iterable, List, Sequence, Tuple

import librosa
import numpy as np
from scipy.ndimage import binary_erosion, maximum_filter


@dataclass(frozen=True)
class FingerprintConfig:
    """Parameters controlling the fingerprint extraction pipeline."""

    sample_rate: int = 22_050
    n_fft: int = 4_096
    hop_length: int = 512
    peak_neighborhood_freq: int = 20
    peak_neighborhood_time: int = 20
    amplitude_threshold: float = -60.0
    fan_value: int = 15
    min_time_delta: float = 0.5
    max_time_delta: float = 5.0
    minhash_size: int = 32
    minhash_seed: int = 1337

    def neighborhood(self) -> Tuple[int, int]:
        return self.peak_neighborhood_freq, self.peak_neighborhood_time


@dataclass(frozen=True)
class SpectralPeak:
    """Represents a salient spectrogram peak."""

    time: float
    frequency: float
    magnitude: float


@dataclass(frozen=True)
class Fingerprint:
    """Fingerprint hash anchored at a given time offset."""

    hash: int
    time_offset: float


class FingerprintExtractor:
    """High-level helper for building fingerprints from audio."""

    def __init__(self, config: FingerprintConfig | None = None) -> None:
        self.config = config or FingerprintConfig()
        rng = np.random.default_rng(self.config.minhash_seed)
        self._minhash_seeds = rng.integers(low=1, high=np.iinfo(np.uint64).max, size=self.config.minhash_size, dtype=np.uint64)

    # ------------------------------------------------------------------
    # Loading / preprocessing
    # ------------------------------------------------------------------
    def load_audio(self, path: str, *, duration: float | None = None, offset: float = 0.0) -> Tuple[np.ndarray, int]:
        """Load audio from disk, returning mono samples and the sampling rate."""

        audio, sr = librosa.load(path, sr=self.config.sample_rate, mono=True, duration=duration, offset=offset)
        return audio, sr

    def compute_spectrogram(self, audio: np.ndarray, sr: int) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """Compute a magnitude spectrogram in decibel scale."""

        stft = librosa.stft(audio, n_fft=self.config.n_fft, hop_length=self.config.hop_length, window="hann")
        magnitude = np.abs(stft)
        db = librosa.amplitude_to_db(magnitude, ref=np.max)
        freqs = librosa.fft_frequencies(sr=sr, n_fft=self.config.n_fft)
        times = librosa.frames_to_time(np.arange(db.shape[1]), sr=sr, hop_length=self.config.hop_length)
        return db, freqs, times

    # ------------------------------------------------------------------
    # Peak detection
    # ------------------------------------------------------------------
    def find_peaks(self, spectrogram_db: np.ndarray, freqs: np.ndarray, times: np.ndarray) -> List[SpectralPeak]:
        """Return the list of local maxima above the amplitude threshold."""

        neighborhood_size = (self.config.peak_neighborhood_freq, self.config.peak_neighborhood_time)
        footprint = np.ones(neighborhood_size, dtype=bool)
        local_max = maximum_filter(spectrogram_db, footprint=footprint) == spectrogram_db

        background = spectrogram_db < self.config.amplitude_threshold
        eroded_background = binary_erosion(background, structure=footprint, border_value=1)
        detected = local_max & ~eroded_background

        peak_indices = np.argwhere(detected)
        peaks: List[SpectralPeak] = []
        for freq_idx, time_idx in peak_indices:
            peaks.append(
                SpectralPeak(
                    time=float(times[time_idx]),
                    frequency=float(freqs[freq_idx]),
                    magnitude=float(spectrogram_db[freq_idx, time_idx]),
                )
            )
        peaks.sort(key=lambda peak: peak.time)
        return peaks

    # ------------------------------------------------------------------
    # Fingerprint hashing
    # ------------------------------------------------------------------
    def generate_fingerprints(self, peaks: Sequence[SpectralPeak]) -> List[Fingerprint]:
        """Generate hashed fingerprints from peak pairs."""

        fingerprints: List[Fingerprint] = []
        fan_value = self.config.fan_value
        min_dt = self.config.min_time_delta
        max_dt = self.config.max_time_delta

        for anchor_idx, anchor in enumerate(peaks):
            for target in peaks[anchor_idx + 1 : anchor_idx + 1 + fan_value]:
                time_delta = target.time - anchor.time
                if time_delta < min_dt or time_delta > max_dt:
                    continue
                freq_anchor = int(anchor.frequency)
                freq_target = int(target.frequency)

                hash_input = f"{freq_anchor}|{freq_target}|{time_delta:.3f}".encode()
                hash_digest = hashlib.blake2b(hash_input, digest_size=8).digest()
                hash_int = int.from_bytes(hash_digest, byteorder="big", signed=False)
                fingerprints.append(Fingerprint(hash=hash_int, time_offset=anchor.time))
        return fingerprints

    # ------------------------------------------------------------------
    # MinHash utilities
    # ------------------------------------------------------------------
    def minhash_signature(self, fingerprint_hashes: Iterable[int]) -> List[int]:
        """Compute a MinHash signature for a collection of fingerprint hashes."""

        signature = np.full(shape=self.config.minhash_size, fill_value=np.iinfo(np.uint64).max, dtype=np.uint64)
        for fp_hash in fingerprint_hashes:
            value = np.uint64(fp_hash)
            for idx, seed in enumerate(self._minhash_seeds):
                candidate = seed ^ value
                if candidate < signature[idx]:
                    signature[idx] = candidate
        return signature.astype(np.uint64).tolist()

    # ------------------------------------------------------------------
    # Serialization helpers for fingerprints (optional convenience)
    # ------------------------------------------------------------------
    def serialize_fingerprints(self, fingerprints: Sequence[Fingerprint]) -> bytes:
        """Serialize fingerprints to a compressed binary blob."""

        buffer = io.BytesIO()
        dtype = np.dtype([("hash", np.uint64), ("time", np.float32)])
        with gzip.GzipFile(fileobj=buffer, mode="wb") as gz_file:
            array = np.array([(fp.hash, fp.time_offset) for fp in fingerprints], dtype=dtype)
            gz_file.write(array.tobytes())
        return buffer.getvalue()

    def deserialize_fingerprints(self, payload: bytes) -> List[Fingerprint]:
        """Inverse of :meth:`serialize_fingerprints`. Useful for caching."""

        dtype = np.dtype([("hash", np.uint64), ("time", np.float32)])
        with gzip.GzipFile(fileobj=io.BytesIO(payload), mode="rb") as gz_file:
            data = gz_file.read()
        array = np.frombuffer(data, dtype=dtype)
        return [Fingerprint(hash=int(item["hash"]), time_offset=float(item["time"])) for item in array]


__all__ = [
    "Fingerprint",
    "FingerprintConfig",
    "FingerprintExtractor",
    "SpectralPeak",
]
