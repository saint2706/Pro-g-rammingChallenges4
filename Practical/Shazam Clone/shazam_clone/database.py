"""Fingerprint database helpers."""

from __future__ import annotations

import gzip
import json
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, Iterator, List, MutableMapping, Optional, Tuple

from .audio_fingerprint import FingerprintConfig, FingerprintExtractor


@dataclass
class TrackMetadata:
    """Lightweight container describing a reference track."""

    track_id: str
    title: str
    artist: str
    duration: float
    path: str
    minhash: List[int]
    extra: Dict[str, str]

    def to_json(self) -> Dict[str, object]:
        payload: Dict[str, object] = {
            "title": self.title,
            "artist": self.artist,
            "duration": self.duration,
            "path": self.path,
            "minhash": self.minhash,
        }
        payload.update(self.extra)
        return payload

    @classmethod
    def from_json(
        cls, track_id: str, payload: MutableMapping[str, object]
    ) -> "TrackMetadata":
        extra_keys = {
            key
            for key in payload.keys()
            if key not in {"title", "artist", "duration", "path", "minhash"}
        }
        extra = {key: str(payload[key]) for key in extra_keys}
        return cls(
            track_id=track_id,
            title=str(payload.get("title", track_id)),
            artist=str(payload.get("artist", "")),
            duration=float(payload.get("duration", 0.0)),
            path=str(payload.get("path", "")),
            minhash=[int(value) for value in payload.get("minhash", [])],
            extra=extra,
        )


class FingerprintDatabase:
    """Stores track metadata plus fingerprint hash buckets."""

    version: int = 1

    def __init__(self, config: FingerprintConfig | None = None) -> None:
        self.config = config or FingerprintConfig()
        self.extractor = FingerprintExtractor(self.config)
        self.tracks: Dict[str, TrackMetadata] = {}
        self.hash_buckets: Dict[str, List[Tuple[str, float]]] = defaultdict(list)

    # ------------------------------------------------------------------
    # Ingestion
    # ------------------------------------------------------------------
    def ingest_file(
        self,
        file_path: str | Path,
        *,
        track_id: str | None = None,
        title: str | None = None,
        artist: str | None = None,
        extra: Optional[Dict[str, str]] = None,
    ) -> TrackMetadata:
        """Load an audio file and add its fingerprints to the database."""

        path = Path(file_path)
        if not track_id:
            track_id = path.stem
        audio, sr = self.extractor.load_audio(str(path))
        duration = len(audio) / sr if sr else 0.0
        spectrogram, freqs, times = self.extractor.compute_spectrogram(audio, sr)
        peaks = self.extractor.find_peaks(spectrogram, freqs, times)
        fingerprints = self.extractor.generate_fingerprints(peaks)
        minhash = self.extractor.minhash_signature(fp.hash for fp in fingerprints)

        metadata = TrackMetadata(
            track_id=track_id,
            title=title or track_id,
            artist=artist or "",
            duration=duration,
            path=str(path),
            minhash=minhash,
            extra=extra or {},
        )

        self.tracks[track_id] = metadata
        for fingerprint in fingerprints:
            self.hash_buckets[str(fingerprint.hash)].append(
                (track_id, fingerprint.time_offset)
            )
        return metadata

    def build_from_paths(self, paths: Iterable[str | Path]) -> None:
        """Ingest a collection of audio file paths."""

        for path in paths:
            self.ingest_file(path)

    # ------------------------------------------------------------------
    # Persistence
    # ------------------------------------------------------------------
    def to_dict(self) -> Dict[str, object]:
        return {
            "version": self.version,
            "config": {
                "sample_rate": self.config.sample_rate,
                "n_fft": self.config.n_fft,
                "hop_length": self.config.hop_length,
                "fan_value": self.config.fan_value,
                "min_hashes": self.config.minhash_size,
                "peak_neighborhood_freq": self.config.peak_neighborhood_freq,
                "peak_neighborhood_time": self.config.peak_neighborhood_time,
                "amplitude_threshold": self.config.amplitude_threshold,
                "min_time_delta": self.config.min_time_delta,
                "max_time_delta": self.config.max_time_delta,
            },
            "tracks": {
                track_id: meta.to_json() for track_id, meta in self.tracks.items()
            },
            "hash_buckets": {
                bucket: pairs for bucket, pairs in self.hash_buckets.items()
            },
        }

    def save(self, path: str | Path) -> None:
        payload = self.to_dict()
        path = Path(path)
        with gzip.open(path, "wt", encoding="utf-8") as handle:
            json.dump(payload, handle)

    # ------------------------------------------------------------------
    # Loading
    # ------------------------------------------------------------------
    @classmethod
    def load(cls, path: str | Path) -> "FingerprintDatabase":
        with gzip.open(path, "rt", encoding="utf-8") as handle:
            payload = json.load(handle)
        config_payload = payload.get("config", {})
        defaults = FingerprintConfig()
        config = FingerprintConfig(
            sample_rate=int(config_payload.get("sample_rate", defaults.sample_rate)),
            n_fft=int(config_payload.get("n_fft", defaults.n_fft)),
            hop_length=int(config_payload.get("hop_length", defaults.hop_length)),
            fan_value=int(config_payload.get("fan_value", defaults.fan_value)),
            minhash_size=int(config_payload.get("min_hashes", defaults.minhash_size)),
            peak_neighborhood_freq=int(
                config_payload.get(
                    "peak_neighborhood_freq", defaults.peak_neighborhood_freq
                )
            ),
            peak_neighborhood_time=int(
                config_payload.get(
                    "peak_neighborhood_time", defaults.peak_neighborhood_time
                )
            ),
            amplitude_threshold=float(
                config_payload.get("amplitude_threshold", defaults.amplitude_threshold)
            ),
            min_time_delta=float(
                config_payload.get("min_time_delta", defaults.min_time_delta)
            ),
            max_time_delta=float(
                config_payload.get("max_time_delta", defaults.max_time_delta)
            ),
        )
        database = cls(config=config)
        tracks_payload = payload.get("tracks", {})
        for track_id, meta in tracks_payload.items():
            database.tracks[track_id] = TrackMetadata.from_json(track_id, meta)
        hash_buckets_payload = payload.get("hash_buckets", {})
        for bucket, pairs in hash_buckets_payload.items():
            database.hash_buckets[bucket] = [
                (str(track), float(offset)) for track, offset in pairs
            ]
        return database

    # ------------------------------------------------------------------
    # Query helpers
    # ------------------------------------------------------------------
    def lookup(self, fingerprint_hash: int) -> List[Tuple[str, float]]:
        return self.hash_buckets.get(str(fingerprint_hash), [])

    def iter_tracks(self) -> Iterator[TrackMetadata]:
        return iter(self.tracks.values())

    def __len__(self) -> int:  # pragma: no cover - convenience only
        return len(self.tracks)


__all__ = ["FingerprintDatabase", "TrackMetadata"]
