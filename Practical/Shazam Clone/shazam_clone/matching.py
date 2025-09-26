"""Fingerprint matching and query utilities."""

from __future__ import annotations

import time
from collections import Counter, defaultdict
from dataclasses import dataclass
from typing import Dict, List, Optional, Sequence, Tuple

import numpy as np

from .audio_fingerprint import Fingerprint, FingerprintExtractor
from .database import FingerprintDatabase

try:  # Optional dependency for microphone capture
    import sounddevice as sd
except Exception:  # pragma: no cover - optional import guard
    sd = None


@dataclass
class QueryResult:
    """Description of a query outcome."""

    track_id: Optional[str]
    title: str
    artist: str
    confidence: float
    offset: float
    votes: int
    total_hashes: int
    elapsed: float


class QueryService:
    """High-level interface for querying the fingerprint database."""

    def __init__(self, database: FingerprintDatabase, *, minhash_threshold: float = 0.3) -> None:
        self.database = database
        self.extractor = FingerprintExtractor(database.config)
        self.minhash_threshold = minhash_threshold

    # ------------------------------------------------------------------
    # Fingerprint helpers
    # ------------------------------------------------------------------
    def _fingerprint_audio(self, audio: np.ndarray, sr: int) -> List[Fingerprint]:
        spectrogram, freqs, times = self.extractor.compute_spectrogram(audio, sr)
        peaks = self.extractor.find_peaks(spectrogram, freqs, times)
        return self.extractor.generate_fingerprints(peaks)

    def fingerprint_file(self, path: str, *, duration: Optional[float] = None) -> List[Fingerprint]:
        audio, sr = self.extractor.load_audio(path, duration=duration)
        return self._fingerprint_audio(audio, sr)

    # ------------------------------------------------------------------
    # Matching
    # ------------------------------------------------------------------
    def match_fingerprints(self, fingerprints: Sequence[Fingerprint]) -> QueryResult:
        start_time = time.perf_counter()
        total_hashes = len(fingerprints)
        if not fingerprints:
            return QueryResult(
                track_id=None,
                title="No match",
                artist="",
                confidence=0.0,
                offset=0.0,
                votes=0,
                total_hashes=0,
                elapsed=0.0,
            )

        query_hashes = [fingerprint.hash for fingerprint in fingerprints]
        query_signature = self.extractor.minhash_signature(query_hashes)
        candidates = self._candidate_tracks(query_signature)

        if not candidates:
            elapsed = time.perf_counter() - start_time
            return QueryResult(
                track_id=None,
                title="No match",
                artist="",
                confidence=0.0,
                offset=0.0,
                votes=0,
                total_hashes=total_hashes,
                elapsed=elapsed,
            )

        vote_table: Dict[str, Counter] = defaultdict(Counter)
        total_votes: Counter = Counter()

        for fingerprint in fingerprints:
            matches = self.database.lookup(fingerprint.hash)
            if not matches:
                continue
            for track_id, ref_offset in matches:
                if track_id not in candidates:
                    continue
                delta = round(ref_offset - fingerprint.time_offset, 2)
                vote_table[track_id][delta] += 1
                total_votes[track_id] += 1

        if not total_votes:
            elapsed = time.perf_counter() - start_time
            return QueryResult(
                track_id=None,
                title="No match",
                artist="",
                confidence=0.0,
                offset=0.0,
                votes=0,
                total_hashes=total_hashes,
                elapsed=elapsed,
            )

        best_track_id, best_votes = max(total_votes.items(), key=lambda item: item[1])
        offset_votes = vote_table[best_track_id]
        best_offset, offset_count = offset_votes.most_common(1)[0]
        confidence = offset_count / max(1, total_hashes)
        metadata = self.database.tracks.get(best_track_id)
        title = metadata.title if metadata else best_track_id
        artist = metadata.artist if metadata else ""
        elapsed = time.perf_counter() - start_time

        return QueryResult(
            track_id=best_track_id,
            title=title,
            artist=artist,
            confidence=confidence,
            offset=best_offset,
            votes=best_votes,
            total_hashes=total_hashes,
            elapsed=elapsed,
        )

    def _candidate_tracks(self, query_signature: Sequence[int]) -> Dict[str, float]:
        """Return tracks whose MinHash similarity surpasses the threshold."""

        candidates: Dict[str, float] = {}
        signature = np.array(query_signature, dtype=np.uint64)
        for track in self.database.iter_tracks():
            if not track.minhash:
                continue
            reference = np.array(track.minhash, dtype=np.uint64)
            limit = min(len(reference), len(signature))
            if limit == 0:
                continue
            matches = int(np.sum(reference[:limit] == signature[:limit]))
            similarity = matches / float(limit)
            if similarity >= self.minhash_threshold:
                candidates[track.track_id] = similarity
        return candidates

    # ------------------------------------------------------------------
    # Capture helpers
    # ------------------------------------------------------------------
    def record_microphone(self, *, duration: float = 5.0) -> Tuple[np.ndarray, int]:
        if sd is None:
            raise RuntimeError("sounddevice is not installed; microphone capture is unavailable.")
        audio = sd.rec(int(duration * self.database.config.sample_rate), samplerate=self.database.config.sample_rate, channels=1)
        sd.wait()
        return audio.flatten(), self.database.config.sample_rate

    def match_microphone(self, *, duration: float = 5.0) -> QueryResult:
        audio, sr = self.record_microphone(duration=duration)
        fingerprints = self._fingerprint_audio(audio, sr)
        return self.match_fingerprints(fingerprints)

    def match_file(self, path: str, *, duration: Optional[float] = None) -> QueryResult:
        fingerprints = self.fingerprint_file(path, duration=duration)
        return self.match_fingerprints(fingerprints)


__all__ = ["QueryResult", "QueryService"]
