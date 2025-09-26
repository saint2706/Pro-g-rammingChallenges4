"""Utility package for the Shazam Clone fingerprinting demo."""

from .audio_fingerprint import (
    Fingerprint,
    FingerprintConfig,
    FingerprintExtractor,
    SpectralPeak,
)
from .database import FingerprintDatabase
from .matching import QueryResult, QueryService

__all__ = [
    "Fingerprint",
    "FingerprintConfig",
    "FingerprintExtractor",
    "FingerprintDatabase",
    "QueryResult",
    "QueryService",
    "SpectralPeak",
]
