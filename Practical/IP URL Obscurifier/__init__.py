"""Convenience re-exports for the IP URL Obscurifier helpers."""

from __future__ import annotations

from pathlib import Path
import sys

if __package__:
    from .obscurifier import (  # type: ignore[import-not-found]
        DEFAULT_MIX_PATTERNS,
        IPv4VariantBundle,
        ObscurifierError,
        decode_ipv4,
        decode_url,
        encode_url,
        generate_ipv4_variants,
    )
else:  # pragma: no cover - import convenience for direct execution
    _PACKAGE_ROOT = Path(__file__).resolve().parent
    if str(_PACKAGE_ROOT) not in sys.path:
        sys.path.insert(0, str(_PACKAGE_ROOT))
    from obscurifier import (  # type: ignore[import-not-found]
        DEFAULT_MIX_PATTERNS,
        IPv4VariantBundle,
        ObscurifierError,
        decode_ipv4,
        decode_url,
        encode_url,
        generate_ipv4_variants,
    )

__all__ = [
    "DEFAULT_MIX_PATTERNS",
    "IPv4VariantBundle",
    "ObscurifierError",
    "decode_ipv4",
    "decode_url",
    "encode_url",
    "generate_ipv4_variants",
]
