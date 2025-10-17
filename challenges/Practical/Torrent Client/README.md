# Torrent Client (Practical Series)

This folder contains a compact, well-documented BitTorrent client implementation focused on
single-file torrents. It is intentionally scoped for learning and experimentation while still
covering the key protocol components required to download data from a swarm.

## Supported Features

- **Metainfo parsing** – Minimal bencode decoder and `TorrentMetaInfo` helper capable of loading
  single-file `.torrent` metadata (info hash, piece hashes, piece length, announce URL).
- **Tracker communication** – `TrackerClient` issues HTTP(S) announce requests, parses peer lists,
  and surfaces scrape error messages when available.
- **BitTorrent handshake** – Correctly formats and validates the canonical handshake exchange,
  including reserved flag negotiation and peer ID matching.
- **Piece management** – Greedy-but-safe `PieceManager` that tracks availability, active requests,
  and verifies SHA-1 hashes before persisting data to disk.
- **Disk storage & resume** – Efficient file preallocation with a JSON-backed resume ledger so
  partial downloads are resumed without hash mismatches.
- **CLI progress UI** – `main.py` exposes a simple command line interface with a tqdm-powered
  progress bar, live piece counts, and optional verbose logging for protocol debugging.
- **Test harness** – Pytest-based tests rely on synthetic peers and a mock tracker so you can
  validate logic without touching the public swarm.

> **Scope:** The current code handles single-file torrents, one peer at a time. Extending to
> multi-file torrents and advanced peer selection strategies is left as an exercise.

## Quick Start

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r ../requirements.txt  # or install requests + tqdm + pytest manually
python main.py path/to/file.torrent --output downloads/
```

Use `--resume` (default) to continue partially downloaded files. Pass `--no-resume` to force a
fresh download.

## Folder Layout

```
torrent_client/
    __init__.py          # Convenience exports
    client.py            # High-level orchestration + CLI entry helpers
    metainfo.py          # Bencode decoding + TorrentMetaInfo wrapper
    peer_protocol.py     # Handshake helpers, PeerConnection, PieceManager
    storage.py           # File allocation, verification, and resume bookkeeping
    tracker.py           # HTTP tracker client
main.py                  # CLI entry point
README.md                # You are here
```

Tests live under `tests/` and can be executed with `pytest` from this directory.

## Limitations & Next Steps

- No DHT, PEX, uTP, or UDP tracker support (HTTP trackers + TCP peers only).
- Sequential piece picking is suboptimal for swarm fairness (rarest-first suggested for growth).
- Single-peer download loop: concurrency, choking/unchoking strategies, and multi-peer coordination
  are intentionally simplified.
- Multi-file torrents (with per-file offsets) require extending the storage layer.
- TLS certificate validation follows `requests` defaults; for self-hosted trackers configure CA
  trust stores appropriately.

Despite these guardrails, the implementation is intentionally "real enough" so you can inspect the
protocol at each stage, add instrumentation, and build more advanced features iteratively.
