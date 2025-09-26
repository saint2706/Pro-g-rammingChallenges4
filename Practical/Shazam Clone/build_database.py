"""CLI for building a fingerprint database from reference audio."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Dict, Iterator, List

from shazam_clone import FingerprintConfig, FingerprintDatabase

SUPPORTED_EXTENSIONS = {".wav", ".mp3", ".flac", ".m4a", ".ogg"}


def iter_audio_files(directory: Path) -> Iterator[Path]:
    for path in directory.rglob("*"):
        if path.suffix.lower() in SUPPORTED_EXTENSIONS:
            yield path


def iter_manifest(path: Path) -> Iterator[Dict[str, str]]:
    data = json.loads(path.read_text())
    if isinstance(data, dict):
        data = data.get("tracks", [])
    if not isinstance(data, list):
        raise ValueError("Manifest JSON must be a list or contain a 'tracks' list.")
    for entry in data:
        if not isinstance(entry, dict):
            raise ValueError("Each manifest entry must be a JSON object.")
        yield entry


def build_database(args: argparse.Namespace) -> None:
    config = FingerprintConfig(
        sample_rate=args.sample_rate,
        n_fft=args.n_fft,
        hop_length=args.hop_length,
        fan_value=args.fan_value,
        minhash_size=args.minhash_size,
        peak_neighborhood_freq=args.peak_neighborhood_freq,
        peak_neighborhood_time=args.peak_neighborhood_time,
        amplitude_threshold=args.amplitude_threshold,
        min_time_delta=args.min_time_delta,
        max_time_delta=args.max_time_delta,
    )

    database = FingerprintDatabase(config=config)

    if args.manifest:
        entries = iter_manifest(Path(args.manifest))
    elif args.audio_dir:
        directory = Path(args.audio_dir)
        entries = ({"path": str(path)} for path in iter_audio_files(directory))
    else:
        raise SystemExit("Provide either --audio-dir or --manifest.")

    for entry in entries:
        path = entry.get("path")
        if not path:
            print("Skipping manifest entry without 'path'", file=sys.stderr)
            continue
        file_path = Path(path)
        if not file_path.exists():
            print(f"Skipping missing file: {path}", file=sys.stderr)
            continue
        track_id = entry.get("track_id") or file_path.stem
        title = entry.get("title") or track_id
        artist = entry.get("artist") or ""
        extra = {key: str(value) for key, value in entry.items() if key not in {"track_id", "path", "title", "artist"}}
        print(f"Fingerprinting {track_id} ({file_path})...")
        database.ingest_file(file_path, track_id=track_id, title=title, artist=artist, extra=extra)

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    database.save(output_path)
    print(f"Saved database with {len(database)} tracks to {output_path}")


def parse_args(argv: List[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    input_group = parser.add_mutually_exclusive_group(required=True)
    input_group.add_argument("--audio-dir", type=str, help="Directory to recursively scan for audio files")
    input_group.add_argument("--manifest", type=str, help="JSON manifest listing tracks to fingerprint")
    parser.add_argument("--output", type=str, required=True, help="Destination gzip JSON path for the fingerprint database")
    parser.add_argument("--sample-rate", type=int, default=22_050)
    parser.add_argument("--n-fft", type=int, default=4_096)
    parser.add_argument("--hop-length", type=int, default=512)
    parser.add_argument("--fan-value", type=int, default=15)
    parser.add_argument("--minhash-size", type=int, default=32)
    parser.add_argument("--peak-neighborhood-freq", type=int, default=20)
    parser.add_argument("--peak-neighborhood-time", type=int, default=20)
    parser.add_argument("--amplitude-threshold", type=float, default=-60.0)
    parser.add_argument("--min-time-delta", type=float, default=0.5)
    parser.add_argument("--max-time-delta", type=float, default=5.0)
    return parser.parse_args(argv)


def main(argv: List[str] | None = None) -> None:
    args = parse_args(argv or sys.argv[1:])
    build_database(args)


if __name__ == "__main__":
    main()
