"""Query the fingerprint database using a file or microphone capture."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

from shazam_clone import FingerprintDatabase, QueryService


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--database", required=True, help="Path to the fingerprint database (.json.gz)")
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("--file", help="Audio file to query")
    source.add_argument("--microphone", action="store_true", help="Record from the microphone instead of using a file")
    parser.add_argument("--duration", type=float, default=8.0, help="Duration (seconds) for file trimming or microphone capture")
    parser.add_argument("--minhash-threshold", type=float, default=0.3, help="Minimum MinHash similarity required to consider a track")
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv or sys.argv[1:])
    database_path = Path(args.database)
    if not database_path.exists():
        raise SystemExit(f"Database not found: {database_path}")

    database = FingerprintDatabase.load(database_path)
    service = QueryService(database, minhash_threshold=args.minhash_threshold)

    if args.microphone:
        print(f"Recording {args.duration:.1f}s from the microphone...")
        try:
            result = service.match_microphone(duration=args.duration)
        except RuntimeError as exc:  # sounddevice missing
            raise SystemExit(str(exc))
    else:
        audio_path = Path(args.file)
        if not audio_path.exists():
            raise SystemExit(f"Audio file not found: {audio_path}")
        print(f"Fingerprinting query clip from {audio_path}...")
        result = service.match_file(str(audio_path), duration=args.duration)

    if not result.track_id:
        print("No confident match found.")
        return 1

    print("Match found!")
    print(f"Track ID  : {result.track_id}")
    print(f"Title     : {result.title}")
    print(f"Artist    : {result.artist}")
    print(f"Offset    : {result.offset:.2f} s")
    print(f"Confidence: {result.confidence:.3f}")
    print(f"Votes     : {result.votes} / {result.total_hashes} hashes")
    print(f"Elapsed   : {result.elapsed:.2f} s")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
