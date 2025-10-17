"""Benchmark fingerprint matching accuracy against a labeled manifest."""

from __future__ import annotations

import argparse
import json
import statistics
import sys
from pathlib import Path
from typing import Dict, Iterator, List

from shazam_clone import FingerprintDatabase, QueryService


def iter_queries(path: Path) -> Iterator[Dict[str, str]]:
    payload = json.loads(path.read_text())
    if isinstance(payload, dict):
        payload = payload.get("queries", [])
    if not isinstance(payload, list):
        raise ValueError(
            "Benchmark manifest must be a list or contain a 'queries' list."
        )
    for entry in payload:
        if not isinstance(entry, dict):
            raise ValueError("Each query entry must be a JSON object.")
        yield entry


def run_benchmark(args: argparse.Namespace) -> Dict[str, object]:
    database = FingerprintDatabase.load(args.database)
    service = QueryService(database, minhash_threshold=args.minhash_threshold)

    results: List[Dict[str, object]] = []
    accuracies: List[int] = []
    elapsed_times: List[float] = []

    for entry in iter_queries(Path(args.manifest)):
        query_path = entry.get("path")
        expected = entry.get("track_id")
        if not query_path or not expected:
            print("Skipping query missing 'path' or 'track_id'", file=sys.stderr)
            continue
        clip = Path(query_path)
        if not clip.exists():
            print(f"Skipping missing clip: {clip}", file=sys.stderr)
            continue
        result = service.match_file(str(clip), duration=args.duration)
        is_correct = int(result.track_id == expected)
        accuracies.append(is_correct)
        elapsed_times.append(result.elapsed)
        results.append(
            {
                "query": str(clip),
                "expected": expected,
                "predicted": result.track_id,
                "confidence": result.confidence,
                "offset": result.offset,
                "votes": result.votes,
                "hashes": result.total_hashes,
                "elapsed": result.elapsed,
                "correct": bool(is_correct),
            }
        )
        status = "✓" if is_correct else "✗"
        print(
            f"{status} {clip.name}: expected={expected}, predicted={result.track_id}, confidence={result.confidence:.3f}"
        )

    summary = {
        "total": len(results),
        "correct": sum(accuracies),
        "accuracy": (sum(accuracies) / len(results)) if results else 0.0,
        "median_elapsed": statistics.median(elapsed_times) if elapsed_times else 0.0,
        "mean_elapsed": statistics.mean(elapsed_times) if elapsed_times else 0.0,
        "queries": results,
    }
    return summary


def parse_args(argv: List[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--database", required=True, help="Fingerprint database path")
    parser.add_argument(
        "--manifest",
        required=True,
        help="JSON manifest with query clips and expected IDs",
    )
    parser.add_argument(
        "--duration",
        type=float,
        default=8.0,
        help="Clip duration to consider for each query",
    )
    parser.add_argument(
        "--minhash-threshold",
        type=float,
        default=0.3,
        help="MinHash similarity threshold for candidate filtering",
    )
    parser.add_argument(
        "--output", type=str, help="Optional path to write benchmark results as JSON"
    )
    return parser.parse_args(argv)


def main(argv: List[str] | None = None) -> None:
    args = parse_args(argv or sys.argv[1:])
    summary = run_benchmark(args)
    print("--- Summary ---")
    print(f"Total queries : {summary['total']}")
    print(f"Correct       : {summary['correct']}")
    print(f"Accuracy      : {summary['accuracy']:.3f}")
    print(f"Median elapsed: {summary['median_elapsed']:.2f} s")
    print(f"Mean elapsed  : {summary['mean_elapsed']:.2f} s")

    if args.output:
        Path(args.output).write_text(json.dumps(summary, indent=2))
        print(f"Detailed results saved to {args.output}")


if __name__ == "__main__":
    main()
