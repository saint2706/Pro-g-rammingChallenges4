"""Utility helpers for creating Breakout JSON layouts.

Example usage to convert an ASCII plan into a JSON file:

    python level_editor.py layout.txt output.json \
        --name "Custom Stage" --ball-speed 360 --paddle-speed 440 \
        --brick-size 64 24

The ASCII file should use the same legend symbols as the shipping levels.
Characters not in the legend are treated as empty space. Update the
`DEFAULT_LEGEND` map to introduce new brick or power-up types.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Dict, List

DEFAULT_LEGEND: Dict[str, Dict[str, object]] = {
    "B": {"hits": 1, "score": 70},
    "S": {"hits": 2, "score": 120},
    "M": {"hits": 2, "score": 140, "powerup": "enlarge"},
    "P": {"hits": 1, "score": 90, "powerup": "multiball"},
    "L": {"hits": 1, "score": 110, "powerup": "life"},
    ".": {"empty": True},
}


def load_ascii_layout(path: Path) -> List[str]:
    rows = [line.rstrip("\n") for line in path.read_text(encoding="utf-8").splitlines()]
    width = max((len(row) for row in rows), default=0)
    return [row.ljust(width, ".") for row in rows]


def build_level_document(args: argparse.Namespace) -> Dict[str, object]:
    layout = load_ascii_layout(args.layout)
    legend = DEFAULT_LEGEND.copy()
    legend.update(json.loads(args.legend) if args.legend else {})
    return {
        "name": args.name,
        "ball_speed": args.ball_speed,
        "paddle_speed": args.paddle_speed,
        "brick_size": [args.brick_width, args.brick_height],
        "layout": layout,
        "legend": legend,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate Breakout JSON layout files")
    parser.add_argument("layout", type=Path, help="Path to ASCII layout file")
    parser.add_argument("output", type=Path, help="Destination JSON file")
    parser.add_argument("--name", default="Custom Level", help="Level name")
    parser.add_argument("--ball-speed", type=float, default=360, dest="ball_speed")
    parser.add_argument("--paddle-speed", type=float, default=440, dest="paddle_speed")
    parser.add_argument(
        "--brick-size",
        type=int,
        nargs=2,
        metavar=("WIDTH", "HEIGHT"),
        default=(64, 24),
        dest="brick_size",
    )
    parser.add_argument(
        "--legend",
        help="Optional JSON string merged into the default legend map",
    )
    args = parser.parse_args()
    args.brick_width, args.brick_height = args.brick_size
    return args


def main() -> None:
    args = parse_args()
    document = build_level_document(args)
    args.output.write_text(json.dumps(document, indent=2), encoding="utf-8")
    print(f"Wrote level '{document['name']}' to {args.output}")


if __name__ == "__main__":
    main()
